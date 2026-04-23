/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#include "jit/LoongArchMinimalJit.h"

#include <string.h>

#include "mozilla/ArrayUtils.h"

#include "jit/ProcessExecutableMemory.h"
#include "js/Vector.h"
#include "js/Value.h"
#include "jsopcode.h"
#include "jsscript.h"
#include "vm/Interpreter.h"
#include "vm/NativeObject-inl.h"
#include "vm/Stack.h"

using namespace js;

#if defined(__loongarch64)

namespace js {
namespace jit {

namespace {

enum LoongArchReg : uint8_t {
    zero = 0,
    a0 = 4,
    a1 = 5,
    a2 = 6,
    a3 = 7,
    a4 = 8,
    a5 = 9,
    a6 = 10,
    a7 = 11,
    t0 = 12,
    t1 = 13,
    t2 = 14,
    t3 = 15,
    t4 = 16,
    t5 = 17,
    t6 = 18,
    t7 = 19,
    t8 = 20
};

static constexpr LoongArchReg ArgRegs[] = { a0, a1 };
static constexpr LoongArchReg LocalRegs[] = { a3, a4, a5, a6, a7 };
static constexpr LoongArchReg StackRegs[] = { t0, t1, t2, t3, t4, t5, t6 };
static constexpr LoongArchReg WideScratch = t7;
static constexpr LoongArchReg NarrowScratch = t8;

enum class MinimalValueKind : uint32_t {
    Int32 = 0,
    Boolean = 1,
    Undefined = 2,
    Null = 3
};

struct MinimalJitResult
{
    uint32_t payload;
    MinimalValueKind kind;
};

static_assert(sizeof(MinimalJitResult) == 8,
              "Minimal JIT results must stay ABI-stable");

static inline bool
IsNumericKind(MinimalValueKind kind)
{
    return kind == MinimalValueKind::Int32 || kind == MinimalValueKind::Boolean;
}

using TinyLoongArchJitCode = bool (*)(int32_t arg0, int32_t arg1, MinimalJitResult* out);

enum MinimalFastPathKind : uint8_t {
    MinimalFastPathUninitialized = 0,
    MinimalFastPathNone = 1,
    MinimalFastPathGetter = 2,
    MinimalFastPathSetter = 3
};

static inline uint32_t
EncodeThreeReg(uint32_t opcode, LoongArchReg rd, LoongArchReg rj, LoongArchReg rk)
{
    return opcode | uint32_t(rd) | (uint32_t(rj) << 5) | (uint32_t(rk) << 10);
}

static inline uint32_t
EncodeImm12(uint32_t opcode, LoongArchReg rd, LoongArchReg rj, int32_t imm)
{
    MOZ_ASSERT(imm >= -2048 && imm <= 2047);
    return opcode | uint32_t(rd) | (uint32_t(rj) << 5) | ((uint32_t(imm) & 0xfff) << 10);
}

static inline uint32_t
EncodeUnsignedImm12(uint32_t opcode, LoongArchReg rd, LoongArchReg rj, uint32_t imm)
{
    MOZ_ASSERT(imm <= 0xfff);
    return opcode | uint32_t(rd) | (uint32_t(rj) << 5) | (imm << 10);
}

static inline uint32_t
EncodeImm20(uint32_t opcode, LoongArchReg rd, int32_t imm)
{
    MOZ_ASSERT(imm >= -(1 << 19) && imm < (1 << 19));
    return opcode | uint32_t(rd) | ((uint32_t(imm) & 0xfffff) << 5);
}

static inline uint32_t
EncodeBranch16(uint32_t opcode, LoongArchReg rj, LoongArchReg rd, int32_t wordOffset)
{
    MOZ_ASSERT(wordOffset >= -(1 << 15) && wordOffset < (1 << 15));
    return opcode | uint32_t(rd) | (uint32_t(rj) << 5) |
           ((uint32_t(wordOffset) & 0xffff) << 10);
}

class AutoExecutableMemory
{
    uint8_t* bytes_;
    size_t size_;

  public:
    AutoExecutableMemory()
      : bytes_(nullptr),
        size_(0)
    { }

    ~AutoExecutableMemory() {
        if (bytes_)
            DeallocateExecutableMemory(bytes_, size_);
    }

    AutoExecutableMemory(const AutoExecutableMemory&) = delete;
    void operator=(const AutoExecutableMemory&) = delete;

    bool allocate(size_t size) {
        bytes_ = static_cast<uint8_t*>(AllocateExecutableMemory(size, ProtectionSetting::Writable));
        if (!bytes_)
            return false;
        size_ = size;
        return true;
    }

    uint8_t* bytes() const { return bytes_; }

    bool makeExecutable() {
        if (!ReprotectRegion(bytes_, size_, ProtectionSetting::Executable))
            return false;
        __builtin___clear_cache(reinterpret_cast<char*>(bytes_),
                                reinterpret_cast<char*>(bytes_ + size_));
        return true;
    }

    uint8_t* release() {
        uint8_t* result = bytes_;
        bytes_ = nullptr;
        size_ = 0;
        return result;
    }
};

enum class BranchKind {
    Always,
    IfTrue,
    IfFalse
};

struct BranchPatch
{
    size_t wordOffset;
    uint32_t targetPcOffset;
    BranchKind kind;
    LoongArchReg reg;
};

class MinimalLoongArchCompiler
{
    struct StackValue
    {
        LoongArchReg reg;
        MinimalValueKind kind;
    };

    JSScript* script_;
    uint32_t words_[512];
    size_t wordCount_;
    size_t failPatches_[128];
    size_t failPatchCount_;
    StackValue stack_[mozilla::ArrayLength(StackRegs)];
    MinimalValueKind argKinds_[mozilla::ArrayLength(ArgRegs)];
    MinimalValueKind localKinds_[mozilla::ArrayLength(LocalRegs)];
    size_t stackDepth_;
    bool sawReturn_;
    Vector<int32_t, 0, SystemAllocPolicy> pcToWord_;
    Vector<BranchPatch, 0, SystemAllocPolicy> branchPatches_;

    bool emit(uint32_t word) {
        if (wordCount_ >= mozilla::ArrayLength(words_))
            return false;
        words_[wordCount_++] = word;
        return true;
    }

    bool markBytecode(jsbytecode* pc) {
        uint32_t offset = uint32_t(pc - script_->code());
        MOZ_ASSERT(offset < pcToWord_.length());
        pcToWord_[offset] = int32_t(wordCount_);
        return true;
    }

    bool emitMove(LoongArchReg dst, LoongArchReg src) {
        if (dst == src)
            return true;
        return emit(EncodeImm12(0x02c00000, dst, src, 0));
    }

    bool emitLoadImm32(LoongArchReg dst, int32_t value) {
        if (value >= -2048 && value <= 2047)
            return emit(EncodeImm12(0x02800000, dst, zero, value));

        uint32_t bits = uint32_t(value);
        int32_t upper20 = int32_t((bits + 0x800) >> 12);
        if (!emit(EncodeImm20(0x14000000, dst, upper20)))
            return false;

        uint32_t low12 = bits & 0xfff;
        if (!low12)
            return true;
        return emit(EncodeUnsignedImm12(0x03800000, dst, dst, low12));
    }

    bool emitXori(LoongArchReg dst, LoongArchReg src, uint32_t imm) {
        return emit(EncodeUnsignedImm12(0x03c00000, dst, src, imm));
    }

    bool emitSltui(LoongArchReg dst, LoongArchReg src, uint32_t imm) {
        return emit(EncodeUnsignedImm12(0x02400000, dst, src, imm));
    }

    bool emitFailureBranch() {
        if (failPatchCount_ >= mozilla::ArrayLength(failPatches_))
            return false;
        failPatches_[failPatchCount_++] = wordCount_;
        return emit(EncodeBranch16(0x5c000000, WideScratch, NarrowScratch, 0));
    }

    bool emitCheckedBinary(JSOp op, LoongArchReg lhs, LoongArchReg rhs, LoongArchReg out) {
        MOZ_ASSERT(op == JSOP_ADD || op == JSOP_SUB);

        uint32_t wideOpcode = (op == JSOP_ADD) ? 0x00108000 : 0x00118000;
        uint32_t narrowOpcode = (op == JSOP_ADD) ? 0x00100000 : 0x00110000;

        if (!emit(EncodeThreeReg(wideOpcode, WideScratch, lhs, rhs)))
            return false;
        if (!emit(EncodeThreeReg(narrowOpcode, NarrowScratch, lhs, rhs)))
            return false;
        if (!emitFailureBranch())
            return false;
        return emitMove(out, NarrowScratch);
    }

    bool emitCheckedIncDec(JSOp op, LoongArchReg srcDst) {
        MOZ_ASSERT(op == JSOP_INC || op == JSOP_DEC);
        int32_t imm = (op == JSOP_INC) ? 1 : -1;
        if (!emit(EncodeImm12(0x02c00000, WideScratch, srcDst, imm)))
            return false;
        if (!emit(EncodeImm12(0x02800000, NarrowScratch, srcDst, imm)))
            return false;
        if (!emitFailureBranch())
            return false;
        return emitMove(srcDst, NarrowScratch);
    }

    bool emitCompare(JSOp op, LoongArchReg lhs, LoongArchReg rhs, LoongArchReg out) {
        switch (op) {
          case JSOP_LT:
            return emit(EncodeThreeReg(0x00120000, out, lhs, rhs));
          case JSOP_LE:
            if (!emit(EncodeThreeReg(0x00120000, out, rhs, lhs)))
                return false;
            return emitXori(out, out, 1);
          case JSOP_GT:
            return emit(EncodeThreeReg(0x00120000, out, rhs, lhs));
          case JSOP_GE:
            if (!emit(EncodeThreeReg(0x00120000, out, lhs, rhs)))
                return false;
            return emitXori(out, out, 1);
          default:
            break;
        }

        MOZ_CRASH("bad comparison opcode");
    }

    bool emitEq(LoongArchReg lhs, LoongArchReg rhs, LoongArchReg out, bool invert) {
        if (!emit(EncodeThreeReg(0x00158000, NarrowScratch, lhs, rhs)))
            return false;
        if (invert)
            return emit(EncodeThreeReg(0x00128000, out, zero, NarrowScratch));
        return emitSltui(out, NarrowScratch, 1);
    }

    bool emitBitBinary(uint32_t opcode, LoongArchReg lhs, LoongArchReg rhs, LoongArchReg out) {
        return emit(EncodeThreeReg(opcode, out, lhs, rhs));
    }

    bool emitBitNot(LoongArchReg src, LoongArchReg out) {
        return emit(EncodeThreeReg(0x00140000, out, src, zero));
    }

    bool emitNot(LoongArchReg src, LoongArchReg out) {
        return emitSltui(out, src, 1);
    }

    bool emitNeg(LoongArchReg src, LoongArchReg out) {
        if (!emit(EncodeThreeReg(0x00110000, out, zero, src)))
            return false;
        if (!emitEq(out, src, WideScratch, false))
            return false;
        if (!emitMove(NarrowScratch, zero))
            return false;
        return emitFailureBranch();
    }

    bool emitShift(JSOp op, LoongArchReg lhs, LoongArchReg rhs, LoongArchReg out) {
        switch (op) {
          case JSOP_LSH:
            return emit(EncodeThreeReg(0x00170000, out, lhs, rhs));
          case JSOP_RSH:
            return emit(EncodeThreeReg(0x00180000, out, lhs, rhs));
          case JSOP_URSH:
            if (!emit(EncodeThreeReg(0x00178000, out, lhs, rhs)))
                return false;
            if (!emit(EncodeThreeReg(0x00120000, WideScratch, out, zero)))
                return false;
            if (!emitMove(NarrowScratch, zero))
                return false;
            return emitFailureBranch();
          default:
            break;
        }

        MOZ_CRASH("bad shift opcode");
    }

    bool emitBranch(BranchKind kind, LoongArchReg reg, uint32_t targetPcOffset) {
        BranchPatch patch = { wordCount_, targetPcOffset, kind, reg };
        if (!branchPatches_.append(patch))
            return false;

        switch (kind) {
          case BranchKind::Always:
            return emit(EncodeBranch16(0x58000000, zero, zero, 0));
          case BranchKind::IfTrue:
            return emit(EncodeBranch16(0x5c000000, reg, zero, 0));
          case BranchKind::IfFalse:
            return emit(EncodeBranch16(0x58000000, reg, zero, 0));
        }

        MOZ_CRASH("bad branch kind");
    }

    bool emitReturn(const StackValue& result) {
        if (!emit(EncodeUnsignedImm12(0x29800000, result.reg, a2, 0)))
            return false;
        if (!emitLoadImm32(NarrowScratch, int32_t(result.kind)))
            return false;
        if (!emit(EncodeUnsignedImm12(0x29800000, NarrowScratch, a2, sizeof(uint32_t))))
            return false;
        if (!emit(EncodeImm12(0x02800000, a0, zero, 1)))
            return false;
        if (!emit(0x4c000020))
            return false;
        sawReturn_ = true;
        return true;
    }

    LoongArchReg allocStackReg() {
        MOZ_ASSERT(stackDepth_ < mozilla::ArrayLength(StackRegs));
        return StackRegs[stackDepth_];
    }

    bool pushTempFromReg(LoongArchReg src, MinimalValueKind kind) {
        if (stackDepth_ >= mozilla::ArrayLength(StackRegs))
            return false;
        LoongArchReg dst = allocStackReg();
        if (!emitMove(dst, src))
            return false;
        stack_[stackDepth_++] = { dst, kind };
        return true;
    }

    bool pushTempFromImm(int32_t imm, MinimalValueKind kind) {
        if (stackDepth_ >= mozilla::ArrayLength(StackRegs))
            return false;
        LoongArchReg dst = allocStackReg();
        if (!emitLoadImm32(dst, imm))
            return false;
        stack_[stackDepth_++] = { dst, kind };
        return true;
    }

    bool duplicateTop() {
        if (!stackDepth_)
            return false;
        const StackValue& top = stack_[stackDepth_ - 1];
        return pushTempFromReg(top.reg, top.kind);
    }

    bool pop(StackValue* value) {
        if (!stackDepth_)
            return false;
        *value = stack_[--stackDepth_];
        return true;
    }

    bool emitStoreLocal(uint32_t slot) {
        if (slot >= script_->nfixed() || !stackDepth_)
            return false;
        if (!emitMove(LocalRegs[slot], stack_[stackDepth_ - 1].reg))
            return false;
        localKinds_[slot] = stack_[stackDepth_ - 1].kind;
        return true;
    }

    bool emitStoreArg(uint32_t slot) {
        if (slot >= script_->numArgs() || !stackDepth_)
            return false;
        if (!emitMove(ArgRegs[slot], stack_[stackDepth_ - 1].reg))
            return false;
        argKinds_[slot] = stack_[stackDepth_ - 1].kind;
        return true;
    }

    bool supportedScriptShape() const {
        return script_->numArgs() <= 2 &&
               script_->nfixed() <= mozilla::ArrayLength(LocalRegs) &&
               !script_->isAsync() &&
               !script_->isStarGenerator() &&
               !script_->isLegacyGenerator() &&
               !script_->hasTopLevelAwait() &&
               !script_->hasRest() &&
               !script_->hasNonSyntacticScope() &&
               !script_->bindingsAccessedDynamically() &&
               !script_->funHasExtensibleScope() &&
               !script_->funHasAnyAliasedFormal() &&
               !script_->argumentsHasVarBinding() &&
               !script_->hasTrynotes() &&
               !script_->isDerivedClassConstructor();
    }

    bool patchBranches() {
        for (size_t i = 0; i < branchPatches_.length(); i++) {
            const BranchPatch& patch = branchPatches_[i];
            if (patch.targetPcOffset >= pcToWord_.length())
                return false;

            int32_t targetWord = pcToWord_[patch.targetPcOffset];
            if (targetWord < 0)
                return false;

            int32_t rel = targetWord - int32_t(patch.wordOffset);
            switch (patch.kind) {
              case BranchKind::Always:
                words_[patch.wordOffset] = EncodeBranch16(0x58000000, zero, zero, rel);
                break;
              case BranchKind::IfTrue:
                words_[patch.wordOffset] = EncodeBranch16(0x5c000000, patch.reg, zero, rel);
                break;
              case BranchKind::IfFalse:
                words_[patch.wordOffset] = EncodeBranch16(0x58000000, patch.reg, zero, rel);
                break;
            }
        }
        return true;
    }

  public:
    explicit MinimalLoongArchCompiler(JSScript* script)
      : script_(script),
        wordCount_(0),
        failPatchCount_(0),
        stackDepth_(0),
        sawReturn_(false)
    {
        for (size_t i = 0; i < mozilla::ArrayLength(argKinds_); i++)
            argKinds_[i] = MinimalValueKind::Int32;
        for (size_t i = 0; i < mozilla::ArrayLength(localKinds_); i++)
            localKinds_[i] = MinimalValueKind::Undefined;
    }

    bool compile() {
        if (!supportedScriptShape())
            return false;

        if (!pcToWord_.appendN(-1, script_->length() + 1))
            return false;

        for (size_t i = 0; i < script_->nfixed(); i++) {
            if (!emitLoadImm32(LocalRegs[i], 0))
                return false;
        }

        jsbytecode* pc = script_->code();
        while (pc < script_->codeEnd()) {
            if (!markBytecode(pc))
                return false;

            uint32_t pcOffset = uint32_t(pc - script_->code());
            JSOp op = JSOp(*pc);
            switch (op) {
              case JSOP_GETARG: {
                uint32_t arg = GET_ARGNO(pc);
                if (arg >= script_->numArgs())
                    return false;
                if (!pushTempFromReg(ArgRegs[arg], argKinds_[arg]))
                    return false;
                break;
              }
              case JSOP_SETARG:
                if (!emitStoreArg(GET_ARGNO(pc)))
                    return false;
                break;
              case JSOP_GETLOCAL: {
                uint32_t local = GET_LOCALNO(pc);
                if (local >= script_->nfixed())
                    return false;
                if (!pushTempFromReg(LocalRegs[local], localKinds_[local]))
                    return false;
                break;
              }
              case JSOP_SETLOCAL:
              case JSOP_INITLEXICAL:
                if (!emitStoreLocal(GET_LOCALNO(pc)))
                    return false;
                break;
              case JSOP_ZERO:
                if (!pushTempFromImm(0, MinimalValueKind::Int32))
                    return false;
                break;
              case JSOP_ONE:
                if (!pushTempFromImm(1, MinimalValueKind::Int32))
                    return false;
                break;
              case JSOP_FALSE:
                if (!pushTempFromImm(0, MinimalValueKind::Boolean))
                    return false;
                break;
              case JSOP_TRUE:
                if (!pushTempFromImm(1, MinimalValueKind::Boolean))
                    return false;
                break;
              case JSOP_NULL:
                if (!pushTempFromImm(0, MinimalValueKind::Null))
                    return false;
                break;
              case JSOP_UNDEFINED:
                if (!pushTempFromImm(0, MinimalValueKind::Undefined))
                    return false;
                break;
              case JSOP_INT8:
                if (!pushTempFromImm(GET_INT8(pc), MinimalValueKind::Int32))
                    return false;
                break;
              case JSOP_INT32:
                if (!pushTempFromImm(GET_INT32(pc), MinimalValueKind::Int32))
                    return false;
                break;
              case JSOP_POP:
                if (!stackDepth_)
                    return false;
                stackDepth_--;
                break;
              case JSOP_DUP:
                if (!duplicateTop())
                    return false;
                break;
              case JSOP_SWAP: {
                if (stackDepth_ < 2)
                    return false;
                StackValue tmp = stack_[stackDepth_ - 1];
                stack_[stackDepth_ - 1] = stack_[stackDepth_ - 2];
                stack_[stackDepth_ - 2] = tmp;
                break;
              }
              case JSOP_TONUMERIC:
              case JSOP_POS:
                if (!stackDepth_ || !IsNumericKind(stack_[stackDepth_ - 1].kind))
                    return false;
                stack_[stackDepth_ - 1].kind = MinimalValueKind::Int32;
                break;
              case JSOP_VOID:
                if (!stackDepth_)
                    return false;
                if (!emitLoadImm32(stack_[stackDepth_ - 1].reg, 0))
                    return false;
                stack_[stackDepth_ - 1].kind = MinimalValueKind::Undefined;
                break;
              case JSOP_CHECKLEXICAL:
              case JSOP_LOOPENTRY:
              case JSOP_JUMPTARGET:
              case JSOP_NOP:
                break;
              case JSOP_ADD:
              case JSOP_SUB: {
                StackValue rhs;
                StackValue lhs;
                if (!pop(&rhs) || !pop(&lhs))
                    return false;
                if (!IsNumericKind(lhs.kind) || !IsNumericKind(rhs.kind))
                    return false;
                LoongArchReg out = allocStackReg();
                if (!emitCheckedBinary(op, lhs.reg, rhs.reg, out))
                    return false;
                stack_[stackDepth_++] = { out, MinimalValueKind::Int32 };
                break;
              }
              case JSOP_INC:
              case JSOP_DEC:
                if (!stackDepth_ || !IsNumericKind(stack_[stackDepth_ - 1].kind))
                    return false;
                if (!emitCheckedIncDec(op, stack_[stackDepth_ - 1].reg))
                    return false;
                stack_[stackDepth_ - 1].kind = MinimalValueKind::Int32;
                break;
              case JSOP_LT:
              case JSOP_LE:
              case JSOP_GT:
              case JSOP_GE: {
                StackValue rhs;
                StackValue lhs;
                if (!pop(&rhs) || !pop(&lhs))
                    return false;
                if (!IsNumericKind(lhs.kind) || !IsNumericKind(rhs.kind))
                    return false;
                LoongArchReg out = allocStackReg();
                if (!emitCompare(op, lhs.reg, rhs.reg, out))
                    return false;
                stack_[stackDepth_++] = { out, MinimalValueKind::Boolean };
                break;
              }
              case JSOP_EQ:
              case JSOP_NE:
              case JSOP_STRICTEQ:
              case JSOP_STRICTNE: {
                StackValue rhs;
                StackValue lhs;
                if (!pop(&rhs) || !pop(&lhs))
                    return false;
                LoongArchReg out = allocStackReg();
                bool invert = op == JSOP_NE || op == JSOP_STRICTNE;
                bool strict = op == JSOP_STRICTEQ || op == JSOP_STRICTNE;
                if (IsNumericKind(lhs.kind) && IsNumericKind(rhs.kind)) {
                    if (!emitEq(lhs.reg, rhs.reg, out, invert))
                        return false;
                } else {
                    bool equal = false;
                    if (strict) {
                        equal = lhs.kind == rhs.kind &&
                                (lhs.kind == MinimalValueKind::Null ||
                                 lhs.kind == MinimalValueKind::Undefined);
                    } else {
                        equal = (lhs.kind == rhs.kind &&
                                 (lhs.kind == MinimalValueKind::Null ||
                                  lhs.kind == MinimalValueKind::Undefined)) ||
                                ((lhs.kind == MinimalValueKind::Null &&
                                  rhs.kind == MinimalValueKind::Undefined) ||
                                 (lhs.kind == MinimalValueKind::Undefined &&
                                  rhs.kind == MinimalValueKind::Null));
                    }
                    if (!emitLoadImm32(out, invert ? !equal : equal))
                        return false;
                }
                stack_[stackDepth_++] = { out, MinimalValueKind::Boolean };
                break;
              }
              case JSOP_NEG:
                if (!stackDepth_ || !IsNumericKind(stack_[stackDepth_ - 1].kind))
                    return false;
                if (!emitNeg(stack_[stackDepth_ - 1].reg, stack_[stackDepth_ - 1].reg))
                    return false;
                stack_[stackDepth_ - 1].kind = MinimalValueKind::Int32;
                break;
              case JSOP_NOT: {
                StackValue input;
                if (!pop(&input))
                    return false;
                LoongArchReg out = allocStackReg();
                if (!emitNot(input.reg, out))
                    return false;
                stack_[stackDepth_++] = { out, MinimalValueKind::Boolean };
                break;
              }
              case JSOP_LSH:
              case JSOP_RSH:
              case JSOP_URSH: {
                StackValue rhs;
                StackValue lhs;
                if (!pop(&rhs) || !pop(&lhs))
                    return false;
                if (!IsNumericKind(lhs.kind) || !IsNumericKind(rhs.kind))
                    return false;
                LoongArchReg out = allocStackReg();
                if (!emitShift(op, lhs.reg, rhs.reg, out))
                    return false;
                stack_[stackDepth_++] = { out, MinimalValueKind::Int32 };
                break;
              }
              case JSOP_BITAND:
              case JSOP_BITOR:
              case JSOP_BITXOR: {
                StackValue rhs;
                StackValue lhs;
                if (!pop(&rhs) || !pop(&lhs))
                    return false;
                if (!IsNumericKind(lhs.kind) || !IsNumericKind(rhs.kind))
                    return false;
                uint32_t opcode = 0;
                switch (op) {
                  case JSOP_BITAND:
                    opcode = 0x00148000;
                    break;
                  case JSOP_BITOR:
                    opcode = 0x00150000;
                    break;
                  case JSOP_BITXOR:
                    opcode = 0x00158000;
                    break;
                  default:
                    MOZ_CRASH("unexpected bitwise opcode");
                }
                LoongArchReg out = allocStackReg();
                if (!emitBitBinary(opcode, lhs.reg, rhs.reg, out))
                    return false;
                stack_[stackDepth_++] = { out, MinimalValueKind::Int32 };
                break;
              }
              case JSOP_BITNOT:
                if (!stackDepth_ || !IsNumericKind(stack_[stackDepth_ - 1].kind))
                    return false;
                if (!emitBitNot(stack_[stackDepth_ - 1].reg, stack_[stackDepth_ - 1].reg))
                    return false;
                stack_[stackDepth_ - 1].kind = MinimalValueKind::Int32;
                break;
              case JSOP_GOTO:
                if (!emitBranch(BranchKind::Always, zero,
                                uint32_t(int32_t(pcOffset) + GET_JUMP_OFFSET(pc))))
                {
                    return false;
                }
                break;
              case JSOP_IFEQ:
              case JSOP_IFNE: {
                StackValue cond;
                if (!pop(&cond))
                    return false;
                BranchKind kind = (op == JSOP_IFNE) ? BranchKind::IfTrue : BranchKind::IfFalse;
                if (!emitBranch(kind, cond.reg,
                                uint32_t(int32_t(pcOffset) + GET_JUMP_OFFSET(pc))))
                {
                    return false;
                }
                break;
              }
              case JSOP_RETURN:
                if (stackDepth_ != 1)
                    return false;
                if (!emitReturn(stack_[stackDepth_ - 1]))
                    return false;
                pc = script_->codeEnd();
                continue;
              default:
                return false;
            }
            pc += GetBytecodeLength(pc);
        }

        pcToWord_[script_->length()] = int32_t(wordCount_);

        if (!sawReturn_)
            return false;
        if (!patchBranches())
            return false;

        size_t failureOffset = wordCount_;
        if (!emit(EncodeImm12(0x02800000, a0, zero, 0)))
            return false;
        if (!emit(0x4c000020))
            return false;

        for (size_t i = 0; i < failPatchCount_; i++) {
            size_t patchOffset = failPatches_[i];
            int32_t rel = int32_t(failureOffset) - int32_t(patchOffset);
            words_[patchOffset] = EncodeBranch16(0x5c000000, WideScratch, NarrowScratch, rel);
        }

        return true;
    }

    size_t codeSize() const {
        return wordCount_ * sizeof(uint32_t);
    }

    void copyCode(uint8_t* dst) const {
        memcpy(dst, words_, codeSize());
    }
};

static bool
CanUseMinimalJit(JSScript* script, const CallArgs& args)
{
    if (!script || script->numArgs() > 2)
        return false;

    if (args.length() < script->numArgs())
        return false;

    for (unsigned i = 0; i < script->numArgs(); i++) {
        if (!args[i].isInt32())
            return false;
    }

    return true;
}

static bool
CanDirectCallMinimalJit(JSContext* cx, HandleFunction fun, JSScript* script, const CallArgs& args)
{
    if (!fun || !fun->isInterpreted() || !script)
        return false;

    if (!CanUseMinimalJit(script, args))
        return false;

    if (fun->isSelfHostedBuiltin() || fun->isArrow() || fun->needsSomeEnvironmentObject())
        return false;

    if (script->selfHosted() || script->strict() || script->treatAsRunOnce())
        return false;

    if (cx->compartment()->isDebuggee() || cx->runtime()->profilingScripts ||
        cx->runtime()->spsProfiler.enabled())
    {
        return false;
    }

    Activation* activation = cx->runtime()->activation();
    if (!activation || !activation->isInterpreter())
        return false;

    return true;
}

static bool
PreparePropertyFastPath(JSScript* script)
{
    if (script->loongArchMinimalFastPathKind() != MinimalFastPathUninitialized)
        return script->loongArchMinimalFastPathKind() != MinimalFastPathNone;

    uint8_t objectArg = 0;
    uint8_t valueArg = 0;
    uint32_t propOffset = 0;
    enum State { Start, SawObject, SawValue, SawProp } state = Start;

    for (jsbytecode* pc = script->code(); pc < script->codeEnd(); pc += GetBytecodeLength(pc)) {
        switch (JSOp(*pc)) {
          case JSOP_JUMPTARGET:
          case JSOP_NOP:
            continue;
          case JSOP_GETARG: {
            uint32_t arg = GET_ARGNO(pc);
            if (state == Start) {
                objectArg = uint8_t(arg);
                state = SawObject;
                continue;
            }
            if (state == SawObject) {
                valueArg = uint8_t(arg);
                state = SawValue;
                continue;
            }
            script->setLoongArchMinimalFastPath(MinimalFastPathNone, 0, 0, 0);
            return false;
          }
          case JSOP_GETPROP:
            if (state == SawObject) {
                propOffset = uint32_t(pc - script->code());
                state = SawProp;
                continue;
            }
            break;
          case JSOP_SETPROP:
          case JSOP_STRICTSETPROP:
            if (state == SawValue) {
                propOffset = uint32_t(pc - script->code());
                script->setLoongArchMinimalFastPath(MinimalFastPathSetter, objectArg, valueArg, propOffset);
                state = SawProp;
                continue;
            }
            break;
          case JSOP_RETURN:
            if (state == SawProp) {
                uint8_t kind = script->loongArchMinimalFastPathKind();
                if (kind == MinimalFastPathSetter)
                    return true;
                script->setLoongArchMinimalFastPath(MinimalFastPathGetter, objectArg, 0, propOffset);
                return true;
            }
            break;
          default:
            break;
        }

        script->setLoongArchMinimalFastPath(MinimalFastPathNone, 0, 0, 0);
        return false;
    }

    script->setLoongArchMinimalFastPath(MinimalFastPathNone, 0, 0, 0);
    return false;
}

static bool
TryFastPropertyPath(JSContext* cx, JSScript* script, const CallArgs& args, bool* handled)
{
    *handled = false;

    if (!PreparePropertyFastPath(script))
        return true;

    uint8_t kind = script->loongArchMinimalFastPathKind();
    uint8_t objectArg = script->loongArchMinimalFastPathObjectArg();
    if (args.length() <= objectArg || !args[objectArg].isObject())
        return true;

    RootedObject obj(cx, &args[objectArg].toObject());
    if (!obj->is<NativeObject>())
        return true;

    jsbytecode* pc = script->code() + script->loongArchMinimalFastPathOffset();
    RootedPropertyName name(cx, script->getName(pc));
    RootedShape shape(cx, obj->as<NativeObject>().lookupPure(name));
    if (!shape || !shape->hasSlot())
        return true;

    if (kind == MinimalFastPathGetter) {
        if (!shape->hasDefaultGetter())
            return true;

        Value result = obj->as<NativeObject>().getSlot(shape->slot());
        if (result.isMagic())
            return true;

        args.rval().set(result);
        *handled = true;
        return true;
    }

    if (kind == MinimalFastPathSetter) {
        uint8_t valueArg = script->loongArchMinimalFastPathValueArg();
        if (args.length() <= valueArg || !shape->hasDefaultSetter() || !shape->writable())
            return true;

        obj->as<NativeObject>().setSlotWithType(cx, shape, args[valueArg]);
        args.rval().set(args[valueArg]);
        *handled = true;
        return true;
    }

    return true;
}

static bool
CanUseMinimalJit(RunState& state, InvokeState& invoke)
{
    if (invoke.constructing())
        return false;

    return CanUseMinimalJit(state.script(), invoke.args());
}

static Value
MinimalJitResultToValue(const MinimalJitResult& result)
{
    switch (result.kind) {
      case MinimalValueKind::Int32:
        return JS::Int32Value(int32_t(result.payload));
      case MinimalValueKind::Boolean:
        return JS::BooleanValue(bool(result.payload));
      case MinimalValueKind::Undefined:
        return JS::UndefinedValue();
      case MinimalValueKind::Null:
        return JS::NullValue();
    }

    MOZ_CRASH("bad minimal JIT value kind");
}

static bool
LookupOrCompileMinimalJit(JSContext* cx, JSScript* script, TinyLoongArchJitCode* fnOut)
{
    if (uint8_t* code = script->loongArchMinimalJitCodeRaw()) {
        *fnOut = JS_DATA_TO_FUNC_PTR(TinyLoongArchJitCode, code);
        return true;
    }

    MinimalLoongArchCompiler compiler(script);
    if (!compiler.compile())
        return false;

    AutoExecutableMemory code;
    if (!code.allocate(compiler.codeSize()))
        return false;

    compiler.copyCode(code.bytes());
    if (!code.makeExecutable())
        return false;

    uint8_t* raw = code.release();
    script->setLoongArchMinimalJitCode(raw, uint32_t(compiler.codeSize()));
    *fnOut = JS_DATA_TO_FUNC_PTR(TinyLoongArchJitCode, raw);
    return true;
}

} // namespace

bool
TryCallLoongArchMinimalJit(JSContext* cx, HandleFunction fun, const CallArgs& args, bool* handled)
{
    *handled = false;

    JSScript* script = fun ? fun->nonLazyScript() : nullptr;
    if (!script)
        return true;

    if (!TryFastPropertyPath(cx, script, args, handled) || *handled)
        return !cx->isExceptionPending();

    if (!CanDirectCallMinimalJit(cx, fun, script, args))
        return true;

    TinyLoongArchJitCode fn;
    if (!LookupOrCompileMinimalJit(cx, script, &fn))
        return true;

    MinimalJitResult result = { 0, MinimalValueKind::Int32 };
    int32_t arg0 = script->numArgs() >= 1 ? args[0].toInt32() : 0;
    int32_t arg1 = script->numArgs() >= 2 ? args[1].toInt32() : 0;
    if (!fn(arg0, arg1, &result))
        return true;

    args.rval().set(MinimalJitResultToValue(result));
    *handled = true;
    return true;
}

bool
TryEnterLoongArchMinimalJit(JSContext* cx, RunState& state)
{
    if (!state.isInvoke())
        return false;

    InvokeState& invoke = *state.asInvoke();
    bool handled = false;
    if (!TryFastPropertyPath(cx, state.script(), invoke.args(), &handled))
        return false;
    if (handled) {
        state.setReturnValue(invoke.args().rval());
        return true;
    }

    if (!CanUseMinimalJit(state, invoke))
        return false;

    TinyLoongArchJitCode fn;
    if (!LookupOrCompileMinimalJit(cx, state.script(), &fn))
        return false;

    MinimalJitResult result = { 0, MinimalValueKind::Int32 };
    int32_t arg0 = state.script()->numArgs() >= 1 ? invoke.args()[0].toInt32() : 0;
    int32_t arg1 = state.script()->numArgs() >= 2 ? invoke.args()[1].toInt32() : 0;
    if (!fn(arg0, arg1, &result))
        return false;

    state.setReturnValue(MinimalJitResultToValue(result));
    return true;
}

} // namespace jit
} // namespace js

#else

bool
js::jit::TryEnterLoongArchMinimalJit(JSContext*, RunState&)
{
    return false;
}

bool
js::jit::TryCallLoongArchMinimalJit(JSContext*, HandleFunction, const CallArgs&, bool* handled)
{
    *handled = false;
    return true;
}

#endif
