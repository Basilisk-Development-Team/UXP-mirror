/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#include "jit/LoongArchMinimalJit.h"

#include <string.h>

#include "mozilla/ArrayUtils.h"

#include "jit/ProcessExecutableMemory.h"
#include "jsopcode.h"
#include "jsscript.h"
#include "js/Value.h"
#include "vm/Interpreter.h"

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

using TinyLoongArchJitCode = bool (*)(int32_t arg0, int32_t arg1, int32_t* out);

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
};

class MinimalLoongArchCompiler
{
    JSScript* script_;
    uint32_t words_[256];
    size_t wordCount_;
    size_t failPatches_[64];
    size_t failPatchCount_;
    LoongArchReg stack_[16];
    size_t stackDepth_;
    bool sawReturn_;

    bool emit(uint32_t word) {
        if (wordCount_ >= ArrayLength(words_))
            return false;
        words_[wordCount_++] = word;
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

    bool emitFailureBranch() {
        if (failPatchCount_ >= ArrayLength(failPatches_))
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

    bool emitReturn(LoongArchReg result) {
        if (!emit(EncodeUnsignedImm12(0x29000000, result, a2, 0)))
            return false;
        if (!emit(EncodeImm12(0x02800000, a0, zero, 1)))
            return false;
        if (!emit(0x4c000020))
            return false;
        sawReturn_ = true;
        return true;
    }

    LoongArchReg allocStackReg() {
        MOZ_ASSERT(stackDepth_ < ArrayLength(stack_));
        return StackRegs[stackDepth_];
    }

    bool pushTempFromReg(LoongArchReg src) {
        if (stackDepth_ >= ArrayLength(StackRegs))
            return false;
        LoongArchReg dst = allocStackReg();
        if (!emitMove(dst, src))
            return false;
        stack_[stackDepth_++] = dst;
        return true;
    }

    bool pushTempFromImm(int32_t imm) {
        if (stackDepth_ >= ArrayLength(StackRegs))
            return false;
        LoongArchReg dst = allocStackReg();
        if (!emitLoadImm32(dst, imm))
            return false;
        stack_[stackDepth_++] = dst;
        return true;
    }

    bool pop(LoongArchReg* reg) {
        if (!stackDepth_)
            return false;
        *reg = stack_[--stackDepth_];
        return true;
    }

    bool supportedScriptShape() const {
        return script_->numArgs() <= 2 &&
               script_->nfixed() <= ArrayLength(LocalRegs) &&
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

  public:
    explicit MinimalLoongArchCompiler(JSScript* script)
      : script_(script),
        wordCount_(0),
        failPatchCount_(0),
        stackDepth_(0),
        sawReturn_(false)
    { }

    bool compile() {
        if (!supportedScriptShape())
            return false;

        jsbytecode* pc = script_->code();
        while (pc < script_->codeEnd()) {
            JSOp op = JSOp(*pc);
            switch (op) {
              case JSOP_GETARG:
                if (GET_ARGNO(pc) >= script_->numArgs())
                    return false;
                if (!pushTempFromReg(ArgRegs[GET_ARGNO(pc)]))
                    return false;
                break;
              case JSOP_GETLOCAL:
                if (GET_LOCALNO(pc) >= script_->nfixed())
                    return false;
                if (!pushTempFromReg(LocalRegs[GET_LOCALNO(pc)]))
                    return false;
                break;
              case JSOP_SETLOCAL: {
                uint32_t slot = GET_LOCALNO(pc);
                if (slot >= script_->nfixed() || !stackDepth_)
                    return false;
                if (!emitMove(LocalRegs[slot], stack_[stackDepth_ - 1]))
                    return false;
                break;
              }
              case JSOP_ZERO:
                if (!pushTempFromImm(0))
                    return false;
                break;
              case JSOP_ONE:
                if (!pushTempFromImm(1))
                    return false;
                break;
              case JSOP_INT8:
                if (!pushTempFromImm(GET_INT8(pc)))
                    return false;
                break;
              case JSOP_INT32:
                if (!pushTempFromImm(GET_INT32(pc)))
                    return false;
                break;
              case JSOP_POP:
                if (!stackDepth_)
                    return false;
                stackDepth_--;
                break;
              case JSOP_ADD:
              case JSOP_SUB: {
                LoongArchReg rhs;
                LoongArchReg lhs;
                if (!pop(&rhs) || !pop(&lhs))
                    return false;
                LoongArchReg out = allocStackReg();
                if (!emitCheckedBinary(op, lhs, rhs, out))
                    return false;
                stack_[stackDepth_++] = out;
                break;
              }
              case JSOP_NOP:
              case JSOP_JUMPTARGET:
                break;
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

        if (!sawReturn_)
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
CanUseMinimalJit(RunState& state, InvokeState& invoke)
{
    if (invoke.constructing())
        return false;

    JSScript* script = state.script();
    if (!script || script->numArgs() > 2)
        return false;

    if (invoke.args().length() < script->numArgs())
        return false;

    for (unsigned i = 0; i < script->numArgs(); i++) {
        if (!invoke.args()[i].isInt32())
            return false;
    }

    return true;
}

} // namespace

bool
TryEnterLoongArchMinimalJit(JSContext* cx, RunState& state)
{
    (void)cx;
    if (!state.isInvoke())
        return false;

    InvokeState& invoke = *state.asInvoke();
    if (!CanUseMinimalJit(state, invoke))
        return false;

    MinimalLoongArchCompiler compiler(state.script());
    if (!compiler.compile())
        return false;

    AutoExecutableMemory code;
    if (!code.allocate(compiler.codeSize()))
        return false;

    compiler.copyCode(code.bytes());

    if (!code.makeExecutable())
        return false;

    TinyLoongArchJitCode fn = JS_DATA_TO_FUNC_PTR(TinyLoongArchJitCode, code.bytes());

    int32_t result = 0;
    int32_t arg0 = state.script()->numArgs() >= 1 ? invoke.args()[0].toInt32() : 0;
    int32_t arg1 = state.script()->numArgs() >= 2 ? invoke.args()[1].toInt32() : 0;
    if (!fn(arg0, arg1, &result))
        return false;

    Value value;
    value.setInt32(result);
    state.setReturnValue(value);
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

#endif
