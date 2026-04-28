/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#include "jit/loongarch64/CodeGenerator-loongarch64.h"

#include "jit/JitFrames.h"
#include "jit/MIR.h"
#include "jit/MIRGraph.h"
#include "vm/Shape.h"
#include "vm/TraceLogging.h"

#include "jit/MacroAssembler-inl.h"
#include "jit/shared/CodeGenerator-shared-inl.h"

using namespace js;
using namespace js::jit;

static constexpr uint32_t WasmTruncateInvalidCauseMask =
    1u << (Assembler::CauseV - Assembler::CauseI);

static void
ExtractWasmTruncateInvalidCause(MacroAssemblerLOONGARCH64Compat& masm, Register reg)
{
    masm.as_cfc1(reg, Assembler::FCSR);
    masm.as_bstrpick_d(reg, reg, Assembler::CauseV, Assembler::CauseI);
    masm.as_andi(reg, reg, WasmTruncateInvalidCauseMask);
}

class js::jit::OutOfLineTableSwitch : public OutOfLineCodeBase<CodeGeneratorLoongArch64>
{
    MTableSwitch* mir_;
    CodeLabel jumpLabel_;

    void accept(CodeGeneratorLoongArch64* codegen) {
        codegen->visitOutOfLineTableSwitch(this);
    }

  public:
    explicit OutOfLineTableSwitch(MTableSwitch* mir)
      : mir_(mir)
    {}

    MTableSwitch* mir() const {
        return mir_;
    }

    CodeLabel* jumpLabel() {
        return &jumpLabel_;
    }
};

void
CodeGeneratorLoongArch64::visitOutOfLineTableSwitch(OutOfLineTableSwitch* ool)
{
    MTableSwitch* mir = ool->mir();

    masm.haltingAlign(sizeof(void*));
    masm.bind(ool->jumpLabel()->target());
    masm.addCodeLabel(*ool->jumpLabel());

    for (size_t i = 0; i < mir->numCases(); i++) {
        LBlock* caseblock = skipTrivialBlocks(mir->getCase(i))->lir();
        Label* caseheader = caseblock->label();
        uint32_t caseoffset = caseheader->offset();

        // Keep loongarch64 table entries at a fixed 32-byte stride so dispatch
        // can jump to a stub entry and let the stub jump to the case body.
        CodeLabel cl;
        masm.ma_li(ScratchRegister, cl.patchAt());
        masm.jump(ScratchRegister);
        masm.as_nop();
        masm.as_nop();
        masm.as_nop();
        masm.as_nop();
        cl.target()->bind(caseoffset);
        masm.addCodeLabel(cl);
    }
}

void
CodeGeneratorLoongArch64::emitTableSwitchDispatch(MTableSwitch* mir, Register index, Register base)
{
    Label* defaultcase = skipTrivialBlocks(mir->getDefault())->lir()->label();

    if (mir->low() != 0)
        masm.subPtr(Imm32(mir->low()), index);

    int32_t cases = mir->numCases();
    masm.branch32(Assembler::AboveOrEqual, index, Imm32(cases), defaultcase);

    OutOfLineTableSwitch* ool = new(alloc()) OutOfLineTableSwitch(mir);
    addOutOfLineCode(ool, mir);

    masm.ma_li(base, ool->jumpLabel()->patchAt());
    masm.lshiftPtr(Imm32(5), index);
    masm.addPtr(index, base);
    masm.jump(base);
}

void
CodeGeneratorLoongArch64::visitCompare(LCompare* comp)
{
    MCompare* mir = comp->mir();
    Assembler::Condition cond = JSOpToCondition(mir->compareType(), comp->jsop());
    const LAllocation* left = comp->getOperand(0);
    const LAllocation* right = comp->getOperand(1);
    const LDefinition* def = comp->getDef(0);

    if (mir->compareType() == MCompare::Compare_Object) {
        if (right->isGeneralReg())
            masm.cmpPtrSet(cond, ToRegister(left), ToRegister(right), ToRegister(def));
        else
            masm.cmpPtrSet(cond, ToRegister(left), ToAddress(right), ToRegister(def));
        return;
    }

    CodeGeneratorMIPSShared::visitCompare(comp);
}

void
CodeGeneratorLoongArch64::visitCompareAndBranch(LCompareAndBranch* comp)
{
    MCompare* mir = comp->cmpMir();
    Assembler::Condition cond = JSOpToCondition(mir->compareType(), comp->jsop());

    if (mir->compareType() == MCompare::Compare_Object) {
        if (comp->right()->isGeneralReg()) {
            emitBranch(ToRegister(comp->left()), ToRegister(comp->right()), cond,
                       comp->ifTrue(), comp->ifFalse());
        } else {
            masm.loadPtr(ToAddress(comp->right()), ScratchRegister);
            emitBranch(ToRegister(comp->left()), ScratchRegister, cond,
                       comp->ifTrue(), comp->ifFalse());
        }
        return;
    }

    CodeGeneratorMIPSShared::visitCompareAndBranch(comp);
}

void
CodeGeneratorLoongArch64::visitDivI(LDivI* ins)
{
    Register lhs = ToRegister(ins->lhs());
    Register rhs = ToRegister(ins->rhs());
    Register dest = ToRegister(ins->output());
    Register temp = ToRegister(ins->getTemp(0));
    MDiv* mir = ins->mir();

    Label done;

    if (mir->canBeDivideByZero()) {
        if (mir->trapOnError()) {
            masm.ma_b(rhs, rhs, trap(mir, wasm::Trap::IntegerDivideByZero), Assembler::Zero);
        } else if (mir->canTruncateInfinities()) {
            Label notzero;
            masm.ma_b(rhs, rhs, &notzero, Assembler::NonZero, ShortJump);
            masm.move32(Imm32(0), dest);
            masm.ma_b(&done, ShortJump);
            masm.bind(&notzero);
        } else {
            MOZ_ASSERT(mir->fallible());
            bailoutCmp32(Assembler::Zero, rhs, rhs, ins->snapshot());
        }
    }

    if (mir->canBeNegativeOverflow()) {
        Label notMinInt;
        masm.move32(Imm32(INT32_MIN), temp);
        masm.ma_b(lhs, temp, &notMinInt, Assembler::NotEqual, ShortJump);

        masm.move32(Imm32(-1), temp);
        if (mir->trapOnError()) {
            masm.ma_b(rhs, temp, trap(mir, wasm::Trap::IntegerOverflow), Assembler::Equal);
        } else if (mir->canTruncateOverflow()) {
            Label skip;
            masm.ma_b(rhs, temp, &skip, Assembler::NotEqual, ShortJump);
            masm.move32(Imm32(INT32_MIN), dest);
            masm.ma_b(&done, ShortJump);
            masm.bind(&skip);
        } else {
            MOZ_ASSERT(mir->fallible());
            bailoutCmp32(Assembler::Equal, rhs, temp, ins->snapshot());
        }
        masm.bind(&notMinInt);
    }

    if (!mir->canTruncateNegativeZero() && mir->canBeNegativeZero()) {
        Label nonzero;
        masm.ma_b(lhs, lhs, &nonzero, Assembler::NonZero, ShortJump);
        bailoutCmp32(Assembler::LessThan, rhs, Imm32(0), ins->snapshot());
        masm.bind(&nonzero);
    }

    if (mir->canTruncateRemainder()) {
        masm.as_div_w(dest, lhs, rhs);
    } else {
        MOZ_ASSERT(mir->fallible());

        Label remainderNonZero;
        masm.ma_div_branch_overflow(dest, lhs, rhs, &remainderNonZero);
        bailoutFrom(&remainderNonZero, ins->snapshot());
    }

    masm.bind(&done);
}

void
CodeGeneratorLoongArch64::visitModI(LModI* ins)
{
    Register lhs = ToRegister(ins->lhs());
    Register rhs = ToRegister(ins->rhs());
    Register dest = ToRegister(ins->output());
    Register callTemp = ToRegister(ins->callTemp());
    MMod* mir = ins->mir();
    Label done, prevent;

    masm.move32(lhs, callTemp);

    if (mir->canBeNegativeDividend()) {
        masm.ma_b(lhs, Imm32(INT_MIN), &prevent, Assembler::NotEqual, ShortJump);
        if (mir->isTruncated()) {
            Label skip;
            masm.ma_b(rhs, Imm32(-1), &skip, Assembler::NotEqual, ShortJump);
            masm.move32(Imm32(0), dest);
            masm.ma_b(&done, ShortJump);
            masm.bind(&skip);
        } else {
            MOZ_ASSERT(mir->fallible());
            bailoutCmp32(Assembler::Equal, rhs, Imm32(-1), ins->snapshot());
        }
        masm.bind(&prevent);
    }

    if (mir->canBeDivideByZero()) {
        if (mir->isTruncated()) {
            if (mir->trapOnError()) {
                masm.ma_b(rhs, rhs, trap(mir, wasm::Trap::IntegerDivideByZero), Assembler::Zero);
            } else {
                Label skip;
                masm.ma_b(rhs, Imm32(0), &skip, Assembler::NotEqual, ShortJump);
                masm.move32(Imm32(0), dest);
                masm.ma_b(&done, ShortJump);
                masm.bind(&skip);
            }
        } else {
            MOZ_ASSERT(mir->fallible());
            bailoutCmp32(Assembler::Equal, rhs, Imm32(0), ins->snapshot());
        }
    }

    if (mir->canBeNegativeDividend()) {
        Label notNegative;
        masm.ma_b(rhs, Imm32(0), &notNegative, Assembler::GreaterThan, ShortJump);
        if (mir->isTruncated()) {
            Label skip;
            masm.ma_b(lhs, Imm32(0), &skip, Assembler::NotEqual, ShortJump);
            masm.move32(Imm32(0), dest);
            masm.ma_b(&done, ShortJump);
            masm.bind(&skip);
        } else {
            MOZ_ASSERT(mir->fallible());
            bailoutCmp32(Assembler::Equal, lhs, Imm32(0), ins->snapshot());
        }
        masm.bind(&notNegative);
    }

    masm.as_mod_w(dest, lhs, rhs);

    if (mir->canBeNegativeDividend() && !mir->isTruncated()) {
        MOZ_ASSERT(mir->fallible());
        masm.ma_b(dest, Imm32(0), &done, Assembler::NotEqual, ShortJump);
        bailoutCmp32(Assembler::Signed, callTemp, Imm32(0), ins->snapshot());
    }
    masm.bind(&done);
}

void
CodeGeneratorLoongArch64::visitUDivOrMod(LUDivOrMod* ins)
{
    Register lhs = ToRegister(ins->lhs());
    Register rhs = ToRegister(ins->rhs());
    Register output = ToRegister(ins->output());
    Label done;

    if (ins->canBeDivideByZero()) {
        if (ins->mir()->isTruncated()) {
            if (ins->trapOnError()) {
                masm.ma_b(rhs, rhs, trap(ins, wasm::Trap::IntegerDivideByZero), Assembler::Zero);
            } else {
                Label notzero;
                masm.ma_b(rhs, rhs, &notzero, Assembler::NonZero, ShortJump);
                masm.move32(Imm32(0), output);
                masm.ma_b(&done, ShortJump);
                masm.bind(&notzero);
            }
        } else {
            bailoutCmp32(Assembler::Equal, rhs, Imm32(0), ins->snapshot());
        }
    }

    masm.as_mod_wu(ScratchRegister, lhs, rhs);
    masm.move32(ScratchRegister, output);

    if (ins->mir()->isDiv()) {
        if (!ins->mir()->toDiv()->canTruncateRemainder())
            bailoutCmp32(Assembler::NonZero, output, output, ins->snapshot());
        masm.as_div_wu(output, lhs, rhs);
    }

    if (!ins->mir()->isTruncated())
        bailoutCmp32(Assembler::LessThan, output, Imm32(0), ins->snapshot());

    masm.bind(&done);
}

void
CodeGeneratorLoongArch64::visitDivOrModI64(LDivOrModI64* lir)
{
    Register lhs = ToRegister(lir->lhs());
    Register rhs = ToRegister(lir->rhs());
    Register output = ToRegister(lir->output());

    Label done;

    if (lir->canBeDivideByZero())
        masm.ma_b(rhs, rhs, trap(lir, wasm::Trap::IntegerDivideByZero), Assembler::Zero);

    if (lir->canBeNegativeOverflow()) {
        Label notmin;
        masm.branchPtr(Assembler::NotEqual, lhs, ImmWord(INT64_MIN), &notmin);
        masm.branchPtr(Assembler::NotEqual, rhs, ImmWord(-1), &notmin);
        if (lir->mir()->isMod()) {
            masm.ma_xor(output, output);
        } else {
            masm.jump(trap(lir, wasm::Trap::IntegerOverflow));
        }
        masm.jump(&done);
        masm.bind(&notmin);
    }

    if (lir->mir()->isMod())
        masm.as_mod_d(output, lhs, rhs);
    else
        masm.as_div_d(output, lhs, rhs);

    masm.bind(&done);
}

void
CodeGeneratorLoongArch64::visitUDivOrModI64(LUDivOrModI64* lir)
{
    Register lhs = ToRegister(lir->lhs());
    Register rhs = ToRegister(lir->rhs());
    Register output = ToRegister(lir->output());

    Label done;

    if (lir->canBeDivideByZero())
        masm.ma_b(rhs, rhs, trap(lir, wasm::Trap::IntegerDivideByZero), Assembler::Zero);

    if (lir->mir()->isMod())
        masm.as_mod_du(output, lhs, rhs);
    else
        masm.as_div_du(output, lhs, rhs);

    masm.bind(&done);
}

void
CodeGeneratorLoongArch64::visitWasmTruncateToInt32(LWasmTruncateToInt32* lir)
{
    auto input = ToFloatRegister(lir->input());
    auto output = ToRegister(lir->output());

    MWasmTruncateToInt32* mir = lir->mir();
    MIRType fromType = mir->input()->type();

    auto* ool = new (alloc()) OutOfLineWasmTruncateCheck(mir, input);
    addOutOfLineCode(ool, mir);

    if (mir->isUnsigned()) {
        if (fromType == MIRType::Double)
            masm.as_truncld(ScratchDoubleReg, input);
        else if (fromType == MIRType::Float32)
            masm.as_truncls(ScratchDoubleReg, input);
        else
            MOZ_CRASH("unexpected type in visitWasmTruncateToInt32");

        masm.moveFromDoubleHi(ScratchDoubleReg, output);
        ExtractWasmTruncateInvalidCause(masm, ScratchRegister);
        masm.ma_or(output, ScratchRegister);
        masm.ma_b(output, Imm32(0), ool->entry(), Assembler::NotEqual);

        masm.moveFromFloat32(ScratchDoubleReg, output);
        return;
    }

    if (fromType == MIRType::Double)
        masm.as_truncwd(ScratchFloat32Reg, input);
    else if (fromType == MIRType::Float32)
        masm.as_truncws(ScratchFloat32Reg, input);
    else
        MOZ_CRASH("unexpected type in visitWasmTruncateToInt32");

    ExtractWasmTruncateInvalidCause(masm, output);
    masm.ma_b(output, Imm32(0), ool->entry(), Assembler::NotEqual);

    masm.bind(ool->rejoin());
    masm.moveFromFloat32(ScratchFloat32Reg, output);
}

void
CodeGeneratorLoongArch64::visitWasmTruncateToInt64(LWasmTruncateToInt64* lir)
{
    FloatRegister input = ToFloatRegister(lir->input());
    Register output = ToRegister(lir->output());

    MWasmTruncateToInt64* mir = lir->mir();
    MIRType fromType = mir->input()->type();

    MOZ_ASSERT(fromType == MIRType::Double || fromType == MIRType::Float32);

    auto* ool = new (alloc()) OutOfLineWasmTruncateCheck(mir, input);
    addOutOfLineCode(ool, mir);

    if (mir->isUnsigned()) {
        Label isLarge, done;

        if (fromType == MIRType::Double) {
            masm.loadConstantDouble(double(INT64_MAX), ScratchDoubleReg);
            masm.ma_bc1d(ScratchDoubleReg, input, &isLarge,
                         Assembler::DoubleLessThanOrEqual, ShortJump);

            masm.as_truncld(ScratchDoubleReg, input);
        } else {
            masm.loadConstantFloat32(float(INT64_MAX), ScratchFloat32Reg);
            masm.ma_bc1s(ScratchFloat32Reg, input, &isLarge,
                         Assembler::DoubleLessThanOrEqual, ShortJump);

            masm.as_truncls(ScratchDoubleReg, input);
        }

        masm.moveFromDouble(ScratchDoubleReg, output);
        ExtractWasmTruncateInvalidCause(masm, ScratchRegister);
        masm.ma_dsrl(SecondScratchReg, output, Imm32(63));
        masm.ma_or(SecondScratchReg, ScratchRegister);
        masm.ma_b(SecondScratchReg, Imm32(0), ool->entry(), Assembler::NotEqual);

        masm.ma_b(&done, ShortJump);

        masm.bind(&isLarge);
        if (fromType == MIRType::Double) {
            masm.as_subd(ScratchDoubleReg, input, ScratchDoubleReg);
            masm.as_truncld(ScratchDoubleReg, ScratchDoubleReg);
        } else {
            masm.as_subs(ScratchDoubleReg, input, ScratchDoubleReg);
            masm.as_truncls(ScratchDoubleReg, ScratchDoubleReg);
        }

        masm.moveFromDouble(ScratchDoubleReg, output);
        ExtractWasmTruncateInvalidCause(masm, ScratchRegister);
        masm.ma_dsrl(SecondScratchReg, output, Imm32(63));
        masm.ma_or(SecondScratchReg, ScratchRegister);
        masm.ma_b(SecondScratchReg, Imm32(0), ool->entry(), Assembler::NotEqual);

        masm.ma_li(ScratchRegister, Imm32(1));
        masm.ma_dins(output, ScratchRegister, Imm32(63), Imm32(1));

        masm.bind(&done);
        return;
    }

    if (fromType == MIRType::Double)
        masm.as_truncld(ScratchDoubleReg, input);
    else
        masm.as_truncls(ScratchDoubleReg, input);

    ExtractWasmTruncateInvalidCause(masm, output);
    masm.ma_b(output, Imm32(0), ool->entry(), Assembler::NotEqual);

    masm.bind(ool->rejoin());
    masm.moveFromDouble(ScratchDoubleReg, output);
}

void
CodeGeneratorLoongArch64::visitWasmAddOffset(LWasmAddOffset* lir)
{
    MWasmAddOffset* mir = lir->mir();
    Register base = ToRegister(lir->base());
    Register out = ToRegister(lir->output());

    Register limitBase = base;
    if (base == out) {
        limitBase = ScratchRegister;
        masm.movePtr(base, limitBase);
    } else {
        masm.movePtr(base, out);
    }

    masm.addPtr(Imm32(mir->offset()), out);
    masm.branchPtr(Assembler::Below, out, limitBase, trap(mir, wasm::Trap::OutOfBounds));
}

template <typename S, typename T>
static void
AtomicBinopToTypedIntArray(MacroAssemblerLOONGARCH64Compat& masm, AtomicOp op,
                           Scalar::Type arrayType, const S& value,
                           const T& mem, Register outTemp,
                           Register valueTemp, Register offsetTemp,
                           Register maskTemp, AnyRegister output)
{
    switch (arrayType) {
      case Scalar::Int8:
        masm.atomicFetchOp(1, true, op, value, mem, valueTemp, offsetTemp,
                           maskTemp, output.gpr());
        break;
      case Scalar::Uint8:
        masm.atomicFetchOp(1, false, op, value, mem, valueTemp, offsetTemp,
                           maskTemp, output.gpr());
        break;
      case Scalar::Int16:
        masm.atomicFetchOp(2, true, op, value, mem, valueTemp, offsetTemp,
                           maskTemp, output.gpr());
        break;
      case Scalar::Uint16:
        masm.atomicFetchOp(2, false, op, value, mem, valueTemp, offsetTemp,
                           maskTemp, output.gpr());
        break;
      case Scalar::Int32:
        masm.atomicFetchOp(4, false, op, value, mem, valueTemp, offsetTemp,
                           maskTemp, output.gpr());
        break;
      case Scalar::Uint32:
        MOZ_ASSERT(output.isFloat());
        MOZ_ASSERT(outTemp != InvalidReg);
        masm.atomicFetchOp(4, false, op, value, mem, valueTemp, offsetTemp,
                           maskTemp, outTemp);
        masm.convertUInt32ToDouble(outTemp, output.fpu());
        break;
      default:
        MOZ_CRASH("Invalid typed array type");
    }
}

template <typename S, typename T>
static void
AtomicBinopToTypedIntArray(MacroAssemblerLOONGARCH64Compat& masm, AtomicOp op,
                           Scalar::Type arrayType, const S& value,
                           const T& mem, Register valueTemp,
                           Register offsetTemp, Register maskTemp)
{
    switch (arrayType) {
      case Scalar::Int8:
      case Scalar::Uint8:
        masm.atomicEffectOp(1, op, value, mem, valueTemp, offsetTemp,
                            maskTemp);
        break;
      case Scalar::Int16:
      case Scalar::Uint16:
        masm.atomicEffectOp(2, op, value, mem, valueTemp, offsetTemp,
                            maskTemp);
        break;
      case Scalar::Int32:
      case Scalar::Uint32:
        masm.atomicEffectOp(4, op, value, mem, valueTemp, offsetTemp,
                            maskTemp);
        break;
      default:
        MOZ_CRASH("Invalid typed array type");
    }
}

template <typename T>
static void
AtomicBinopToTypedArray(MacroAssemblerLOONGARCH64Compat& masm, AtomicOp op,
                        Scalar::Type arrayType, const LAllocation* value,
                        const T& mem, Register outTemp, Register valueTemp,
                        Register offsetTemp, Register maskTemp,
                        AnyRegister output)
{
    if (value->isConstant()) {
        AtomicBinopToTypedIntArray(masm, op, arrayType, Imm32(ToInt32(value)),
                                   mem, outTemp, valueTemp, offsetTemp,
                                   maskTemp, output);
    } else {
        AtomicBinopToTypedIntArray(masm, op, arrayType, ToRegister(value),
                                   mem, outTemp, valueTemp, offsetTemp,
                                   maskTemp, output);
    }
}

template <typename T>
static void
AtomicBinopToTypedArray(MacroAssemblerLOONGARCH64Compat& masm, AtomicOp op,
                        Scalar::Type arrayType, const LAllocation* value,
                        const T& mem, Register valueTemp, Register offsetTemp,
                        Register maskTemp)
{
    if (value->isConstant()) {
        AtomicBinopToTypedIntArray(masm, op, arrayType, Imm32(ToInt32(value)),
                                   mem, valueTemp, offsetTemp, maskTemp);
    } else {
        AtomicBinopToTypedIntArray(masm, op, arrayType, ToRegister(value),
                                   mem, valueTemp, offsetTemp, maskTemp);
    }
}

void
CodeGeneratorLoongArch64::visitAsmJSCompareExchangeHeap(LAsmJSCompareExchangeHeap* ins)
{
    MAsmJSCompareExchangeHeap* mir = ins->mir();
    Scalar::Type vt = mir->access().type();
    Register ptrReg = ToRegister(ins->ptr());
    BaseIndex srcAddr(HeapReg, ptrReg, TimesOne);
    MOZ_ASSERT(ins->addrTemp()->isBogusTemp());

    Register oldval = ToRegister(ins->oldValue());
    Register newval = ToRegister(ins->newValue());
    Register valueTemp = ToRegister(ins->valueTemp());
    Register offsetTemp = ToRegister(ins->offsetTemp());
    Register maskTemp = ToRegister(ins->maskTemp());

    masm.compareExchangeToTypedIntArray(
        vt == Scalar::Uint32 ? Scalar::Int32 : vt, srcAddr, oldval, newval,
        InvalidReg, valueTemp, offsetTemp, maskTemp,
        ToAnyRegister(ins->output()));
}

void
CodeGeneratorLoongArch64::visitAsmJSAtomicExchangeHeap(LAsmJSAtomicExchangeHeap* ins)
{
    MAsmJSAtomicExchangeHeap* mir = ins->mir();
    Scalar::Type vt = mir->access().type();
    Register ptrReg = ToRegister(ins->ptr());
    Register value = ToRegister(ins->value());
    BaseIndex srcAddr(HeapReg, ptrReg, TimesOne);
    MOZ_ASSERT(ins->addrTemp()->isBogusTemp());

    Register valueTemp = ToRegister(ins->valueTemp());
    Register offsetTemp = ToRegister(ins->offsetTemp());
    Register maskTemp = ToRegister(ins->maskTemp());

    masm.atomicExchangeToTypedIntArray(
        vt == Scalar::Uint32 ? Scalar::Int32 : vt, srcAddr, value, InvalidReg,
        valueTemp, offsetTemp, maskTemp, ToAnyRegister(ins->output()));
}

void
CodeGeneratorLoongArch64::visitAsmJSAtomicBinopHeap(LAsmJSAtomicBinopHeap* ins)
{
    MOZ_ASSERT(ins->mir()->hasUses());
    MOZ_ASSERT(ins->addrTemp()->isBogusTemp());

    MAsmJSAtomicBinopHeap* mir = ins->mir();
    Scalar::Type vt = mir->access().type();
    Register ptrReg = ToRegister(ins->ptr());
    Register valueTemp = ToRegister(ins->valueTemp());
    Register offsetTemp = ToRegister(ins->offsetTemp());
    Register maskTemp = ToRegister(ins->maskTemp());
    const LAllocation* value = ins->value();
    AtomicOp op = mir->operation();

    BaseIndex srcAddr(HeapReg, ptrReg, TimesOne);

    AtomicBinopToTypedArray(masm, op,
                            vt == Scalar::Uint32 ? Scalar::Int32 : vt, value,
                            srcAddr, InvalidReg, valueTemp, offsetTemp,
                            maskTemp, ToAnyRegister(ins->output()));
}

void
CodeGeneratorLoongArch64::visitAsmJSAtomicBinopHeapForEffect(LAsmJSAtomicBinopHeapForEffect* ins)
{
    MOZ_ASSERT(!ins->mir()->hasUses());
    MOZ_ASSERT(ins->addrTemp()->isBogusTemp());

    MAsmJSAtomicBinopHeap* mir = ins->mir();
    Scalar::Type vt = mir->access().type();
    Register ptrReg = ToRegister(ins->ptr());
    Register valueTemp = ToRegister(ins->valueTemp());
    Register offsetTemp = ToRegister(ins->offsetTemp());
    Register maskTemp = ToRegister(ins->maskTemp());
    const LAllocation* value = ins->value();
    AtomicOp op = mir->operation();

    BaseIndex srcAddr(HeapReg, ptrReg, TimesOne);

    AtomicBinopToTypedArray(masm, op, vt, value, srcAddr, valueTemp,
                            offsetTemp, maskTemp);
}

void
CodeGeneratorLoongArch64::visitAtomicTypedArrayElementBinop(LAtomicTypedArrayElementBinop* lir)
{
    MOZ_ASSERT(lir->mir()->hasUses());

    AnyRegister output = ToAnyRegister(lir->output());
    Register elements = ToRegister(lir->elements());
    Register outTemp = lir->temp2()->isBogusTemp() ? InvalidReg
                                                   : ToRegister(lir->temp2());
    Register valueTemp = ToRegister(lir->valueTemp());
    Register offsetTemp = ToRegister(lir->offsetTemp());
    Register maskTemp = ToRegister(lir->maskTemp());
    const LAllocation* value = lir->value();

    Scalar::Type arrayType = lir->mir()->arrayType();
    int width = Scalar::byteSize(arrayType);

    if (lir->index()->isConstant()) {
        Address mem(elements, ToInt32(lir->index()) * width);
        AtomicBinopToTypedArray(masm, lir->mir()->operation(), arrayType, value,
                                mem, outTemp, valueTemp, offsetTemp, maskTemp,
                                output);
    } else {
        BaseIndex mem(elements, ToRegister(lir->index()),
                      ScaleFromElemWidth(width));
        AtomicBinopToTypedArray(masm, lir->mir()->operation(), arrayType, value,
                                mem, outTemp, valueTemp, offsetTemp, maskTemp,
                                output);
    }
}

void
CodeGeneratorLoongArch64::visitAtomicTypedArrayElementBinopForEffect(LAtomicTypedArrayElementBinopForEffect* lir)
{
    MOZ_ASSERT(!lir->mir()->hasUses());

    Register elements = ToRegister(lir->elements());
    Register valueTemp = ToRegister(lir->valueTemp());
    Register offsetTemp = ToRegister(lir->offsetTemp());
    Register maskTemp = ToRegister(lir->maskTemp());
    const LAllocation* value = lir->value();
    Scalar::Type arrayType = lir->mir()->arrayType();
    int width = Scalar::byteSize(arrayType);

    if (lir->index()->isConstant()) {
        Address mem(elements, ToInt32(lir->index()) * width);
        AtomicBinopToTypedArray(masm, lir->mir()->operation(), arrayType, value,
                                mem, valueTemp, offsetTemp, maskTemp);
    } else {
        BaseIndex mem(elements, ToRegister(lir->index()),
                      ScaleFromElemWidth(width));
        AtomicBinopToTypedArray(masm, lir->mir()->operation(), arrayType, value,
                                mem, valueTemp, offsetTemp, maskTemp);
    }
}

void
CodeGeneratorLoongArch64::visitCompareExchangeTypedArrayElement(LCompareExchangeTypedArrayElement* lir)
{
    Register elements = ToRegister(lir->elements());
    AnyRegister output = ToAnyRegister(lir->output());
    Register temp = lir->temp()->isBogusTemp() ? InvalidReg
                                               : ToRegister(lir->temp());

    Register oldval = ToRegister(lir->oldval());
    Register newval = ToRegister(lir->newval());
    Register valueTemp = ToRegister(lir->valueTemp());
    Register offsetTemp = ToRegister(lir->offsetTemp());
    Register maskTemp = ToRegister(lir->maskTemp());

    Scalar::Type arrayType = lir->mir()->arrayType();
    int width = Scalar::byteSize(arrayType);

    if (lir->index()->isConstant()) {
        Address dest(elements, ToInt32(lir->index()) * width);
        masm.compareExchangeToTypedIntArray(arrayType, dest, oldval, newval,
                                            temp, valueTemp, offsetTemp,
                                            maskTemp, output);
    } else {
        BaseIndex dest(elements, ToRegister(lir->index()),
                       ScaleFromElemWidth(width));
        masm.compareExchangeToTypedIntArray(arrayType, dest, oldval, newval,
                                            temp, valueTemp, offsetTemp,
                                            maskTemp, output);
    }
}

void
CodeGeneratorLoongArch64::visitAtomicExchangeTypedArrayElement(LAtomicExchangeTypedArrayElement* lir)
{
    Register elements = ToRegister(lir->elements());
    AnyRegister output = ToAnyRegister(lir->output());
    Register temp = lir->temp()->isBogusTemp() ? InvalidReg
                                               : ToRegister(lir->temp());

    Register value = ToRegister(lir->value());
    Register valueTemp = ToRegister(lir->valueTemp());
    Register offsetTemp = ToRegister(lir->offsetTemp());
    Register maskTemp = ToRegister(lir->maskTemp());

    Scalar::Type arrayType = lir->mir()->arrayType();
    int width = Scalar::byteSize(arrayType);

    if (lir->index()->isConstant()) {
        Address dest(elements, ToInt32(lir->index()) * width);
        masm.atomicExchangeToTypedIntArray(arrayType, dest, value, temp,
                                           valueTemp, offsetTemp, maskTemp,
                                           output);
    } else {
        BaseIndex dest(elements, ToRegister(lir->index()),
                       ScaleFromElemWidth(width));
        masm.atomicExchangeToTypedIntArray(arrayType, dest, value, temp,
                                           valueTemp, offsetTemp, maskTemp,
                                           output);
    }
}
