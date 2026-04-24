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

void
CodeGeneratorLoongArch64::visitAsmJSCompareExchangeHeap(LAsmJSCompareExchangeHeap* ins)
{
    (void)ins;
    MOZ_CRASH("asm.js atomics are not supported on loongarch64 yet");
}

void
CodeGeneratorLoongArch64::visitAsmJSAtomicExchangeHeap(LAsmJSAtomicExchangeHeap* ins)
{
    (void)ins;
    MOZ_CRASH("asm.js atomics are not supported on loongarch64 yet");
}

void
CodeGeneratorLoongArch64::visitAsmJSAtomicBinopHeap(LAsmJSAtomicBinopHeap* ins)
{
    (void)ins;
    MOZ_CRASH("asm.js atomics are not supported on loongarch64 yet");
}

void
CodeGeneratorLoongArch64::visitAsmJSAtomicBinopHeapForEffect(LAsmJSAtomicBinopHeapForEffect* ins)
{
    (void)ins;
    MOZ_CRASH("asm.js atomics are not supported on loongarch64 yet");
}

void
CodeGeneratorLoongArch64::visitAtomicTypedArrayElementBinop(LAtomicTypedArrayElementBinop* lir)
{
    (void)lir;
    MOZ_CRASH("atomics are not supported on loongarch64 Ion yet");
}

void
CodeGeneratorLoongArch64::visitAtomicTypedArrayElementBinopForEffect(LAtomicTypedArrayElementBinopForEffect* lir)
{
    (void)lir;
    MOZ_CRASH("atomics are not supported on loongarch64 Ion yet");
}

void
CodeGeneratorLoongArch64::visitCompareExchangeTypedArrayElement(LCompareExchangeTypedArrayElement* lir)
{
    (void)lir;
    MOZ_CRASH("atomics are not supported on loongarch64 Ion yet");
}

void
CodeGeneratorLoongArch64::visitAtomicExchangeTypedArrayElement(LAtomicExchangeTypedArrayElement* lir)
{
    (void)lir;
    MOZ_CRASH("atomics are not supported on loongarch64 Ion yet");
}
