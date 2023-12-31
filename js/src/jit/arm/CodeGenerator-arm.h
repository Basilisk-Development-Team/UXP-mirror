/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef jit_arm_CodeGenerator_arm_h
#define jit_arm_CodeGenerator_arm_h

#include "jit/arm/Assembler-arm.h"
#include "jit/shared/CodeGenerator-shared.h"

namespace js {
namespace jit {

class OutOfLineBailout;
class OutOfLineTableSwitch;

class CodeGeneratorARM : public CodeGeneratorShared
{
    friend class MoveResolverARM;

    CodeGeneratorARM* thisFromCtor() {return this;}

  protected:
    NonAssertingLabel deoptLabel_;

    MoveOperand toMoveOperand(LAllocation a) const;

    void bailoutIf(Assembler::Condition condition, LSnapshot* snapshot);
    void bailoutFrom(Label* label, LSnapshot* snapshot);
    void bailout(LSnapshot* snapshot);

    template <typename T1, typename T2>
    void bailoutCmpPtr(Assembler::Condition c, T1 lhs, T2 rhs, LSnapshot* snapshot) {
        masm.cmpPtr(lhs, rhs);
        bailoutIf(c, snapshot);
    }
    void bailoutTestPtr(Assembler::Condition c, Register lhs, Register rhs, LSnapshot* snapshot) {
        masm.testPtr(lhs, rhs);
        bailoutIf(c, snapshot);
    }
    template <typename T1, typename T2>
    void bailoutCmp32(Assembler::Condition c, T1 lhs, T2 rhs, LSnapshot* snapshot) {
        masm.cmp32(lhs, rhs);
        bailoutIf(c, snapshot);
    }
    template <typename T1, typename T2>
    void bailoutTest32(Assembler::Condition c, T1 lhs, T2 rhs, LSnapshot* snapshot) {
        masm.test32(lhs, rhs);
        bailoutIf(c, snapshot);
    }
    void bailoutIfFalseBool(Register reg, LSnapshot* snapshot) {
        masm.test32(reg, Imm32(0xFF));
        bailoutIf(Assembler::Zero, snapshot);
    }

    template<class T>
    void generateUDivModZeroCheck(Register rhs, Register output, Label* done, LSnapshot* snapshot,
                                  T* mir);

  protected:
    bool generateOutOfLineCode();

    void emitRoundDouble(FloatRegister src, Register dest, Label* fail);

    // Emits a branch that directs control flow to the true block if |cond| is
    // true, and the false block if |cond| is false.
    void emitBranch(Assembler::Condition cond, MBasicBlock* ifTrue, MBasicBlock* ifFalse);

    void testNullEmitBranch(Assembler::Condition cond, const ValueOperand& value,
                            MBasicBlock* ifTrue, MBasicBlock* ifFalse)
    {
        cond = masm.testNull(cond, value);
        emitBranch(cond, ifTrue, ifFalse);
    }
    void testUndefinedEmitBranch(Assembler::Condition cond, const ValueOperand& value,
                                 MBasicBlock* ifTrue, MBasicBlock* ifFalse)
    {
        cond = masm.testUndefined(cond, value);
        emitBranch(cond, ifTrue, ifFalse);
    }
    void testObjectEmitBranch(Assembler::Condition cond, const ValueOperand& value,
                              MBasicBlock* ifTrue, MBasicBlock* ifFalse)
    {
        cond = masm.testObject(cond, value);
        emitBranch(cond, ifTrue, ifFalse);
    }
    void testZeroEmitBranch(Assembler::Condition cond, Register reg,
                            MBasicBlock* ifTrue, MBasicBlock* ifFalse)
    {
        MOZ_ASSERT(cond == Assembler::Equal || cond == Assembler::NotEqual);
        masm.cmpPtr(reg, ImmWord(0));
        emitBranch(cond, ifTrue, ifFalse);
    }

    void emitTableSwitchDispatch(MTableSwitch* mir, Register index, Register base);

    template <typename T>
    void emitWasmLoad(T* ins);
    template <typename T>
    void emitWasmUnalignedLoad(T* ins);
    template <typename T>
    void emitWasmStore(T* ins);
    template <typename T>
    void emitWasmUnalignedStore(T* ins);

  public:
    // Instruction visitors.
    virtual void visitMinMaxD(LMinMaxD* ins);
    virtual void visitMinMaxF(LMinMaxF* ins);
    virtual void visitAbsD(LAbsD* ins);
    virtual void visitAbsF(LAbsF* ins);
    virtual void visitSqrtD(LSqrtD* ins);
    virtual void visitSqrtF(LSqrtF* ins);
    virtual void visitAddI(LAddI* ins);
    virtual void visitSubI(LSubI* ins);
    virtual void visitBitNotI(LBitNotI* ins);
    virtual void visitBitOpI(LBitOpI* ins);

    virtual void visitMulI(LMulI* ins);

    virtual void visitDivI(LDivI* ins);
    virtual void visitSoftDivI(LSoftDivI* ins);
    virtual void visitDivPowTwoI(LDivPowTwoI* ins);
    virtual void visitModI(LModI* ins);
    virtual void visitSoftModI(LSoftModI* ins);
    virtual void visitModPowTwoI(LModPowTwoI* ins);
    virtual void visitModMaskI(LModMaskI* ins);
    virtual void visitPowHalfD(LPowHalfD* ins);
    virtual void visitShiftI(LShiftI* ins);
    virtual void visitShiftI64(LShiftI64* ins);
    virtual void visitUrshD(LUrshD* ins);

    virtual void visitClzI(LClzI* ins);
    virtual void visitCtzI(LCtzI* ins);
    virtual void visitPopcntI(LPopcntI* ins);

    virtual void visitTestIAndBranch(LTestIAndBranch* test);
    virtual void visitCompare(LCompare* comp);
    virtual void visitCompareAndBranch(LCompareAndBranch* comp);
    virtual void visitTestDAndBranch(LTestDAndBranch* test);
    virtual void visitTestFAndBranch(LTestFAndBranch* test);
    virtual void visitCompareD(LCompareD* comp);
    virtual void visitCompareF(LCompareF* comp);
    virtual void visitCompareDAndBranch(LCompareDAndBranch* comp);
    virtual void visitCompareFAndBranch(LCompareFAndBranch* comp);
    virtual void visitCompareB(LCompareB* lir);
    virtual void visitCompareBAndBranch(LCompareBAndBranch* lir);
    virtual void visitCompareBitwise(LCompareBitwise* lir);
    virtual void visitCompareBitwiseAndBranch(LCompareBitwiseAndBranch* lir);
    virtual void visitBitAndAndBranch(LBitAndAndBranch* baab);
    virtual void visitWasmUint32ToDouble(LWasmUint32ToDouble* lir);
    virtual void visitWasmUint32ToFloat32(LWasmUint32ToFloat32* lir);
    virtual void visitNotI(LNotI* ins);
    virtual void visitNotD(LNotD* ins);
    virtual void visitNotF(LNotF* ins);

    virtual void visitMathD(LMathD* math);
    virtual void visitMathF(LMathF* math);
    virtual void visitFloor(LFloor* lir);
    virtual void visitFloorF(LFloorF* lir);
    virtual void visitCeil(LCeil* lir);
    virtual void visitCeilF(LCeilF* lir);
    virtual void visitRound(LRound* lir);
    virtual void visitRoundF(LRoundF* lir);
    virtual void visitTruncateDToInt32(LTruncateDToInt32* ins);
    virtual void visitTruncateFToInt32(LTruncateFToInt32* ins);

    virtual void visitWrapInt64ToInt32(LWrapInt64ToInt32* lir);
    virtual void visitExtendInt32ToInt64(LExtendInt32ToInt64* lir);
    virtual void visitSignExtendInt64(LSignExtendInt64* ins);
    virtual void visitAddI64(LAddI64* lir);
    virtual void visitSubI64(LSubI64* lir);
    virtual void visitMulI64(LMulI64* lir);
    virtual void visitDivOrModI64(LDivOrModI64* lir);
    virtual void visitUDivOrModI64(LUDivOrModI64* lir);
    virtual void visitCompareI64(LCompareI64* lir);
    virtual void visitCompareI64AndBranch(LCompareI64AndBranch* lir);
    virtual void visitBitOpI64(LBitOpI64* lir);
    virtual void visitRotateI64(LRotateI64* lir);
    virtual void visitWasmStackArgI64(LWasmStackArgI64* lir);
    virtual void visitWasmSelectI64(LWasmSelectI64* lir);
    virtual void visitWasmReinterpretFromI64(LWasmReinterpretFromI64* lir);
    virtual void visitWasmReinterpretToI64(LWasmReinterpretToI64* lir);
    virtual void visitPopcntI64(LPopcntI64* ins);
    virtual void visitClzI64(LClzI64* ins);
    virtual void visitCtzI64(LCtzI64* ins);
    virtual void visitNotI64(LNotI64* ins);
    virtual void visitWasmTruncateToInt64(LWasmTruncateToInt64* ins);
    virtual void visitInt64ToFloatingPointCall(LInt64ToFloatingPointCall* lir);
    virtual void visitTestI64AndBranch(LTestI64AndBranch* lir);

    // Out of line visitors.
    void visitOutOfLineBailout(OutOfLineBailout* ool);
    void visitOutOfLineTableSwitch(OutOfLineTableSwitch* ool);

  protected:
    ValueOperand ToValue(LInstruction* ins, size_t pos);
    ValueOperand ToOutValue(LInstruction* ins);
    ValueOperand ToTempValue(LInstruction* ins, size_t pos);

    Register64 ToOperandOrRegister64(const LInt64Allocation input);

    // Functions for LTestVAndBranch.
    Register splitTagForTest(const ValueOperand& value);

    void divICommon(MDiv* mir, Register lhs, Register rhs, Register output, LSnapshot* snapshot,
                    Label& done);
    void modICommon(MMod* mir, Register lhs, Register rhs, Register output, LSnapshot* snapshot,
                    Label& done);

  public:
    CodeGeneratorARM(MIRGenerator* gen, LIRGraph* graph, MacroAssembler* masm);

  public:
    void visitBox(LBox* box);
    void visitBoxFloatingPoint(LBoxFloatingPoint* box);
    void visitUnbox(LUnbox* unbox);
    void visitValue(LValue* value);
    void visitDouble(LDouble* ins);
    void visitFloat32(LFloat32* ins);

    void visitGuardShape(LGuardShape* guard);
    void visitGuardObjectGroup(LGuardObjectGroup* guard);
    void visitGuardClass(LGuardClass* guard);

    void visitNegI(LNegI* lir);
    void visitNegD(LNegD* lir);
    void visitNegF(LNegF* lir);
    void visitLoadTypedArrayElementStatic(LLoadTypedArrayElementStatic* ins);
    void visitStoreTypedArrayElementStatic(LStoreTypedArrayElementStatic* ins);
    void visitAtomicTypedArrayElementBinop(LAtomicTypedArrayElementBinop* lir);
    void visitAtomicTypedArrayElementBinopForEffect(LAtomicTypedArrayElementBinopForEffect* lir);
    void visitCompareExchangeTypedArrayElement(LCompareExchangeTypedArrayElement* lir);
    void visitAtomicExchangeTypedArrayElement(LAtomicExchangeTypedArrayElement* lir);
    void visitWasmSelect(LWasmSelect* ins);
    void visitWasmReinterpret(LWasmReinterpret* ins);
    void emitWasmCall(LWasmCallBase* ins);
    void visitWasmCall(LWasmCall* ins);
    void visitWasmCallI64(LWasmCallI64* ins);
    void visitWasmLoad(LWasmLoad* ins);
    void visitWasmLoadI64(LWasmLoadI64* ins);
    void visitWasmUnalignedLoad(LWasmUnalignedLoad* ins);
    void visitWasmUnalignedLoadI64(LWasmUnalignedLoadI64* ins);
    void visitWasmAddOffset(LWasmAddOffset* ins);
    void visitWasmStore(LWasmStore* ins);
    void visitWasmStoreI64(LWasmStoreI64* ins);
    void visitWasmUnalignedStore(LWasmUnalignedStore* ins);
    void visitWasmUnalignedStoreI64(LWasmUnalignedStoreI64* ins);
    void visitWasmLoadGlobalVar(LWasmLoadGlobalVar* ins);
    void visitWasmLoadGlobalVarI64(LWasmLoadGlobalVarI64* ins);
    void visitWasmStoreGlobalVar(LWasmStoreGlobalVar* ins);
    void visitWasmStoreGlobalVarI64(LWasmStoreGlobalVarI64* ins);
    void visitAsmJSLoadHeap(LAsmJSLoadHeap* ins);
    void visitAsmJSStoreHeap(LAsmJSStoreHeap* ins);
    void visitAsmJSCompareExchangeHeap(LAsmJSCompareExchangeHeap* ins);
    void visitAsmJSCompareExchangeCallout(LAsmJSCompareExchangeCallout* ins);
    void visitAsmJSAtomicExchangeHeap(LAsmJSAtomicExchangeHeap* ins);
    void visitAsmJSAtomicExchangeCallout(LAsmJSAtomicExchangeCallout* ins);
    void visitAsmJSAtomicBinopHeap(LAsmJSAtomicBinopHeap* ins);
    void visitAsmJSAtomicBinopHeapForEffect(LAsmJSAtomicBinopHeapForEffect* ins);
    void visitAsmJSAtomicBinopCallout(LAsmJSAtomicBinopCallout* ins);
    void visitWasmStackArg(LWasmStackArg* ins);
    void visitWasmTruncateToInt32(LWasmTruncateToInt32* ins);
    void visitOutOfLineWasmTruncateCheck(OutOfLineWasmTruncateCheck* ool);
    void visitCopySignD(LCopySignD* ins);
    void visitCopySignF(LCopySignF* ins);

    void visitMemoryBarrier(LMemoryBarrier* ins);

    void generateInvalidateEpilogue();

    void setReturnDoubleRegs(LiveRegisterSet* regs);

    // Generating a result.
    template<typename S, typename T>
    void atomicBinopToTypedIntArray(AtomicOp op, Scalar::Type arrayType, const S& value,
                                    const T& mem, Register flagTemp, Register outTemp,
                                    AnyRegister output);

    // Generating no result.
    template<typename S, typename T>
    void atomicBinopToTypedIntArray(AtomicOp op, Scalar::Type arrayType, const S& value,
                                    const T& mem, Register flagTemp);

  protected:
    void visitEffectiveAddress(LEffectiveAddress* ins);
    void visitUDiv(LUDiv* ins);
    void visitUMod(LUMod* ins);
    void visitSoftUDivOrMod(LSoftUDivOrMod* ins);
};

typedef CodeGeneratorARM CodeGeneratorSpecific;

// An out-of-line bailout thunk.
class OutOfLineBailout : public OutOfLineCodeBase<CodeGeneratorARM>
{
  protected: // Silence Clang warning.
    LSnapshot* snapshot_;
    uint32_t frameSize_;

  public:
    OutOfLineBailout(LSnapshot* snapshot, uint32_t frameSize)
      : snapshot_(snapshot),
        frameSize_(frameSize)
    { }

    void accept(CodeGeneratorARM* codegen);

    LSnapshot* snapshot() const {
        return snapshot_;
    }
};

} // namespace jit
} // namespace js

#endif /* jit_arm_CodeGenerator_arm_h */
