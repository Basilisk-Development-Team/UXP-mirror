/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef jit_loongarch64_CodeGenerator_loongarch64_h
#define jit_loongarch64_CodeGenerator_loongarch64_h

#define CodeGeneratorSpecific CodeGeneratorMIPS64Specific
#include "jit/mips64/CodeGenerator-mips64.h"
#undef CodeGeneratorSpecific

namespace js {
namespace jit {

class CodeGeneratorLoongArch64 : public CodeGeneratorMIPS64
{
  public:
    using CodeGeneratorMIPS64::CodeGeneratorMIPS64;

    void visitCompare(LCompare* comp);
    void visitOutOfLineTableSwitch(OutOfLineTableSwitch* ool);
    void visitCompareAndBranch(LCompareAndBranch* comp);
    void visitDivI(LDivI* ins);
    void visitModI(LModI* ins);
    void visitUDivOrMod(LUDivOrMod* ins);
    void visitDivOrModI64(LDivOrModI64* lir);
    void visitUDivOrModI64(LUDivOrModI64* lir);
    void visitWasmTruncateToInt32(LWasmTruncateToInt32* lir);
    void visitWasmTruncateToInt64(LWasmTruncateToInt64* lir);
    void visitWasmAddOffset(LWasmAddOffset* lir);
    void visitAsmJSCompareExchangeHeap(LAsmJSCompareExchangeHeap* ins);
    void visitAsmJSAtomicExchangeHeap(LAsmJSAtomicExchangeHeap* ins);
    void visitAsmJSAtomicBinopHeap(LAsmJSAtomicBinopHeap* ins);
    void visitAsmJSAtomicBinopHeapForEffect(LAsmJSAtomicBinopHeapForEffect* ins);
    void visitAtomicTypedArrayElementBinop(LAtomicTypedArrayElementBinop* lir);
    void visitAtomicTypedArrayElementBinopForEffect(LAtomicTypedArrayElementBinopForEffect* lir);
    void visitCompareExchangeTypedArrayElement(LCompareExchangeTypedArrayElement* lir);
    void visitAtomicExchangeTypedArrayElement(LAtomicExchangeTypedArrayElement* lir);

  protected:
    void emitTableSwitchDispatch(MTableSwitch* mir, Register index, Register base);
};

typedef CodeGeneratorLoongArch64 CodeGeneratorSpecific;

} // namespace jit
} // namespace js

#endif /* jit_loongarch64_CodeGenerator_loongarch64_h */
