/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#include "jit/IonCaches.h"

#include "mozilla/SizePrintfMacros.h"
#include "mozilla/TemplateLib.h"

#include "jstypes.h"

#include "builtin/TypedObject.h"
#include "jit/BaselineIC.h"
#include "jit/Ion.h"
#include "jit/JitcodeMap.h"
#include "jit/JitSpewer.h"
#include "jit/Linker.h"
#include "jit/Lowering.h"
#ifdef JS_ION_PERF
# include "jit/PerfSpewer.h"
#endif
#include "jit/VMFunctions.h"
#include "js/Proxy.h"
#include "proxy/Proxy.h"
#include "vm/Shape.h"
#include "vm/Stack.h"

#include "jit/JitFrames-inl.h"
#include "jit/MacroAssembler-inl.h"
#include "jit/shared/Lowering-shared-inl.h"
#include "vm/Interpreter-inl.h"
#include "vm/Shape-inl.h"
#include "vm/UnboxedObject-inl.h"

using namespace js;
using namespace js::jit;

using mozilla::tl::FloorLog2;

typedef Rooted<TypedArrayObject*> RootedTypedArrayObject;

void
CodeLocationJump::repoint(JitCode* code, MacroAssembler* masm)
{
    MOZ_ASSERT(state_ == Relative);
    size_t new_off = (size_t)raw_;
#ifdef JS_SMALL_BRANCH
    size_t jumpTableEntryOffset = reinterpret_cast<size_t>(jumpTableEntry_);
#endif
    if (masm != nullptr) {
#ifdef JS_CODEGEN_X64
        MOZ_ASSERT((uint64_t)raw_ <= UINT32_MAX);
#endif
        new_off = (uintptr_t)raw_;
#ifdef JS_SMALL_BRANCH
        jumpTableEntryOffset = masm->actualIndex(jumpTableEntryOffset);
#endif
    }
    raw_ = code->raw() + new_off;
#ifdef JS_SMALL_BRANCH
    jumpTableEntry_ = Assembler::PatchableJumpAddress(code, (size_t) jumpTableEntryOffset);
#endif
    setAbsolute();
}

void
CodeLocationLabel::repoint(JitCode* code, MacroAssembler* masm)
{
     MOZ_ASSERT(state_ == Relative);
     size_t new_off = (size_t)raw_;
     if (masm != nullptr) {
#ifdef JS_CODEGEN_X64
        MOZ_ASSERT((uint64_t)raw_ <= UINT32_MAX);
#endif
        new_off = (uintptr_t)raw_;
     }
     MOZ_ASSERT(new_off < code->instructionsSize());

     raw_ = code->raw() + new_off;
     setAbsolute();
}

void
CodeOffsetJump::fixup(MacroAssembler* masm)
{
#ifdef JS_SMALL_BRANCH
     jumpTableIndex_ = masm->actualIndex(jumpTableIndex_);
#endif
}

const char*
IonCache::CacheName(IonCache::Kind kind)
{
    static const char * const names[] =
    {
#define NAME(x) #x,
        IONCACHE_KIND_LIST(NAME)
#undef NAME
    };
    return names[kind];
}

const size_t IonCache::MAX_STUBS = 16;

// Helper class which encapsulates logic to attach a stub to an IC by hooking
// up rejoins and next stub jumps.
//
// The simplest stubs have a single jump to the next stub and look like the
// following:
//
//    branch guard NEXTSTUB
//    ... IC-specific code ...
//    jump REJOIN
//
// This corresponds to:
//
//    attacher.branchNextStub(masm, ...);
//    ... emit IC-specific code ...
//    attacher.jumpRejoin(masm);
//
// Whether the stub needs multiple next stub jumps look like:
//
//   branch guard FAILURES
//   ... IC-specific code ...
//   branch another-guard FAILURES
//   ... IC-specific code ...
//   jump REJOIN
//   FAILURES:
//   jump NEXTSTUB
//
// This corresponds to:
//
//   Label failures;
//   masm.branchX(..., &failures);
//   ... emit IC-specific code ...
//   masm.branchY(..., failures);
//   ... emit more IC-specific code ...
//   attacher.jumpRejoin(masm);
//   masm.bind(&failures);
//   attacher.jumpNextStub(masm);
//
// A convenience function |branchNextStubOrLabel| is provided in the case that
// the stub sometimes has multiple next stub jumps and sometimes a single
// one. If a non-nullptr label is passed in, a |branchPtr| will be made to
// that label instead of a |branchPtrWithPatch| to the next stub.
class IonCache::StubAttacher
{
  protected:
    bool hasNextStubOffset_ : 1;
    bool hasStubCodePatchOffset_ : 1;

    IonCache& cache_;

    CodeLocationLabel rejoinLabel_;
    CodeOffsetJump nextStubOffset_;
    CodeOffsetJump rejoinOffset_;
    CodeOffset stubCodePatchOffset_;

  public:
    explicit StubAttacher(IonCache& cache)
      : hasNextStubOffset_(false),
        hasStubCodePatchOffset_(false),
        cache_(cache),
        rejoinLabel_(cache.rejoinLabel_),
        nextStubOffset_(),
        rejoinOffset_(),
        stubCodePatchOffset_()
    { }

    // Value used instead of the JitCode self-reference of generated
    // stubs. This value is needed for marking calls made inside stubs. This
    // value would be replaced by the attachStub function after the allocation
    // of the JitCode. The self-reference is used to keep the stub path alive
    // even if the IonScript is invalidated or if the IC is flushed.
    static const void* const STUB_ADDR;

    template <class T1, class T2>
    void branchNextStub(MacroAssembler& masm, Assembler::Condition cond, T1 op1, T2 op2) {
        MOZ_ASSERT(!hasNextStubOffset_);
        RepatchLabel nextStub;
        nextStubOffset_ = masm.branchPtrWithPatch(cond, op1, op2, &nextStub);
        hasNextStubOffset_ = true;
        masm.bind(&nextStub);
    }

    template <class T1, class T2>
    void branchNextStubOrLabel(MacroAssembler& masm, Assembler::Condition cond, T1 op1, T2 op2,
                               Label* label)
    {
        if (label != nullptr)
            masm.branchPtr(cond, op1, op2, label);
        else
            branchNextStub(masm, cond, op1, op2);
    }

    void jumpRejoin(MacroAssembler& masm) {
        RepatchLabel rejoin;
        rejoinOffset_ = masm.jumpWithPatch(&rejoin);
        masm.bind(&rejoin);
    }

    void jumpNextStub(MacroAssembler& masm) {
        MOZ_ASSERT(!hasNextStubOffset_);
        RepatchLabel nextStub;
        nextStubOffset_ = masm.jumpWithPatch(&nextStub);
        hasNextStubOffset_ = true;
        masm.bind(&nextStub);
    }

    void pushStubCodePointer(MacroAssembler& masm) {
        // Push the JitCode pointer for the stub we're generating.
        // WARNING:
        // WARNING: If JitCode ever becomes relocatable, the following code is incorrect.
        // WARNING: Note that we're not marking the pointer being pushed as an ImmGCPtr.
        // WARNING: This location will be patched with the pointer of the generated stub,
        // WARNING: such as it can be marked when a call is made with this stub. Be aware
        // WARNING: that ICs are not marked and so this stub will only be kept alive iff
        // WARNING: it is on the stack at the time of the GC. No ImmGCPtr is needed as the
        // WARNING: stubs are flushed on GC.
        // WARNING:
        MOZ_ASSERT(!hasStubCodePatchOffset_);
        stubCodePatchOffset_ = masm.PushWithPatch(ImmPtr(STUB_ADDR));
        hasStubCodePatchOffset_ = true;
    }

    void patchRejoinJump(MacroAssembler& masm, JitCode* code) {
        rejoinOffset_.fixup(&masm);
        CodeLocationJump rejoinJump(code, rejoinOffset_);
        PatchJump(rejoinJump, rejoinLabel_);
    }

    void patchStubCodePointer(JitCode* code) {
        if (hasStubCodePatchOffset_) {
            Assembler::PatchDataWithValueCheck(CodeLocationLabel(code, stubCodePatchOffset_),
                                               ImmPtr(code), ImmPtr(STUB_ADDR));
        }
    }

    void patchNextStubJump(MacroAssembler& masm, JitCode* code) {
        // If this path is not taken, we are producing an entry which can no
        // longer go back into the update function.
        if (hasNextStubOffset_) {
            nextStubOffset_.fixup(&masm);
            CodeLocationJump nextStubJump(code, nextStubOffset_);
            PatchJump(nextStubJump, cache_.fallbackLabel_);

            // When the last stub fails, it fallback to the ool call which can
            // produce a stub. Next time we generate a stub, we will patch the
            // nextStub jump to try the new stub.
            cache_.lastJump_ = nextStubJump;
        }
    }
};

const void* const IonCache::StubAttacher::STUB_ADDR = (void*)0xdeadc0de;

void
IonCache::emitInitialJump(MacroAssembler& masm, RepatchLabel& entry)
{
    initialJump_ = masm.jumpWithPatch(&entry);
    lastJump_ = initialJump_;
    Label label;
    masm.bind(&label);
    rejoinLabel_ = CodeOffset(label.offset());
}

void
IonCache::attachStub(MacroAssembler& masm, StubAttacher& attacher, CodeLocationJump lastJump,
                     Handle<JitCode*> code)
{
    MOZ_ASSERT(canAttachStub());
    incrementStubCount();

    // Patch the previous nextStubJump of the last stub, or the jump from the
    // codeGen, to jump into the newly allocated code.
    PatchJump(lastJump, CodeLocationLabel(code), Reprotect);
}

IonCache::LinkStatus
IonCache::linkCode(JSContext* cx, MacroAssembler& masm, StubAttacher& attacher, IonScript* ion,
                   JitCode** code)
{
    Linker linker(masm);
    *code = linker.newCode<CanGC>(cx, ION_CODE);
    if (!*code)
        return LINK_ERROR;

    if (ion->invalidated())
        return CACHE_FLUSHED;

    // Update the success path to continue after the IC initial jump.
    attacher.patchRejoinJump(masm, *code);

    // Replace the STUB_ADDR constant by the address of the generated stub, such
    // as it can be kept alive even if the cache is flushed (see
    // MarkJitExitFrame).
    attacher.patchStubCodePointer(*code);

    // Update the failure path.
    attacher.patchNextStubJump(masm, *code);

    return LINK_GOOD;
}

bool
IonCache::linkAndAttachStub(JSContext* cx, MacroAssembler& masm, StubAttacher& attacher,
                            IonScript* ion, const char* attachKind,
                            JS::TrackedOutcome trackedOutcome)
{
    CodeLocationJump lastJumpBefore = lastJump_;
    Rooted<JitCode*> code(cx);
    {
        // Need to exit the AutoFlushICache context to flush the cache
        // before attaching the stub below.
        AutoFlushICache afc("IonCache");
        LinkStatus status = linkCode(cx, masm, attacher, ion, code.address());
        if (status != LINK_GOOD)
            return status != LINK_ERROR;
    }

    if (pc_) {
        JitSpew(JitSpew_IonIC, "Cache %p(%s:%" PRIuSIZE "/%" PRIuSIZE ") generated %s %s stub at %p",
                this, script_->filename(), script_->lineno(), script_->pcToOffset(pc_),
                attachKind, CacheName(kind()), code->raw());
    } else {
        JitSpew(JitSpew_IonIC, "Cache %p generated %s %s stub at %p",
                this, attachKind, CacheName(kind()), code->raw());
    }

#ifdef JS_ION_PERF
    writePerfSpewerJitCodeProfile(code, "IonCache");
#endif

    attachStub(masm, attacher, lastJumpBefore, code);

    // Add entry to native => bytecode mapping for this stub if needed.
    if (cx->runtime()->jitRuntime()->isProfilerInstrumentationEnabled(cx->runtime())) {
        JitcodeGlobalEntry::IonCacheEntry entry;
        entry.init(code, code->raw(), code->rawEnd(), rejoinAddress(), trackedOutcome);

        // Add entry to the global table.
        JitcodeGlobalTable* globalTable = cx->runtime()->jitRuntime()->getJitcodeGlobalTable();
        if (!globalTable->addEntry(entry, cx->runtime())) {
            entry.destroy();
            ReportOutOfMemory(cx);
            return false;
        }

        // Mark the jitcode as having a bytecode map.
        code->setHasBytecodeMap();
    } else {
        JitcodeGlobalEntry::DummyEntry entry;
        entry.init(code, code->raw(), code->rawEnd());

        // Add entry to the global table.
        JitcodeGlobalTable* globalTable = cx->runtime()->jitRuntime()->getJitcodeGlobalTable();
        if (!globalTable->addEntry(entry, cx->runtime())) {
            entry.destroy();
            ReportOutOfMemory(cx);
            return false;
        }

        // Mark the jitcode as having a bytecode map.
        code->setHasBytecodeMap();
    }

    // Report masm OOM errors here, so all our callers can:
    // return linkAndAttachStub(...);
    if (masm.oom()) {
        ReportOutOfMemory(cx);
        return false;
    }

    return true;
}

void
IonCache::updateBaseAddress(JitCode* code, MacroAssembler& masm)
{
    fallbackLabel_.repoint(code, &masm);
    initialJump_.repoint(code, &masm);
    lastJump_.repoint(code, &masm);
    rejoinLabel_.repoint(code, &masm);
}

void
IonCache::trace(JSTracer* trc)
{
    if (script_)
        TraceManuallyBarrieredEdge(trc, &script_, "IonCache::script_");
}

void*
jit::GetReturnAddressToIonCode(JSContext* cx)
{
    JitFrameIterator iter(cx);
    MOZ_ASSERT(iter.type() == JitFrame_Exit,
               "An exit frame is expected as update functions are called with a VMFunction.");

    void* returnAddr = iter.returnAddress();
#ifdef DEBUG
    ++iter;
    MOZ_ASSERT(iter.isIonJS());
#endif
    return returnAddr;
}

static void
GeneratePrototypeGuards(JSContext* cx, IonScript* ion, MacroAssembler& masm, JSObject* obj,
                        JSObject* holder, Register objectReg, Register scratchReg,
                        Label* failures)
{
    /*
     * The guards here protect against the effects of JSObject::swap(). If the prototype chain
     * is directly altered, then TI will toss the jitcode, so we don't have to worry about
     * it, and any other change to the holder, or adding a shadowing property will result
     * in reshaping the holder, and thus the failure of the shape guard.
     */
    MOZ_ASSERT(obj != holder);

    if (obj->hasUncacheableProto()) {
        // Note: objectReg and scratchReg may be the same register, so we cannot
        // use objectReg in the rest of this function.
        masm.loadPtr(Address(objectReg, JSObject::offsetOfGroup()), scratchReg);
        Address proto(scratchReg, ObjectGroup::offsetOfProto());
        masm.branchPtr(Assembler::NotEqual, proto, ImmGCPtr(obj->staticPrototype()), failures);
    }

    JSObject* pobj = obj->staticPrototype();
    if (!pobj)
        return;
    while (pobj != holder) {
        if (pobj->hasUncacheableProto()) {
            masm.movePtr(ImmGCPtr(pobj), scratchReg);
            Address groupAddr(scratchReg, JSObject::offsetOfGroup());
            if (pobj->isSingleton()) {
                // Singletons can have their group's |proto| mutated directly.
                masm.loadPtr(groupAddr, scratchReg);
                Address protoAddr(scratchReg, ObjectGroup::offsetOfProto());
                masm.branchPtr(Assembler::NotEqual, protoAddr, ImmGCPtr(pobj->staticPrototype()),
                              failures);
            } else {
                masm.branchPtr(Assembler::NotEqual, groupAddr, ImmGCPtr(pobj->group()), failures);
            }
        }

        pobj = pobj->staticPrototype();
    }
}

// Note: This differs from IsCacheableProtoChain in BaselineIC.cpp in that
// Ion caches can deal with objects on the proto chain that have uncacheable
// prototypes.
bool
jit::IsCacheableProtoChainForIonOrCacheIR(JSObject* obj, JSObject* holder)
{
    while (obj != holder) {
        /*
         * We cannot assume that we find the holder object on the prototype
         * chain and must check for null proto. The prototype chain can be
         * altered during the lookupProperty call.
         */
        JSObject* proto = obj->staticPrototype();
        if (!proto || !proto->isNative())
            return false;
        obj = proto;
    }
    return true;
}

bool
jit::IsCacheableGetPropReadSlotForIonOrCacheIR(JSObject* obj, JSObject* holder, PropertyResult prop)
{
    if (!prop || !IsCacheableProtoChainForIonOrCacheIR(obj, holder))
        return false;

    Shape* shape = prop.shape();
    if (!shape->hasSlot() || !shape->hasDefaultGetter())
        return false;

    return true;
}

static bool
IsCacheableNoProperty(JSObject* obj, JSObject* holder, PropertyResult prop, jsbytecode* pc,
                      const TypedOrValueRegister& output)
{
    if (prop)
        return false;

    MOZ_ASSERT(!holder);

    // Just because we didn't find the property on the object doesn't mean it
    // won't magically appear through various engine hacks.
    if (obj->getClass()->getGetProperty())
        return false;

    // Don't generate missing property ICs if we skipped a non-native object, as
    // lookups may extend beyond the prototype chain (e.g. for DOMProxy
    // proxies).
    JSObject* obj2 = obj;
    while (obj2) {
        if (!obj2->isNative())
            return false;
        obj2 = obj2->staticPrototype();
    }

    // The pc is nullptr if the cache is idempotent. We cannot share missing
    // properties between caches because TI can only try to prove that a type is
    // contained, but does not attempts to check if something does not exists.
    // So the infered type of getprop would be missing and would not contain
    // undefined, as expected for missing properties.
    if (!pc)
        return false;

    // TI has not yet monitored an Undefined value. The fallback path will
    // monitor and invalidate the script.
    if (!output.hasValue())
        return false;

    return true;
}

static bool
IsOptimizableArgumentsObjectForLength(JSObject* obj)
{
    if (!obj->is<ArgumentsObject>())
        return false;

    if (obj->as<ArgumentsObject>().hasOverriddenLength())
        return false;

    return true;
}

static bool
IsOptimizableArgumentsObjectForGetElem(JSObject* obj, const Value& idval)
{
    if (!IsOptimizableArgumentsObjectForLength(obj))
        return false;

    ArgumentsObject& argsObj = obj->as<ArgumentsObject>();

    if (argsObj.isAnyElementDeleted())
        return false;

    if (argsObj.hasOverriddenElement())
        return false;

    if (!idval.isInt32())
        return false;

    int32_t idint = idval.toInt32();
    if (idint < 0 || static_cast<uint32_t>(idint) >= argsObj.initialLength())
        return false;

    return true;
}

bool
jit::IsCacheableGetPropCallNative(JSObject* obj, JSObject* holder, Shape* shape)
{
    if (!shape || !IsCacheableProtoChainForIonOrCacheIR(obj, holder))
        return false;

    if (!shape->hasGetterValue() || !shape->getterValue().isObject())
        return false;

    if (!shape->getterValue().toObject().is<JSFunction>())
        return false;

    JSFunction& getter = shape->getterValue().toObject().as<JSFunction>();
    if (!getter.isNative())
        return false;

    // Check for a getter that has jitinfo and whose jitinfo says it's
    // OK with both inner and outer objects.
    if (getter.jitInfo() && !getter.jitInfo()->needsOuterizedThisObject())
        return true;

    // For getters that need the WindowProxy (instead of the Window) as this
    // object, don't cache if obj is the Window, since our cache will pass that
    // instead of the WindowProxy.
    return !IsWindow(obj);
}

bool
jit::IsCacheableGetPropCallScripted(JSObject* obj, JSObject* holder, Shape* shape,
                                    bool* isTemporarilyUnoptimizable)
{
    if (!shape || !IsCacheableProtoChainForIonOrCacheIR(obj, holder))
        return false;

    if (!shape->hasGetterValue() || !shape->getterValue().isObject())
        return false;

    if (!shape->getterValue().toObject().is<JSFunction>())
        return false;

    // See IsCacheableGetPropCallNative.
    if (IsWindow(obj))
        return false;

    JSFunction& getter = shape->getterValue().toObject().as<JSFunction>();
    if (getter.isNative())
        return false;

    if (!getter.hasJITCode()) {
        if (isTemporarilyUnoptimizable)
            *isTemporarilyUnoptimizable = true;
        return false;
    }

    return true;
}

static bool
IsCacheableGetPropCallPropertyOp(JSObject* obj, JSObject* holder, Shape* shape)
{
    if (!shape || !IsCacheableProtoChainForIonOrCacheIR(obj, holder))
        return false;

    if (shape->hasSlot() || shape->hasGetterValue() || shape->hasDefaultGetter())
        return false;

    return true;
}

static void
TestMatchingReceiver(MacroAssembler& masm, IonCache::StubAttacher& attacher,
                     Register object, JSObject* obj, Label* failure,
                     bool alwaysCheckGroup = false)
{
    if (obj->is<UnboxedPlainObject>()) {
        MOZ_ASSERT(failure);

        masm.branchTestObjGroup(Assembler::NotEqual, object, obj->group(), failure);
        Address expandoAddress(object, UnboxedPlainObject::offsetOfExpando());
        if (UnboxedExpandoObject* expando = obj->as<UnboxedPlainObject>().maybeExpando()) {
            masm.branchPtr(Assembler::Equal, expandoAddress, ImmWord(0), failure);
            Label success;
            masm.push(object);
            masm.loadPtr(expandoAddress, object);
            masm.branchTestObjShape(Assembler::Equal, object, expando->lastProperty(),
                                    &success);
            masm.pop(object);
            masm.jump(failure);
            masm.bind(&success);
            masm.pop(object);
        } else {
            masm.branchPtr(Assembler::NotEqual, expandoAddress, ImmWord(0), failure);
        }
    } else if (obj->is<UnboxedArrayObject>()) {
        MOZ_ASSERT(failure);
        masm.branchTestObjGroup(Assembler::NotEqual, object, obj->group(), failure);
    } else if (obj->is<TypedObject>()) {
        attacher.branchNextStubOrLabel(masm, Assembler::NotEqual,
                                       Address(object, JSObject::offsetOfGroup()),
                                       ImmGCPtr(obj->group()), failure);
    } else {
        Shape* shape = obj->maybeShape();
        MOZ_ASSERT(shape);

        attacher.branchNextStubOrLabel(masm, Assembler::NotEqual,
                                       Address(object, ShapedObject::offsetOfShape()),
                                       ImmGCPtr(shape), failure);

        if (alwaysCheckGroup)
            masm.branchTestObjGroup(Assembler::NotEqual, object, obj->group(), failure);
    }
}

static inline void
EmitLoadSlot(MacroAssembler& masm, NativeObject* holder, Shape* shape, Register holderReg,
             TypedOrValueRegister output, Register scratchReg)
{
    MOZ_ASSERT(holder);
    NativeObject::slotsSizeMustNotOverflow();
    if (holder->isFixedSlot(shape->slot())) {
        Address addr(holderReg, NativeObject::getFixedSlotOffset(shape->slot()));
        masm.loadTypedOrValue(addr, output);
    } else {
        masm.loadPtr(Address(holderReg, NativeObject::offsetOfSlots()), scratchReg);

        Address addr(scratchReg, holder->dynamicSlotIndex(shape->slot()) * sizeof(Value));
        masm.loadTypedOrValue(addr, output);
    }
}

static void
GenerateReadSlot(JSContext* cx, IonScript* ion, MacroAssembler& masm,
                 IonCache::StubAttacher& attacher, MaybeCheckTDZ checkTDZ,
                 JSObject* obj, JSObject* holder, PropertyResult prop, Register object,
                 TypedOrValueRegister output, Label* failures = nullptr)
{
    // If there's a single jump to |failures|, we can patch the shape guard
    // jump directly. Otherwise, jump to the end of the stub, so there's a
    // common point to patch.
    bool multipleFailureJumps = (obj != holder)
                             || obj->is<UnboxedPlainObject>()
                             || (checkTDZ && output.hasValue())
                             || (failures != nullptr && failures->used());

    // If we have multiple failure jumps but didn't get a label from the
    // outside, make one ourselves.
    Label failures_;
    if (multipleFailureJumps && !failures)
        failures = &failures_;

    TestMatchingReceiver(masm, attacher, object, obj, failures);

    // If we need a scratch register, use either an output register or the
    // object register. After this point, we cannot jump directly to
    // |failures| since we may still have to pop the object register.
    bool restoreScratch = false;
    Register scratchReg = Register::FromCode(0); // Quell compiler warning.

    if (obj != holder ||
        obj->is<UnboxedPlainObject>() ||
        !holder->as<NativeObject>().isFixedSlot(prop.shape()->slot()))
    {
        if (output.hasValue()) {
            scratchReg = output.valueReg().scratchReg();
        } else if (output.type() == MIRType::Double) {
            scratchReg = object;
            masm.push(scratchReg);
            restoreScratch = true;
        } else {
            scratchReg = output.typedReg().gpr();
        }
    }

    // Fast path: single failure jump, no prototype guards.
    if (!multipleFailureJumps) {
        EmitLoadSlot(masm, &holder->as<NativeObject>(), prop.shape(), object, output, scratchReg);
        if (restoreScratch)
            masm.pop(scratchReg);
        attacher.jumpRejoin(masm);
        return;
    }

    // Slow path: multiple jumps; generate prototype guards.
    Label prototypeFailures;
    Register holderReg;
    if (obj != holder) {
        // Note: this may clobber the object register if it's used as scratch.
        GeneratePrototypeGuards(cx, ion, masm, obj, holder, object, scratchReg,
                                &prototypeFailures);

        if (holder) {
            // Guard on the holder's shape.
            holderReg = scratchReg;
            masm.movePtr(ImmGCPtr(holder), holderReg);
            masm.branchPtr(Assembler::NotEqual,
                           Address(holderReg, ShapedObject::offsetOfShape()),
                           ImmGCPtr(holder->as<NativeObject>().lastProperty()),
                           &prototypeFailures);
        } else {
            // The property does not exist. Guard on everything in the
            // prototype chain.
            JSObject* proto = obj->staticPrototype();
            Register lastReg = object;
            MOZ_ASSERT(scratchReg != object);
            while (proto) {
                masm.loadObjProto(lastReg, scratchReg);

                // Guard the shape of the current prototype.
                MOZ_ASSERT(proto->hasStaticPrototype());
                masm.branchPtr(Assembler::NotEqual,
                               Address(scratchReg, ShapedObject::offsetOfShape()),
                               ImmGCPtr(proto->as<NativeObject>().lastProperty()),
                               &prototypeFailures);

                proto = proto->staticPrototype();
                lastReg = scratchReg;
            }

            holderReg = InvalidReg;
        }
    } else if (obj->is<UnboxedPlainObject>()) {
        holder = obj->as<UnboxedPlainObject>().maybeExpando();
        holderReg = scratchReg;
        masm.loadPtr(Address(object, UnboxedPlainObject::offsetOfExpando()), holderReg);
    } else {
        holderReg = object;
    }

    // Slot access.
    if (holder) {
        EmitLoadSlot(masm, &holder->as<NativeObject>(), prop.shape(), holderReg, output,
                     scratchReg);
        if (checkTDZ && output.hasValue())
            masm.branchTestMagic(Assembler::Equal, output.valueReg(), failures);
    } else {
        masm.moveValue(UndefinedValue(), output.valueReg());
    }

    // Restore scratch on success.
    if (restoreScratch)
        masm.pop(scratchReg);

    attacher.jumpRejoin(masm);

    masm.bind(&prototypeFailures);
    if (restoreScratch)
        masm.pop(scratchReg);
    masm.bind(failures);

    attacher.jumpNextStub(masm);
}

static void
GenerateReadUnboxed(JSContext* cx, IonScript* ion, MacroAssembler& masm,
                    IonCache::StubAttacher& attacher, JSObject* obj,
                    const UnboxedLayout::Property* property,
                    Register object, TypedOrValueRegister output,
                    Label* failures = nullptr)
{
    // Guard on the group of the object.
    attacher.branchNextStubOrLabel(masm, Assembler::NotEqual,
                                   Address(object, JSObject::offsetOfGroup()),
                                   ImmGCPtr(obj->group()), failures);

    Address address(object, UnboxedPlainObject::offsetOfData() + property->offset);

    masm.loadUnboxedProperty(address, property->type, output);

    attacher.jumpRejoin(masm);

    if (failures) {
        masm.bind(failures);
        attacher.jumpNextStub(masm);
    }
}

static bool
EmitGetterCall(JSContext* cx, MacroAssembler& masm,
               IonCache::StubAttacher& attacher, JSObject* obj,
               JSObject* holder, HandleShape shape, bool holderIsReceiver,
               LiveRegisterSet liveRegs, Register object,
               TypedOrValueRegister output,
               void* returnAddr)
{
    MOZ_ASSERT(output.hasValue());
    MacroAssembler::AfterICSaveLive aic = masm.icSaveLive(liveRegs);

    MOZ_ASSERT_IF(obj != holder, !holderIsReceiver);

    // Remaining registers should basically be free, but we need to use |object| still
    // so leave it alone.
    AllocatableRegisterSet regSet(RegisterSet::All());
    regSet.take(AnyRegister(object));

    // This is a slower stub path, and we're going to be doing a call anyway.  Don't need
    // to try so hard to not use the stack.  Scratch regs are just taken from the register
    // set not including the input, current value saved on the stack, and restored when
    // we're done with it.
    Register scratchReg = regSet.takeAnyGeneral();

    // Shape has a JSNative, PropertyOp or scripted getter function.
    if (IsCacheableGetPropCallNative(obj, holder, shape)) {
        Register argJSContextReg = regSet.takeAnyGeneral();
        Register argUintNReg     = regSet.takeAnyGeneral();
        Register argVpReg        = regSet.takeAnyGeneral();

        JSFunction* target = &shape->getterValue().toObject().as<JSFunction>();
        MOZ_ASSERT(target);
        MOZ_ASSERT(target->isNative());

        // Native functions have the signature:
        //  bool (*)(JSContext*, unsigned, Value* vp)
        // Where vp[0] is space for an outparam, vp[1] is |this|, and vp[2] onward
        // are the function arguments.

        // Construct vp array:
        // Push object value for |this|
        masm.Push(TypedOrValueRegister(MIRType::Object, AnyRegister(object)));
        // Push callee/outparam.
        masm.Push(ObjectValue(*target));

        // Preload arguments into registers.
        masm.loadJSContext(argJSContextReg);
        masm.move32(Imm32(0), argUintNReg);
        masm.moveStackPtrTo(argVpReg);

        // Push marking data for later use.
        masm.Push(argUintNReg);
        attacher.pushStubCodePointer(masm);

        if (!masm.icBuildOOLFakeExitFrame(returnAddr, aic))
            return false;
        masm.enterFakeExitFrame(scratchReg, IonOOLNativeExitFrameLayoutToken);

        // Construct and execute call.
        masm.setupUnalignedABICall(scratchReg);
        masm.passABIArg(argJSContextReg);
        masm.passABIArg(argUintNReg);
        masm.passABIArg(argVpReg);
        masm.callWithABI(JS_FUNC_TO_DATA_PTR(void*, target->native()));

        // Test for failure.
        masm.branchIfFalseBool(ReturnReg, masm.exceptionLabel());

        // Load the outparam vp[0] into output register(s).
        Address outparam(masm.getStackPointer(), IonOOLNativeExitFrameLayout::offsetOfResult());
        masm.loadTypedOrValue(outparam, output);

        // masm.leaveExitFrame & pop locals
        masm.adjustStack(IonOOLNativeExitFrameLayout::Size(0));
    } else if (IsCacheableGetPropCallPropertyOp(obj, holder, shape)) {
        Register argJSContextReg = regSet.takeAnyGeneral();
        Register argObjReg       = regSet.takeAnyGeneral();
        Register argIdReg        = regSet.takeAnyGeneral();
        Register argVpReg        = regSet.takeAnyGeneral();

        GetterOp target = shape->getterOp();
        MOZ_ASSERT(target);

        // Push stubCode for marking.
        attacher.pushStubCodePointer(masm);

        // JSGetterOp: bool fn(JSContext* cx, HandleObject obj, HandleId id, MutableHandleValue vp)

        // Push args on stack first so we can take pointers to make handles.
        masm.Push(UndefinedValue());
        masm.moveStackPtrTo(argVpReg);

        // Push canonical jsid from shape instead of propertyname.
        masm.Push(shape->propid(), scratchReg);
        masm.moveStackPtrTo(argIdReg);

        // Push the holder.
        if (holderIsReceiver) {
            // When the holder is also the current receiver, we just have a shape guard,
            // so we might end up with a random object which is also guaranteed to have
            // this JSGetterOp.
            masm.Push(object);
        } else {
            // If the holder is on the prototype chain, the prototype-guarding
            // only allows objects with the same holder.
            masm.movePtr(ImmGCPtr(holder), scratchReg);
            masm.Push(scratchReg);
        }
        masm.moveStackPtrTo(argObjReg);

        masm.loadJSContext(argJSContextReg);

        if (!masm.icBuildOOLFakeExitFrame(returnAddr, aic))
            return false;
        masm.enterFakeExitFrame(scratchReg, IonOOLPropertyOpExitFrameLayoutToken);

        // Make the call.
        masm.setupUnalignedABICall(scratchReg);
        masm.passABIArg(argJSContextReg);
        masm.passABIArg(argObjReg);
        masm.passABIArg(argIdReg);
        masm.passABIArg(argVpReg);
        masm.callWithABI(JS_FUNC_TO_DATA_PTR(void*, target));

        // Test for failure.
        masm.branchIfFalseBool(ReturnReg, masm.exceptionLabel());

        // Load the outparam vp[0] into output register(s).
        Address outparam(masm.getStackPointer(), IonOOLPropertyOpExitFrameLayout::offsetOfResult());
        masm.loadTypedOrValue(outparam, output);

        // masm.leaveExitFrame & pop locals.
        masm.adjustStack(IonOOLPropertyOpExitFrameLayout::Size());
    } else {
        MOZ_ASSERT(IsCacheableGetPropCallScripted(obj, holder, shape));

        JSFunction* target = &shape->getterValue().toObject().as<JSFunction>();
        uint32_t framePushedBefore = masm.framePushed();

        // Construct IonICCallFrameLayout.
        uint32_t descriptor = MakeFrameDescriptor(masm.framePushed(), JitFrame_IonJS,
                                                  IonICCallFrameLayout::Size());
        attacher.pushStubCodePointer(masm);
        masm.Push(Imm32(descriptor));
        masm.Push(ImmPtr(returnAddr));

        // The JitFrameLayout pushed below will be aligned to JitStackAlignment,
        // so we just have to make sure the stack is aligned after we push the
        // |this| + argument Values.
        uint32_t argSize = (target->nargs() + 1) * sizeof(Value);
        uint32_t padding = ComputeByteAlignment(masm.framePushed() + argSize, JitStackAlignment);
        MOZ_ASSERT(padding % sizeof(uintptr_t) == 0);
        MOZ_ASSERT(padding < JitStackAlignment);
        masm.reserveStack(padding);

        for (size_t i = 0; i < target->nargs(); i++)
            masm.Push(UndefinedValue());
        masm.Push(TypedOrValueRegister(MIRType::Object, AnyRegister(object)));

        masm.movePtr(ImmGCPtr(target), scratchReg);

        descriptor = MakeFrameDescriptor(argSize + padding, JitFrame_IonICCall,
                                         JitFrameLayout::Size());
        masm.Push(Imm32(0)); // argc
        masm.Push(scratchReg);
        masm.Push(Imm32(descriptor));

        // Check stack alignment. Add sizeof(uintptr_t) for the return address.
        MOZ_ASSERT(((masm.framePushed() + sizeof(uintptr_t)) % JitStackAlignment) == 0);

        // The getter has JIT code now and we will only discard the getter's JIT
        // code when discarding all JIT code in the Zone, so we can assume it'll
        // still have JIT code.
        MOZ_ASSERT(target->hasJITCode());
        masm.loadPtr(Address(scratchReg, JSFunction::offsetOfNativeOrScript()), scratchReg);
        masm.loadBaselineOrIonRaw(scratchReg, scratchReg, nullptr);
        masm.callJit(scratchReg);
        masm.storeCallResultValue(output);

        masm.freeStack(masm.framePushed() - framePushedBefore);
    }

    masm.icRestoreLive(liveRegs, aic);
    return true;
}

static bool
GenerateCallGetter(JSContext* cx, IonScript* ion, MacroAssembler& masm,
                   IonCache::StubAttacher& attacher, JSObject* obj,
                   JSObject* holder, HandleShape shape, LiveRegisterSet& liveRegs, Register object,
                   TypedOrValueRegister output, void* returnAddr, Label* failures = nullptr)
{
    MOZ_ASSERT(output.hasValue());

    // Use the passed in label if there was one. Otherwise, we'll have to make our own.
    Label stubFailure;
    failures = failures ? failures : &stubFailure;

    TestMatchingReceiver(masm, attacher, object, obj, failures);

    Register scratchReg = output.valueReg().scratchReg();
    bool spillObjReg = scratchReg == object;
    Label pop1AndFail;
    Label* maybePopAndFail = failures;

    // If we're calling a getter on the global, inline the logic for the
    // 'this' hook on the global lexical env and manually push the global.
    if (IsGlobalLexicalEnvironment(obj)) {
        masm.extractObject(Address(object, EnvironmentObject::offsetOfEnclosingEnvironment()),
                           object);
    }

    // Save off the object register if it aliases the scratchReg
    if (spillObjReg) {
        masm.push(object);
        maybePopAndFail = &pop1AndFail;
    }

    // Note: this may clobber the object register if it's used as scratch.
    if (obj != holder)
        GeneratePrototypeGuards(cx, ion, masm, obj, holder, object, scratchReg, maybePopAndFail);

    // Guard on the holder's shape.
    Register holderReg = scratchReg;
    masm.movePtr(ImmGCPtr(holder), holderReg);
    masm.branchPtr(Assembler::NotEqual,
                   Address(holderReg, ShapedObject::offsetOfShape()),
                   ImmGCPtr(holder->as<NativeObject>().lastProperty()),
                   maybePopAndFail);

    if (spillObjReg)
        masm.pop(object);

    // Now we're good to go to invoke the native call.
    bool holderIsReceiver = (obj == holder);
    if (!EmitGetterCall(cx, masm, attacher, obj, holder, shape, holderIsReceiver, liveRegs, object,
                        output, returnAddr))
        return false;

    // Rejoin jump.
    attacher.jumpRejoin(masm);

    // Jump to next stub.
    if (spillObjReg) {
        masm.bind(&pop1AndFail);
        masm.pop(object);
    }
    masm.bind(failures);
    attacher.jumpNextStub(masm);

    return true;
}

static bool
GenerateArrayLength(JSContext* cx, MacroAssembler& masm, IonCache::StubAttacher& attacher,
                    JSObject* obj, Register object, TypedOrValueRegister output, Label* failures)
{
    MOZ_ASSERT(obj->is<ArrayObject>());

    // Guard object is a dense array.
    RootedShape shape(cx, obj->as<ArrayObject>().lastProperty());
    if (!shape)
        return false;
    masm.branchTestObjShape(Assembler::NotEqual, object, shape, failures);

    // Load length.
    Register outReg;
    if (output.hasValue()) {
        outReg = output.valueReg().scratchReg();
    } else {
        MOZ_ASSERT(output.type() == MIRType::Int32);
        outReg = output.typedReg().gpr();
    }

    masm.loadPtr(Address(object, NativeObject::offsetOfElements()), outReg);
    masm.load32(Address(outReg, ObjectElements::offsetOfLength()), outReg);

    // The length is an unsigned int, but the value encodes a signed int.
    MOZ_ASSERT(object != outReg);
    masm.branchTest32(Assembler::Signed, outReg, outReg, failures);

    if (output.hasValue())
        masm.tagValue(JSVAL_TYPE_INT32, outReg, output.valueReg());

    /* Success. */
    attacher.jumpRejoin(masm);

    /* Failure. */
    masm.bind(failures);
    attacher.jumpNextStub(masm);

    return true;
}

static void
GenerateUnboxedArrayLength(JSContext* cx, MacroAssembler& masm, IonCache::StubAttacher& attacher,
                           JSObject* array, Register object, TypedOrValueRegister output,
                           Label* failures)
{
    Register outReg;
    if (output.hasValue()) {
        outReg = output.valueReg().scratchReg();
    } else {
        MOZ_ASSERT(output.type() == MIRType::Int32);
        outReg = output.typedReg().gpr();
    }
    MOZ_ASSERT(object != outReg);

    TestMatchingReceiver(masm, attacher, object, array, failures);

    // Load length.
    masm.load32(Address(object, UnboxedArrayObject::offsetOfLength()), outReg);

    // Check for a length that fits in an int32.
    masm.branchTest32(Assembler::Signed, outReg, outReg, failures);

    if (output.hasValue())
        masm.tagValue(JSVAL_TYPE_INT32, outReg, output.valueReg());

    // Success.
    attacher.jumpRejoin(masm);

    // Failure.
    masm.bind(failures);
    attacher.jumpNextStub(masm);
}

// In this case, the code for TypedArray and SharedTypedArray is not the same,
// because the code embeds pointers to the respective class arrays.  Code that
// caches the stub code must distinguish between the two cases.
static void
GenerateTypedArrayLength(JSContext* cx, MacroAssembler& masm, IonCache::StubAttacher& attacher,
                         Register object, TypedOrValueRegister output, Label* failures)
{
    Register tmpReg;
    if (output.hasValue()) {
        tmpReg = output.valueReg().scratchReg();
    } else {
        MOZ_ASSERT(output.type() == MIRType::Int32);
        tmpReg = output.typedReg().gpr();
    }
    MOZ_ASSERT(object != tmpReg);

    // Implement the negated version of JSObject::isTypedArray predicate.
    masm.loadObjClass(object, tmpReg);
    masm.branchPtr(Assembler::Below, tmpReg, ImmPtr(&TypedArrayObject::classes[0]),
                   failures);
    masm.branchPtr(Assembler::AboveOrEqual, tmpReg,
                   ImmPtr(&TypedArrayObject::classes[Scalar::MaxTypedArrayViewType]),
                   failures);

    // Load length.
    masm.loadTypedOrValue(Address(object, TypedArrayObject::lengthOffset()), output);

    /* Success. */
    attacher.jumpRejoin(masm);

    /* Failure. */
    masm.bind(failures);
    attacher.jumpNextStub(masm);
}

static bool
IsCacheableArrayLength(JSContext* cx, HandleObject obj, TypedOrValueRegister output)
{
    if (!obj->is<ArrayObject>())
        return false;

    if (output.type() != MIRType::Value && output.type() != MIRType::Int32) {
        // The stub assumes that we always output Int32, so make sure our output
        // is equipped to handle that.
        return false;
    }

    // The emitted stub can only handle int32 lengths. If the length of the
    // actual object does not fit in an int32 then don't attach a stub, as if
    // the cache is idempotent we won't end up invalidating the compiled script
    // otherwise.
    if (obj->as<ArrayObject>().length() > INT32_MAX)
        return false;

    return true;
}

template <class GetPropCache>
static GetPropertyIC::NativeGetPropCacheability
CanAttachNativeGetProp(JSContext* cx, const GetPropCache& cache,
                       HandleObject obj, HandleId id,
                       MutableHandleNativeObject holder, MutableHandleShape shape,
                       bool skipArrayLen = false)
{
    MOZ_ASSERT(JSID_IS_STRING(id) || JSID_IS_SYMBOL(id));

    if (!obj)
        return GetPropertyIC::CanAttachNone;

    // The lookup needs to be universally pure, otherwise we risk calling hooks out
    // of turn. We don't mind doing this even when purity isn't required, because we
    // only miss out on shape hashification, which is only a temporary perf cost.
    // The limits were arbitrarily set, anyways.
    JSObject* baseHolder = nullptr;
    PropertyResult prop;
    if (!LookupPropertyPure(cx, obj, id, &baseHolder, &prop))
        return GetPropertyIC::CanAttachNone;

    MOZ_ASSERT(!holder);
    if (baseHolder) {
        if (!baseHolder->isNative())
            return GetPropertyIC::CanAttachNone;
        holder.set(&baseHolder->as<NativeObject>());
    }
    shape.set(prop.maybeShape());

    RootedScript script(cx);
    jsbytecode* pc;
    cache.getScriptedLocation(&script, &pc);
    if (IsCacheableGetPropReadSlotForIonOrCacheIR(obj, holder, prop) ||
        IsCacheableNoProperty(obj, holder, prop, pc, cache.output()))
    {
        return GetPropertyIC::CanAttachReadSlot;
    }

    // |length| is a non-configurable getter property on ArrayObjects. Any time this
    // check would have passed, we can install a getter stub instead. Allow people to
    // make that decision themselves with skipArrayLen
    if (!skipArrayLen && JSID_IS_ATOM(id, cx->names().length) && cache.allowArrayLength(cx) &&
        IsCacheableArrayLength(cx, obj, cache.output()))
    {
        // The array length property is non-configurable, which means both that
        // checking the class of the object and the name of the property is enough
        // and that we don't need to worry about monitoring, since we know the
        // return type statically.
        return GetPropertyIC::CanAttachArrayLength;
    }

    // IonBuilder guarantees that it's impossible to generate a GetPropertyIC with
    // allowGetters() true and cache.output().hasValue() false. If this isn't true,
    // we will quickly assert during stub generation.
    //
    // Be careful when adding support for other getters here: for outer window
    // proxies, IonBuilder can innerize and pass us the inner window (the global),
    // see IonBuilder::getPropTryInnerize. This is fine for native/scripted getters
    // because IsCacheableGetPropCallNative and IsCacheableGetPropCallScripted
    // handle this.
    if (cache.allowGetters() &&
        (IsCacheableGetPropCallNative(obj, holder, shape) ||
         IsCacheableGetPropCallPropertyOp(obj, holder, shape) ||
         IsCacheableGetPropCallScripted(obj, holder, shape)))
    {
        // Don't enable getter call if cache is idempotent, since they can be
        // effectful. This is handled by allowGetters()
        return GetPropertyIC::CanAttachCallGetter;
    }

    return GetPropertyIC::CanAttachNone;
}

bool
GetPropertyIC::allowArrayLength(JSContext* cx) const
{
    if (!idempotent())
        return true;

    uint32_t locationIndex, numLocations;
    getLocationInfo(&locationIndex, &numLocations);

    IonScript* ion = GetTopJitJSScript(cx)->ionScript();
    CacheLocation* locs = ion->getCacheLocs(locationIndex);
    for (size_t i = 0; i < numLocations; i++) {
        CacheLocation& curLoc = locs[i];
        StackTypeSet* bcTypes = TypeScript::BytecodeTypes(curLoc.script, curLoc.pc);

        if (!bcTypes->hasType(TypeSet::Int32Type()))
            return false;
    }

    return true;
}

bool
GetPropertyIC::tryAttachNative(JSContext* cx, HandleScript outerScript, IonScript* ion,
                               HandleObject obj, HandleId id, void* returnAddr, bool* emitted)
{
    MOZ_ASSERT(canAttachStub());
    MOZ_ASSERT(!*emitted);
    MOZ_ASSERT(outerScript->ionScript() == ion);

    RootedShape shape(cx);
    RootedNativeObject holder(cx);

    NativeGetPropCacheability type =
        CanAttachNativeGetProp(cx, *this, obj, id, &holder, &shape);
    if (type == CanAttachNone)
        return true;

    *emitted = true;

    MacroAssembler masm(cx, ion, outerScript, profilerLeavePc_);

    StubAttacher attacher(*this);
    const char* attachKind;

    JS::TrackedOutcome outcome = JS::TrackedOutcome::ICOptStub_GenericSuccess;

    Label failures;
    emitIdGuard(masm, id, &failures);
    Label* maybeFailures = failures.used() ? &failures : nullptr;

    switch (type) {
      case CanAttachReadSlot:
        GenerateReadSlot(cx, ion, masm, attacher, DontCheckTDZ, obj, holder,
                         PropertyResult(shape), object(), output(), maybeFailures);
        attachKind = idempotent() ? "idempotent reading"
                                    : "non idempotent reading";
        outcome = JS::TrackedOutcome::ICGetPropStub_ReadSlot;
        break;
      case CanAttachCallGetter:
        if (!GenerateCallGetter(cx, ion, masm, attacher, obj, holder, shape,
                                liveRegs_, object(), output(), returnAddr, maybeFailures))
        {
            return false;
        }
        attachKind = "getter call";
        outcome = JS::TrackedOutcome::ICGetPropStub_CallGetter;
        break;
      case CanAttachArrayLength:
        if (!GenerateArrayLength(cx, masm, attacher, obj, object(), output(), &failures))
            return false;

        attachKind = "array length";
        outcome = JS::TrackedOutcome::ICGetPropStub_ArrayLength;
        break;
      default:
        MOZ_CRASH("Bad NativeGetPropCacheability");
    }
    return linkAndAttachStub(cx, masm, attacher, ion, attachKind, outcome);
}

bool
GetPropertyIC::tryAttachUnboxed(JSContext* cx, HandleScript outerScript, IonScript* ion,
                                HandleObject obj, HandleId id, void* returnAddr, bool* emitted)
{
    MOZ_ASSERT(canAttachStub());
    MOZ_ASSERT(!*emitted);
    MOZ_ASSERT(outerScript->ionScript() == ion);

    if (!obj->is<UnboxedPlainObject>())
        return true;
    const UnboxedLayout::Property* property = obj->as<UnboxedPlainObject>().layout().lookup(id);
    if (!property)
        return true;

    *emitted = true;

    MacroAssembler masm(cx, ion, outerScript, profilerLeavePc_);

    Label failures;
    emitIdGuard(masm, id, &failures);
    Label* maybeFailures = failures.used() ? &failures : nullptr;

    StubAttacher attacher(*this);
    GenerateReadUnboxed(cx, ion, masm, attacher, obj, property, object(), output(), maybeFailures);
    return linkAndAttachStub(cx, masm, attacher, ion, "read unboxed",
                             JS::TrackedOutcome::ICGetPropStub_UnboxedRead);
}

bool
GetPropertyIC::tryAttachUnboxedExpando(JSContext* cx, HandleScript outerScript, IonScript* ion,
                                       HandleObject obj, HandleId id, void* returnAddr, bool* emitted)
{
    MOZ_ASSERT(canAttachStub());
    MOZ_ASSERT(!*emitted);
    MOZ_ASSERT(outerScript->ionScript() == ion);

    if (!obj->is<UnboxedPlainObject>())
        return true;
    Rooted<UnboxedExpandoObject*> expando(cx, obj->as<UnboxedPlainObject>().maybeExpando());
    if (!expando)
        return true;

    Shape* shape = expando->lookup(cx, id);
    if (!shape || !shape->hasDefaultGetter() || !shape->hasSlot())
        return true;

    *emitted = true;

    MacroAssembler masm(cx, ion, outerScript, profilerLeavePc_);

    Label failures;
    emitIdGuard(masm, id, &failures);
    Label* maybeFailures = failures.used() ? &failures : nullptr;

    StubAttacher attacher(*this);
    GenerateReadSlot(cx, ion, masm, attacher, DontCheckTDZ, obj, obj,
                     PropertyResult(shape), object(), output(), maybeFailures);
    return linkAndAttachStub(cx, masm, attacher, ion, "read unboxed expando",
                             JS::TrackedOutcome::ICGetPropStub_UnboxedReadExpando);
}

bool
GetPropertyIC::tryAttachUnboxedArrayLength(JSContext* cx, HandleScript outerScript, IonScript* ion,
                                           HandleObject obj, HandleId id, void* returnAddr,
                                           bool* emitted)
{
    MOZ_ASSERT(canAttachStub());
    MOZ_ASSERT(!*emitted);
    MOZ_ASSERT(outerScript->ionScript() == ion);

    if (!obj->is<UnboxedArrayObject>())
        return true;

    if (!JSID_IS_ATOM(id, cx->names().length))
        return true;

    if (obj->as<UnboxedArrayObject>().length() > INT32_MAX)
        return true;

    if (!allowArrayLength(cx))
        return true;

    *emitted = true;

    MacroAssembler masm(cx, ion, outerScript, profilerLeavePc_);

    Label failures;
    emitIdGuard(masm, id, &failures);

    StubAttacher attacher(*this);
    GenerateUnboxedArrayLength(cx, masm, attacher, obj, object(), output(), &failures);
    return linkAndAttachStub(cx, masm, attacher, ion, "unboxed array length",
                             JS::TrackedOutcome::ICGetPropStub_UnboxedArrayLength);
}

bool
GetPropertyIC::tryAttachTypedArrayLength(JSContext* cx, HandleScript outerScript, IonScript* ion,
                                         HandleObject obj, HandleId id, bool* emitted)
{
    MOZ_ASSERT(canAttachStub());
    MOZ_ASSERT(!*emitted);

    if (!obj->is<TypedArrayObject>())
        return true;

    if (!JSID_IS_ATOM(id, cx->names().length))
        return true;

    if (hasTypedArrayLengthStub(obj))
        return true;

    if (output().type() != MIRType::Value && output().type() != MIRType::Int32) {
        // The next execution should cause an invalidation because the type
        // does not fit.
        return true;
    }

    if (idempotent())
        return true;

    *emitted = true;

    MacroAssembler masm(cx, ion, outerScript, profilerLeavePc_);
    StubAttacher attacher(*this);

    Label failures;
    emitIdGuard(masm, id, &failures);

    GenerateTypedArrayLength(cx, masm, attacher, object(), output(), &failures);

    setHasTypedArrayLengthStub(obj);
    return linkAndAttachStub(cx, masm, attacher, ion, "typed array length",
                             JS::TrackedOutcome::ICGetPropStub_TypedArrayLength);
}

static bool
EmitCallProxyGet(JSContext* cx, MacroAssembler& masm, IonCache::StubAttacher& attacher,
                 jsid id, LiveRegisterSet liveRegs, Register object, TypedOrValueRegister output,
                 jsbytecode* pc, void* returnAddr)
{
    MOZ_ASSERT(output.hasValue());
    MacroAssembler::AfterICSaveLive aic = masm.icSaveLive(liveRegs);

    // Remaining registers should be free, but we need to use |object| still
    // so leave it alone.
    AllocatableRegisterSet regSet(RegisterSet::All());
    regSet.take(AnyRegister(object));

    // ProxyGetProperty(JSContext* cx, HandleObject proxy, HandleId id,
    //                  MutableHandleValue vp)
    Register argJSContextReg = regSet.takeAnyGeneral();
    Register argProxyReg     = regSet.takeAnyGeneral();
    Register argIdReg        = regSet.takeAnyGeneral();
    Register argVpReg        = regSet.takeAnyGeneral();

    Register scratch         = regSet.takeAnyGeneral();

    // Push stubCode for marking.
    attacher.pushStubCodePointer(masm);

    // Push args on stack first so we can take pointers to make handles.
    masm.Push(UndefinedValue());
    masm.moveStackPtrTo(argVpReg);

    masm.Push(id, scratch);
    masm.moveStackPtrTo(argIdReg);

    // Push the proxy. Also used as receiver.
    masm.Push(object);
    masm.moveStackPtrTo(argProxyReg);

    masm.loadJSContext(argJSContextReg);

    if (!masm.icBuildOOLFakeExitFrame(returnAddr, aic))
        return false;
    masm.enterFakeExitFrame(scratch, IonOOLProxyExitFrameLayoutToken);

    // Make the call.
    masm.setupUnalignedABICall(scratch);
    masm.passABIArg(argJSContextReg);
    masm.passABIArg(argProxyReg);
    masm.passABIArg(argIdReg);
    masm.passABIArg(argVpReg);
    masm.callWithABI(JS_FUNC_TO_DATA_PTR(void*, ProxyGetProperty));

    // Test for failure.
    masm.branchIfFalseBool(ReturnReg, masm.exceptionLabel());

    // Load the outparam vp[0] into output register(s).
    Address outparam(masm.getStackPointer(), IonOOLProxyExitFrameLayout::offsetOfResult());
    masm.loadTypedOrValue(outparam, output);

    // masm.leaveExitFrame & pop locals
    masm.adjustStack(IonOOLProxyExitFrameLayout::Size());

    masm.icRestoreLive(liveRegs, aic);
    return true;
}

bool
GetPropertyIC::tryAttachDOMProxyShadowed(JSContext* cx, HandleScript outerScript, IonScript* ion,
                                         HandleObject obj, HandleId id, void* returnAddr,
                                         bool* emitted)
{
    MOZ_ASSERT(canAttachStub());
    MOZ_ASSERT(!*emitted);
    MOZ_ASSERT(IsCacheableDOMProxy(obj));
    MOZ_ASSERT(monitoredResult());
    MOZ_ASSERT(output().hasValue());

    if (idempotent())
        return true;

    *emitted = true;

    Label failures;
    MacroAssembler masm(cx, ion, outerScript, profilerLeavePc_);
    StubAttacher attacher(*this);

    emitIdGuard(masm, id, &failures);

    // Guard on the shape of the object.
    attacher.branchNextStubOrLabel(masm, Assembler::NotEqual,
                                   Address(object(), ShapedObject::offsetOfShape()),
                                   ImmGCPtr(obj->maybeShape()),
                                   &failures);

    // No need for more guards: we know this is a DOM proxy, since the shape
    // guard enforces a given JSClass, so just go ahead and emit the call to
    // ProxyGet.

    if (!EmitCallProxyGet(cx, masm, attacher, id, liveRegs_, object(), output(),
                          pc(), returnAddr))
    {
        return false;
    }

    // Success.
    attacher.jumpRejoin(masm);

    // Failure.
    masm.bind(&failures);
    attacher.jumpNextStub(masm);

    return linkAndAttachStub(cx, masm, attacher, ion, "list base shadowed get",
                             JS::TrackedOutcome::ICGetPropStub_DOMProxyShadowed);
}

bool
GetPropertyIC::tryAttachDOMProxyUnshadowed(JSContext* cx, HandleScript outerScript, IonScript* ion,
                                           HandleObject obj, HandleId id, bool resetNeeded,
                                           void* returnAddr, bool* emitted)
{
    MOZ_ASSERT(canAttachStub());
    MOZ_ASSERT(!*emitted);
    MOZ_ASSERT(IsCacheableDOMProxy(obj));
    MOZ_ASSERT(monitoredResult());
    MOZ_ASSERT(output().hasValue());

    RootedObject checkObj(cx, obj->staticPrototype());
    RootedNativeObject holder(cx);
    RootedShape shape(cx);

    NativeGetPropCacheability canCache =
        CanAttachNativeGetProp(cx, *this, checkObj, id, &holder, &shape,
                               /* skipArrayLen = */true);
    MOZ_ASSERT(canCache != CanAttachArrayLength);

    if (canCache == CanAttachNone)
        return true;

    // Make sure we observe our invariants if we're gonna deoptimize.
    if (!holder && idempotent())
        return true;

    *emitted = true;

    if (resetNeeded) {
        // If we know that we have a DoesntShadowUnique object, then
        // we reset the cache to clear out an existing IC for the object
        // (if there is one). The generation is a constant in the generated
        // code and we will not have the same generation again for this
        // object, so the generation check in the existing IC would always
        // fail anyway.
        reset(Reprotect);
    }

    Label failures;
    MacroAssembler masm(cx, ion, outerScript, profilerLeavePc_);
    StubAttacher attacher(*this);

    emitIdGuard(masm, id, &failures);

    // Guard on the shape of the object.
    attacher.branchNextStubOrLabel(masm, Assembler::NotEqual,
                                   Address(object(), ShapedObject::offsetOfShape()),
                                   ImmGCPtr(obj->maybeShape()),
                                   &failures);

    // Guard that our expando object hasn't started shadowing this property.
    CheckDOMProxyExpandoDoesNotShadow(cx, masm, obj, id, object(), &failures);

    if (holder) {
        // Found the property on the prototype chain. Treat it like a native
        // getprop.
        Register scratchReg = output().valueReg().scratchReg();
        GeneratePrototypeGuards(cx, ion, masm, obj, holder, object(), scratchReg, &failures);

        // Rename scratch for clarity.
        Register holderReg = scratchReg;

        // Guard on the holder of the property
        masm.movePtr(ImmGCPtr(holder), holderReg);
        masm.branchPtr(Assembler::NotEqual,
                    Address(holderReg, ShapedObject::offsetOfShape()),
                    ImmGCPtr(holder->lastProperty()),
                    &failures);

        if (canCache == CanAttachReadSlot) {
            EmitLoadSlot(masm, holder, shape, holderReg, output(), scratchReg);
        } else {
            // EmitGetterCall() expects |obj| to be the object the property is
            // on to do some checks. Since we actually looked at checkObj, and
            // no extra guards will be generated, we can just pass that instead.
            // The holderIsReceiver check needs to use |obj| though.
            MOZ_ASSERT(canCache == CanAttachCallGetter);
            MOZ_ASSERT(!idempotent());
            bool holderIsReceiver = (obj == holder);
            if (!EmitGetterCall(cx, masm, attacher, checkObj, holder, shape, holderIsReceiver,
                                liveRegs_, object(), output(), returnAddr))
            {
                return false;
            }
        }
    } else {
        // Property was not found on the prototype chain. Deoptimize down to
        // proxy get call
        MOZ_ASSERT(!idempotent());
        if (!EmitCallProxyGet(cx, masm, attacher, id, liveRegs_, object(), output(),
                              pc(), returnAddr))
        {
            return false;
        }
    }

    attacher.jumpRejoin(masm);
    masm.bind(&failures);
    attacher.jumpNextStub(masm);

    return linkAndAttachStub(cx, masm, attacher, ion, "unshadowed proxy get",
                             JS::TrackedOutcome::ICGetPropStub_DOMProxyUnshadowed);
}

bool
GetPropertyIC::tryAttachProxy(JSContext* cx, HandleScript outerScript, IonScript* ion,
                              HandleObject obj, HandleId id, void* returnAddr, bool* emitted)
{
    MOZ_ASSERT(canAttachStub());
    MOZ_ASSERT(!*emitted);

    if (!obj->is<ProxyObject>())
        return true;

    // TI can't be sure about our properties, so make sure anything
    // we return can be monitored directly.
    if (!monitoredResult())
        return true;

    // Skim off DOM proxies.
    if (IsCacheableDOMProxy(obj)) {
        DOMProxyShadowsResult shadows = GetDOMProxyShadowsCheck()(cx, obj, id);
        if (shadows == ShadowCheckFailed)
            return false;
        if (DOMProxyIsShadowing(shadows))
            return tryAttachDOMProxyShadowed(cx, outerScript, ion, obj, id, returnAddr, emitted);

        MOZ_ASSERT(shadows == DoesntShadow || shadows == DoesntShadowUnique);
        return tryAttachDOMProxyUnshadowed(cx, outerScript, ion, obj, id,
                                           shadows == DoesntShadowUnique, returnAddr, emitted);
    }

    return tryAttachGenericProxy(cx, outerScript, ion, obj, id, returnAddr, emitted);
}

bool
GetPropertyIC::tryAttachGenericProxy(JSContext* cx, HandleScript outerScript, IonScript* ion,
                                     HandleObject obj, HandleId id, void* returnAddr,
                                     bool* emitted)
{
    MOZ_ASSERT(canAttachStub());
    MOZ_ASSERT(!*emitted);
    MOZ_ASSERT(obj->is<ProxyObject>());
    MOZ_ASSERT(monitoredResult());
    MOZ_ASSERT(output().hasValue());

    if (hasGenericProxyStub())
        return true;

    if (idempotent())
        return true;

    *emitted = true;

    Label failures;
    MacroAssembler masm(cx, ion, outerScript, profilerLeavePc_);
    StubAttacher attacher(*this);

    emitIdGuard(masm, id, &failures);

    Register scratchReg = output().valueReg().scratchReg();

    masm.branchTestObjectIsProxy(false, object(), scratchReg, &failures);

    // Ensure that the incoming object is not a DOM proxy, so that we can get to
    // the specialized stubs
    masm.branchTestProxyHandlerFamily(Assembler::Equal, object(), scratchReg,
                                      GetDOMProxyHandlerFamily(), &failures);

    if (!EmitCallProxyGet(cx, masm, attacher, id, liveRegs_, object(), output(),
                          pc(), returnAddr))
    {
        return false;
    }

    attacher.jumpRejoin(masm);

    masm.bind(&failures);
    attacher.jumpNextStub(masm);

    MOZ_ASSERT(!hasGenericProxyStub_);
    hasGenericProxyStub_ = true;

    return linkAndAttachStub(cx, masm, attacher, ion, "Generic Proxy get",
                             JS::TrackedOutcome::ICGetPropStub_GenericProxy);
}

bool
GetPropertyIC::tryAttachArgumentsLength(JSContext* cx, HandleScript outerScript, IonScript* ion,
                                        HandleObject obj, HandleId id, bool* emitted)
{
    MOZ_ASSERT(canAttachStub());
    MOZ_ASSERT(!*emitted);

    if (!JSID_IS_ATOM(id, cx->names().length))
        return true;
    if (!IsOptimizableArgumentsObjectForLength(obj))
        return true;

    MIRType outputType = output().type();
    if (!(outputType == MIRType::Value || outputType == MIRType::Int32))
        return true;

    if (hasArgumentsLengthStub(obj->is<MappedArgumentsObject>()))
        return true;

    *emitted = true;

    MOZ_ASSERT(!idempotent());

    Label failures;
    MacroAssembler masm(cx, ion, outerScript, profilerLeavePc_);
    StubAttacher attacher(*this);

    emitIdGuard(masm, id, &failures);

    Register tmpReg;
    if (output().hasValue()) {
        tmpReg = output().valueReg().scratchReg();
    } else {
        MOZ_ASSERT(output().type() == MIRType::Int32);
        tmpReg = output().typedReg().gpr();
    }
    MOZ_ASSERT(object() != tmpReg);

    masm.branchTestObjClass(Assembler::NotEqual, object(), tmpReg, obj->getClass(), &failures);

    // Get initial ArgsObj length value, test if length has been overridden.
    masm.unboxInt32(Address(object(), ArgumentsObject::getInitialLengthSlotOffset()), tmpReg);
    masm.branchTest32(Assembler::NonZero, tmpReg, Imm32(ArgumentsObject::LENGTH_OVERRIDDEN_BIT),
                      &failures);

    masm.rshiftPtr(Imm32(ArgumentsObject::PACKED_BITS_COUNT), tmpReg);

    // If output is Int32, result is already in right place, otherwise box it into output.
    if (output().hasValue())
        masm.tagValue(JSVAL_TYPE_INT32, tmpReg, output().valueReg());

    // Success.
    attacher.jumpRejoin(masm);

    // Failure.
    masm.bind(&failures);
    attacher.jumpNextStub(masm);

    if (obj->is<UnmappedArgumentsObject>()) {
        MOZ_ASSERT(!hasUnmappedArgumentsLengthStub_);
        hasUnmappedArgumentsLengthStub_ = true;
        return linkAndAttachStub(cx, masm, attacher, ion, "ArgsObj length (unmapped)",
                                 JS::TrackedOutcome::ICGetPropStub_ArgumentsLength);
    }

    MOZ_ASSERT(!hasMappedArgumentsLengthStub_);
    hasMappedArgumentsLengthStub_ = true;
    return linkAndAttachStub(cx, masm, attacher, ion, "ArgsObj length (mapped)",
                                 JS::TrackedOutcome::ICGetPropStub_ArgumentsLength);
}

static void
GenerateReadModuleNamespace(JSContext* cx, IonScript* ion, MacroAssembler& masm,
                            IonCache::StubAttacher& attacher, ModuleNamespaceObject* ns,
                            ModuleEnvironmentObject* env, Shape* shape, Register object,
                            TypedOrValueRegister output, Label* failures)
{
    MOZ_ASSERT(ns);
    MOZ_ASSERT(env);

    // If we have multiple failure jumps but didn't get a label from the
    // outside, make one ourselves.
    Label failures_;
    if (!failures)
        failures = &failures_;

    // Check for the specific namespace object.
    attacher.branchNextStubOrLabel(masm, Assembler::NotEqual, object, ImmGCPtr(ns), failures);

    // If we need a scratch register, use either an output register or the
    // object register.
    bool restoreScratch = false;
    Register scratchReg = InvalidReg; // Quell compiler warning.

    if (output.hasValue()) {
        scratchReg = output.valueReg().scratchReg();
    } else if (output.type() == MIRType::Double) {
        masm.push(object);
        scratchReg = object;
        restoreScratch = true;
    } else {
        scratchReg = output.typedReg().gpr();
    }

    // Slot access.
    Register envReg = scratchReg;
    masm.movePtr(ImmGCPtr(env), envReg);
    EmitLoadSlot(masm, &env->as<NativeObject>(), shape, envReg, output, scratchReg);

    // Restore scratch on success.
    if (restoreScratch)
        masm.pop(object);

    attacher.jumpRejoin(masm);

    masm.bind(failures);
    attacher.jumpNextStub(masm);
}

bool
GetPropertyIC::tryAttachModuleNamespace(JSContext* cx, HandleScript outerScript, IonScript* ion,
                                        HandleObject obj, HandleId id, void* returnAddr,
                                        bool* emitted)
{
    MOZ_ASSERT(canAttachStub());
    MOZ_ASSERT(!*emitted);
    MOZ_ASSERT(outerScript->ionScript() == ion);

    if (!obj->is<ModuleNamespaceObject>())
        return true;

    Rooted<ModuleNamespaceObject*> ns(cx, &obj->as<ModuleNamespaceObject>());

    RootedModuleEnvironmentObject env(cx);
    RootedShape shape(cx);
    if (!ns->bindings().lookup(id, env.address(), shape.address()))
        return true;

    // Don't emit a stub until the target binding has been initialized.
    if (env->getSlot(shape->slot()).isMagic(JS_UNINITIALIZED_LEXICAL))
        return true;

    *emitted = true;

    MacroAssembler masm(cx, ion, outerScript, profilerLeavePc_);

    StubAttacher attacher(*this);

    Label failures;
    emitIdGuard(masm, id, &failures);
    Label* maybeFailures = failures.used() ? &failures : nullptr;

    GenerateReadModuleNamespace(cx, ion, masm, attacher, ns, env,
                                shape, object(), output(), maybeFailures);
    return linkAndAttachStub(cx, masm, attacher, ion, "module namespace",
                             JS::TrackedOutcome::ICGetPropStub_ReadSlot);
}

bool
jit::ValueToNameOrSymbolId(JSContext* cx, HandleValue idval, MutableHandleId id,
                           bool* nameOrSymbol)
{
    *nameOrSymbol = false;

    if (!idval.isString() && !idval.isSymbol())
        return true;

    if (!ValueToId<CanGC>(cx, idval, id))
        return false;

    if (!JSID_IS_STRING(id) && !JSID_IS_SYMBOL(id)) {
        id.set(JSID_VOID);
        return true;
    }

    uint32_t dummy;
    if (JSID_IS_STRING(id) && JSID_TO_ATOM(id)->isIndex(&dummy)) {
        id.set(JSID_VOID);
        return true;
    }

    *nameOrSymbol = true;
    return true;
}

bool
GetPropertyIC::tryAttachStub(JSContext* cx, HandleScript outerScript, IonScript* ion,
                             HandleObject obj, HandleValue idval, bool* emitted)
{
    MOZ_ASSERT(!*emitted);

    if (!canAttachStub())
        return true;

    RootedId id(cx);
    bool nameOrSymbol;
    if (!ValueToNameOrSymbolId(cx, idval, &id, &nameOrSymbol))
        return false;

    if (nameOrSymbol) {
        if (!*emitted && !tryAttachArgumentsLength(cx, outerScript, ion, obj, id, emitted))
            return false;

        void* returnAddr = GetReturnAddressToIonCode(cx);

        if (!*emitted && !tryAttachModuleNamespace(cx, outerScript, ion, obj, id, returnAddr, emitted))
            return false;

        if (!*emitted && !tryAttachProxy(cx, outerScript, ion, obj, id, returnAddr, emitted))
            return false;

        if (!*emitted && !tryAttachNative(cx, outerScript, ion, obj, id, returnAddr, emitted))
            return false;

        if (!*emitted && !tryAttachUnboxed(cx, outerScript, ion, obj, id, returnAddr, emitted))
            return false;

        if (!*emitted && !tryAttachUnboxedExpando(cx, outerScript, ion, obj, id, returnAddr, emitted))
            return false;

        if (!*emitted && !tryAttachUnboxedArrayLength(cx, outerScript, ion, obj, id, returnAddr, emitted))
            return false;

        if (!*emitted && !tryAttachTypedArrayLength(cx, outerScript, ion, obj, id, emitted))
            return false;
    }

    if (idval.isInt32()) {
        if (!*emitted && !tryAttachArgumentsElement(cx, outerScript, ion, obj, idval, emitted))
            return false;
        if (!*emitted && !tryAttachDenseElement(cx, outerScript, ion, obj, idval, emitted))
            return false;
        if (!*emitted && !tryAttachDenseElementHole(cx, outerScript, ion, obj, idval, emitted))
            return false;
    }

    if (idval.isInt32() || idval.isString()) {
        if (!*emitted && !tryAttachTypedOrUnboxedArrayElement(cx, outerScript, ion, obj, idval, emitted))
            return false;
    }

    if (!*emitted)
        JitSpew(JitSpew_IonIC, "Failed to attach GETPROP cache");

    return true;
}

/* static */ bool
GetPropertyIC::update(JSContext* cx, HandleScript outerScript, size_t cacheIndex,
                      HandleObject obj, HandleValue idval, MutableHandleValue vp)
{
    IonScript* ion = outerScript->ionScript();

    GetPropertyIC& cache = ion->getCache(cacheIndex).toGetProperty();

    // Override the return value if we are invalidated (bug 728188).
    AutoDetectInvalidation adi(cx, vp, ion);

    // If the cache is idempotent, we will redo the op in the interpreter.
    if (cache.idempotent())
        adi.disable();

    // For now, just stop generating new stubs once we hit the stub count
    // limit. Once we can make calls from within generated stubs, a new call
    // stub will be generated instead and the previous stubs unlinked.
    bool emitted = false;
    if (!cache.isDisabled()) {
        if (!cache.tryAttachStub(cx, outerScript, ion, obj, idval, &emitted))
            return false;
        cache.maybeDisable(emitted);
    }

    if (cache.idempotent() && !emitted) {
        // Invalidate the cache if the property was not found, or was found on
        // a non-native object. This ensures:
        // 1) The property read has no observable side-effects.
        // 2) There's no need to dynamically monitor the return type. This would
        //    be complicated since (due to GVN) there can be multiple pc's
        //    associated with a single idempotent cache.
        JitSpew(JitSpew_IonIC, "Invalidating from idempotent cache %s:%" PRIuSIZE,
                outerScript->filename(), outerScript->lineno());

        outerScript->setInvalidatedIdempotentCache();

        // Do not re-invalidate if the lookup already caused invalidation.
        if (outerScript->hasIonScript())
            Invalidate(cx, outerScript);

        return true;
    }

    jsbytecode* pc = cache.idempotent() ? nullptr : cache.pc();

    if (!pc || *pc == JSOP_GETPROP || *pc == JSOP_CALLPROP || *pc == JSOP_LENGTH) {
        if (!GetProperty(cx, obj, obj, idval.toString()->asAtom().asPropertyName(), vp))
            return false;
    } else {
        MOZ_ASSERT(*pc == JSOP_GETELEM || *pc == JSOP_CALLELEM);
        if (!GetObjectElementOperation(cx, JSOp(*pc), obj, obj, idval, vp))
            return false;
    }

    if (!cache.idempotent()) {
        RootedScript script(cx);
        jsbytecode* pc;
        cache.getScriptedLocation(&script, &pc);

        // Monitor changes to cache entry.
        if (!cache.monitoredResult())
            TypeScript::Monitor(cx, script, pc, vp);
    }

    return true;
}

void
GetPropertyIC::reset(ReprotectCode reprotect)
{
    IonCache::reset(reprotect);
    hasTypedArrayLengthStub_ = false;
    hasMappedArgumentsLengthStub_ = false;
    hasUnmappedArgumentsLengthStub_ = false;
    hasMappedArgumentsElementStub_ = false;
    hasUnmappedArgumentsElementStub_ = false;
    hasGenericProxyStub_ = false;
    hasDenseStub_ = false;
}

void
IonCache::disable()
{
    reset(Reprotect);
    this->disabled_ = 1;
}

void
GetPropertyIC::maybeDisable(bool emitted)
{
    if (emitted) {
        failedUpdates_ = 0;
        return;
    }

    if (!canAttachStub() && id().constant()) {
        // Don't disable the cache (and discard stubs) if we have a GETPROP and
        // attached the maximum number of stubs. This can happen when JS code
        // uses an AST-like data structure and accesses a field of a "base
        // class", like node.nodeType. This should be temporary until we handle
        // this case better, see bug 1107515.
        return;
    }

    if (++failedUpdates_ > MAX_FAILED_UPDATES) {
        JitSpew(JitSpew_IonIC, "Disable inline cache");
        disable();
    }
}

void
IonCache::reset(ReprotectCode reprotect)
{
    this->stubCount_ = 0;
    PatchJump(initialJump_, fallbackLabel_, reprotect);
    lastJump_ = initialJump_;
}

bool
jit::IsCacheableSetPropCallNative(JSObject* obj, JSObject* holder, Shape* shape)
{
    if (!shape || !IsCacheableProtoChainForIonOrCacheIR(obj, holder))
        return false;

    if (!shape->hasSetterValue())
        return false;

    if (!shape->setterObject() || !shape->setterObject()->is<JSFunction>())
        return false;

    JSFunction& setter = shape->setterObject()->as<JSFunction>();
    if (!setter.isNative())
        return false;

    if (setter.jitInfo() && !setter.jitInfo()->needsOuterizedThisObject())
        return true;

    return !IsWindow(obj);
}

bool
jit::IsCacheableSetPropCallScripted(JSObject* obj, JSObject* holder, Shape* shape,
                                    bool* isTemporarilyUnoptimizable)
{
    if (!shape || !IsCacheableProtoChainForIonOrCacheIR(obj, holder))
        return false;

    if (IsWindow(obj))
        return false;

    if (!shape->hasSetterValue())
        return false;

    if (!shape->setterObject() || !shape->setterObject()->is<JSFunction>())
        return false;

    JSFunction& setter = shape->setterObject()->as<JSFunction>();
    if (setter.isNative())
        return false;

    if (!setter.hasJITCode()) {
        if (isTemporarilyUnoptimizable)
            *isTemporarilyUnoptimizable = true;
        return false;
    }

    return true;
}

void


/* static */ bool
GetPropertyIC::canAttachDenseElementHole(JSObject* obj, HandleValue idval, TypedOrValueRegister output)
{

    if (!idval.isInt32() || idval.toInt32() < 0)
        return false;

    if (!output.hasValue())
        return false;

    if (!obj->isNative())
        return false;

    if (obj->as<NativeObject>().getDenseInitializedLength() == 0)
        return false;

    do {
        if (obj->isIndexed())
            return false;

        if (ClassCanHaveExtraProperties(obj->getClass()))
            return false;

        JSObject* proto = obj->staticPrototype();
        if (!proto)
            break;

        if (!proto->isNative())
            return false;

        // Make sure objects on the prototype don't have dense elements.
        if (proto->as<NativeObject>().getDenseInitializedLength() != 0)
            return false;

        obj = proto;
    } while (obj);

    return true;
}

static bool
GenerateDenseElementHole(JSContext* cx, MacroAssembler& masm, IonCache::StubAttacher& attacher,
                         IonScript* ion, JSObject* obj, HandleValue idval,
                         Register object, TypedOrValueRegister index, TypedOrValueRegister output)
{
    MOZ_ASSERT(GetPropertyIC::canAttachDenseElementHole(obj, idval, output));

    Register scratchReg = output.valueReg().scratchReg();

    // Guard on the shape and group, to prevent non-dense elements from appearing.
    Label failures;
    attacher.branchNextStubOrLabel(masm, Assembler::NotEqual,
                                   Address(object, ShapedObject::offsetOfShape()),
                                   ImmGCPtr(obj->as<NativeObject>().lastProperty()), &failures);


    if (obj->hasUncacheableProto()) {
        masm.loadPtr(Address(object, JSObject::offsetOfGroup()), scratchReg);
        Address proto(scratchReg, ObjectGroup::offsetOfProto());
        masm.branchPtr(Assembler::NotEqual, proto, ImmGCPtr(obj->staticPrototype()), &failures);
    }

    JSObject* pobj = obj->staticPrototype();
    while (pobj) {
        MOZ_ASSERT(pobj->as<NativeObject>().lastProperty());

        masm.movePtr(ImmGCPtr(pobj), scratchReg);

        // Non-singletons with uncacheable protos can change their proto
        // without a shape change, so also guard on the group (which determines
        // the proto) in this case.
        if (pobj->hasUncacheableProto() && !pobj->isSingleton()) {
            Address groupAddr(scratchReg, JSObject::offsetOfGroup());
            masm.branchPtr(Assembler::NotEqual, groupAddr, ImmGCPtr(pobj->group()), &failures);
        }

        // Make sure the shape matches, to avoid non-dense elements.
        masm.branchPtr(Assembler::NotEqual, Address(scratchReg, ShapedObject::offsetOfShape()),
                       ImmGCPtr(pobj->as<NativeObject>().lastProperty()), &failures);

        // Load elements vector.
        masm.loadPtr(Address(scratchReg, NativeObject::offsetOfElements()), scratchReg);

        // Also make sure there are no dense elements.
        Label hole;
        Address initLength(scratchReg, ObjectElements::offsetOfInitializedLength());
        masm.branch32(Assembler::NotEqual, initLength, Imm32(0), &failures);

        pobj = pobj->staticPrototype();
    }

    // Ensure the index is an int32 value.
    Register indexReg;
    if (index.hasValue()) {
        // Unbox the index.
        ValueOperand val = index.valueReg();
        masm.branchTestInt32(Assembler::NotEqual, val, &failures);
        indexReg = scratchReg;
        masm.unboxInt32(val, indexReg);
    } else {
        MOZ_ASSERT(index.type() == MIRType::Int32);
        indexReg = index.typedReg().gpr();
    }

    // Make sure index is nonnegative.
    masm.branch32(Assembler::LessThan, indexReg, Imm32(0), &failures);

    // Save the object register.
    Register elementsReg = object;
    masm.push(object);

    // Load elements vector.
    masm.loadPtr(Address(object, NativeObject::offsetOfElements()), elementsReg);

    // Guard on the initialized length.
    Label hole;
    Address initLength(elementsReg, ObjectElements::offsetOfInitializedLength());
    masm.branch32(Assembler::BelowOrEqual, initLength, indexReg, &hole);

    // Load the value.
    Label done;
    masm.loadValue(BaseObjectElementIndex(elementsReg, indexReg), output.valueReg());
    masm.branchTestMagic(Assembler::NotEqual, output.valueReg(), &done);

    // Load undefined for the hole.
    masm.bind(&hole);
    masm.moveValue(UndefinedValue(), output.valueReg());

    masm.bind(&done);
    // Restore the object register.
    if (elementsReg == object)
        masm.pop(object);
    attacher.jumpRejoin(masm);

    // All failure flows through here.
    masm.bind(&failures);
    attacher.jumpNextStub(masm);

    return true;
}

bool
GetPropertyIC::tryAttachDenseElementHole(JSContext* cx, HandleScript outerScript, IonScript* ion,
                                         HandleObject obj, HandleValue idval, bool* emitted)
{
    MOZ_ASSERT(canAttachStub());
    MOZ_ASSERT(!*emitted);

    if (!monitoredResult())
        return true;

    if (!canAttachDenseElementHole(obj, idval, output()))
        return true;

    *emitted = true;

    MacroAssembler masm(cx, ion, outerScript, profilerLeavePc_);
    StubAttacher attacher(*this);
    GenerateDenseElementHole(cx, masm, attacher, ion, obj, idval, object(), id().reg(), output());

    return linkAndAttachStub(cx, masm, attacher, ion, "dense hole",
                             JS::TrackedOutcome::ICGetElemStub_DenseHole);
}

/* static */ bool
GetPropertyIC::canAttachTypedOrUnboxedArrayElement(JSObject* obj, const Value& idval,
                                                   TypedOrValueRegister output)
{
    if (!obj->is<TypedArrayObject>() && !obj->is<UnboxedArrayObject>())
        return false;

    MOZ_ASSERT(idval.isInt32() || idval.isString());

    // Don't emit a stub if the access is out of bounds. We make to make
    // certain that we monitor the type coming out of the typed array when
    // we generate the stub. Out of bounds accesses will hit the fallback
    // path.
    uint32_t index;
    if (idval.isInt32()) {
        index = idval.toInt32();
    } else {
        int32_t indexInt32 = GetIndexFromString(idval.toString());
        if (indexInt32 < 0)
            return false;
         index = indexInt32;
    }

    if (obj->is<TypedArrayObject>()) {
        if (index >= obj->as<TypedArrayObject>().length())
            return false;

        // The output register is not yet specialized as a float register, the only
        // way to accept float typed arrays for now is to return a Value type.
        uint32_t arrayType = obj->as<TypedArrayObject>().type();
        if (arrayType == Scalar::Float32 || arrayType == Scalar::Float64)
            return output.hasValue();

        return output.hasValue() || !output.typedReg().isFloat();
    }

    if (index >= obj->as<UnboxedArrayObject>().initializedLength())
        return false;

    JSValueType elementType = obj->as<UnboxedArrayObject>().elementType();
    if (elementType == JSVAL_TYPE_DOUBLE)
        return output.hasValue();

    return output.hasValue() || !output.typedReg().isFloat();
}

static void
GenerateGetTypedOrUnboxedArrayElement(JSContext* cx, MacroAssembler& masm,
                                      IonCache::StubAttacher& attacher,
                                      HandleObject array, const Value& idval, Register object,
                                      const ConstantOrRegister& index, TypedOrValueRegister output,
                                      bool allowDoubleResult)
{
    MOZ_ASSERT(GetPropertyIC::canAttachTypedOrUnboxedArrayElement(array, idval, output));

    Label failures;

    TestMatchingReceiver(masm, attacher, object, array, &failures);

    // Decide to what type index the stub should be optimized
    Register tmpReg = output.scratchReg().gpr();
    MOZ_ASSERT(tmpReg != InvalidReg);
    Register indexReg = tmpReg;
    if (idval.isString()) {
        MOZ_ASSERT(GetIndexFromString(idval.toString()) >= 0);

        if (index.constant()) {
            MOZ_ASSERT(idval == index.value());
            masm.move32(Imm32(GetIndexFromString(idval.toString())), indexReg);
        } else {
            // Part 1: Get the string into a register
            Register str;
            if (index.reg().hasValue()) {
                ValueOperand val = index.reg().valueReg();
                masm.branchTestString(Assembler::NotEqual, val, &failures);

                str = masm.extractString(val, indexReg);
            } else {
                MOZ_ASSERT(!index.reg().typedReg().isFloat());
                str = index.reg().typedReg().gpr();
            }

            // Part 2: Call to translate the str into index
            AllocatableRegisterSet regs(RegisterSet::Volatile());
            LiveRegisterSet save(regs.asLiveSet());
            masm.PushRegsInMask(save);
            regs.takeUnchecked(str);

            Register temp = regs.takeAnyGeneral();

            masm.setupUnalignedABICall(temp);
            masm.passABIArg(str);
            masm.callWithABI(JS_FUNC_TO_DATA_PTR(void*, GetIndexFromString));
            masm.mov(ReturnReg, indexReg);

            LiveRegisterSet ignore;
            ignore.add(indexReg);
            masm.PopRegsInMaskIgnore(save, ignore);

            masm.branchTest32(Assembler::Signed, indexReg, indexReg, &failures);
        }
    } else {
        MOZ_ASSERT(idval.isInt32());
        MOZ_ASSERT(!index.constant());

        if (index.reg().hasValue()) {
            ValueOperand val = index.reg().valueReg();
            masm.branchTestInt32(Assembler::NotEqual, val, &failures);

            // Unbox the index.
            masm.unboxInt32(val, indexReg);
        } else {
            MOZ_ASSERT(!index.reg().typedReg().isFloat());
            indexReg = index.reg().typedReg().gpr();
        }
    }

    Label popObjectAndFail;

    if (array->is<TypedArrayObject>()) {
        // Guard on the initialized length.
        Address length(object, TypedArrayObject::lengthOffset());
        masm.branch32(Assembler::BelowOrEqual, length, indexReg, &failures);

        // Save the object register on the stack in case of failure.
        Register elementReg = object;
        masm.push(object);

        // Load elements vector.
        masm.loadPtr(Address(object, TypedArrayObject::dataOffset()), elementReg);

        // Load the value. We use an invalid register because the destination
        // register is necessary a non double register.
        Scalar::Type arrayType = array->as<TypedArrayObject>().type();
        int width = Scalar::byteSize(arrayType);
        BaseIndex source(elementReg, indexReg, ScaleFromElemWidth(width));
        if (output.hasValue()) {
            masm.loadFromTypedArray(arrayType, source, output.valueReg(), allowDoubleResult,
                                    elementReg, &popObjectAndFail);
        } else {
            masm.loadFromTypedArray(arrayType, source, output.typedReg(), elementReg, &popObjectAndFail);
        }
    } else {
        // Save the object register on the stack in case of failure.
        masm.push(object);

        // Guard on the initialized length.
        masm.load32(Address(object, UnboxedArrayObject::offsetOfCapacityIndexAndInitializedLength()), object);
        masm.and32(Imm32(UnboxedArrayObject::InitializedLengthMask), object);
        masm.branch32(Assembler::BelowOrEqual, object, indexReg, &popObjectAndFail);

        // Load elements vector.
        Register elementReg = object;
        masm.loadPtr(Address(masm.getStackPointer(), 0), object);
        masm.loadPtr(Address(object, UnboxedArrayObject::offsetOfElements()), elementReg);

        JSValueType elementType = array->as<UnboxedArrayObject>().elementType();
        BaseIndex source(elementReg, indexReg, ScaleFromElemWidth(UnboxedTypeSize(elementType)));
        masm.loadUnboxedProperty(source, elementType, output);
    }

    masm.pop(object);
    attacher.jumpRejoin(masm);

    // Restore the object before continuing to the next stub.
    masm.bind(&popObjectAndFail);
    masm.pop(object);
    masm.bind(&failures);

    attacher.jumpNextStub(masm);
}

bool
GetPropertyIC::tryAttachTypedOrUnboxedArrayElement(JSContext* cx, HandleScript outerScript,
                                                   IonScript* ion, HandleObject obj,
                                                   HandleValue idval, bool* emitted)
{
    MOZ_ASSERT(canAttachStub());
    MOZ_ASSERT(!*emitted);

    if (!canAttachTypedOrUnboxedArrayElement(obj, idval, output()))
        return true;

    *emitted = true;

    MacroAssembler masm(cx, ion, outerScript, profilerLeavePc_);
    StubAttacher attacher(*this);
    GenerateGetTypedOrUnboxedArrayElement(cx, masm, attacher, obj, idval, object(), id(),
                                          output(), allowDoubleResult_);
    return linkAndAttachStub(cx, masm, attacher, ion, "typed array",
                             JS::TrackedOutcome::ICGetElemStub_TypedArray);
}

bool
GetPropertyIC::tryAttachArgumentsElement(JSContext* cx, HandleScript outerScript, IonScript* ion,
                                         HandleObject obj, HandleValue idval, bool* emitted)
{
    MOZ_ASSERT(canAttachStub());
    MOZ_ASSERT(!*emitted);

    if (!IsOptimizableArgumentsObjectForGetElem(obj, idval))
        return true;

    MOZ_ASSERT(obj->is<ArgumentsObject>());

    if (hasArgumentsElementStub(obj->is<MappedArgumentsObject>()))
        return true;

    TypedOrValueRegister index = id().reg();
    if (index.type() != MIRType::Value && index.type() != MIRType::Int32)
        return true;

    MOZ_ASSERT(output().hasValue());

    *emitted = true;

    Label failures;
    MacroAssembler masm(cx, ion, outerScript, profilerLeavePc_);
    StubAttacher attacher(*this);

    Register tmpReg = output().scratchReg().gpr();
    MOZ_ASSERT(tmpReg != InvalidReg);

    masm.branchTestObjClass(Assembler::NotEqual, object(), tmpReg, obj->getClass(), &failures);

    // Get initial ArgsObj length value, test if length or any element have
    // been overridden.
    masm.unboxInt32(Address(object(), ArgumentsObject::getInitialLengthSlotOffset()), tmpReg);
    masm.branchTest32(Assembler::NonZero, tmpReg,
                      Imm32(ArgumentsObject::LENGTH_OVERRIDDEN_BIT |
                            ArgumentsObject::ELEMENT_OVERRIDDEN_BIT),
                      &failures);
    masm.rshiftPtr(Imm32(ArgumentsObject::PACKED_BITS_COUNT), tmpReg);

    // Decide to what type index the stub should be optimized
    Register indexReg;

    // Check index against length.
    Label failureRestoreIndex;
    if (index.hasValue()) {
        ValueOperand val = index.valueReg();
        masm.branchTestInt32(Assembler::NotEqual, val, &failures);
        indexReg = val.scratchReg();

        masm.unboxInt32(val, indexReg);
        masm.branch32(Assembler::AboveOrEqual, indexReg, tmpReg, &failureRestoreIndex);
    } else {
        MOZ_ASSERT(index.type() == MIRType::Int32);
        indexReg = index.typedReg().gpr();
        masm.branch32(Assembler::AboveOrEqual, indexReg, tmpReg, &failures);
    }

    // Fail if we have a RareArgumentsData (elements were deleted).
    masm.loadPrivate(Address(object(), ArgumentsObject::getDataSlotOffset()), tmpReg);
    masm.branchPtr(Assembler::NotEqual,
                   Address(tmpReg, offsetof(ArgumentsData, rareData)),
                   ImmWord(0),
                   &failureRestoreIndex);

    // Get the address to load from into tmpReg
    masm.loadPrivate(Address(object(), ArgumentsObject::getDataSlotOffset()), tmpReg);
    masm.addPtr(Imm32(ArgumentsData::offsetOfArgs()), tmpReg);

    BaseValueIndex elemIdx(tmpReg, indexReg);

    // Ensure result is not magic value, and type-check result.
    masm.branchTestMagic(Assembler::Equal, elemIdx, &failureRestoreIndex);

    masm.loadTypedOrValue(elemIdx, output());

    // indexReg may need to be reconstructed if it was originally a value.
    if (index.hasValue())
        masm.tagValue(JSVAL_TYPE_INT32, indexReg, index.valueReg());

    // Success.
    attacher.jumpRejoin(masm);

    // Restore the object before continuing to the next stub.
    masm.pop(indexReg);
    masm.bind(&failureRestoreIndex);
    if (index.hasValue())
        masm.tagValue(JSVAL_TYPE_INT32, indexReg, index.valueReg());
    masm.bind(&failures);
    attacher.jumpNextStub(masm);

    if (obj->is<UnmappedArgumentsObject>()) {
        MOZ_ASSERT(!hasUnmappedArgumentsElementStub_);
        hasUnmappedArgumentsElementStub_ = true;
        return linkAndAttachStub(cx, masm, attacher, ion, "ArgsObj element (unmapped)",
                                 JS::TrackedOutcome::ICGetElemStub_ArgsElementUnmapped);
    }

    MOZ_ASSERT(!hasMappedArgumentsElementStub_);
    hasMappedArgumentsElementStub_ = true;
    return linkAndAttachStub(cx, masm, attacher, ion, "ArgsObj element (mapped)",
                             JS::TrackedOutcome::ICGetElemStub_ArgsElementMapped);
}

jit::EmitIonStoreDenseElement(MacroAssembler& masm, const ConstantOrRegister& value,
                              Register elements, BaseObjectElementIndex target)
{
    // If the ObjectElements::CONVERT_DOUBLE_ELEMENTS flag is set, int32 values
    // have to be converted to double first. If the value is not int32, it can
    // always be stored directly.

    Address elementsFlags(elements, ObjectElements::offsetOfFlags());
    if (value.constant()) {
        Value v = value.value();
        Label done;
        if (v.isInt32()) {
            Label dontConvert;
            masm.branchTest32(Assembler::Zero, elementsFlags,
                              Imm32(ObjectElements::CONVERT_DOUBLE_ELEMENTS),
                              &dontConvert);
            masm.storeValue(DoubleValue(v.toInt32()), target);
            masm.jump(&done);
            masm.bind(&dontConvert);
        }
        masm.storeValue(v, target);
        masm.bind(&done);
        return;
    }

    TypedOrValueRegister reg = value.reg();
    if (reg.hasTyped() && reg.type() != MIRType::Int32) {
        masm.storeTypedOrValue(reg, target);
        return;
    }

    Label convert, storeValue, done;
    masm.branchTest32(Assembler::NonZero, elementsFlags,
                      Imm32(ObjectElements::CONVERT_DOUBLE_ELEMENTS),
                      &convert);
    masm.bind(&storeValue);
    masm.storeTypedOrValue(reg, target);
    masm.jump(&done);

    masm.bind(&convert);
    if (reg.hasValue()) {
        masm.branchTestInt32(Assembler::NotEqual, reg.valueReg(), &storeValue);
        masm.int32ValueToDouble(reg.valueReg(), ScratchDoubleReg);
        masm.storeDouble(ScratchDoubleReg, target);
    } else {
        MOZ_ASSERT(reg.type() == MIRType::Int32);
        masm.convertInt32ToDouble(reg.typedReg().gpr(), ScratchDoubleReg);
        masm.storeDouble(ScratchDoubleReg, target);
    }

    masm.bind(&done);
}

static bool
GenerateSetDenseElement(JSContext* cx, MacroAssembler& masm, IonCache::StubAttacher& attacher,
                        JSObject* obj, const Value& idval, bool guardHoles, Register object,
                        TypedOrValueRegister index, const ConstantOrRegister& value,
                        Register tempToUnboxIndex, Register temp,
                        bool needsTypeBarrier, bool checkTypeset)
{
    MOZ_ASSERT(obj->isNative());
    MOZ_ASSERT(idval.isInt32());

    Label failures;

    // Guard object is a dense array.
    Shape* shape = obj->as<NativeObject>().lastProperty();
    if (!shape)
        return false;
    masm.branchTestObjShape(Assembler::NotEqual, object, shape, &failures);

    // Guard that the incoming value is in the type set for the property
    // if a type barrier is required.
    if (needsTypeBarrier) {
        masm.branchTestObjGroup(Assembler::NotEqual, object, obj->group(), &failures);
        if (checkTypeset)
            CheckTypeSetForWrite(masm, obj, JSID_VOID, temp, value, &failures);
    }

    // Ensure the index is an int32 value.
    Register indexReg;
    if (index.hasValue()) {
        ValueOperand val = index.valueReg();
        masm.branchTestInt32(Assembler::NotEqual, val, &failures);

        indexReg = masm.extractInt32(val, tempToUnboxIndex);
    } else {
        MOZ_ASSERT(!index.typedReg().isFloat());
        indexReg = index.typedReg().gpr();
    }

    {
        // Load obj->elements.
        Register elements = temp;
        masm.loadPtr(Address(object, NativeObject::offsetOfElements()), elements);

        // Compute the location of the element.
        BaseObjectElementIndex target(elements, indexReg);

        Label storeElement;

        // If TI cannot help us deal with HOLES by preventing indexed properties
        // on the prototype chain, we have to be very careful to check for ourselves
        // to avoid stomping on what should be a setter call. Start by only allowing things
        // within the initialized length.
        if (guardHoles) {
            Address initLength(elements, ObjectElements::offsetOfInitializedLength());
            masm.branch32(Assembler::BelowOrEqual, initLength, indexReg, &failures);
        } else {
            // Guard that we can increase the initialized length.
            Address capacity(elements, ObjectElements::offsetOfCapacity());
            masm.branch32(Assembler::BelowOrEqual, capacity, indexReg, &failures);

            // Guard on the initialized length.
            Address initLength(elements, ObjectElements::offsetOfInitializedLength());
            masm.branch32(Assembler::Below, initLength, indexReg, &failures);

            // if (initLength == index)
            Label inBounds;
            masm.branch32(Assembler::NotEqual, initLength, indexReg, &inBounds);
            {
                // Increase initialize length.
                Register newLength = indexReg;
                masm.add32(Imm32(1), newLength);
                masm.store32(newLength, initLength);

                // Increase length if needed.
                Label bumpedLength;
                Address length(elements, ObjectElements::offsetOfLength());
                masm.branch32(Assembler::AboveOrEqual, length, indexReg, &bumpedLength);
                masm.store32(newLength, length);
                masm.bind(&bumpedLength);

                // Restore the index.
                masm.add32(Imm32(-1), newLength);
                masm.jump(&storeElement);
            }
            // else
            masm.bind(&inBounds);
        }
        if (cx->zone()->needsIncrementalBarrier())
            masm.guardedCallPreBarrier(target, MIRType::Value);

        // Store the value.
        if (guardHoles)
            masm.branchTestMagic(Assembler::Equal, target, &failures);
        else
            masm.bind(&storeElement);
        StoreDenseElement(masm, value, elements, target);
    }
    attacher.jumpRejoin(masm);

    masm.bind(&failures);
    attacher.jumpNextStub(masm);

    return true;
}

bool
SetPropertyIC::tryAttachDenseElement(JSContext* cx, HandleScript outerScript, IonScript* ion,
                                     HandleObject obj, const Value& idval, bool* emitted)
{
    MOZ_ASSERT(!*emitted);
    MOZ_ASSERT(canAttachStub());

    if (hasDenseStub())
        return true;

    bool checkTypeset = false;
    if (!IsDenseElementSetInlineable(obj, idval, value(), needsTypeBarrier(), &checkTypeset))
        return true;

    *emitted = true;

    MacroAssembler masm(cx, ion, outerScript, profilerLeavePc_);
    StubAttacher attacher(*this);
    if (!GenerateSetDenseElement(cx, masm, attacher, obj, idval,
                                 guardHoles(), object(), id().reg(),
                                 value(), tempToUnboxIndex(), temp(),
                                 needsTypeBarrier(), checkTypeset))
    {
        return false;
    }

    setHasDenseStub();
    const char* message = guardHoles() ?  "dense array (holes)" : "dense array";
    return linkAndAttachStub(cx, masm, attacher, ion, message,
                             JS::TrackedOutcome::ICSetElemStub_Dense);
}

bool
BindNameIC::attachGlobal(JSContext* cx, HandleScript outerScript, IonScript* ion,
                         HandleObject envChain)
{
    MOZ_ASSERT(envChain->is<GlobalObject>());

    MacroAssembler masm(cx, ion, outerScript, profilerLeavePc_);
    StubAttacher attacher(*this);

    // Guard on the env chain.
    attacher.branchNextStub(masm, Assembler::NotEqual, environmentChainReg(),
                            ImmGCPtr(envChain));
    masm.movePtr(ImmGCPtr(envChain), outputReg());

    attacher.jumpRejoin(masm);

    return linkAndAttachStub(cx, masm, attacher, ion, "global");
}

static inline void
GenerateEnvironmentChainGuard(MacroAssembler& masm, JSObject* envObj,
                              Register envObjReg, Shape* shape, Label* failures)
{
    if (envObj->is<CallObject>()) {
        // We can skip a guard on the call object if the script's bindings are
        // guaranteed to be immutable (and thus cannot introduce shadowing
        // variables).
        CallObject* callObj = &envObj->as<CallObject>();
        JSFunction* fun = &callObj->callee();
        // The function might have been relazified under rare conditions.
        // In that case, we pessimistically create the guard, as we'd
        // need to root various pointers to delazify,
        if (fun->hasScript()) {
            JSScript* script = fun->nonLazyScript();
            if (!script->funHasExtensibleScope())
                return;
        }
    } else if (envObj->is<GlobalObject>()) {
        // If this is the last object on the scope walk, and the property we've
        // found is not configurable, then we don't need a shape guard because
        // the shape cannot be removed.
        if (shape && !shape->configurable())
            return;
    }

    Address shapeAddr(envObjReg, ShapedObject::offsetOfShape());
    masm.branchPtr(Assembler::NotEqual, shapeAddr,
                   ImmGCPtr(envObj->as<NativeObject>().lastProperty()), failures);
}

static void
GenerateEnvironmentChainGuards(MacroAssembler& masm, JSObject* envChain, JSObject* holder,
                               Register outputReg, Label* failures, bool skipLastGuard = false)
{
    JSObject* tobj = envChain;

    // Walk up the env chain. Note that IsCacheableEnvironmentChain guarantees the
    // |tobj == holder| condition terminates the loop.
    while (true) {
        MOZ_ASSERT(IsCacheableEnvironment(tobj) || tobj->is<GlobalObject>());

        if (skipLastGuard && tobj == holder)
            break;

        GenerateEnvironmentChainGuard(masm, tobj, outputReg, nullptr, failures);

        if (tobj == holder)
            break;

        // Load the next link.
        tobj = &tobj->as<EnvironmentObject>().enclosingEnvironment();
        masm.extractObject(Address(outputReg, EnvironmentObject::offsetOfEnclosingEnvironment()),
                           outputReg);
    }
}

bool
BindNameIC::attachNonGlobal(JSContext* cx, HandleScript outerScript, IonScript* ion,
                            HandleObject envChain, HandleObject holder)
{
    MOZ_ASSERT(IsCacheableEnvironment(envChain));

    MacroAssembler masm(cx, ion, outerScript, profilerLeavePc_);
    StubAttacher attacher(*this);

    // Guard on the shape of the env chain.
    Label failures;
    attacher.branchNextStubOrLabel(masm, Assembler::NotEqual,
                                   Address(environmentChainReg(), ShapedObject::offsetOfShape()),
                                   ImmGCPtr(envChain->as<NativeObject>().lastProperty()),
                                   holder != envChain ? &failures : nullptr);

    if (holder != envChain) {
        JSObject* parent = &envChain->as<EnvironmentObject>().enclosingEnvironment();
        masm.extractObject(Address(environmentChainReg(),
                                   EnvironmentObject::offsetOfEnclosingEnvironment()),
                           outputReg());

        GenerateEnvironmentChainGuards(masm, parent, holder, outputReg(), &failures);
    } else {
        masm.movePtr(environmentChainReg(), outputReg());
    }

    // At this point outputReg holds the object on which the property
    // was found, so we're done.
    attacher.jumpRejoin(masm);

    // All failures flow to here, so there is a common point to patch.
    if (holder != envChain) {
        masm.bind(&failures);
        attacher.jumpNextStub(masm);
    }

    return linkAndAttachStub(cx, masm, attacher, ion, "non-global");
}

static bool
IsCacheableNonGlobalEnvironmentChain(JSObject* envChain, JSObject* holder)
{
    while (true) {
        if (!IsCacheableEnvironment(envChain)) {
            JitSpew(JitSpew_IonIC, "Non-cacheable object on env chain");
            return false;
        }

        if (envChain == holder)
            return true;

        envChain = &envChain->as<EnvironmentObject>().enclosingEnvironment();
        if (!envChain) {
            JitSpew(JitSpew_IonIC, "env chain indirect hit");
            return false;
        }
    }

    MOZ_CRASH("Invalid env chain");
}

JSObject*
BindNameIC::update(JSContext* cx, HandleScript outerScript, size_t cacheIndex,
                   HandleObject envChain)
{
    IonScript* ion = outerScript->ionScript();
    BindNameIC& cache = ion->getCache(cacheIndex).toBindName();
    HandlePropertyName name = cache.name();

    RootedObject holder(cx);
    if (!LookupNameUnqualified(cx, name, envChain, &holder))
        return nullptr;

    // Stop generating new stubs once we hit the stub count limit, see
    // GetPropertyCache.
    if (cache.canAttachStub()) {
        if (envChain->is<GlobalObject>()) {
            if (!cache.attachGlobal(cx, outerScript, ion, envChain))
                return nullptr;
        } else if (IsCacheableNonGlobalEnvironmentChain(envChain, holder)) {
            if (!cache.attachNonGlobal(cx, outerScript, ion, envChain, holder))
                return nullptr;
        } else {
            JitSpew(JitSpew_IonIC, "BINDNAME uncacheable env chain");
        }
    }

    return holder;
}

bool
NameIC::attachReadSlot(JSContext* cx, HandleScript outerScript, IonScript* ion,
                       HandleObject envChain, HandleObject holderBase,
                       HandleNativeObject holder, Handle<PropertyResult> prop)
{
    MacroAssembler masm(cx, ion, outerScript, profilerLeavePc_);
    Label failures;
    StubAttacher attacher(*this);

    Register scratchReg = outputReg().valueReg().scratchReg();

    // Don't guard the base of the proto chain the name was found on. It will be guarded
    // by GenerateReadSlot().
    masm.mov(environmentChainReg(), scratchReg);
    GenerateEnvironmentChainGuards(masm, envChain, holderBase, scratchReg, &failures,
                             /* skipLastGuard = */true);

    // GenerateEnvironmentChain leaves the last env chain in scratchReg, even though it
    // doesn't generate the extra guard.
    //
    // NAME ops must do their own TDZ checks.
    GenerateReadSlot(cx, ion, masm, attacher, CheckTDZ, holderBase, holder, prop, scratchReg,
                     outputReg(), failures.used() ? &failures : nullptr);

    return linkAndAttachStub(cx, masm, attacher, ion, "generic",
                             JS::TrackedOutcome::ICNameStub_ReadSlot);
}

static bool
IsCacheableEnvironmentChain(JSObject* envChain, JSObject* obj)
{
    JSObject* obj2 = envChain;
    while (obj2) {
        if (!IsCacheableEnvironment(obj2) && !obj2->is<GlobalObject>())
            return false;

        // Stop once we hit the global or target obj.
        if (obj2->is<GlobalObject>() || obj2 == obj)
            break;

        obj2 = obj2->enclosingEnvironment();
    }

    return obj == obj2;
}

static bool
IsCacheableNameReadSlot(JSContext* cx, HandleObject envChain, HandleObject obj,
                        HandleObject holder, Handle<PropertyResult> prop, jsbytecode* pc,
                        const TypedOrValueRegister& output)
{
    if (!prop)
        return false;
    if (!obj->isNative())
        return false;

    if (obj->is<GlobalObject>()) {
        // Support only simple property lookups.
        if (!IsCacheableGetPropReadSlotForIonOrCacheIR(obj, holder, prop) &&
            !IsCacheableNoProperty(obj, holder, prop, pc, output))
            return false;
    } else if (obj->is<ModuleEnvironmentObject>()) {
        // We don't yet support lookups in a module environment.
        return false;
    } else if (obj->is<CallObject>()) {
        MOZ_ASSERT(obj == holder);
        if (!prop.shape()->hasDefaultGetter())
            return false;
    } else {
        // We don't yet support lookups on Block or DeclEnv objects.
        return false;
    }

    return IsCacheableEnvironmentChain(envChain, obj);
}

bool
NameIC::attachCallGetter(JSContext* cx, HandleScript outerScript, IonScript* ion,
                         HandleObject envChain, HandleObject obj, HandleObject holder,
                         HandleShape shape, void* returnAddr)
{
    MacroAssembler masm(cx, ion, outerScript, profilerLeavePc_);
    StubAttacher attacher(*this);

    Label failures;
    Register scratchReg = outputReg().valueReg().scratchReg();

    // Don't guard the base of the proto chain the name was found on. It will be guarded
    // by GenerateCallGetter().
    masm.mov(environmentChainReg(), scratchReg);
    GenerateEnvironmentChainGuards(masm, envChain, obj, scratchReg, &failures,
                             /* skipLastGuard = */true);

    // GenerateEnvironmentChain leaves the last env chain in scratchReg, even though it
    // doesn't generate the extra guard.
    if (!GenerateCallGetter(cx, ion, masm, attacher, obj, holder, shape, liveRegs_,
                            scratchReg, outputReg(), returnAddr,
                            failures.used() ? &failures : nullptr))
    {
         return false;
    }

    const char* attachKind = "name getter";
    return linkAndAttachStub(cx, masm, attacher, ion, attachKind,
                             JS::TrackedOutcome::ICNameStub_CallGetter);
}

static bool
IsCacheableNameCallGetter(HandleObject envChain, HandleObject obj, HandleObject holder,
                          Handle<PropertyResult> prop)
{
    if (!prop)
        return false;
    if (!obj->is<GlobalObject>())
        return false;

    if (!IsCacheableEnvironmentChain(envChain, obj))
        return false;

    if (!prop || !IsCacheableProtoChainForIonOrCacheIR(obj, holder))
        return false;

    Shape* shape = prop.shape();
    return IsCacheableGetPropCallNative(obj, holder, shape) ||
        IsCacheableGetPropCallPropertyOp(obj, holder, shape) ||
        IsCacheableGetPropCallScripted(obj, holder, shape);
}

bool
NameIC::attachTypeOfNoProperty(JSContext* cx, HandleScript outerScript, IonScript* ion,
                               HandleObject envChain)
{
    MacroAssembler masm(cx, ion, outerScript, profilerLeavePc_);
    Label failures;
    StubAttacher attacher(*this);

    Register scratchReg = outputReg().valueReg().scratchReg();

    masm.movePtr(environmentChainReg(), scratchReg);

    // Generate env chain guards.
    // Since the property was not defined on any object, iterate until reaching the global.
    JSObject* tobj = envChain;
    while (true) {
        GenerateEnvironmentChainGuard(masm, tobj, scratchReg, nullptr, &failures);

        if (tobj->is<GlobalObject>())
            break;

        // Load the next link.
        tobj = &tobj->as<EnvironmentObject>().enclosingEnvironment();
        masm.extractObject(Address(scratchReg, EnvironmentObject::offsetOfEnclosingEnvironment()),
                           scratchReg);
    }

    masm.moveValue(UndefinedValue(), outputReg().valueReg());
    attacher.jumpRejoin(masm);

    masm.bind(&failures);
    attacher.jumpNextStub(masm);

    return linkAndAttachStub(cx, masm, attacher, ion, "generic",
                             JS::TrackedOutcome::ICNameStub_TypeOfNoProperty);
}

static bool
IsCacheableNameNoProperty(HandleObject envChain, HandleObject obj,
                          HandleObject holder, Handle<PropertyResult> prop, jsbytecode* pc,
                          NameIC& cache)
{
    if (cache.isTypeOf() && !prop) {
        MOZ_ASSERT(!obj);
        MOZ_ASSERT(!holder);
        MOZ_ASSERT(envChain);

        // Assert those extra things checked by IsCacheableNoProperty().
        MOZ_ASSERT(cache.outputReg().hasValue());
        MOZ_ASSERT(pc != nullptr);

        return true;
    }

    return false;
}

bool
NameIC::update(JSContext* cx, HandleScript outerScript, size_t cacheIndex, HandleObject envChain,
               MutableHandleValue vp)
{
    IonScript* ion = outerScript->ionScript();

    NameIC& cache = ion->getCache(cacheIndex).toName();
    RootedPropertyName name(cx, cache.name());

    RootedScript script(cx);
    jsbytecode* pc;
    cache.getScriptedLocation(&script, &pc);

    RootedObject obj(cx);
    RootedObject holder(cx);
    Rooted<PropertyResult> prop(cx);
    if (!LookupName(cx, name, envChain, &obj, &holder, &prop))
        return false;

    // Look first. Don't generate cache entries if the lookup fails.
    if (cache.isTypeOf()) {
        if (!FetchName<true>(cx, obj, holder, name, prop, vp))
            return false;
    } else {
        if (!FetchName<false>(cx, obj, holder, name, prop, vp))
            return false;
    }

    if (cache.canAttachStub()) {
        if (IsCacheableNameReadSlot(cx, envChain, obj, holder, prop, pc, cache.outputReg())) {
            if (!cache.attachReadSlot(cx, outerScript, ion, envChain, obj,
                                      holder.as<NativeObject>(), prop))
            {
                return false;
            }
        } else if (IsCacheableNameCallGetter(envChain, obj, holder, prop)) {
            void* returnAddr = GetReturnAddressToIonCode(cx);
            RootedShape shape(cx, prop.shape());
            if (!cache.attachCallGetter(cx, outerScript, ion, envChain, obj, holder, shape,
                                        returnAddr))
            {
                return false;
            }
        } else if (IsCacheableNameNoProperty(envChain, obj, holder, prop, pc, cache)) {
            if (!cache.attachTypeOfNoProperty(cx, outerScript, ion, envChain))
                return false;
        }
    }

    // Monitor changes to cache entry.
    TypeScript::Monitor(cx, script, pc, vp);

    return true;
}
