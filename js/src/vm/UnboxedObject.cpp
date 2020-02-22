/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * vim: set ts=8 sts=4 et sw=4 tw=99:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#include "vm/UnboxedObject-inl.h"

#include "jit/BaselineIC.h"
#include "jit/ExecutableAllocator.h"
#include "jit/JitCommon.h"
#include "jit/Linker.h"

#include "jsobjinlines.h"

#include "gc/Nursery-inl.h"
#include "jit/MacroAssembler-inl.h"
#include "vm/Shape-inl.h"

using mozilla::ArrayLength;
using mozilla::DebugOnly;
using mozilla::PodCopy;

using namespace js;

/////////////////////////////////////////////////////////////////////
// UnboxedLayout
/////////////////////////////////////////////////////////////////////

void
UnboxedLayout::trace(JSTracer* trc)
{
    for (size_t i = 0; i < properties_.length(); i++)
        TraceManuallyBarrieredEdge(trc, &properties_[i].name, "unboxed_layout_name");

    if (newScript())
        newScript()->trace(trc);

    TraceNullableEdge(trc, &nativeGroup_, "unboxed_layout_nativeGroup");
    TraceNullableEdge(trc, &nativeShape_, "unboxed_layout_nativeShape");
    TraceNullableEdge(trc, &allocationScript_, "unboxed_layout_allocationScript");
    TraceNullableEdge(trc, &replacementGroup_, "unboxed_layout_replacementGroup");
    TraceNullableEdge(trc, &constructorCode_, "unboxed_layout_constructorCode");
}

size_t
UnboxedLayout::sizeOfIncludingThis(mozilla::MallocSizeOf mallocSizeOf)
{
    return mallocSizeOf(this)
         + properties_.sizeOfExcludingThis(mallocSizeOf)
         + (newScript() ? newScript()->sizeOfIncludingThis(mallocSizeOf) : 0)
         + mallocSizeOf(traceList());
}

void
UnboxedLayout::setNewScript(TypeNewScript* newScript, bool writeBarrier /* = true */)
{
    if (newScript_ && writeBarrier)
        TypeNewScript::writeBarrierPre(newScript_);
    newScript_ = newScript;
}

// Constructor code returns a 0x1 value to indicate the constructor code should
// be cleared.
static const uintptr_t CLEAR_CONSTRUCTOR_CODE_TOKEN = 0x1;

/* static */ bool
UnboxedLayout::makeConstructorCode(JSContext* cx, HandleObjectGroup group)
{
    gc::AutoSuppressGC suppress(cx);

    using namespace jit;

    if (!cx->compartment()->ensureJitCompartmentExists(cx))
        return false;

    UnboxedLayout& layout = group->unboxedLayout();
    MOZ_ASSERT(!layout.constructorCode());

    UnboxedPlainObject* templateObject = UnboxedPlainObject::create(cx, group, TenuredObject);
    if (!templateObject)
        return false;

    JitContext jitContext(cx, nullptr);

    MacroAssembler masm;

    Register propertiesReg, newKindReg;
#ifdef JS_CODEGEN_X86
    propertiesReg = eax;
    newKindReg = ecx;
    masm.loadPtr(Address(masm.getStackPointer(), sizeof(void*)), propertiesReg);
    masm.loadPtr(Address(masm.getStackPointer(), 2 * sizeof(void*)), newKindReg);
#else
    propertiesReg = IntArgReg0;
    newKindReg = IntArgReg1;
#endif

#ifdef JS_CODEGEN_ARM64
    // ARM64 communicates stack address via sp, but uses a pseudo-sp for addressing.
    masm.initStackPtr();
#endif

    MOZ_ASSERT(propertiesReg.volatile_());
    MOZ_ASSERT(newKindReg.volatile_());

    AllocatableGeneralRegisterSet regs(GeneralRegisterSet::All());
    regs.take(propertiesReg);
    regs.take(newKindReg);
    Register object = regs.takeAny(), scratch1 = regs.takeAny(), scratch2 = regs.takeAny();

    LiveGeneralRegisterSet savedNonVolatileRegisters = SavedNonVolatileRegisters(regs);
    masm.PushRegsInMask(savedNonVolatileRegisters);

    // The scratch double register might be used by MacroAssembler methods.
    if (ScratchDoubleReg.volatile_())
        masm.push(ScratchDoubleReg);

    Label failure, tenuredObject, allocated;
    masm.branch32(Assembler::NotEqual, newKindReg, Imm32(GenericObject), &tenuredObject);
    masm.branchTest32(Assembler::NonZero, AbsoluteAddress(group->addressOfFlags()),
                      Imm32(OBJECT_FLAG_PRE_TENURE), &tenuredObject);

    // Allocate an object in the nursery
    masm.createGCObject(object, scratch1, templateObject, gc::DefaultHeap, &failure,
                        /* initFixedSlots = */ false);

    masm.jump(&allocated);
    masm.bind(&tenuredObject);

    // Allocate an object in the tenured heap.
    masm.createGCObject(object, scratch1, templateObject, gc::TenuredHeap, &failure,
                        /* initFixedSlots = */ false);

    // If any of the properties being stored are in the nursery, add a store
    // buffer entry for the new object.
    Label postBarrier;
    for (size_t i = 0; i < layout.properties().length(); i++) {
        const UnboxedLayout::Property& property = layout.properties()[i];
        if (property.type == JSVAL_TYPE_OBJECT) {
            Address valueAddress(propertiesReg, i * sizeof(IdValuePair) + offsetof(IdValuePair, value));
            Label notObject;
            masm.branchTestObject(Assembler::NotEqual, valueAddress, &notObject);
            Register valueObject = masm.extractObject(valueAddress, scratch1);
            masm.branchPtrInNurseryChunk(Assembler::Equal, valueObject, scratch2, &postBarrier);
            masm.bind(&notObject);
        }
    }

    masm.jump(&allocated);
    masm.bind(&postBarrier);

    LiveGeneralRegisterSet liveVolatileRegisters;
    liveVolatileRegisters.add(propertiesReg);
    if (object.volatile_())
        liveVolatileRegisters.add(object);
    masm.PushRegsInMask(liveVolatileRegisters);

    masm.mov(ImmPtr(cx->runtime()), scratch1);
    masm.setupUnalignedABICall(scratch2);
    masm.passABIArg(scratch1);
    masm.passABIArg(object);
    masm.callWithABI(JS_FUNC_TO_DATA_PTR(void*, PostWriteBarrier));

    masm.PopRegsInMask(liveVolatileRegisters);

    masm.bind(&allocated);

    ValueOperand valueOperand;
#ifdef JS_NUNBOX32
    valueOperand = ValueOperand(scratch1, scratch2);
#else
    valueOperand = ValueOperand(scratch1);
#endif

    Label failureStoreOther, failureStoreObject;

    for (size_t i = 0; i < layout.properties().length(); i++) {
        const UnboxedLayout::Property& property = layout.properties()[i];
        Address valueAddress(propertiesReg, i * sizeof(IdValuePair) + offsetof(IdValuePair, value));
        Address targetAddress(object, UnboxedPlainObject::offsetOfData() + property.offset);

        masm.loadValue(valueAddress, valueOperand);

        if (property.type == JSVAL_TYPE_OBJECT) {
            HeapTypeSet* types = group->maybeGetProperty(IdToTypeId(NameToId(property.name)));

            Label notObject;
            masm.branchTestObject(Assembler::NotEqual, valueOperand,
                                  types->mightBeMIRType(MIRType::Null) ? &notObject : &failureStoreObject);

            Register payloadReg = masm.extractObject(valueOperand, scratch1);

            if (!types->hasType(TypeSet::AnyObjectType())) {
                Register scratch = (payloadReg == scratch1) ? scratch2 : scratch1;
                masm.guardObjectType(payloadReg, types, scratch, &failureStoreObject);
            }

            masm.storeUnboxedProperty(targetAddress, JSVAL_TYPE_OBJECT,
                                      TypedOrValueRegister(MIRType::Object,
                                                           AnyRegister(payloadReg)), nullptr);

            if (notObject.used()) {
                Label done;
                masm.jump(&done);
                masm.bind(&notObject);
                masm.branchTestNull(Assembler::NotEqual, valueOperand, &failureStoreOther);
                masm.storeUnboxedProperty(targetAddress, JSVAL_TYPE_OBJECT, NullValue(), nullptr);
                masm.bind(&done);
            }
        } else {
            masm.storeUnboxedProperty(targetAddress, property.type,
                                      ConstantOrRegister(valueOperand), &failureStoreOther);
        }
    }

    Label done;
    masm.bind(&done);

    if (object != ReturnReg)
        masm.movePtr(object, ReturnReg);

    // Restore non-volatile registers which were saved on entry.
    if (ScratchDoubleReg.volatile_())
        masm.pop(ScratchDoubleReg);
    masm.PopRegsInMask(savedNonVolatileRegisters);

    masm.abiret();

    masm.bind(&failureStoreOther);

    // There was a failure while storing a value which cannot be stored at all
    // in the unboxed object. Initialize the object so it is safe for GC and
    // return null.
    masm.initUnboxedObjectContents(object, templateObject);

    masm.bind(&failure);

    masm.movePtr(ImmWord(0), object);
    masm.jump(&done);

    masm.bind(&failureStoreObject);

    // There was a failure while storing a value to an object slot of the
    // unboxed object. If the value is storable, the failure occurred due to
    // incomplete type information in the object, so return a token to trigger
    // regeneration of the jitcode after a new object is created in the VM.
    {
        Label isObject;
        masm.branchTestObject(Assembler::Equal, valueOperand, &isObject);
        masm.branchTestNull(Assembler::NotEqual, valueOperand, &failureStoreOther);
        masm.bind(&isObject);
    }

    // Initialize the object so it is safe for GC.
    masm.initUnboxedObjectContents(object, templateObject);

    masm.movePtr(ImmWord(CLEAR_CONSTRUCTOR_CODE_TOKEN), object);
    masm.jump(&done);

    Linker linker(masm);
    AutoFlushICache afc("UnboxedObject");
    JitCode* code = linker.newCode<NoGC>(cx, OTHER_CODE);
    if (!code)
        return false;

    layout.setConstructorCode(code);
    return true;
}

void
UnboxedLayout::detachFromCompartment()
{
    if (isInList())
        remove();
}

/////////////////////////////////////////////////////////////////////
// UnboxedPlainObject
/////////////////////////////////////////////////////////////////////

bool
UnboxedPlainObject::setValue(ExclusiveContext* cx, const UnboxedLayout::Property& property,
                             const Value& v)
{
    uint8_t* p = &data_[property.offset];
    return SetUnboxedValue(cx, this, NameToId(property.name), p, property.type, v,
                           /* preBarrier = */ true);
}

Value
UnboxedPlainObject::getValue(const UnboxedLayout::Property& property,
                             bool maybeUninitialized /* = false */)
{
    uint8_t* p = &data_[property.offset];
    return GetUnboxedValue(p, property.type, maybeUninitialized);
}

void
UnboxedPlainObject::trace(JSTracer* trc, JSObject* obj)
{
    if (obj->as<UnboxedPlainObject>().expando_) {
        TraceManuallyBarrieredEdge(trc,
            reinterpret_cast<NativeObject**>(&obj->as<UnboxedPlainObject>().expando_),
            "unboxed_expando");
    }

    const UnboxedLayout& layout = obj->as<UnboxedPlainObject>().layoutDontCheckGeneration();
    const int32_t* list = layout.traceList();
    if (!list)
        return;

    uint8_t* data = obj->as<UnboxedPlainObject>().data();
    while (*list != -1) {
        GCPtrString* heap = reinterpret_cast<GCPtrString*>(data + *list);
        TraceEdge(trc, heap, "unboxed_string");
        list++;
    }
    list++;
    while (*list != -1) {
        GCPtrObject* heap = reinterpret_cast<GCPtrObject*>(data + *list);
        TraceNullableEdge(trc, heap, "unboxed_object");
        list++;
    }

    // Unboxed objects don't have Values to trace.
    MOZ_ASSERT(*(list + 1) == -1);
}

/* static */ UnboxedExpandoObject*
UnboxedPlainObject::ensureExpando(JSContext* cx, Handle<UnboxedPlainObject*> obj)
{
    if (obj->expando_)
        return obj->expando_;

    UnboxedExpandoObject* expando =
        NewObjectWithGivenProto<UnboxedExpandoObject>(cx, nullptr, gc::AllocKind::OBJECT4);
    if (!expando)
        return nullptr;

    // Don't track property types for expando objects. This allows Baseline
    // and Ion AddSlot ICs to guard on the unboxed group without guarding on
    // the expando group.
    MarkObjectGroupUnknownProperties(cx, expando->group());

    // If the expando is tenured then the original object must also be tenured.
    // Otherwise barriers triggered on the original object for writes to the
    // expando (as can happen in the JIT) won't see the tenured->nursery edge.
    // See WholeCellEdges::mark.
    MOZ_ASSERT_IF(!IsInsideNursery(expando), !IsInsideNursery(obj));

    // As with setValue(), we need to manually trigger post barriers on the
    // whole object. If we treat the field as a GCPtrObject and later
    // convert the object to its native representation, we will end up with a
    // corrupted store buffer entry.
    if (IsInsideNursery(expando) && !IsInsideNursery(obj))
        cx->runtime()->gc.storeBuffer.putWholeCell(obj);

    obj->expando_ = expando;
    return expando;
}

bool
UnboxedPlainObject::containsUnboxedOrExpandoProperty(ExclusiveContext* cx, jsid id) const
{
    if (layout().lookup(id))
        return true;

    if (maybeExpando() && maybeExpando()->containsShapeOrElement(cx, id))
        return true;

    return false;
}

static bool
PropagatePropertyTypes(JSContext* cx, jsid id, ObjectGroup* oldGroup, ObjectGroup* newGroup)
{
    HeapTypeSet* typeProperty = oldGroup->maybeGetProperty(id);
    TypeSet::TypeList types;
    if (!typeProperty->enumerateTypes(&types)) {
        ReportOutOfMemory(cx);
        return false;
    }
    for (size_t j = 0; j < types.length(); j++)
        AddTypePropertyId(cx, newGroup, nullptr, id, types[j]);
    return true;
}

static PlainObject*
MakeReplacementTemplateObject(JSContext* cx, HandleObjectGroup group, const UnboxedLayout &layout)
{
    PlainObject* obj = NewObjectWithGroup<PlainObject>(cx, group, layout.getAllocKind(),
                                                       TenuredObject);
    if (!obj)
        return nullptr;

    for (size_t i = 0; i < layout.properties().length(); i++) {
        const UnboxedLayout::Property& property = layout.properties()[i];
        if (!obj->addDataProperty(cx, NameToId(property.name), i, JSPROP_ENUMERATE))
            return nullptr;
        MOZ_ASSERT(obj->slotSpan() == i + 1);
        MOZ_ASSERT(!obj->inDictionaryMode());
    }

    return obj;
}

/* static */ bool
UnboxedLayout::makeNativeGroup(JSContext* cx, ObjectGroup* group)
{
    AutoEnterAnalysis enter(cx);

    UnboxedLayout& layout = group->unboxedLayout();
    Rooted<TaggedProto> proto(cx, group->proto());

    MOZ_ASSERT(!layout.nativeGroup());

    RootedObjectGroup replacementGroup(cx);

    // Immediately clear any new script on the group. This is done by replacing
    // the existing new script with one for a replacement default new group.
    // This is done so that the size of the replacment group's objects is the
    // same as that for the unboxed group, so that we do not see polymorphic
    // slot accesses later on for sites that see converted objects from this
    // group and objects that were allocated using the replacement new group.
    if (layout.newScript()) {
        replacementGroup = ObjectGroupCompartment::makeGroup(cx, &PlainObject::class_, proto);
        if (!replacementGroup)
            return false;

        PlainObject* templateObject = MakeReplacementTemplateObject(cx, replacementGroup, layout);
        if (!templateObject)
            return false;

        TypeNewScript* replacementNewScript =
            TypeNewScript::makeNativeVersion(cx, layout.newScript(), templateObject);
        if (!replacementNewScript)
            return false;

        replacementGroup->setNewScript(replacementNewScript);
        gc::TraceTypeNewScript(replacementGroup);

        group->clearNewScript(cx, replacementGroup);
    }

    // Similarly, if this group is keyed to an allocation site, replace its
    // entry with a new group that has no unboxed layout.
    if (layout.allocationScript()) {
        RootedScript script(cx, layout.allocationScript());
        jsbytecode* pc = layout.allocationPc();

        replacementGroup = ObjectGroupCompartment::makeGroup(cx, &PlainObject::class_, proto);
        if (!replacementGroup)
            return false;

        PlainObject* templateObject = &script->getObject(pc)->as<PlainObject>();
        replacementGroup->addDefiniteProperties(cx, templateObject->lastProperty());

        cx->compartment()->objectGroups.replaceAllocationSiteGroup(script, pc, JSProto_Object,
                                                                   replacementGroup);

        // Clear any baseline information at this opcode which might use the old group.
        if (script->hasBaselineScript()) {
            jit::ICEntry& entry = script->baselineScript()->icEntryFromPCOffset(script->pcToOffset(pc));
            jit::ICFallbackStub* fallback = entry.fallbackStub();
            for (jit::ICStubIterator iter = fallback->beginChain(); !iter.atEnd(); iter++)
                iter.unlink(cx);
            if (fallback->isNewObject_Fallback())
                fallback->toNewObject_Fallback()->setTemplateObject(nullptr);
            else if (fallback->isNewArray_Fallback())
                fallback->toNewArray_Fallback()->setTemplateGroup(replacementGroup);
        }
    }

    size_t nfixed = gc::GetGCKindSlots(layout.getAllocKind());

    RootedShape shape(cx, EmptyShape::getInitialShape(cx, &PlainObject::class_, proto, nfixed, 0));
    if (!shape)
        return false;

    // Add shapes for each property, if this is for a plain object.
    for (size_t i = 0; i < layout.properties().length(); i++) {
        const UnboxedLayout::Property& property = layout.properties()[i];

        Rooted<StackShape> child(cx, StackShape(shape->base()->unowned(), NameToId(property.name),
                                                i, JSPROP_ENUMERATE, 0));
        shape = cx->zone()->propertyTree.getChild(cx, shape, child);
        if (!shape)
            return false;
    }

    ObjectGroup* nativeGroup =
        ObjectGroupCompartment::makeGroup(cx, &PlainObject::class_, proto,
                                          group->flags() & OBJECT_FLAG_DYNAMIC_MASK);
    if (!nativeGroup)
        return false;

    // No sense propagating if we don't know what we started with.
    if (!group->unknownProperties()) {
        // Propagate all property types from the old group to the new group.
        for (size_t i = 0; i < layout.properties().length(); i++) {
            const UnboxedLayout::Property& property = layout.properties()[i];
            jsid id = NameToId(property.name);
            if (!PropagatePropertyTypes(cx, id, group, nativeGroup))
                return false;

            // If we are OOM we may not be able to propagate properties.
            if (nativeGroup->unknownProperties())
                break;

            HeapTypeSet* nativeProperty = nativeGroup->maybeGetProperty(id);
            if (nativeProperty && nativeProperty->canSetDefinite(i))
                nativeProperty->setDefinite(i);
        }
    } else {
        // If we skip, though, the new group had better agree.
        MOZ_ASSERT(nativeGroup->unknownProperties());
    }

    layout.nativeGroup_ = nativeGroup;
    layout.nativeShape_ = shape;
    layout.replacementGroup_ = replacementGroup;

    nativeGroup->setOriginalUnboxedGroup(group);

    group->markStateChange(cx);

    return true;
}

/* static */ bool
UnboxedPlainObject::convertToNative(JSContext* cx, JSObject* obj)
{
    const UnboxedLayout& layout = obj->as<UnboxedPlainObject>().layout();
    UnboxedExpandoObject* expando = obj->as<UnboxedPlainObject>().maybeExpando();

    if (!layout.nativeGroup()) {
        if (!UnboxedLayout::makeNativeGroup(cx, obj->group()))
            return false;

        // makeNativeGroup can reentrantly invoke this method.
        if (obj->is<PlainObject>())
            return true;
    }

    AutoValueVector values(cx);
    for (size_t i = 0; i < layout.properties().length(); i++) {
        // We might be reading properties off the object which have not been
        // initialized yet. Make sure any double values we read here are
        // canonicalized.
        if (!values.append(obj->as<UnboxedPlainObject>().getValue(layout.properties()[i], true)))
            return false;
    }

    // We are eliminating the expando edge with the conversion, so trigger a
    // pre barrier.
    JSObject::writeBarrierPre(expando);

    // Additionally trigger a post barrier on the expando itself. Whole cell
    // store buffer entries can be added on the original unboxed object for
    // writes to the expando (see WholeCellEdges::trace), so after conversion
    // we need to make sure the expando itself will still be traced.
    if (expando && !IsInsideNursery(expando))
        cx->runtime()->gc.storeBuffer.putWholeCell(expando);

    obj->setGroup(layout.nativeGroup());
    obj->as<PlainObject>().setLastPropertyMakeNative(cx, layout.nativeShape());

    for (size_t i = 0; i < values.length(); i++)
        obj->as<PlainObject>().initSlotUnchecked(i, values[i]);

    if (expando) {
        // Add properties from the expando object to the object, in order.
        // Suppress GC here, so that callers don't need to worry about this
        // method collecting. The stuff below can only fail due to OOM, in
        // which case the object will not have been completely filled back in.
        gc::AutoSuppressGC suppress(cx);

        Vector<jsid> ids(cx);
        for (Shape::Range<NoGC> r(expando->lastProperty()); !r.empty(); r.popFront()) {
            if (!ids.append(r.front().propid()))
                return false;
        }
        for (size_t i = 0; i < expando->getDenseInitializedLength(); i++) {
            if (!expando->getDenseElement(i).isMagic(JS_ELEMENTS_HOLE)) {
                if (!ids.append(INT_TO_JSID(i)))
                    return false;
            }
        }
        ::Reverse(ids.begin(), ids.end());

        RootedPlainObject nobj(cx, &obj->as<PlainObject>());
        Rooted<UnboxedExpandoObject*> nexpando(cx, expando);
        RootedId id(cx);
        Rooted<PropertyDescriptor> desc(cx);
        for (size_t i = 0; i < ids.length(); i++) {
            id = ids[i];
            if (!GetOwnPropertyDescriptor(cx, nexpando, id, &desc))
                return false;
            ObjectOpResult result;
            if (!DefineProperty(cx, nobj, id, desc, result))
                return false;
            MOZ_ASSERT(result.ok());
        }
    }

    return true;
}

/* static */
UnboxedPlainObject*
UnboxedPlainObject::create(ExclusiveContext* cx, HandleObjectGroup group, NewObjectKind newKind)
{
    AutoSetNewObjectMetadata metadata(cx);

    MOZ_ASSERT(group->clasp() == &class_);
    gc::AllocKind allocKind = group->unboxedLayout().getAllocKind();

    UnboxedPlainObject* res =
        NewObjectWithGroup<UnboxedPlainObject>(cx, group, allocKind, newKind);
    if (!res)
        return nullptr;

    // Overwrite the dummy shape which was written to the object's expando field.
    res->initExpando();

    // Initialize reference fields of the object. All fields in the object will
    // be overwritten shortly, but references need to be safe for the GC.
    const int32_t* list = res->layout().traceList();
    if (list) {
        uint8_t* data = res->data();
        while (*list != -1) {
            GCPtrString* heap = reinterpret_cast<GCPtrString*>(data + *list);
            heap->init(cx->names().empty);
            list++;
        }
        list++;
        while (*list != -1) {
            GCPtrObject* heap = reinterpret_cast<GCPtrObject*>(data + *list);
            heap->init(nullptr);
            list++;
        }
        // Unboxed objects don't have Values to initialize.
        MOZ_ASSERT(*(list + 1) == -1);
    }

    return res;
}

/* static */ JSObject*
UnboxedPlainObject::createWithProperties(ExclusiveContext* cx, HandleObjectGroup group,
                                         NewObjectKind newKind, IdValuePair* properties)
{
    MOZ_ASSERT(newKind == GenericObject || newKind == TenuredObject);

    UnboxedLayout& layout = group->unboxedLayout();

    if (layout.constructorCode()) {
        MOZ_ASSERT(cx->isJSContext());

        typedef JSObject* (*ConstructorCodeSignature)(IdValuePair*, NewObjectKind);
        ConstructorCodeSignature function =
            reinterpret_cast<ConstructorCodeSignature>(layout.constructorCode()->raw());

        JSObject* obj;
        {
            JS::AutoSuppressGCAnalysis nogc;
            obj = reinterpret_cast<JSObject*>(CALL_GENERATED_2(function, properties, newKind));
        }
        if (obj > reinterpret_cast<JSObject*>(CLEAR_CONSTRUCTOR_CODE_TOKEN))
            return obj;

        if (obj == reinterpret_cast<JSObject*>(CLEAR_CONSTRUCTOR_CODE_TOKEN))
            layout.setConstructorCode(nullptr);
    }

    UnboxedPlainObject* obj = UnboxedPlainObject::create(cx, group, newKind);
    if (!obj)
        return nullptr;

    for (size_t i = 0; i < layout.properties().length(); i++) {
        if (!obj->setValue(cx, layout.properties()[i], properties[i].value))
            return NewPlainObjectWithProperties(cx, properties, layout.properties().length(), newKind);
    }

#ifndef JS_CODEGEN_NONE
    if (cx->isJSContext() &&
        !group->unknownProperties() &&
        !layout.constructorCode() &&
        cx->asJSContext()->runtime()->jitSupportsFloatingPoint &&
        jit::CanLikelyAllocateMoreExecutableMemory())
    {
        if (!UnboxedLayout::makeConstructorCode(cx->asJSContext(), group))
            return nullptr;
    }
#endif

    return obj;
}

/* static */ bool
UnboxedPlainObject::obj_lookupProperty(JSContext* cx, HandleObject obj,
                                       HandleId id, MutableHandleObject objp,
                                       MutableHandleShape propp)
{
    if (obj->as<UnboxedPlainObject>().containsUnboxedOrExpandoProperty(cx, id)) {
        MarkNonNativePropertyFound<CanGC>(propp);
        objp.set(obj);
        return true;
    }

    RootedObject proto(cx, obj->staticPrototype());
    if (!proto) {
        objp.set(nullptr);
        propp.set(nullptr);
        return true;
    }

    return LookupProperty(cx, proto, id, objp, propp);
}

/* static */ bool
UnboxedPlainObject::obj_defineProperty(JSContext* cx, HandleObject obj, HandleId id,
                                       Handle<PropertyDescriptor> desc,
                                       ObjectOpResult& result)
{
    const UnboxedLayout& layout = obj->as<UnboxedPlainObject>().layout();

    if (const UnboxedLayout::Property* property = layout.lookup(id)) {
        if (!desc.getter() && !desc.setter() && desc.attributes() == JSPROP_ENUMERATE) {
            // This define is equivalent to setting an existing property.
            if (obj->as<UnboxedPlainObject>().setValue(cx, *property, desc.value()))
                return result.succeed();
        }

        // Trying to incompatibly redefine an existing property requires the
        // object to be converted to a native object.
        if (!convertToNative(cx, obj))
            return false;

        return DefineProperty(cx, obj, id, desc, result);
    }

    // Define the property on the expando object.
    Rooted<UnboxedExpandoObject*> expando(cx, ensureExpando(cx, obj.as<UnboxedPlainObject>()));
    if (!expando)
        return false;

    // Update property types on the unboxed object as well.
    AddTypePropertyId(cx, obj, id, desc.value());

    return DefineProperty(cx, expando, id, desc, result);
}

/* static */ bool
UnboxedPlainObject::obj_hasProperty(JSContext* cx, HandleObject obj, HandleId id, bool* foundp)
{
    if (obj->as<UnboxedPlainObject>().containsUnboxedOrExpandoProperty(cx, id)) {
        *foundp = true;
        return true;
    }

    RootedObject proto(cx, obj->staticPrototype());
    if (!proto) {
        *foundp = false;
        return true;
    }

    return HasProperty(cx, proto, id, foundp);
}

/* static */ bool
UnboxedPlainObject::obj_getProperty(JSContext* cx, HandleObject obj, HandleValue receiver,
                                    HandleId id, MutableHandleValue vp)
{
    const UnboxedLayout& layout = obj->as<UnboxedPlainObject>().layout();

    if (const UnboxedLayout::Property* property = layout.lookup(id)) {
        vp.set(obj->as<UnboxedPlainObject>().getValue(*property));
        return true;
    }

    if (UnboxedExpandoObject* expando = obj->as<UnboxedPlainObject>().maybeExpando()) {
        if (expando->containsShapeOrElement(cx, id)) {
            RootedObject nexpando(cx, expando);
            return GetProperty(cx, nexpando, receiver, id, vp);
        }
    }

    RootedObject proto(cx, obj->staticPrototype());
    if (!proto) {
        vp.setUndefined();
        return true;
    }

    return GetProperty(cx, proto, receiver, id, vp);
}

/* static */ bool
UnboxedPlainObject::obj_setProperty(JSContext* cx, HandleObject obj, HandleId id, HandleValue v,
                                    HandleValue receiver, ObjectOpResult& result)
{
    const UnboxedLayout& layout = obj->as<UnboxedPlainObject>().layout();

    if (const UnboxedLayout::Property* property = layout.lookup(id)) {
        if (receiver.isObject() && obj == &receiver.toObject()) {
            if (obj->as<UnboxedPlainObject>().setValue(cx, *property, v))
                return result.succeed();

            if (!convertToNative(cx, obj))
                return false;
            return SetProperty(cx, obj, id, v, receiver, result);
        }

        return SetPropertyByDefining(cx, id, v, receiver, result);
    }

    if (UnboxedExpandoObject* expando = obj->as<UnboxedPlainObject>().maybeExpando()) {
        if (expando->containsShapeOrElement(cx, id)) {
            // Update property types on the unboxed object as well.
            AddTypePropertyId(cx, obj, id, v);

            RootedObject nexpando(cx, expando);
            return SetProperty(cx, nexpando, id, v, receiver, result);
        }
    }

    return SetPropertyOnProto(cx, obj, id, v, receiver, result);
}

/* static */ bool
UnboxedPlainObject::obj_getOwnPropertyDescriptor(JSContext* cx, HandleObject obj, HandleId id,
                                                 MutableHandle<PropertyDescriptor> desc)
{
    const UnboxedLayout& layout = obj->as<UnboxedPlainObject>().layout();

    if (const UnboxedLayout::Property* property = layout.lookup(id)) {
        desc.value().set(obj->as<UnboxedPlainObject>().getValue(*property));
        desc.setAttributes(JSPROP_ENUMERATE);
        desc.object().set(obj);
        return true;
    }

    if (UnboxedExpandoObject* expando = obj->as<UnboxedPlainObject>().maybeExpando()) {
        if (expando->containsShapeOrElement(cx, id)) {
            RootedObject nexpando(cx, expando);
            if (!GetOwnPropertyDescriptor(cx, nexpando, id, desc))
                return false;
            if (desc.object() == nexpando)
                desc.object().set(obj);
            return true;
        }
    }

    desc.object().set(nullptr);
    return true;
}

/* static */ bool
UnboxedPlainObject::obj_deleteProperty(JSContext* cx, HandleObject obj, HandleId id,
                                       ObjectOpResult& result)
{
    if (!convertToNative(cx, obj))
        return false;
    return DeleteProperty(cx, obj, id, result);
}

/* static */ bool
UnboxedPlainObject::obj_enumerate(JSContext* cx, HandleObject obj, AutoIdVector& properties,
                                  bool enumerableOnly)
{
    // Ignore expando properties here, they are special-cased by the property
    // enumeration code.

    const UnboxedLayout::PropertyVector& unboxed = obj->as<UnboxedPlainObject>().layout().properties();
    for (size_t i = 0; i < unboxed.length(); i++) {
        if (!properties.append(NameToId(unboxed[i].name)))
            return false;
    }

    return true;
}

const Class UnboxedExpandoObject::class_ = {
    "UnboxedExpandoObject",
    0
};

static const ClassOps UnboxedPlainObjectClassOps = {
    nullptr,        /* addProperty */
    nullptr,        /* delProperty */
    nullptr,        /* getProperty */
    nullptr,        /* setProperty */
    nullptr,        /* enumerate   */
    nullptr,        /* resolve     */
    nullptr,        /* mayResolve  */
    nullptr,        /* finalize    */
    nullptr,        /* call        */
    nullptr,        /* hasInstance */
    nullptr,        /* construct   */
    UnboxedPlainObject::trace,
};

static const ObjectOps UnboxedPlainObjectObjectOps = {
    UnboxedPlainObject::obj_lookupProperty,
    UnboxedPlainObject::obj_defineProperty,
    UnboxedPlainObject::obj_hasProperty,
    UnboxedPlainObject::obj_getProperty,
    UnboxedPlainObject::obj_setProperty,
    UnboxedPlainObject::obj_getOwnPropertyDescriptor,
    UnboxedPlainObject::obj_deleteProperty,
    nullptr,   /* getElements */
    UnboxedPlainObject::obj_enumerate,
    nullptr    /* funToString */
};

const Class UnboxedPlainObject::class_ = {
    js_Object_str,
    Class::NON_NATIVE |
    JSCLASS_HAS_CACHED_PROTO(JSProto_Object) |
    JSCLASS_DELAY_METADATA_BUILDER,
    &UnboxedPlainObjectClassOps,
    JS_NULL_CLASS_SPEC,
    JS_NULL_CLASS_EXT,
    &UnboxedPlainObjectObjectOps
};

/////////////////////////////////////////////////////////////////////
// API
/////////////////////////////////////////////////////////////////////

static inline Value
NextValue(Handle<GCVector<Value>> values, size_t* valueCursor)
{
    return values[(*valueCursor)++];
}

void
UnboxedPlainObject::fillAfterConvert(ExclusiveContext* cx,
                                     Handle<GCVector<Value>> values, size_t* valueCursor)
{
    initExpando();
    memset(data(), 0, layout().size());
    for (size_t i = 0; i < layout().properties().length(); i++)
        JS_ALWAYS_TRUE(setValue(cx, layout().properties()[i], NextValue(values, valueCursor)));
}
