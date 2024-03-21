/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifdef MOZ_VALGRIND
# include <valgrind/memcheck.h>
#endif

#include "mozilla/IntegerPrintfMacros.h"
#include "mozilla/Sprintf.h"

#include "jscntxt.h"
#include "jsgc.h"
#include "jsprf.h"

#include "gc/GCInternals.h"
#include "gc/Zone.h"
#include "js/GCAPI.h"
#include "js/HashTable.h"

#include "jscntxtinlines.h"
#include "jsgcinlines.h"

using namespace js;
using namespace js::gc;

#if defined(JSGC_HASH_TABLE_CHECKS) || defined(DEBUG)

class HeapCheckTracerBase : public JS::CallbackTracer
{
  public:
    explicit HeapCheckTracerBase(JSRuntime* rt, WeakMapTraceKind weakTraceKind);
    bool init();
    bool traceHeap(AutoLockForExclusiveAccess& lock);
    virtual void checkCell(Cell* cell) = 0;

  protected:
    void dumpCellPath();

    Cell* parentCell() {
        return parentIndex == -1 ? nullptr : stack[parentIndex].thing.asCell();
    }

    size_t failures;

  private:
    void onChild(const JS::GCCellPtr& thing) override;

    struct WorkItem {
        WorkItem(JS::GCCellPtr thing, const char* name, int parentIndex)
          : thing(thing), name(name), parentIndex(parentIndex), processed(false)
        {}

        JS::GCCellPtr thing;
        const char* name;
        int parentIndex;
        bool processed;
    };

    JSRuntime* rt;
    bool oom;
    HashSet<Cell*, DefaultHasher<Cell*>, SystemAllocPolicy> visited;
    Vector<WorkItem, 0, SystemAllocPolicy> stack;
    int parentIndex;
};

HeapCheckTracerBase::HeapCheckTracerBase(JSRuntime* rt, WeakMapTraceKind weakTraceKind)
  : CallbackTracer(rt, weakTraceKind),
    failures(0),
    rt(rt),
    oom(false),
    parentIndex(-1)
{
#ifdef DEBUG
    setCheckEdges(false);
#endif
}

bool
HeapCheckTracerBase::init()
{
    return visited.init();
}

void
HeapCheckTracerBase::onChild(const JS::GCCellPtr& thing)
{
    Cell* cell = thing.asCell();
    checkCell(cell);

    if (visited.lookup(cell))
        return;

    if (!visited.put(cell)) {
        oom = true;
        return;
    }

    // Don't trace into GC in zones being used by helper threads.
    Zone* zone = thing.is<JSObject>() ? thing.as<JSObject>().zone() : cell->asTenured().zone();
    if (zone->group() && zone->group()->usedByHelperThread)
        return;

    WorkItem item(thing, contextName(), parentIndex);
    if (!stack.append(item))
        oom = true;
}

bool
HeapCheckTracerBase::traceHeap(AutoLockForExclusiveAccess& lock)
{
    // The analysis thinks that traceRuntime might GC by calling a GC callback.
    JS::AutoSuppressGCAnalysis nogc;
    if (!rt->isBeingDestroyed())
        rt->gc.traceRuntime(this, lock);

    while (!stack.empty() && !oom) {
        WorkItem item = stack.back();
        if (item.processed) {
            stack.popBack();
        } else {
            parentIndex = stack.length() - 1;
            stack.back().processed = true;
            TraceChildren(this, item.thing);
        }
    }

    return !oom;
}

void
HeapCheckTracerBase::dumpCellPath()
{
    const char* name = contextName();
    for (int index = parentIndex; index != -1; index = stack[index].parentIndex) {
        const WorkItem& parent = stack[index];
        Cell* cell = parent.thing.asCell();
       fprintf(stderr, "  from %s %p %s edge\n",
                GCTraceKindToAscii(cell->getTraceKind()), cell, name);
        name = parent.name;
    }
    fprintf(stderr, "  from root %s\n", name);
}

#endif // defined(JSGC_HASH_TABLE_CHECKS) || defined(DEBUG)

#ifdef JSGC_HASH_TABLE_CHECKS

class CheckHeapTracer final : public HeapCheckTracerBase
{
  public:
    explicit CheckHeapTracer(JSRuntime* rt);
    void check(AutoLockForExclusiveAccess& lock);

  private:
    void checkCell(Cell* cell) override;
};

CheckHeapTracer::CheckHeapTracer(JSRuntime* rt)
  : HeapCheckTracerBase(rt, TraceWeakMapKeysValues)
{}

inline static bool
IsValidGCThingPointer(Cell* cell)
{
    return (uintptr_t(cell) & CellMask) == 0;
}

void
CheckHeapTracer::checkCell(Cell* cell)
{
    if (!IsValidGCThingPointer(cell) || !IsGCThingValidAfterMovingGC(cell)) {
        failures++;
        fprintf(stderr, "Bad pointer %p\n", cell);
        dumpCellPath();
    }
}

void
CheckHeapTracer::check(AutoLockForExclusiveAccess& lock)
{
    if (!traceHeap(lock))
        return;

    if (failures)
        fprintf(stderr, "Heap check: %" PRIuSIZE " failure(s)\n", failures);
    MOZ_RELEASE_ASSERT(failures == 0);
}

void
js::gc::CheckHeapAfterGC(JSRuntime* rt)
{
    AutoTraceSession session(rt, JS::HeapState::Tracing);
    CheckHeapTracer tracer(rt);
    if (tracer.init())
        tracer.check(session.lock);
}

#endif /* JSGC_HASH_TABLE_CHECKS */


#ifdef DEBUG

class CheckGrayMarkingTracer final : public HeapCheckTracerBase
{
  public:
    explicit CheckGrayMarkingTracer(JSRuntime* rt);
    bool check(AutoLockForExclusiveAccess& lock);

  private:
    void checkCell(Cell* cell) override;
};

CheckGrayMarkingTracer::CheckGrayMarkingTracer(JSRuntime* rt)
  : HeapCheckTracerBase(rt, DoNotTraceWeakMaps)
{
    // Weak gray->black edges are allowed.
    setTraceWeakEdges(false);
}

void
CheckGrayMarkingTracer::checkCell(Cell* cell)
{
    Cell* parent = parentCell();
    if (!cell->isTenured() || !parent || !parent->isTenured())
        return;

    TenuredCell* tenuredCell = &cell->asTenured();
    TenuredCell* tenuredParent = &parent->asTenured();
    if (tenuredParent->isMarked(BLACK) && !tenuredParent->isMarked(GRAY) &&
        tenuredCell->isMarked(GRAY))
    {
        failures++;
        fprintf(stderr, "Found black to gray edge %p\n", cell);
        dumpCellPath();
    }
}

bool
CheckGrayMarkingTracer::check(AutoLockForExclusiveAccess& lock)
{
    if (!traceHeap(lock))
        return true; // Ignore failure.

    return failures == 0;
}

JS_FRIEND_API(bool)
js::CheckGrayMarkingState(JSContext* cx)
{
    JSRuntime* rt = cx->runtime();
    MOZ_ASSERT(!JS::CurrentThreadIsHeapCollecting());
    MOZ_ASSERT(!rt->gc.isIncrementalGCInProgress());
    if (!rt->gc.areGrayBitsValid())
        return true;

    gcstats::AutoPhase ap(rt->gc.stats(), gcstats::PHASE_TRACE_HEAP);
    AutoTraceSession session(rt, JS::HeapState::Tracing);
    CheckGrayMarkingTracer tracer(rt);
    if (!tracer.init())
        return true; // Ignore failure

    return tracer.check(session.lock);
}

#endif // DEBUG
