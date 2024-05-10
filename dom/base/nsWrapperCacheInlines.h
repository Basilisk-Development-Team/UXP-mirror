/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef nsWrapperCacheInline_h___
#define nsWrapperCacheInline_h___

#include "nsWrapperCache.h"
#include "js/GCAPI.h"
#include "js/TracingAPI.h"

inline JSObject*
nsWrapperCache::GetWrapperPreserveColor() const
{
  JSObject* obj = mWrapper;
  if (obj && js::gc::EdgeNeedsSweepUnbarriered(&obj)) {
    // The object has been found to be dead and is in the process of being
    // finalized, so don't let the caller see it. As an optimisation, remove it
    // from the cache so we don't have to do this check in future.
    const_cast<nsWrapperCache*>(this)->ClearWrapper();
    return nullptr;
  }
  MOZ_ASSERT(obj == mWrapper);
  return obj;
}

inline JSObject*
nsWrapperCache::GetWrapper() const
{
    JSObject* obj = GetWrapperPreserveColor();
    if (obj) {
      JS::ExposeObjectToActiveJS(obj);
    }
    return obj;
}

inline bool
nsWrapperCache::IsBlack() const
{
  JSObject* o = GetWrapperPreserveColor();
  return o && !JS::ObjectIsMarkedGray(o);
}

static void
SearchGray(JS::GCCellPtr aGCThing, const char* aName, void* aClosure)
{
  bool* hasGrayObjects = static_cast<bool*>(aClosure);
  if (!*hasGrayObjects && aGCThing && JS::GCThingIsMarkedGray(aGCThing)) {
    *hasGrayObjects = true;
  }
}

inline bool
nsWrapperCache::HasNothingToTrace(nsISupports* aThis)
{
  nsXPCOMCycleCollectionParticipant* participant = nullptr;
  CallQueryInterface(aThis, &participant);
  bool hasGrayObjects = false;
  participant->Trace(aThis, TraceCallbackFunc(SearchGray), &hasGrayObjects);
  return !hasGrayObjects;
}

inline bool
nsWrapperCache::IsBlackAndDoesNotNeedTracing(nsISupports* aThis)
{
  return IsBlack() && HasNothingToTrace(aThis);
}

#endif /* nsWrapperCache_h___ */
