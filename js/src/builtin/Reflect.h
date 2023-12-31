/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef builtin_Reflect_h
#define builtin_Reflect_h

#include "jsobj.h"

namespace js {

extern JSObject*
InitReflect(JSContext* cx, js::HandleObject obj);

}

namespace js {

[[nodiscard]] extern bool
Reflect_getPrototypeOf(JSContext* cx, unsigned argc, Value* vp);

[[nodiscard]] extern bool
Reflect_isExtensible(JSContext* cx, unsigned argc, Value* vp);

}

#endif /* builtin_Reflect_h */
