/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef jit_LoongArchMinimalJit_h
#define jit_LoongArchMinimalJit_h

#include "jspubtd.h"
#include "js/CallArgs.h"

namespace js {

class RunState;

namespace jit {

[[nodiscard]] bool TryEnterLoongArchMinimalJit(JSContext* cx, RunState& state);
[[nodiscard]] bool TryCallLoongArchMinimalJit(JSContext* cx, HandleFunction fun, const CallArgs& args, bool* handled);

} // namespace jit
} // namespace js

#endif /* jit_LoongArchMinimalJit_h */
