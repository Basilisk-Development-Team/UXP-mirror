/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef jit_LoongArchMinimalJit_h
#define jit_LoongArchMinimalJit_h

namespace js {

class JSContext;
class RunState;

namespace jit {

[[nodiscard]] bool TryEnterLoongArchMinimalJit(JSContext* cx, RunState& state);

} // namespace jit
} // namespace js

#endif /* jit_LoongArchMinimalJit_h */
