/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*-
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. 
 *
 * This Source Code Form is "Incompatible With Secondary Licenses", as
 * defined by the Mozilla Public License, v. 2.0.
 */

#include "new-regexp/RegExpAPI.h"

#include "new-regexp/regexp-parser.h"
#include "new-regexp/regexp-shim.h"
#include "new-regexp/regexp.h"

namespace js {
namespace irregexp {

using frontend::TokenStream;

using v8::internal::FlatStringReader;
using v8::internal::RegExpCompileData;
using v8::internal::RegExpParser;
using v8::internal::Zone;

Isolate* CreateIsolate(JSContext* cx) {
  auto isolate = MakeUnique<Isolate>(cx);
  if (!isolate || !isolate->init()) {
    return nullptr;
  }
  return isolate.release();
}

static bool CheckPatternSyntaxImpl(JSContext* cx, FlatStringReader* pattern,
                                   JS::RegExpFlags flags,
                                   RegExpCompileData* result) {
  LifoAllocScope allocScope(&cx->tempLifoAlloc());
  Zone zone(allocScope.alloc());

  v8::internal::HandleScope handleScope(cx->isolate);
  v8::internal::JSRegExp::Flags passflags = static_cast<uint8_t>(flags.value());
  return RegExpParser::ParseRegExp(cx->isolate, &zone, pattern, passflags, result);
}

bool CheckPatternSyntax(JSContext* cx, TokenStream& ts,
                        const mozilla::Range<const char16_t> chars,
                        JS::RegExpFlags flags) {
  FlatStringReader reader(chars.begin().get(), chars.length());
  RegExpCompileData result;
  if (!CheckPatternSyntaxImpl(cx, &reader, flags, &result)) {
    // TODO: Report syntax error
    return false;
  }
  return true;
}

bool CheckPatternSyntax(JSContext* cx, TokenStream& ts,
                        HandleAtom pattern, JS::RegExpFlags flags) {
  FlatStringReader reader(pattern);
  RegExpCompileData result;
  if (!CheckPatternSyntaxImpl(cx, &reader, flags, &result)) {
    // TODO: Report syntax error
    return false;
  }
  return true;
}

}  // namespace irregexp
}  // namespace js
