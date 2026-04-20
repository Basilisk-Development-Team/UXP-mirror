/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#if defined(__MINGW32__) && defined(__i386__)

#include "WebGLContext.h"

namespace mozilla {

extern "C" nsresult __attribute__((stdcall))
WebGLContext_InitializeWithDrawTarget_stdcall(
    WebGLContext* self,
    nsIDocShell* aDocShell,
    NotNull<gfx::DrawTarget*> aDrawTarget)
    __asm__("__ZN7mozilla12WebGLContext24InitializeWithDrawTargetEP11nsIDocShellNS_7NotNullIPNS_3gfx10DrawTargetEEE@12");

extern "C" nsresult __attribute__((stdcall))
WebGLContext_InitializeWithDrawTarget_stdcall(
    WebGLContext* self,
    nsIDocShell* aDocShell,
    NotNull<gfx::DrawTarget*> aDrawTarget)
{
  return self->WebGLContext::InitializeWithDrawTarget(aDocShell, aDrawTarget);
}

}  // namespace mozilla

#endif