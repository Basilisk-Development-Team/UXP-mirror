/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

/**
 * This is not a generated file. It contains common utility functions
 * invoked from the JavaScript code generated from IDL interfaces.
 * The goal of the utility functions is to cut down on the size of
 * the generated code itself.
 */

#include "nsJSUtils.h"
#include "jsapi.h"
#include "jsfriendapi.h"
#include "js/SourceBufferHolder.h"
#include "nsIScriptContext.h"
#include "nsIScriptGlobalObject.h"
#include "nsIXPConnect.h"
#include "nsCOMPtr.h"
#include "nsIScriptSecurityManager.h"
#include "nsPIDOMWindow.h"
#include "GeckoProfiler.h"
#include "nsJSPrincipals.h"
#include "xpcpublic.h"
#include "nsContentUtils.h"
#include "nsGlobalWindow.h"
#include "mozilla/CycleCollectedJSContext.h"
#include "mozilla/dom/BindingUtils.h"
#include "mozilla/dom/Date.h"
#include "mozilla/dom/Element.h"
#include "mozilla/dom/ScriptSettings.h"

using namespace mozilla;
using namespace mozilla::dom;

bool
nsJSUtils::GetCallingLocation(JSContext* aContext, nsACString& aFilename,
                              uint32_t* aLineno, uint32_t* aColumn)
{
  JS::AutoFilename filename;
  if (!JS::DescribeScriptedCaller(aContext, &filename, aLineno, aColumn)) {
    return false;
  }

  aFilename.Assign(filename.get());
  return true;
}

bool
nsJSUtils::GetCallingLocation(JSContext* aContext, nsAString& aFilename,
                              uint32_t* aLineno, uint32_t* aColumn)
{
  JS::AutoFilename filename;
  if (!JS::DescribeScriptedCaller(aContext, &filename, aLineno, aColumn)) {
    return false;
  }

  aFilename.Assign(NS_ConvertUTF8toUTF16(filename.get()));
  return true;
}

nsIScriptGlobalObject *
nsJSUtils::GetStaticScriptGlobal(JSObject* aObj)
{
  if (!aObj)
    return nullptr;
  return xpc::WindowGlobalOrNull(aObj);
}

nsIScriptContext *
nsJSUtils::GetStaticScriptContext(JSObject* aObj)
{
  nsIScriptGlobalObject *nativeGlobal = GetStaticScriptGlobal(aObj);
  if (!nativeGlobal)
    return nullptr;

  return nativeGlobal->GetScriptContext();
}

uint64_t
nsJSUtils::GetCurrentlyRunningCodeInnerWindowID(JSContext *aContext)
{
  if (!aContext)
    return 0;

  nsGlobalWindow* win = xpc::CurrentWindowOrNull(aContext);
  return win ? win->WindowID() : 0;
}

nsresult
nsJSUtils::CompileFunction(AutoJSAPI& jsapi,
                           JS::AutoObjectVector& aScopeChain,
                           JS::CompileOptions& aOptions,
                           const nsACString& aName,
                           uint32_t aArgCount,
                           const char** aArgArray,
                           const nsAString& aBody,
                           JSObject** aFunctionObject)
{
  JSContext* cx = jsapi.cx();
  MOZ_ASSERT(js::GetEnterCompartmentDepth(cx) > 0);
  MOZ_ASSERT_IF(aScopeChain.length() != 0,
                js::IsObjectInContextCompartment(aScopeChain[0], cx));
  MOZ_ASSERT_IF(aOptions.versionSet, aOptions.version != JSVERSION_UNKNOWN);

  // Do the junk Gecko is supposed to do before calling into JSAPI.
  for (size_t i = 0; i < aScopeChain.length(); ++i) {
    JS::ExposeObjectToActiveJS(aScopeChain[i]);
  }

  // Compile.
  JS::Rooted<JSFunction*> fun(cx);
  if (!JS::CompileFunction(cx, aScopeChain, aOptions,
                           PromiseFlatCString(aName).get(),
                           aArgCount, aArgArray,
                           PromiseFlatString(aBody).get(),
                           aBody.Length(), &fun))
  {
    return NS_ERROR_FAILURE;
  }

  *aFunctionObject = JS_GetFunctionObject(fun);
  return NS_OK;
}

static nsresult
EvaluationExceptionToNSResult(JSContext* aCx)
{
  if (JS_IsExceptionPending(aCx)) {
    return NS_SUCCESS_DOM_SCRIPT_EVALUATION_THREW;
  }
  return NS_SUCCESS_DOM_SCRIPT_EVALUATION_THREW_UNCATCHABLE;
}

nsJSUtils::ExecutionContext::ExecutionContext(JSContext* aCx,
                                              JS::Handle<JSObject*> aGlobal)
  : mCx(aCx)
  , mCompartment(aCx, aGlobal)
  , mRetValue(aCx)
  , mScopeChain(aCx)
  , mScript(aCx)
  , mRv(NS_OK)
  , mSkip(false)
  , mCoerceToString(false)
  , mEncodeBytecode(false)
#ifdef DEBUG
  , mWantsReturnValue(false)
  , mExpectScopeChain(false)
  , mScriptUsed(false)
#endif
{
  MOZ_ASSERT(aCx == nsContentUtils::GetCurrentJSContext());
  MOZ_ASSERT(NS_IsMainThread());
  MOZ_ASSERT(mRetValue.isUndefined());

  MOZ_ASSERT(js::GetGlobalForObjectCrossCompartment(aGlobal) == aGlobal);
  if (MOZ_UNLIKELY(!xpc::Scriptability::Get(aGlobal).Allowed())) {
    mSkip = true;
    mRv = NS_OK;
  }
}

void
nsJSUtils::ExecutionContext::SetScopeChain(
  const JS::AutoObjectVector& aScopeChain)
{
  if (mSkip) {
    return;
  }

#ifdef DEBUG
  mExpectScopeChain = true;
#endif
  // Now make sure to wrap the scope chain into the right compartment.
  if (!mScopeChain.reserve(aScopeChain.length())) {
    mSkip = true;
    mRv = NS_ERROR_OUT_OF_MEMORY;
    return;
  }

  for (size_t i = 0; i < aScopeChain.length(); ++i) {
    JS::ExposeObjectToActiveJS(aScopeChain[i]);
    mScopeChain.infallibleAppend(aScopeChain[i]);
    if (!JS_WrapObject(mCx, mScopeChain[i])) {
      mSkip = true;
      mRv = NS_ERROR_OUT_OF_MEMORY;
      return;
    }
  }
}

nsresult
nsJSUtils::ExecutionContext::JoinCompile(void** aOffThreadToken)
{
  if (mSkip) {
    return mRv;
  }

  MOZ_ASSERT(!mWantsReturnValue);
  MOZ_ASSERT(!mExpectScopeChain);
  MOZ_ASSERT(!mScript);
  mScript.set(JS::FinishOffThreadScript(mCx, *aOffThreadToken));
  *aOffThreadToken = nullptr; // Mark the token as having been finished.
  if (!mScript) {
    mSkip = true;
    mRv = EvaluationExceptionToNSResult(mCx);
    return mRv;
  }

  if (mEncodeBytecode && !StartIncrementalEncoding(mCx, mScript)) {
    mSkip = true;
    mRv = EvaluationExceptionToNSResult(mCx);
    return mRv;
  }
 
  return NS_OK;
}

nsresult
nsJSUtils::ExecutionContext::Compile(JS::CompileOptions& aCompileOptions,
                                     JS::SourceBufferHolder& aSrcBuf)
{
  if (mSkip) {
    return mRv;
  }
 
  MOZ_ASSERT(aSrcBuf.get());
  MOZ_ASSERT(mRetValue.isUndefined());
#ifdef DEBUG
  mWantsReturnValue = !aCompileOptions.noScriptRval;
#endif
 
  MOZ_ASSERT(!mScript);
  bool compiled = true;
  if (mScopeChain.length() == 0) {
    compiled = JS::Compile(mCx, aCompileOptions, aSrcBuf, &mScript);
  } else {
    compiled = JS::CompileForNonSyntacticScope(mCx, aCompileOptions, aSrcBuf,
                                               &mScript);
  }

  MOZ_ASSERT_IF(compiled, mScript);
  if (!compiled) {
    mSkip = true;
    mRv = EvaluationExceptionToNSResult(mCx);
    return mRv;
  }
 
  if (mEncodeBytecode && !StartIncrementalEncoding(mCx, mScript)) {
    mSkip = true;
    mRv = EvaluationExceptionToNSResult(mCx);
    return mRv;
  }
 
  return NS_OK;
}

nsresult
nsJSUtils::ExecutionContext::Compile(JS::CompileOptions& aCompileOptions,
                                     const nsAString& aScript)
{
  if (mSkip) {
    return mRv;
  }

  const nsPromiseFlatString& flatScript = PromiseFlatString(aScript);
  JS::SourceBufferHolder srcBuf(flatScript.get(), aScript.Length(),
                                JS::SourceBufferHolder::NoOwnership);
  return Compile(aCompileOptions, srcBuf);
}

nsresult
nsJSUtils::ExecutionContext::Decode(JS::CompileOptions& aCompileOptions,
                                    mozilla::Vector<uint8_t>& aBytecodeBuf,
                                    size_t aBytecodeIndex)
{
  if (mSkip) {
    return mRv;
  }

  MOZ_ASSERT(!mWantsReturnValue);
  JS::TranscodeResult tr = JS::DecodeScript(mCx, aBytecodeBuf, &mScript, aBytecodeIndex);
  // These errors are external parameters which should be handled before the
  // decoding phase, and which are the only reasons why you might want to
  // fallback on decoding failures.
  MOZ_ASSERT(tr != JS::TranscodeResult_Failure_BadBuildId &&
             tr != JS::TranscodeResult_Failure_WrongCompileOption);
  if (tr != JS::TranscodeResult_Ok) {
    mSkip = true;
    mRv = NS_ERROR_DOM_JS_DECODING_ERROR;
    return mRv;
  }

  return mRv;
}

nsresult
nsJSUtils::ExecutionContext::JoinDecode(void **aOffThreadToken)
{
  if (mSkip) {
    return mRv;
  }

  MOZ_ASSERT(!mWantsReturnValue);
  MOZ_ASSERT(!mExpectScopeChain);
  mScript.set(JS::FinishOffThreadScriptDecoder(mCx, *aOffThreadToken));
  *aOffThreadToken = nullptr; // Mark the token as having been finished.
  if (!mScript) {
    mSkip = true;
    mRv = EvaluationExceptionToNSResult(mCx);
    return mRv;
  }

  return NS_OK;
}

JSScript* nsJSUtils::ExecutionContext::GetScript() {
  if (mSkip) {
    return nullptr;
  }

#ifdef DEBUG
  MOZ_ASSERT(mScript);
  mScriptUsed = true;
#endif

  return mScript;
}

nsresult nsJSUtils::ExecutionContext::ExecScript() {
  if (mSkip) {
    return mRv;
  }

  MOZ_ASSERT(mScript);

  if (!JS_ExecuteScript(mCx, mScopeChain, mScript)) {
    mSkip = true;
    mRv = EvaluationExceptionToNSResult(mCx);
    return mRv;
  }

  return NS_OK;
}

static bool IsPromiseValue(JSContext* aCx, JS::Handle<JS::Value> aValue) {
  if (!aValue.isObject()) {
    return false;
  }

  JS::Rooted<JSObject*> obj(aCx, js::CheckedUnwrap(&aValue.toObject()));
  if (!obj) {
    return false;
  }

  return JS::IsPromiseObject(obj);
}

nsresult
nsJSUtils::ExecutionContext::ExecScript(JS::MutableHandle<JS::Value> aRetValue)
{
  if (mSkip) {
      return mRv;
  }

  MOZ_ASSERT(mScript);
  MOZ_ASSERT(mWantsReturnValue);

  if (!JS_ExecuteScript(mCx, mScopeChain, mScript, aRetValue)) {
    mSkip = true;
    mRv = EvaluationExceptionToNSResult(mCx);
    return mRv;
  }

#ifdef DEBUG
  mWantsReturnValue = false;
#endif
  if (mCoerceToString && IsPromiseValue(mCx, aRetValue)) {
    // We're a javascript: url and we should treat Promise return values as
    // undefined.
    //
    // Once bug 1477821 is fixed this code might be able to go away, or will
    // become enshrined in the spec, depending.
    aRetValue.setUndefined();
  }

  if (mCoerceToString && !aRetValue.isUndefined()) {
    JSString* str = JS::ToString(mCx, aRetValue);
    if (!str) {
      // ToString can be a function call, so an exception can be raised while
      // executing the function.
      mSkip = true;
      return EvaluationExceptionToNSResult(mCx);
    }
    aRetValue.set(JS::StringValue(str));
  }

  return NS_OK;
}

nsresult
nsJSUtils::CompileModule(JSContext* aCx,
                       JS::SourceBufferHolder& aSrcBuf,
                       JS::Handle<JSObject*> aEvaluationGlobal,
                       JS::CompileOptions &aCompileOptions,
                       JS::MutableHandle<JSObject*> aModule)
{
  PROFILER_LABEL("nsJSUtils", "CompileModule",
    js::ProfileEntry::Category::JS);

  MOZ_ASSERT_IF(aCompileOptions.versionSet,
                aCompileOptions.version != JSVERSION_UNKNOWN);
  MOZ_ASSERT(aCx == nsContentUtils::GetCurrentJSContext());
  MOZ_ASSERT(aSrcBuf.get());
  MOZ_ASSERT(js::GetGlobalForObjectCrossCompartment(aEvaluationGlobal) ==
             aEvaluationGlobal);
  MOZ_ASSERT(JS::CurrentGlobalOrNull(aCx) == aEvaluationGlobal);
  MOZ_ASSERT(NS_IsMainThread());
  MOZ_ASSERT(CycleCollectedJSContext::Get() &&
             CycleCollectedJSContext::Get()->MicroTaskLevel());

  NS_ENSURE_TRUE(xpc::Scriptability::Get(aEvaluationGlobal).Allowed(), NS_OK);

  bool compileResult = JS::CompileModule(aCx, aCompileOptions, aSrcBuf, aModule);
 
  if (!compileResult) {
    return NS_ERROR_FAILURE;
  }
  return NS_OK;
}

nsresult
nsJSUtils::ModuleInstantiate(JSContext* aCx, JS::Handle<JSObject*> aModule)
{
  PROFILER_LABEL("nsJSUtils", "ModuleInstantiate",
    js::ProfileEntry::Category::JS);

  MOZ_ASSERT(aCx == nsContentUtils::GetCurrentJSContext());
  MOZ_ASSERT(NS_IsMainThread());
  MOZ_ASSERT(CycleCollectedJSContext::Get() &&
             CycleCollectedJSContext::Get()->MicroTaskLevel());

  NS_ENSURE_TRUE(xpc::Scriptability::Get(aModule).Allowed(), NS_OK);

  if (!JS::ModuleInstantiate(aCx, aModule)) {
    return NS_ERROR_FAILURE;
  }

  return NS_OK;
}

nsresult
nsJSUtils::ModuleEvaluate(JSContext* aCx, JS::Handle<JSObject*> aModule)
{
  PROFILER_LABEL("nsJSUtils", "ModuleEvaluate",
    js::ProfileEntry::Category::JS);

  MOZ_ASSERT(aCx == nsContentUtils::GetCurrentJSContext());
  MOZ_ASSERT(NS_IsMainThread());
  MOZ_ASSERT(CycleCollectedJSContext::Get() &&
             CycleCollectedJSContext::Get()->MicroTaskLevel());

  NS_ENSURE_TRUE(xpc::Scriptability::Get(aModule).Allowed(), NS_OK);

  if (!JS::ModuleEvaluate(aCx, aModule)) {
    return NS_ERROR_FAILURE;
  }

  return NS_OK;
}

/* static */
bool
nsJSUtils::GetScopeChainForElement(JSContext* aCx,
                                   mozilla::dom::Element* aElement,
                                   JS::AutoObjectVector& aScopeChain)
{
  for (nsINode* cur = aElement; cur; cur = cur->GetScopeChainParent()) {
    JS::RootedValue val(aCx);
    if (!GetOrCreateDOMReflector(aCx, cur, &val)) {
      return false;
    }

    if (!aScopeChain.append(&val.toObject())) {
      return false;
    }
  }

  return true;
}

/* static */
void
nsJSUtils::ResetTimeZone()
{
  JS::ResetTimeZone();
}

//
// nsDOMJSUtils.h
//

bool nsAutoJSString::init(const JS::Value &v)
{
  // Note: it's okay to use danger::GetJSContext here instead of AutoJSAPI,
  // because the init() call below is careful not to run script (for instance,
  // it only calls JS::ToString for non-object values).
  JSContext* cx = danger::GetJSContext();
  if (!init(cx, v)) {
    JS_ClearPendingException(cx);
    return false;
  }

  return true;
}

