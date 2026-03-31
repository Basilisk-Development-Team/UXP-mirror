/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#include "ChannelWrapper.h"

#include "jsapi.h"
#include "xpcpublic.h"

#include "mozilla/BasePrincipal.h"
#include "nsSystemPrincipal.h"

#include "NSSErrorsService.h"
#include "nsITransportSecurityInfo.h"

#include "mozilla/AddonManagerWebAPI.h"
#include "mozilla/ClearOnShutdown.h"
#include "mozilla/ErrorNames.h"
#include "mozilla/Unused.h"
#include "mozilla/dom/Element.h"
#include "mozilla/dom/Event.h"
#include "mozilla/dom/EventBinding.h"
#include "mozilla/dom/TabParent.h"
#include "nsIAtom.h"
#include "nsContentUtils.h"
#include "nsIContentPolicy.h"
#include "nsIHttpChannelInternal.h"
#include "nsIHttpHeaderVisitor.h"
#include "nsIInterfaceRequestor.h"
#include "nsIInterfaceRequestorUtils.h"
#include "nsILoadContext.h"
#include "nsIDOMElement.h"
#include "nsIDOMEvent.h"
#include "nsIProxiedChannel.h"
#include "nsIProxyInfo.h"
#include "nsITraceableChannel.h"
#include "nsIWritablePropertyBag.h"
#include "nsIWritablePropertyBag2.h"
#include "nsNetUtil.h"
#include "nsProxyRelease.h"
#include "nsPrintfCString.h"
#include "nsReadableUtils.h"

using namespace mozilla::dom;
using namespace JS;

namespace mozilla {
namespace extensions {

#define CHANNELWRAPPER_PROP_KEY \
  NS_LITERAL_STRING("ChannelWrapper::CachedInstance")

/*****************************************************************************
 * Lifetimes
 *****************************************************************************/

namespace {
class ChannelListHolder : public LinkedList<ChannelWrapper> {
 public:
  ChannelListHolder() : LinkedList<ChannelWrapper>() {}

  ~ChannelListHolder();
};

}  // anonymous namespace

ChannelListHolder::~ChannelListHolder() {
  while (ChannelWrapper* wrapper = popFirst()) {
    wrapper->Die();
  }
}

static LinkedList<ChannelWrapper>& ChannelList() {
  static UniquePtr<ChannelListHolder> sChannelList;
  if (!sChannelList) {
    sChannelList.reset(new ChannelListHolder());
    ClearOnShutdown(&sChannelList, ShutdownPhase::Shutdown);
  }
  return *sChannelList;
}

NS_IMPL_CYCLE_COLLECTING_ADDREF(ChannelWrapper::ChannelWrapperStub)
NS_IMPL_CYCLE_COLLECTING_RELEASE(ChannelWrapper::ChannelWrapperStub)

NS_IMPL_CYCLE_COLLECTION(ChannelWrapper::ChannelWrapperStub, mChannelWrapper)

NS_INTERFACE_MAP_BEGIN_CYCLE_COLLECTION(ChannelWrapper::ChannelWrapperStub)
  NS_INTERFACE_MAP_ENTRY_TEAROFF(ChannelWrapper, mChannelWrapper)
  NS_INTERFACE_MAP_ENTRY(nsISupports)
NS_INTERFACE_MAP_END

/*****************************************************************************
 * Initialization
 *****************************************************************************/

ChannelWrapper::ChannelWrapper(nsISupports* aParent, nsIChannel* aChannel)
    : ChannelHolder(aChannel), mParent(aParent) {
  mContentTypeHdr.SetIsVoid(true);
  mStub = new ChannelWrapperStub(this);

  ChannelList().insertBack(this);
}

ChannelWrapper::~ChannelWrapper() {
  if (LinkedListElement<ChannelWrapper>::isInList()) {
    LinkedListElement<ChannelWrapper>::remove();
  }
}

void ChannelWrapper::Die() {
  if (mStub) {
    mStub->mChannelWrapper = nullptr;
  }
}

/* static */
already_AddRefed<ChannelWrapper> ChannelWrapper::Get(const GlobalObject& global,
                                                     nsIChannel* channel) {
  RefPtr<ChannelWrapper> wrapper;

  nsCOMPtr<nsIWritablePropertyBag2> props = do_QueryInterface(channel);
  if (props) {
    Unused << props->GetPropertyAsInterface(CHANNELWRAPPER_PROP_KEY,
                                            NS_GET_IID(ChannelWrapper),
                                            getter_AddRefs(wrapper));

    if (wrapper) {
      // Assume cached attributes may have changed at this point.
      wrapper->ClearCachedAttributes();
    }
  }

  if (!wrapper) {
    wrapper = new ChannelWrapper(global.GetAsSupports(), channel);
    if (props) {
      Unused << props->SetPropertyAsInterface(CHANNELWRAPPER_PROP_KEY,
                                              wrapper->mStub);
    }
  }

  return wrapper.forget();
}

already_AddRefed<ChannelWrapper> ChannelWrapper::GetRegisteredChannel(
    const GlobalObject& global, uint64_t aChannelId,
    const nsAString& aAddonId, nsISupports* aBrowserParent) {
  nsIContentParent* contentParent = nullptr;
  nsCOMPtr<nsITabParent> tabParent = do_QueryInterface(aBrowserParent);
  if (tabParent) {
    if (TabParent* parent = TabParent::GetFrom(tabParent)) {
      contentParent = parent->Manager();
    }
  }

  auto& webreq = WebRequestService::GetSingleton();
  nsCOMPtr<nsIAtom> addonId = NS_Atomize(aAddonId);

  nsCOMPtr<nsITraceableChannel> channel =
      webreq.GetTraceableChannel(aChannelId, addonId, contentParent);
  if (!channel) {
    return nullptr;
  }
  nsCOMPtr<nsIChannel> chan(do_QueryInterface(channel));
  return ChannelWrapper::Get(global, chan);
}

void ChannelWrapper::SetChannel(nsIChannel* aChannel) {
  detail::ChannelHolder::SetChannel(aChannel);
  ClearCachedAttributes();
  ChannelWrapperBinding::ClearCachedFinalURIValue(this);
  ChannelWrapperBinding::ClearCachedFinalURLValue(this);
  ChannelWrapperBinding::ClearCachedProxyInfoValue(this);
}

void ChannelWrapper::ClearCachedAttributes() {
  ChannelWrapperBinding::ClearCachedRemoteAddressValue(this);
  ChannelWrapperBinding::ClearCachedStatusCodeValue(this);
  ChannelWrapperBinding::ClearCachedStatusLineValue(this);
  if (!mFiredErrorEvent) {
    ChannelWrapperBinding::ClearCachedErrorStringValue(this);
  }
}

/*****************************************************************************
 * ...
 *****************************************************************************/

void ChannelWrapper::Cancel(uint32_t aResult, uint32_t aReason,
                            ErrorResult& aRv) {
  nsresult rv = NS_ERROR_UNEXPECTED;
  if (nsCOMPtr<nsIChannel> chan = MaybeChannel()) {
    (void)aReason;
    rv = chan->Cancel(nsresult(aResult));
    ErrorCheck();
  }
  if (NS_FAILED(rv)) {
    aRv.Throw(rv);
  }
}

void ChannelWrapper::RedirectTo(nsIURI* aURI, ErrorResult& aRv) {
  nsresult rv = NS_ERROR_UNEXPECTED;
  if (nsCOMPtr<nsIHttpChannel> chan = MaybeHttpChannel()) {
    rv = chan->RedirectTo(aURI);
  }
  if (NS_FAILED(rv)) {
    aRv.Throw(rv);
  }
}

void ChannelWrapper::UpgradeToSecure(ErrorResult& aRv) {
  aRv.Throw(NS_ERROR_NOT_IMPLEMENTED);
}

void ChannelWrapper::SetSuspended(bool aSuspended, ErrorResult& aRv) {
  if (aSuspended != mSuspended) {
    nsresult rv = NS_ERROR_UNEXPECTED;
    if (nsCOMPtr<nsIChannel> chan = MaybeChannel()) {
      if (aSuspended) {
        rv = chan->Suspend();
      } else {
        rv = chan->Resume();
      }
    }
    if (NS_FAILED(rv)) {
      aRv.Throw(rv);
    } else {
      mSuspended = aSuspended;
    }
  }
}

void ChannelWrapper::GetContentType(nsCString& aContentType) const {
  if (nsCOMPtr<nsIHttpChannel> chan = MaybeHttpChannel()) {
    Unused << chan->GetContentType(aContentType);
  }
}

void ChannelWrapper::SetContentType(const nsACString& aContentType) {
  if (nsCOMPtr<nsIHttpChannel> chan = MaybeHttpChannel()) {
    Unused << chan->SetContentType(aContentType);
  }
}

/*****************************************************************************
 * Headers
 *****************************************************************************/

namespace {

class MOZ_STACK_CLASS HeaderVisitor final : public nsIHttpHeaderVisitor {
 public:
  NS_DECL_NSIHTTPHEADERVISITOR

  explicit HeaderVisitor(nsTArray<dom::MozHTTPHeader>& aHeaders)
      : mHeaders(aHeaders) {
    mContentTypeHdr.SetIsVoid(true);
  }

  HeaderVisitor(nsTArray<dom::MozHTTPHeader>& aHeaders,
                const nsCString& aContentTypeHdr)
      : mHeaders(aHeaders), mContentTypeHdr(aContentTypeHdr) {}

  void VisitRequestHeaders(nsIHttpChannel* aChannel, ErrorResult& aRv) {
    CheckResult(aChannel->VisitRequestHeaders(this), aRv);
  }

  void VisitResponseHeaders(nsIHttpChannel* aChannel, ErrorResult& aRv) {
    CheckResult(aChannel->VisitResponseHeaders(this), aRv);
  }

  NS_IMETHOD QueryInterface(REFNSIID aIID, void** aInstancePtr) override;

  // Stub AddRef/Release since this is a stack class.
  NS_IMETHOD_(MozExternalRefCountType) AddRef(void) override {
    return ++mRefCnt;
  }

  NS_IMETHOD_(MozExternalRefCountType) Release(void) override {
    return --mRefCnt;
  }

  virtual ~HeaderVisitor() { MOZ_DIAGNOSTIC_ASSERT(mRefCnt == 0); }

 private:
  bool CheckResult(nsresult aNSRv, ErrorResult& aRv) {
    if (NS_FAILED(aNSRv)) {
      aRv.Throw(aNSRv);
      return false;
    }
    return true;
  }

  nsTArray<dom::MozHTTPHeader>& mHeaders;
  nsCString mContentTypeHdr;

  nsrefcnt mRefCnt = 0;
};

NS_IMETHODIMP
HeaderVisitor::VisitHeader(const nsACString& aHeader,
                           const nsACString& aValue) {
  auto dict = mHeaders.AppendElement(fallible);
  if (!dict) {
    return NS_ERROR_OUT_OF_MEMORY;
  }
  dict->mName = aHeader;

  if (!mContentTypeHdr.IsVoid() &&
      aHeader.LowerCaseEqualsLiteral("content-type")) {
    dict->mValue = mContentTypeHdr;
  } else {
    dict->mValue = aValue;
  }

  return NS_OK;
}

NS_IMPL_QUERY_INTERFACE(HeaderVisitor, nsIHttpHeaderVisitor)

}  // anonymous namespace

void ChannelWrapper::GetRequestHeaders(nsTArray<dom::MozHTTPHeader>& aRetVal,
                                       ErrorResult& aRv) const {
  if (nsCOMPtr<nsIHttpChannel> chan = MaybeHttpChannel()) {
    HeaderVisitor visitor(aRetVal);
    visitor.VisitRequestHeaders(chan, aRv);
  } else {
    aRv.Throw(NS_ERROR_UNEXPECTED);
  }
}

void ChannelWrapper::GetRequestHeader(const nsCString& aHeader,
                                      nsCString& aResult,
                                      ErrorResult& aRv) const {
  aResult.SetIsVoid(true);
  if (nsCOMPtr<nsIHttpChannel> chan = MaybeHttpChannel()) {
    Unused << chan->GetRequestHeader(aHeader, aResult);
  } else {
    aRv.Throw(NS_ERROR_UNEXPECTED);
  }
}

void ChannelWrapper::GetResponseHeaders(nsTArray<dom::MozHTTPHeader>& aRetVal,
                                        ErrorResult& aRv) const {
  if (nsCOMPtr<nsIHttpChannel> chan = MaybeHttpChannel()) {
    HeaderVisitor visitor(aRetVal, mContentTypeHdr);
    visitor.VisitResponseHeaders(chan, aRv);
  } else {
    aRv.Throw(NS_ERROR_UNEXPECTED);
  }
}

void ChannelWrapper::SetRequestHeader(const nsCString& aHeader,
                                      const nsCString& aValue, bool aMerge,
                                      ErrorResult& aRv) {
  nsresult rv = NS_ERROR_UNEXPECTED;
  if (nsCOMPtr<nsIHttpChannel> chan = MaybeHttpChannel()) {
    rv = chan->SetRequestHeader(aHeader, aValue, aMerge);
  }
  if (NS_FAILED(rv)) {
    aRv.Throw(rv);
  }
}

void ChannelWrapper::SetResponseHeader(const nsCString& aHeader,
                                       const nsCString& aValue, bool aMerge,
                                       ErrorResult& aRv) {
  nsresult rv = NS_ERROR_UNEXPECTED;
  if (nsCOMPtr<nsIHttpChannel> chan = MaybeHttpChannel()) {
    if (aHeader.LowerCaseEqualsLiteral("content-type")) {
      rv = chan->SetContentType(aValue);
      if (NS_SUCCEEDED(rv)) {
        mContentTypeHdr = aValue;
      }
    } else {
      rv = chan->SetResponseHeader(aHeader, aValue, aMerge);
    }
  }
  if (NS_FAILED(rv)) {
    aRv.Throw(rv);
  }
}

/*****************************************************************************
 * LoadInfo
 *****************************************************************************/

already_AddRefed<nsILoadContext> ChannelWrapper::GetLoadContext() const {
  if (nsCOMPtr<nsIChannel> chan = MaybeChannel()) {
    nsCOMPtr<nsILoadContext> ctxt;
    NS_QueryNotificationCallbacks(chan, ctxt);
    return ctxt.forget();
  }
  return nullptr;
}

already_AddRefed<Element> ChannelWrapper::GetBrowserElement() const {
  if (nsCOMPtr<nsILoadContext> ctxt = GetLoadContext()) {
    nsCOMPtr<nsIDOMElement> domElem;
    if (NS_SUCCEEDED(ctxt->GetTopFrameElement(getter_AddRefs(domElem)))) {
      nsCOMPtr<Element> elem = do_QueryInterface(domElem);
      return elem.forget();
    }
  }
  return nullptr;
}

static inline bool IsSystemPrincipal(nsIPrincipal* aPrincipal) {
  bool isSystem = false;
  Unused << aPrincipal->GetIsSystemPrincipal(&isSystem);
  return isSystem;
}

static already_AddRefed<nsIPrincipal> GetLoadingPrincipal(nsILoadInfo* aLoadInfo) {
  nsCOMPtr<nsIPrincipal> prin;
  if (aLoadInfo) {
    Unused << aLoadInfo->GetLoadingPrincipal(getter_AddRefs(prin));
  }
  return prin.forget();
}

bool ChannelWrapper::IsSystemLoad() const {
  if (nsCOMPtr<nsILoadInfo> loadInfo = GetLoadInfo()) {
    nsCOMPtr<nsIPrincipal> loadingPrin = GetLoadingPrincipal(loadInfo);
    if (loadingPrin) {
      nsIPrincipal* prin = loadingPrin;
      return IsSystemPrincipal(prin);
    }

    if (nsIPrincipal* prin = loadInfo->PrincipalToInherit()) {
      return IsSystemPrincipal(prin);
    }
    if (nsIPrincipal* prin = loadInfo->TriggeringPrincipal()) {
      return IsSystemPrincipal(prin);
    }
  }
  return false;
}

bool ChannelWrapper::CanModify() const {
  if (nsCOMPtr<nsILoadInfo> loadInfo = GetLoadInfo()) {
    nsCOMPtr<nsIPrincipal> loadingPrin = GetLoadingPrincipal(loadInfo);
    if (loadingPrin) {
      nsIPrincipal* prin = loadingPrin;
      if (IsSystemPrincipal(prin)) {
        return false;
      }
    }
  }
  return true;
}

already_AddRefed<nsIURI> ChannelWrapper::GetOriginURI() const {
  nsCOMPtr<nsIURI> uri;
  if (nsCOMPtr<nsILoadInfo> loadInfo = GetLoadInfo()) {
    if (nsIPrincipal* prin = loadInfo->TriggeringPrincipal()) {
      if (prin->GetIsCodebasePrincipal()) {
        auto* basePrin = BasePrincipal::Cast(prin);
        Unused << basePrin->GetURI(getter_AddRefs(uri));
      }
    }
  }
  return uri.forget();
}

already_AddRefed<nsIURI> ChannelWrapper::GetDocumentURI() const {
  nsCOMPtr<nsIURI> uri;
  if (nsCOMPtr<nsILoadInfo> loadInfo = GetLoadInfo()) {
    nsCOMPtr<nsIPrincipal> loadingPrin = GetLoadingPrincipal(loadInfo);
    if (loadingPrin) {
      nsIPrincipal* prin = loadingPrin;
      if (prin->GetIsCodebasePrincipal()) {
        auto* basePrin = BasePrincipal::Cast(prin);
        Unused << basePrin->GetURI(getter_AddRefs(uri));
      }
    }
  }
  return uri.forget();
}

void ChannelWrapper::GetOriginURL(nsCString& aRetVal) const {
  if (nsCOMPtr<nsIURI> uri = GetOriginURI()) {
    Unused << uri->GetSpec(aRetVal);
  }
}

void ChannelWrapper::GetDocumentURL(nsCString& aRetVal) const {
  if (nsCOMPtr<nsIURI> uri = GetDocumentURI()) {
    Unused << uri->GetSpec(aRetVal);
  }
}

bool ChannelWrapper::Matches(
    const dom::MozRequestFilter& aFilter, const nsAString& aExtensionId,
    const dom::MozRequestMatchOptions& aOptions) const {
  (void)aExtensionId;
  (void)aOptions;

  if (!HaveChannel()) {
    return false;
  }

  if (!aFilter.mTypes.IsNull() && !aFilter.mTypes.Value().Contains(Type())) {
    return false;
  }

  nsCOMPtr<nsILoadInfo> loadInfo = GetLoadInfo();
  bool isPrivate =
      loadInfo && loadInfo->GetOriginAttributes().mPrivateBrowsingId > 0;
  if (!aFilter.mIncognito.IsNull() && aFilter.mIncognito.Value() != isPrivate) {
    return false;
  }

  return true;
}

int64_t NormalizeWindowID(nsILoadInfo* aLoadInfo, uint64_t windowID) {
  (void)aLoadInfo;
  return windowID;
}

uint64_t ChannelWrapper::WindowId(nsILoadInfo* aLoadInfo) const {
  auto frameID = aLoadInfo->GetFrameOuterWindowID();
  if (!frameID) {
    frameID = aLoadInfo->GetOuterWindowID();
  }
  return frameID;
}

int64_t ChannelWrapper::WindowId() const {
  if (nsCOMPtr<nsILoadInfo> loadInfo = GetLoadInfo()) {
    return NormalizeWindowID(loadInfo, WindowId(loadInfo));
  }
  return 0;
}

int64_t ChannelWrapper::ParentWindowId() const {
  if (nsCOMPtr<nsILoadInfo> loadInfo = GetLoadInfo()) {
    uint64_t parentID;
    if (loadInfo->GetFrameOuterWindowID()) {
      parentID = loadInfo->GetOuterWindowID();
    } else {
      parentID = loadInfo->GetParentOuterWindowID();
    }
    return NormalizeWindowID(loadInfo, parentID);
  }
  return -1;
}

void ChannelWrapper::GetFrameAncestors(
    dom::Nullable<nsTArray<dom::MozFrameAncestorInfo>>& aFrameAncestors,
    ErrorResult& aRv) const {
  nsCOMPtr<nsILoadInfo> loadInfo = GetLoadInfo();
  if (!loadInfo || WindowId(loadInfo) == 0) {
    aFrameAncestors.SetNull();
    return;
  }

  nsresult rv = GetFrameAncestors(loadInfo, aFrameAncestors.SetValue());
  if (NS_FAILED(rv)) {
    aRv.Throw(rv);
  }
}

nsresult ChannelWrapper::GetFrameAncestors(
    nsILoadInfo* aLoadInfo,
    nsTArray<dom::MozFrameAncestorInfo>& aFrameAncestors) const {
  (void)aLoadInfo;

  int64_t parentId = ParentWindowId();
  if (parentId >= 0) {
    auto ancestor = aFrameAncestors.AppendElement(fallible);
    if (!ancestor) {
      return NS_ERROR_OUT_OF_MEMORY;
    }
    GetDocumentURL(ancestor->mUrl);
    ancestor->mFrameId = parentId;
  }
  return NS_OK;
}

/*****************************************************************************
 * Response filtering
 *****************************************************************************/

void ChannelWrapper::RegisterTraceableChannel(const nsAString& aAddonId,
                                              nsISupports* aBrowserParent) {
  // We can't attach new listeners after the response has started, so don't
  // bother registering anything.
  if (mResponseStarted || !CanModify()) {
    return;
  }

  nsCOMPtr<nsIAtom> addonId = NS_Atomize(aAddonId);
  nsCOMPtr<nsITabParent> tabParent = do_QueryInterface(aBrowserParent);
  mAddonEntries.Put(addonId, tabParent);
  if (!mChannelEntry) {
    mChannelEntry = WebRequestService::GetSingleton().RegisterChannel(this);
    CheckEventListeners();
  }
}

already_AddRefed<nsITraceableChannel> ChannelWrapper::GetTraceableChannel(
  nsIAtom* aAddonId, dom::nsIContentParent* aContentParent) const {
  nsCOMPtr<nsITabParent> browserParent;
  if (mAddonEntries.Get(aAddonId, getter_AddRefs(browserParent))) {
    nsIContentParent* contentParent = nullptr;
    if (browserParent) {
      if (TabParent* parent = TabParent::GetFrom(browserParent)) {
        contentParent = parent->Manager();
      }
    }

    if (contentParent == aContentParent) {
      nsCOMPtr<nsITraceableChannel> chan = QueryChannel();
      return chan.forget();
    }
  }
  return nullptr;
}

/*****************************************************************************
 * ...
 *****************************************************************************/

MozContentPolicyType GetContentPolicyType(nsContentPolicyType aType) {
  // Note: Please keep this function in sync with the external types in
  // nsIContentPolicy.idl
  switch (aType) {
    case nsIContentPolicy::TYPE_DOCUMENT:
      return MozContentPolicyType::Main_frame;
    case nsIContentPolicy::TYPE_SUBDOCUMENT:
      return MozContentPolicyType::Sub_frame;
    case nsIContentPolicy::TYPE_STYLESHEET:
      return MozContentPolicyType::Stylesheet;
    case nsIContentPolicy::TYPE_SCRIPT:
      return MozContentPolicyType::Script;
    case nsIContentPolicy::TYPE_IMAGE:
      return MozContentPolicyType::Image;
    case nsIContentPolicy::TYPE_OBJECT:
      return MozContentPolicyType::Object;
    case nsIContentPolicy::TYPE_OBJECT_SUBREQUEST:
      return MozContentPolicyType::Object_subrequest;
    case nsIContentPolicy::TYPE_XMLHTTPREQUEST:
      return MozContentPolicyType::Xmlhttprequest;
    // TYPE_FETCH returns xmlhttprequest for cross-browser compatibility.
    case nsIContentPolicy::TYPE_FETCH:
      return MozContentPolicyType::Xmlhttprequest;
    case nsIContentPolicy::TYPE_XSLT:
      return MozContentPolicyType::Xslt;
    case nsIContentPolicy::TYPE_PING:
      return MozContentPolicyType::Ping;
    case nsIContentPolicy::TYPE_BEACON:
      return MozContentPolicyType::Beacon;
    case nsIContentPolicy::TYPE_DTD:
      return MozContentPolicyType::Xml_dtd;
    case nsIContentPolicy::TYPE_FONT:
      return MozContentPolicyType::Font;
    case nsIContentPolicy::TYPE_MEDIA:
      return MozContentPolicyType::Media;
    case nsIContentPolicy::TYPE_WEBSOCKET:
      return MozContentPolicyType::Websocket;
    case nsIContentPolicy::TYPE_CSP_REPORT:
      return MozContentPolicyType::Csp_report;
    case nsIContentPolicy::TYPE_IMAGESET:
      return MozContentPolicyType::Imageset;
    case nsIContentPolicy::TYPE_WEB_MANIFEST:
      return MozContentPolicyType::Web_manifest;
    case nsIContentPolicy::TYPE_INTERNAL_SCRIPT_PRELOAD:
      return MozContentPolicyType::Speculative;
    case nsIContentPolicy::TYPE_INVALID:
    case nsIContentPolicy::TYPE_OTHER:
    case nsIContentPolicy::TYPE_SAVEAS_DOWNLOAD:
      break;
      // Do not add default: so that compilers can catch the missing case.
  }
  return MozContentPolicyType::Other;
}

MozContentPolicyType ChannelWrapper::Type() const {
  if (nsCOMPtr<nsILoadInfo> loadInfo = GetLoadInfo()) {
    return GetContentPolicyType(loadInfo->GetExternalContentPolicyType());
  }
  return MozContentPolicyType::Other;
}

void ChannelWrapper::GetMethod(nsCString& aMethod) const {
  if (nsCOMPtr<nsIHttpChannel> chan = MaybeHttpChannel()) {
    Unused << chan->GetRequestMethod(aMethod);
  }
}

/*****************************************************************************
 * ...
 *****************************************************************************/

uint32_t ChannelWrapper::StatusCode() const {
  uint32_t result = 0;
  if (nsCOMPtr<nsIHttpChannel> chan = MaybeHttpChannel()) {
    Unused << chan->GetResponseStatus(&result);
  }
  return result;
}

void ChannelWrapper::GetStatusLine(nsCString& aRetVal) const {
  nsCOMPtr<nsIHttpChannel> chan = MaybeHttpChannel();
  nsCOMPtr<nsIHttpChannelInternal> internal = do_QueryInterface(chan);

  if (internal) {
    nsAutoCString statusText;
    uint32_t major, minor, status;
    if (NS_FAILED(chan->GetResponseStatus(&status)) ||
        NS_FAILED(chan->GetResponseStatusText(statusText)) ||
        NS_FAILED(internal->GetResponseVersion(&major, &minor))) {
      return;
    }

    aRetVal = nsPrintfCString("HTTP/%u.%u %u %s", major, minor, status,
                              statusText.get());
  }
}

/*****************************************************************************
 * ...
 *****************************************************************************/

already_AddRefed<nsIURI> ChannelWrapper::FinalURI() const {
  nsCOMPtr<nsIURI> uri;
  if (nsCOMPtr<nsIChannel> chan = MaybeChannel()) {
    NS_GetFinalChannelURI(chan, getter_AddRefs(uri));
  }
  return uri.forget();
}

void ChannelWrapper::GetFinalURL(nsString& aRetVal) const {
  if (HaveChannel()) {
    if (nsCOMPtr<nsIURI> uri = FinalURI()) {
      nsCString spec;
      if (NS_SUCCEEDED(uri->GetSpec(spec))) {
        CopyUTF8toUTF16(spec, aRetVal);
      }
    }
  }
}

/*****************************************************************************
 * ...
 *****************************************************************************/

nsresult FillProxyInfo(MozProxyInfo& aDict, nsIProxyInfo* aProxyInfo) {
  nsresult rv = aProxyInfo->GetHost(aDict.mHost);
  if (NS_FAILED(rv)) {
    return rv;
  }
  rv = aProxyInfo->GetPort(&aDict.mPort);
  if (NS_FAILED(rv)) {
    return rv;
  }
  rv = aProxyInfo->GetType(aDict.mType);
  if (NS_FAILED(rv)) {
    return rv;
  }
  rv = aProxyInfo->GetUsername(aDict.mUsername);
  if (NS_FAILED(rv)) {
    return rv;
  }
  aDict.mProxyAuthorizationHeader.Truncate();
  aDict.mConnectionIsolationKey.Truncate();
  rv = aProxyInfo->GetFailoverTimeout(&aDict.mFailoverTimeout.Construct());
  if (NS_FAILED(rv)) {
    return rv;
  }

  uint32_t flags;
  rv = aProxyInfo->GetFlags(&flags);
  if (NS_FAILED(rv)) {
    return rv;
  }
  aDict.mProxyDNS = flags & nsIProxyInfo::TRANSPARENT_PROXY_RESOLVES_HOST;

  return NS_OK;
}

void ChannelWrapper::GetProxyInfo(dom::Nullable<MozProxyInfo>& aRetVal,
                                  ErrorResult& aRv) const {
  nsCOMPtr<nsIProxyInfo> proxyInfo;
  if (nsCOMPtr<nsIProxiedChannel> proxied = QueryChannel()) {
    Unused << proxied->GetProxyInfo(getter_AddRefs(proxyInfo));
  }
  if (proxyInfo) {
    MozProxyInfo result;

    nsresult rv = FillProxyInfo(result, proxyInfo);
    if (NS_FAILED(rv)) {
      aRv.Throw(rv);
    } else {
      aRetVal.SetValue(std::move(result));
    }
  }
}

void ChannelWrapper::GetRemoteAddress(nsCString& aRetVal) const {
  aRetVal.SetIsVoid(true);
  if (nsCOMPtr<nsIHttpChannelInternal> internal = QueryChannel()) {
    Unused << internal->GetRemoteAddress(aRetVal);
  }
}

/*****************************************************************************
 * Error handling
 *****************************************************************************/

void ChannelWrapper::GetErrorString(nsString& aRetVal) const {
  if (nsCOMPtr<nsIChannel> chan = MaybeChannel()) {
    nsCOMPtr<nsISupports> securityInfo;
    Unused << chan->GetSecurityInfo(getter_AddRefs(securityInfo));
    if (nsCOMPtr<nsITransportSecurityInfo> tsi =
            do_QueryInterface(securityInfo)) {
      int32_t errorCode = 0;
      tsi->GetErrorCode(&errorCode);
      if (psm::IsNSSErrorCode(errorCode)) {
        nsCOMPtr<nsINSSErrorsService> nsserr =
            do_GetService(NS_NSS_ERRORS_SERVICE_CONTRACTID);

        nsresult rv = psm::GetXPCOMFromNSSError(errorCode);
        if (nsserr && NS_SUCCEEDED(nsserr->GetErrorMessage(rv, aRetVal))) {
          return;
        }
      }
    }

    nsresult status;
    if (NS_SUCCEEDED(chan->GetStatus(&status)) && NS_FAILED(status)) {
      nsAutoCString name;
      GetErrorName(status, name);
      AppendUTF8toUTF16(name, aRetVal);
    } else {
      aRetVal.SetIsVoid(true);
    }
  } else {
    aRetVal.AssignLiteral("NS_ERROR_UNEXPECTED");
  }
}

void ChannelWrapper::ErrorCheck() {
  if (!mFiredErrorEvent) {
    nsAutoString error;
    GetErrorString(error);
    if (error.Length()) {
      mChannelEntry = nullptr;
      mFiredErrorEvent = true;
      ChannelWrapperBinding::ClearCachedErrorStringValue(this);
      FireEvent(NS_LITERAL_STRING("error"));
    }
  }
}

/*****************************************************************************
 * nsIWebRequestListener
 *****************************************************************************/

NS_IMPL_ISUPPORTS(ChannelWrapper::RequestListener, nsIStreamListener,
                  nsIRequestObserver, nsIThreadRetargetableStreamListener)

ChannelWrapper::RequestListener::~RequestListener() {
  NS_ReleaseOnMainThread(mChannelWrapper.forget());
}

nsresult ChannelWrapper::RequestListener::Init() {
  if (nsCOMPtr<nsITraceableChannel> chan = mChannelWrapper->QueryChannel()) {
    return chan->SetNewListener(this, getter_AddRefs(mOrigStreamListener));
  }
  return NS_ERROR_UNEXPECTED;
}

NS_IMETHODIMP
ChannelWrapper::RequestListener::OnStartRequest(nsIRequest* request,
                                                nsISupports* context) {
  MOZ_ASSERT(mOrigStreamListener, "Should have mOrigStreamListener");

  mChannelWrapper->mChannelEntry = nullptr;
  mChannelWrapper->mResponseStarted = true;
  mChannelWrapper->ErrorCheck();
  mChannelWrapper->FireEvent(NS_LITERAL_STRING("start"));

  return mOrigStreamListener->OnStartRequest(request, context);
}

NS_IMETHODIMP
ChannelWrapper::RequestListener::OnStopRequest(nsIRequest* request,
                                               nsISupports* context,
                                               nsresult aStatus) {
  MOZ_ASSERT(mOrigStreamListener, "Should have mOrigStreamListener");

  mChannelWrapper->mChannelEntry = nullptr;
  mChannelWrapper->ErrorCheck();
  mChannelWrapper->FireEvent(NS_LITERAL_STRING("stop"));

  return mOrigStreamListener->OnStopRequest(request, context, aStatus);
}

NS_IMETHODIMP
ChannelWrapper::RequestListener::OnDataAvailable(nsIRequest* request,
                                                 nsISupports* context,
                                                 nsIInputStream* inStr,
                                                 uint64_t sourceOffset,
                                                 uint32_t count) {
  MOZ_ASSERT(mOrigStreamListener, "Should have mOrigStreamListener");
  return mOrigStreamListener->OnDataAvailable(request, context, inStr,
                                              sourceOffset, count);
}

NS_IMETHODIMP
ChannelWrapper::RequestListener::CheckListenerChain() {
  MOZ_ASSERT(NS_IsMainThread(), "Should be on main thread!");
  nsresult rv;
  nsCOMPtr<nsIThreadRetargetableStreamListener> retargetableListener =
      do_QueryInterface(mOrigStreamListener, &rv);
  if (retargetableListener) {
    return retargetableListener->CheckListenerChain();
  }
  return rv;
}

/*****************************************************************************
 * Event dispatching
 *****************************************************************************/

void ChannelWrapper::FireEvent(const nsAString& aType) {
  EventInit init;
  init.mBubbles = false;
  init.mCancelable = false;

  RefPtr<Event> event = Event::Constructor(this, aType, init);
  event->SetTrusted(true);

  nsCOMPtr<nsIDOMEvent> domEvent = do_QueryInterface(event);
  bool dummy = false;
  Unused << DispatchEvent(domEvent, &dummy);
}

void ChannelWrapper::CheckEventListeners() {
  if (!mAddedStreamListener &&
      (HasListenersFor(nsGkAtoms::onerror) ||
       HasListenersFor(nsGkAtoms::onstart) ||
       HasListenersFor(nsGkAtoms::onstop) || mChannelEntry)) {
    RefPtr<RequestListener> listener = new RequestListener(this);
    if (!NS_WARN_IF(NS_FAILED(listener->Init()))) {
      mAddedStreamListener = true;
    }
  }
}

void ChannelWrapper::EventListenerAdded(nsIAtom* aType) {
  CheckEventListeners();
}

void ChannelWrapper::EventListenerRemoved(nsIAtom* aType) {
  CheckEventListeners();
}

/*****************************************************************************
 * Glue
 *****************************************************************************/

JSObject* ChannelWrapper::WrapObject(JSContext* aCx, HandleObject aGivenProto) {
  return ChannelWrapperBinding::Wrap(aCx, this, aGivenProto);
}

NS_IMPL_CYCLE_COLLECTION_CLASS(ChannelWrapper)

NS_INTERFACE_MAP_BEGIN_CYCLE_COLLECTION(ChannelWrapper)
  NS_INTERFACE_MAP_ENTRY(ChannelWrapper)
NS_INTERFACE_MAP_END_INHERITING(DOMEventTargetHelper)

NS_IMPL_CYCLE_COLLECTION_UNLINK_BEGIN_INHERITED(ChannelWrapper,
                                                DOMEventTargetHelper)
  NS_IMPL_CYCLE_COLLECTION_UNLINK(mParent)
  NS_IMPL_CYCLE_COLLECTION_UNLINK(mStub)
NS_IMPL_CYCLE_COLLECTION_UNLINK_END

NS_IMPL_CYCLE_COLLECTION_TRAVERSE_BEGIN_INHERITED(ChannelWrapper,
                                                  DOMEventTargetHelper)
  NS_IMPL_CYCLE_COLLECTION_TRAVERSE(mParent)
  NS_IMPL_CYCLE_COLLECTION_TRAVERSE(mStub)
NS_IMPL_CYCLE_COLLECTION_TRAVERSE_END

NS_IMPL_CYCLE_COLLECTION_TRACE_BEGIN_INHERITED(ChannelWrapper,
                                               DOMEventTargetHelper)
NS_IMPL_CYCLE_COLLECTION_TRACE_END

NS_IMPL_ADDREF_INHERITED(ChannelWrapper, DOMEventTargetHelper)
NS_IMPL_RELEASE_INHERITED(ChannelWrapper, DOMEventTargetHelper)

}  // namespace extensions
}  // namespace mozilla
