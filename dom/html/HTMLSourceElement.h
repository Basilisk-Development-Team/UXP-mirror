/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef mozilla_dom_HTMLSourceElement_h
#define mozilla_dom_HTMLSourceElement_h

#include "mozilla/Attributes.h"
#include "nsIDOMHTMLSourceElement.h"
#include "nsGenericHTMLElement.h"
#include "mozilla/dom/HTMLMediaElement.h"

class nsMediaList;
class nsAttrValue;

namespace mozilla {
namespace dom {

class HTMLSourceElement final : public nsGenericHTMLElement,
                                public nsIDOMHTMLSourceElement
{
public:
  explicit HTMLSourceElement(already_AddRefed<mozilla::dom::NodeInfo>& aNodeInfo);

  // nsISupports
  NS_DECL_ISUPPORTS_INHERITED
  NS_DECL_CYCLE_COLLECTION_CLASS_INHERITED(HTMLSourceElement,
                                           nsGenericHTMLElement)

  NS_IMPL_FROMCONTENT_HTML_WITH_TAG(HTMLSourceElement, source)

  // nsIDOMHTMLSourceElement
  NS_DECL_NSIDOMHTMLSOURCEELEMENT

  virtual nsresult Clone(mozilla::dom::NodeInfo* aNodeInfo, nsINode** aResult) const override;

  // Override BindToTree() so that we can trigger a load when we add a
  // child source element.
  virtual nsresult BindToTree(nsIDocument* aDocument, nsIContent* aParent,
                              nsIContent* aBindingParent,
                              bool aCompileEventHandlers) override;

  // If this element's media attr matches for its owner document.  Returns true
  // if no media attr was set.
  bool MatchesCurrentMedia();

  // True if a source tag would match the given media attribute for the
  // specified document. Used by the preloader to determine valid <source> tags
  // prior to DOM creation.
  static bool WouldMatchMediaForDocument(const nsAString& aMediaStr,
                                         const nsIDocument *aDocument);

  // Return the MediaSource object if any associated with the src attribute
  // when it was set.
  MediaSource* GetSrcMediaSource() { return mSrcMediaSource; };

  // WebIDL
  void GetSrc(nsString& aSrc, nsIPrincipal&)
  {
    GetURIAttr(nsGkAtoms::src, nullptr, aSrc);
  }
  void SetSrc(const nsAString& aSrc, nsIPrincipal& aTriggeringPrincipal, mozilla::ErrorResult& rv)
  {
    SetHTMLAttr(nsGkAtoms::src, aSrc, aTriggeringPrincipal, rv);
  }
  nsIPrincipal* GetSrcTriggeringPrincipal() const
  {
    return mSrcTriggeringPrincipal;
  }

  nsIPrincipal* GetSrcsetTriggeringPrincipal() const
  {
    return mSrcsetTriggeringPrincipal;
  }

  void GetType(DOMString& aType)
  {
    GetHTMLAttr(nsGkAtoms::type, aType);
  }
  void SetType(const nsAString& aType, ErrorResult& rv)
  {
    SetHTMLAttr(nsGkAtoms::type, aType, rv);
  }

  void GetSrcset(DOMString& aSrcset, nsIPrincipal&)
  {
    GetHTMLAttr(nsGkAtoms::srcset, aSrcset);
  }
  void SetSrcset(const nsAString& aSrcset, nsIPrincipal& aTriggeringPrincipal, mozilla::ErrorResult& rv)
  {
    SetHTMLAttr(nsGkAtoms::srcset, aSrcset, aTriggeringPrincipal, rv);
  }

  void GetSizes(DOMString& aSizes)
  {
    GetHTMLAttr(nsGkAtoms::sizes, aSizes);
  }
  void SetSizes(const nsAString& aSizes, mozilla::ErrorResult& rv)
  {
    SetHTMLAttr(nsGkAtoms::sizes, aSizes, rv);
  }

  void GetMedia(DOMString& aMedia)
  {
    GetHTMLAttr(nsGkAtoms::media, aMedia);
  }
  void SetMedia(const nsAString& aMedia, mozilla::ErrorResult& rv)
  {
    SetHTMLAttr(nsGkAtoms::media, aMedia, rv);
  }

protected:
  virtual ~HTMLSourceElement();

  virtual JSObject* WrapNode(JSContext* aCx, JS::Handle<JSObject*> aGivenProto) override;

protected:
  virtual nsresult AfterSetAttr(int32_t aNameSpaceID, nsIAtom* aName,
                                const nsAttrValue* aValue,
                                const nsAttrValue* aOldValue,
                                nsIPrincipal* aMaybeScriptedPrincipal,
                                bool aNotify) override;

private:
  RefPtr<nsMediaList> mMediaList;
  RefPtr<MediaSource> mSrcMediaSource;

  // The triggering principal for the src attribute.
  nsCOMPtr<nsIPrincipal> mSrcTriggeringPrincipal;

  // The triggering principal for the srcset attribute.
  nsCOMPtr<nsIPrincipal> mSrcsetTriggeringPrincipal;

  // Generates a new nsMediaList using the given input
  void UpdateMediaList(const nsAttrValue* aValue);
};

} // namespace dom
} // namespace mozilla

#endif // mozilla_dom_HTMLSourceElement_h
