/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/#the-script-element
 * http://www.whatwg.org/specs/web-apps/current-work/#other-elements,-attributes-and-apis
 */

[HTMLConstructor]
interface HTMLScriptElement : HTMLElement {
  [CEReactions, NeedsSubjectPrincipal, SetterThrows]
  attribute DOMString src;
  [CEReactions, SetterThrows]
  attribute DOMString type;
  [CEReactions, SetterThrows, Pref="dom.moduleScripts.enabled"]
  attribute boolean noModule;
  [CEReactions, SetterThrows]
  attribute DOMString charset;
  [CEReactions, SetterThrows]
  attribute boolean async;
  [CEReactions, SetterThrows]
  attribute boolean defer;
  [CEReactions, SetterThrows]
  attribute DOMString? crossOrigin;
  [CEReactions, SetterThrows]
  attribute DOMString referrerPolicy;
  [CEReactions, SetterThrows]
  attribute DOMString text;
  [CEReactions, SetterThrows, Pure]
  attribute DOMString nonce;
  [CEReactions, SetterThrows, Pure]
  attribute DOMString integrity;
};

// http://www.whatwg.org/specs/web-apps/current-work/#other-elements,-attributes-and-apis
partial interface HTMLScriptElement {
  [CEReactions, SetterThrows]
  attribute DOMString event;
  [CEReactions, SetterThrows]
  attribute DOMString htmlFor;
};
