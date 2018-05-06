# -*- Mode: javascript; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*-
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

var FullScreen = {
  _XULNS: "http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul",

  toggle: function () {
    var enterFS = window.fullScreen;

    // Toggle the View:FullScreen command, which controls elements like the
    // fullscreen menuitem, menubars, and the appmenu.
    let fullscreenCommand = document.getElementById("View:FullScreen");
    if (enterFS) {
      fullscreenCommand.setAttribute("checked", enterFS);
    } else {
      fullscreenCommand.removeAttribute("checked");
    }

#ifdef XP_MACOSX
    // Make sure the menu items are adjusted.
    document.getElementById("enterFullScreenItem").hidden = enterFS;
    document.getElementById("exitFullScreenItem").hidden = !enterFS;
#endif

    if (!this._fullScrToggler) {
      this._fullScrToggler = document.getElementById("fullscr-toggler");
      this._fullScrToggler.addEventListener("mouseover", this._expandCallback, false);
      this._fullScrToggler.addEventListener("dragenter", this._expandCallback, false);
    }

    // On OS X Lion we don't want to hide toolbars when entering fullscreen, unless
    // we're entering DOM fullscreen, in which case we should hide the toolbars.
    // If we're leaving fullscreen, then we'll go through the exit code below to
    // make sure toolbars are made visible in the case of DOM fullscreen.
    if (enterFS && this.useLionFullScreen) {
      if (document.mozFullScreen) {
        this.showXULChrome("toolbar", false);
      }
      else {
        gNavToolbox.setAttribute("inFullscreen", true);
        document.documentElement.setAttribute("inFullscreen", true);
      }
      return;
    }

    // show/hide menubars, toolbars (except the full screen toolbar)
    this.showXULChrome("toolbar", !enterFS);

    if (enterFS) {
      document.addEventListener("keypress", this._keyToggleCallback, false);
      document.addEventListener("popupshown", this._setPopupOpen, false);
      document.addEventListener("popuphidden", this._setPopupOpen, false);
      this._shouldAnimate = true;
      // We don't animate the toolbar collapse if in DOM full-screen mode,
      // as the size of the content area would still be changing after the
      // mozfullscreenchange event fired, which could confuse content script.
      this.hideNavToolbox(document.mozFullScreen);
    }
    else {
      this.showNavToolbox(false);
      // This is needed if they use the context menu to quit fullscreen
      this._isPopupOpen = false;

      document.documentElement.removeAttribute("inDOMFullscreen");

      this.cleanup();
    }
  },

  exitDomFullScreen : function() {
    document.mozCancelFullScreen();
  },

  handleEvent: function (event) {
    switch (event.type) {
      case "activate":
        if (document.mozFullScreen) {
          this.showWarning(this.fullscreenDoc);
        }
        break;
      case "transitionend":
        if (event.propertyName == "opacity")
          this.cancelWarning();
        break;
    }
  },

  enterDomFullscreen : function(event) {
    if (!document.mozFullScreen)
      return;

    // However, if we receive a "MozDOMFullscreen:NewOrigin" event for a document
    // which is not a subdocument of a currently active (ie. visible) browser
    // or iframe, we know that we've switched to a different frame since the
    // request to enter full-screen was made, so we should exit full-screen
    // since the "full-screen document" isn't acutally visible.
    if (!event.target.defaultView.QueryInterface(Ci.nsIInterfaceRequestor)
                                 .getInterface(Ci.nsIWebNavigation)
                                 .QueryInterface(Ci.nsIDocShell).isActive) {
      document.mozCancelFullScreen();
      return;
    }

    let focusManager = Cc["@mozilla.org/focus-manager;1"].getService(Ci.nsIFocusManager);
    if (focusManager.activeWindow != window) {
      // The top-level window has lost focus since the request to enter
      // full-screen was made. Cancel full-screen.
      document.mozCancelFullScreen();
      return;
    }

    document.documentElement.setAttribute("inDOMFullscreen", true);

    if (gFindBarInitialized)
      gFindBar.close();

    this.showWarning(event.target);

    // Exit DOM full-screen mode upon open, close, or change tab.
    gBrowser.tabContainer.addEventListener("TabOpen", this.exitDomFullScreen);
    gBrowser.tabContainer.addEventListener("TabClose", this.exitDomFullScreen);
    gBrowser.tabContainer.addEventListener("TabSelect", this.exitDomFullScreen);

    // Add listener to detect when the fullscreen window is re-focused.
    // If a fullscreen window loses focus, we show a warning when the
    // fullscreen window is refocused.
    if (!this.useLionFullScreen) {
      window.addEventListener("activate", this);
    }

    // Cancel any "hide the toolbar" animation which is in progress, and make
    // the toolbar hide immediately.
    this.hideNavToolbox(true);
  },

  cleanup: function () {
    if (!window.fullScreen) {
      MousePosTracker.removeListener(this);
      document.removeEventListener("keypress", this._keyToggleCallback, false);
      document.removeEventListener("popupshown", this._setPopupOpen, false);
      document.removeEventListener("popuphidden", this._setPopupOpen, false);

      this.cancelWarning();
      gBrowser.tabContainer.removeEventListener("TabOpen", this.exitDomFullScreen);
      gBrowser.tabContainer.removeEventListener("TabClose", this.exitDomFullScreen);
      gBrowser.tabContainer.removeEventListener("TabSelect", this.exitDomFullScreen);
      if (!this.useLionFullScreen)
        window.removeEventListener("activate", this);
      this.fullscreenDoc = null;
    }
  },

  getMouseTargetRect: function()
  {
    return this._mouseTargetRect;
  },

  // Event callbacks
  _expandCallback: function()
  {
    FullScreen.showNavToolbox();
  },
  onMouseEnter: function()
  {
    FullScreen.hideNavToolbox();
  },
  _keyToggleCallback: function(aEvent)
  {
    // if we can use the keyboard (eg Ctrl+L or Ctrl+E) to open the toolbars, we
    // should provide a way to collapse them too.
    if (aEvent.keyCode == aEvent.DOM_VK_ESCAPE) {
      FullScreen.hideNavToolbox(true);
    }
    // F6 is another shortcut to the address bar, but its not covered in OpenLocation()
    else if (aEvent.keyCode == aEvent.DOM_VK_F6)
      FullScreen.showNavToolbox();
  },

  // Checks whether we are allowed to collapse the chrome
  _isPopupOpen: false,
  _isChromeCollapsed: false,
  _safeToCollapse: function(forceHide)
  {
    if (!gPrefService.getBoolPref("browser.fullscreen.autohide"))
      return false;

    // a popup menu is open in chrome: don't collapse chrome
    if (!forceHide && this._isPopupOpen)
      return false;

    // a textbox in chrome is focused (location bar anyone?): don't collapse chrome
    if (document.commandDispatcher.focusedElement &&
        document.commandDispatcher.focusedElement.ownerDocument == document &&
        document.commandDispatcher.focusedElement.localName == "input") {
      if (forceHide)
        // hidden textboxes that still have focus are bad bad bad
        document.commandDispatcher.focusedElement.blur();
      else
        return false;
    }
    return true;
  },

  _setPopupOpen: function(aEvent)
  {
    // Popups should only veto chrome collapsing if they were opened when the chrome was not collapsed.
    // Otherwise, they would not affect chrome and the user would expect the chrome to go away.
    // e.g. we wouldn't want the autoscroll icon firing this event, so when the user
    // toggles chrome when moving mouse to the top, it doesn't go away again.
    if (aEvent.type == "popupshown" && !FullScreen._isChromeCollapsed &&
        aEvent.target.localName != "tooltip" && aEvent.target.localName != "window")
      FullScreen._isPopupOpen = true;
    else if (aEvent.type == "popuphidden" && aEvent.target.localName != "tooltip" &&
             aEvent.target.localName != "window")
      FullScreen._isPopupOpen = false;
  },

  // Autohide helpers for the context menu item
  getAutohide: function(aItem)
  {
    aItem.setAttribute("checked", gPrefService.getBoolPref("browser.fullscreen.autohide"));
  },
  setAutohide: function()
  {
    gPrefService.setBoolPref("browser.fullscreen.autohide", !gPrefService.getBoolPref("browser.fullscreen.autohide"));
  },

  // Animate the toolbars disappearing
  _shouldAnimate: true,

  cancelWarning: function(event) {
    if (!this.warningBox)
      return;
    this.warningBox.removeEventListener("transitionend", this);
    if (this.warningFadeOutTimeout) {
      clearTimeout(this.warningFadeOutTimeout);
      this.warningFadeOutTimeout = null;
    }

    // Ensure focus switches away from the (now hidden) warning box. If the user
    // clicked buttons in the fullscreen key authorization UI, it would have been
    // focused, and any key events would be directed at the (now hidden) chrome
    // document instead of the target document.
    gBrowser.selectedBrowser.focus();

    this.warningBox.setAttribute("hidden", true);
    this.warningBox.removeAttribute("fade-warning-out");
    this.warningBox.removeAttribute("obscure-browser");
    this.warningBox = null;
  },

  setFullscreenAllowed: function(isApproved) {
    // The "remember decision" checkbox is hidden when showing for documents that
    // the permission manager can't handle (documents with URIs without a host).
    // We simply require those to be approved every time instead.
    let rememberCheckbox = document.getElementById("full-screen-remember-decision");
    let uri = this.fullscreenDoc.nodePrincipal.URI;
    if (!rememberCheckbox.hidden) {
      if (rememberCheckbox.checked)
        Services.perms.add(uri,
                           "fullscreen",
                           isApproved ? Services.perms.ALLOW_ACTION : Services.perms.DENY_ACTION,
                           Services.perms.EXPIRE_NEVER);
      else if (isApproved) {
        // The user has only temporarily approved fullscren for this fullscreen
        // session only. Add the permission (so Goanna knows to approve any further
        // fullscreen requests for this host in this fullscreen session) but add
        // a listener to revoke the permission when the chrome document exits
        // fullscreen.
        Services.perms.add(uri,
                           "fullscreen",
                           Services.perms.ALLOW_ACTION,
                           Services.perms.EXPIRE_SESSION);
        var onFullscreenchange = function onFullscreenchange(event) {
          if (event.target == document && document.mozFullScreenElement == null) {
            // The chrome document has left fullscreen. Remove the temporary permission grant.
            Services.perms.remove(uri, "fullscreen");
            document.removeEventListener("mozfullscreenchange", onFullscreenchange);
          }
        }
        document.addEventListener("mozfullscreenchange", onFullscreenchange);
      }
    }
    if (this.warningBox)
      this.warningBox.setAttribute("fade-warning-out", "true");
    // If the document has been granted fullscreen, notify Goanna so it can resume
    // any pending pointer lock requests, otherwise exit fullscreen; the user denied
    // the fullscreen request.
    if (isApproved)
      Services.obs.notifyObservers(this.fullscreenDoc, "fullscreen-approved", "");
    else
      document.mozCancelFullScreen();
  },

  warningBox: null,
  warningFadeOutTimeout: null,
  fullscreenDoc: null,

  // Shows the fullscreen approval UI, or if the domain has already been approved
  // for fullscreen, shows a warning that the site has entered fullscreen for a short
  // duration.
  showWarning: function(targetDoc) {
    if (!document.mozFullScreen ||
        !gPrefService.getBoolPref("full-screen-api.approval-required"))
      return;

    // Set the strings on the fullscreen approval UI.
    this.fullscreenDoc = targetDoc;
    let uri = this.fullscreenDoc.nodePrincipal.URI;
    let host = null;
    try {
      host = uri.host;
    } catch (e) { }
    let hostLabel = document.getElementById("full-screen-domain-text");
    let rememberCheckbox = document.getElementById("full-screen-remember-decision");
    let isApproved = false;
    if (host) {
      // Document's principal's URI has a host. Display a warning including the hostname and
      // show UI to enable the user to permanently grant this host permission to enter fullscreen.
      let utils = {};
      Cu.import("resource://gre/modules/DownloadUtils.jsm", utils);
      let displayHost = utils.DownloadUtils.getURIHost(uri.spec)[0];
      let bundle = Services.strings.createBundle("chrome://browser/locale/browser.properties");

      hostLabel.textContent = bundle.formatStringFromName("fullscreen.entered", [displayHost], 1);
      hostLabel.removeAttribute("hidden");

      rememberCheckbox.label = bundle.formatStringFromName("fullscreen.rememberDecision", [displayHost], 1);
      rememberCheckbox.checked = false;
      rememberCheckbox.removeAttribute("hidden");

      // Note we only allow documents whose principal's URI has a host to
      // store permission grants.
      isApproved = Services.perms.testPermission(uri, "fullscreen") == Services.perms.ALLOW_ACTION;
    } else {
      hostLabel.setAttribute("hidden", "true");
      rememberCheckbox.setAttribute("hidden", "true");
    }

    // Note: the warning box can be non-null if the warning box from the previous request
    // wasn't hidden before another request was made.
    if (!this.warningBox) {
      this.warningBox = document.getElementById("full-screen-warning-container");
      // Add a listener to clean up state after the warning is hidden.
      this.warningBox.addEventListener("transitionend", this);
      this.warningBox.removeAttribute("hidden");
    } else {
      if (this.warningFadeOutTimeout) {
        clearTimeout(this.warningFadeOutTimeout);
        this.warningFadeOutTimeout = null;
      }
      this.warningBox.removeAttribute("fade-warning-out");
    }

    // If fullscreen mode has not yet been approved for the fullscreen
    // document's domain, show the approval UI and don't auto fade out the
    // fullscreen warning box. Otherwise, we're just notifying of entry into
    // fullscreen mode. Note if the resource's host is null, we must be
    // showing a local file or a local data URI, and we require explicit
    // approval every time.
    let authUI = document.getElementById("full-screen-approval-pane");
    if (isApproved) {
      authUI.setAttribute("hidden", "true");
      this.warningBox.removeAttribute("obscure-browser");
    } else {
      // Partially obscure the <browser> element underneath the approval UI.
      this.warningBox.setAttribute("obscure-browser", "true");
      authUI.removeAttribute("hidden");
    }

    // If we're not showing the fullscreen approval UI, we're just notifying the user
    // of the transition, so set a timeout to fade the warning out after a few moments.
    if (isApproved)
      this.warningFadeOutTimeout =
        setTimeout(
          function() {
            if (this.warningBox)
              this.warningBox.setAttribute("fade-warning-out", "true");
          }.bind(this),
          3000);
  },

  showNavToolbox: function(trackMouse = true) {
    this._fullScrToggler.hidden = true;
    gNavToolbox.removeAttribute("fullscreenShouldAnimate");
    gNavToolbox.style.marginTop = "";

    if (!this._isChromeCollapsed) {
      return;
    }

    // Track whether mouse is near the toolbox
    this._isChromeCollapsed = false;
    if (trackMouse) {
      let rect = gBrowser.mPanelContainer.getBoundingClientRect();
      this._mouseTargetRect = {
        top: rect.top + 50,
        bottom: rect.bottom,
        left: rect.left,
        right: rect.right
      };
      MousePosTracker.addListener(this);
    }
  },

  hideNavToolbox: function(forceHide = false) {
    this._fullScrToggler.hidden = document.mozFullScreen;
    if (this._isChromeCollapsed) {
      if (forceHide) {
        gNavToolbox.removeAttribute("fullscreenShouldAnimate");
      }
      return;
    }
    if (!this._safeToCollapse(forceHide)) {
      this._fullScrToggler.hidden = true;
      return;
    }

    // browser.fullscreen.animateUp
    // 0 - never animate up
    // 1 - animate only for first collapse after entering fullscreen (default for perf's sake)
    // 2 - animate every time it collapses
    let animateUp = gPrefService.getIntPref("browser.fullscreen.animateUp");
    if (animateUp == 0) {
      this._shouldAnimate = false;
    } else if (animateUp == 2) {
      this._shouldAnimate = true;
    }
    if (this._shouldAnimate && !forceHide) {
      gNavToolbox.setAttribute("fullscreenShouldAnimate", true);
      this._shouldAnimate = false;
      // Hide the fullscreen toggler until the transition ends.
      let listener = () => {
        gNavToolbox.removeEventListener("transitionend", listener, true);
        if (this._isChromeCollapsed)
          this._fullScrToggler.hidden = false;
      };
      gNavToolbox.addEventListener("transitionend", listener, true);
      this._fullScrToggler.hidden = true;
    }

    gNavToolbox.style.marginTop =
      -gNavToolbox.getBoundingClientRect().height + "px";
    this._isChromeCollapsed = true;
    MousePosTracker.removeListener(this);
  },

  showXULChrome: function(aTag, aShow)
  {
    var els = document.getElementsByTagNameNS(this._XULNS, aTag);

    for (let el of els) {
      // XXX don't interfere with previously collapsed toolbars
      if (el.getAttribute("fullscreentoolbar") == "true") {
        if (!aShow) {

          var toolbarMode = el.getAttribute("mode");
          if (toolbarMode != "text") {
            el.setAttribute("saved-mode", toolbarMode);
            el.setAttribute("saved-iconsize", el.getAttribute("iconsize"));
            el.setAttribute("mode", "icons");
            el.setAttribute("iconsize", "small");
          }

          // Give the main nav bar and the tab bar the fullscreen context menu,
          // otherwise remove context menu to prevent breakage
          el.setAttribute("saved-context", el.getAttribute("context"));
          if (el.id == "nav-bar" || el.id == "TabsToolbar")
            el.setAttribute("context", "autohide-context");
          else
            el.removeAttribute("context");

          // Set the inFullscreen attribute to allow specific styling
          // in fullscreen mode
          el.setAttribute("inFullscreen", true);
        }
        else {
          var restoreAttr = function restoreAttr(attrName) {
            var savedAttr = "saved-" + attrName;
            if (el.hasAttribute(savedAttr)) {
              el.setAttribute(attrName, el.getAttribute(savedAttr));
              el.removeAttribute(savedAttr);
            }
          }

          restoreAttr("mode");
          restoreAttr("iconsize");
          restoreAttr("context");

          el.removeAttribute("inFullscreen");
        }
      } else {
        // use moz-collapsed so it doesn't persist hidden/collapsed,
        // so that new windows don't have missing toolbars
        if (aShow)
          el.removeAttribute("moz-collapsed");
        else
          el.setAttribute("moz-collapsed", "true");
      }
    }

    if (aShow) {
      gNavToolbox.removeAttribute("inFullscreen");
      document.documentElement.removeAttribute("inFullscreen");
    } else {
      gNavToolbox.setAttribute("inFullscreen", true);
      document.documentElement.setAttribute("inFullscreen", true);
    }

    // In tabs-on-top mode, move window controls to the tab bar,
    // and in tabs-on-bottom mode, move them back to the navigation toolbar.
    // When there is a chance the tab bar may be collapsed, put window
    // controls on nav bar.
    var fullscreenctls = document.getElementById("window-controls");
    var navbar = document.getElementById("nav-bar");
    var ctlsOnTabbar = window.toolbar.visible &&
                        (navbar.collapsed || (TabsOnTop.enabled &&
                        !gPrefService.getBoolPref("browser.tabs.autoHide")));
    if (fullscreenctls.parentNode == navbar && ctlsOnTabbar) {
      fullscreenctls.removeAttribute("flex");
      document.getElementById("TabsToolbar").appendChild(fullscreenctls);
    }
    else if (fullscreenctls.parentNode.id == "TabsToolbar" && !ctlsOnTabbar) {
      fullscreenctls.setAttribute("flex", "1");
      navbar.appendChild(fullscreenctls);
    }
    fullscreenctls.hidden = aShow;
    
    ToolbarIconColor.inferFromText();
  }
};
XPCOMUtils.defineLazyGetter(FullScreen, "useLionFullScreen", function() {
  // We'll only use OS X Lion full screen if we're
  // * on OS X
  // * on Lion or higher (Darwin 11+)
  // * have fullscreenbutton="true"
#ifdef XP_MACOSX
  return parseFloat(Services.sysinfo.getProperty("version")) >= 11 &&
         document.documentElement.getAttribute("fullscreenbutton") == "true";
#else
  return false;
#endif
});
