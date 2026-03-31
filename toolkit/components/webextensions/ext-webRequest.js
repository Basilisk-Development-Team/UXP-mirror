"use strict";

var {classes: Cc, interfaces: Ci, utils: Cu} = Components;

Cu.import("resource://gre/modules/XPCOMUtils.jsm");

XPCOMUtils.defineLazyModuleGetter(this, "MatchPattern",
                                  "resource://gre/modules/MatchPattern.jsm");
XPCOMUtils.defineLazyModuleGetter(this, "WebRequest",
                                  "resource://gre/modules/WebRequest.jsm");

Cu.import("resource://gre/modules/ExtensionManagement.jsm");
Cu.import("resource://gre/modules/ExtensionUtils.jsm");
var {
  SingletonEventManager,
} = ExtensionUtils;

// EventManager-like class specifically for WebRequest. Inherits from
// SingletonEventManager. Takes care of converting |details| parameter
// when invoking listeners.
function WebRequestEventManager(context, eventName) {
  let name = `webRequest.${eventName}`;
  let register = (callback, filter, info) => {
    let listener = data => {
      // Prevent listening in on requests originating from system principal to
      // prevent tinkering with OCSP, app and addon updates, etc.
      if (data.isSystemPrincipal) {
        return;
      }

      let data2 = {
        requestId: data.requestId,
        url: data.url,
        originUrl: data.originUrl,
        method: data.method,
        type: data.type,
        timeStamp: Date.now(),
        frameId: data.type == "main_frame" ? 0 : ExtensionManagement.getFrameId(data.windowId),
        parentFrameId: ExtensionManagement.getParentFrameId(data.parentWindowId, data.windowId),
      };

      const maybeCached = ["onResponseStarted", "onBeforeRedirect", "onCompleted", "onErrorOccurred"];
      if (maybeCached.includes(eventName)) {
        data2.fromCache = !!data.fromCache;
      }

      if ("ip" in data) {
        data2.ip = data.ip;
      }

      extensions.emit("fill-browser-data", data.browser, data2);

      let optional = ["requestHeaders", "responseHeaders", "statusCode", "statusLine", "error", "redirectUrl",
                      "requestBody"];
      for (let opt of optional) {
        if (opt in data) {
          data2[opt] = data[opt];
        }
      }

      return context.runSafe(callback, data2);
    };

    let filter2 = {};
    filter2.urls = new MatchPattern(filter.urls);
    if (filter.types) {
      filter2.types = filter.types;
    }
    if (filter.tabId) {
      filter2.tabId = filter.tabId;
    }
    if (filter.windowId) {
      filter2.windowId = filter.windowId;
    }

    let info2 = [];
    if (info) {
      for (let desc of info) {
        if (desc == "blocking" && !context.extension.hasPermission("webRequestBlocking")) {
          Cu.reportError("Using webRequest.addListener with the blocking option " +
                         "requires the 'webRequestBlocking' permission.");
        } else {
          info2.push(desc);
        }
      }
    }

    WebRequest[eventName].addListener(listener, filter2, info2);
    return () => {
      WebRequest[eventName].removeListener(listener);
    };
  };

  return SingletonEventManager.call(this, context, name, register);
}

WebRequestEventManager.prototype = Object.create(SingletonEventManager.prototype);

// StreamFilter stub for response data filtering
// Modern extensions may use filterResponseData(), we provide a basic stub
function StreamFilterStub(requestId) {
  this.requestId = requestId;
  this.status = "unattached";
  this.onstart = null;
  this.ondata = null;
  this.onstop = null;
  this.onerror = null;
}

StreamFilterStub.prototype = {
  write: function(data) {
    // Stub: no-op - just accept the data without modification
  },
  close: function() {
    this.status = "closed";
    if (this.onstop) {
      try {
        this.onstop();
      } catch (e) {}
    }
  },
  error: function(error) {
    this.status = "errored";
    if (this.onerror) {
      try {
        this.onerror(error);
      } catch (e) {}
    }
  },
  disconnect: function() {
    this.status = "closed";
  }
};

extensions.registerSchemaAPI("webRequest", "addon_parent", context => {
  return {
    webRequest: {
      onBeforeRequest: new WebRequestEventManager(context, "onBeforeRequest").api(),
      onBeforeSendHeaders: new WebRequestEventManager(context, "onBeforeSendHeaders").api(),
      onSendHeaders: new WebRequestEventManager(context, "onSendHeaders").api(),
      onHeadersReceived: new WebRequestEventManager(context, "onHeadersReceived").api(),
      onBeforeRedirect: new WebRequestEventManager(context, "onBeforeRedirect").api(),
      onResponseStarted: new WebRequestEventManager(context, "onResponseStarted").api(),
      onErrorOccurred: new WebRequestEventManager(context, "onErrorOccurred").api(),
      onCompleted: new WebRequestEventManager(context, "onCompleted").api(),
      handlerBehaviorChanged: function() {
        // TODO: Flush all caches.
      },
      // Resource type constants for feature detection and filtering
      ResourceType: Object.freeze({
        MAIN_FRAME: "main_frame",
        SUB_FRAME: "sub_frame",
        STYLESHEET: "stylesheet",
        SCRIPT: "script",
        IMAGE: "image",
        OBJECT: "object",
        XMLHTTPREQUEST: "xmlhttprequest",
        XBL: "xbl",
        XSLT: "xslt",
        PING: "ping",
        BEACON: "beacon",
        XML_DTD: "xml_dtd",
        FONT: "font",
        MEDIA: "media",
        WEBSOCKET: "websocket",
        CSP_REPORT: "csp_report",
        IMAGESET: "imageset",
        WEB_MANIFEST: "web_manifest",
        OTHER: "other"
      }),
      // Stub implementation of filterResponseData for extensions that need it
      // Returns a StreamFilter-like object that extensions can use to intercept responses
      filterResponseData: function(requestId) {
        return new StreamFilterStub(requestId);
      },
      // Maximum handler behavior change calls per 10 minutes (per MDN spec)
      MAX_HANDLER_BEHAVIOR_CHANGED_CALLS_PER_10_MINUTES: 20,
    },
  };
});
