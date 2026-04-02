/* Dark Mode Theme Manager
 * Handles OS dark mode detection and manual theme toggle
 */

var ThemeManager = window.ThemeManager || {};

ThemeManager.getPrefBranch = function() {
  if (!this.prefBranch) {
    this.prefBranch = Components.classes["@mozilla.org/preferences-service;1"]
      .getService(Components.interfaces.nsIPrefService)
      .getBranch("browser.theme.");
  }

  return this.prefBranch;
};

ThemeManager.getGlobalPrefBranch = function() {
  if (!this.globalPrefBranch) {
    this.globalPrefBranch = Components.classes["@mozilla.org/preferences-service;1"]
      .getService(Components.interfaces.nsIPrefService)
      .getBranch("");
  }

  return this.globalPrefBranch;
};

ThemeManager.init = function() {
  this.getPrefBranch();

  // Check stored preference first, then OS preference.
  this.applyTheme();

  // Listen for preference and system theme changes.
  this.setupPrefObserver();
  this.setupMediaQueryListener();
};

ThemeManager.setupPrefObserver = function() {
  if (!this.prefObserverRegistered) {
    this.getPrefBranch().addObserver("", this, false);
    this.prefObserverRegistered = true;
  }
};

ThemeManager.observe = function(subject, topic, data) {
  // browser.theme.mode changes are intentionally applied on next startup
  // so the preferences UI setting is restart-required.
  if (topic === "nsPref:changed" && data === "mode") {
    return;
  }
};

ThemeManager.shouldFollowSystem = function() {
  try {
    return this.getPrefBranch().getCharPref("mode") === "system";
  } catch (e) {
    return true;
  }
};

ThemeManager.readWindowsThemeValue = function(rootKey, valueName) {
  var regKey = Components.classes["@mozilla.org/windows-registry-key;1"]
    .createInstance(Components.interfaces.nsIWindowsRegKey);
  var winReg = Components.interfaces.nsIWindowsRegKey;
  var personalizePath = "Software\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize";

  try {
    regKey.open(rootKey, personalizePath, winReg.ACCESS_READ);
    if (regKey.hasValue(valueName)) {
      return regKey.readIntValue(valueName);
    }
  } catch (e) {
    // Ignore and return null below.
  }

  try {
    regKey.close();
  } catch (e2) {
  }

  return null;
};

ThemeManager.isSystemDarkModePreferred = function() {
  // Prefer Windows registry signal when available.
  try {
    var winReg = Components.interfaces.nsIWindowsRegKey;
    var appsUseLight = this.readWindowsThemeValue(winReg.ROOT_KEY_CURRENT_USER, "AppsUseLightTheme");
    var systemUseLight = this.readWindowsThemeValue(winReg.ROOT_KEY_CURRENT_USER, "SystemUsesLightTheme");

    // Some setups expose values under HKLM only.
    if (appsUseLight === null) {
      appsUseLight = this.readWindowsThemeValue(winReg.ROOT_KEY_LOCAL_MACHINE, "AppsUseLightTheme");
    }
    if (systemUseLight === null) {
      systemUseLight = this.readWindowsThemeValue(winReg.ROOT_KEY_LOCAL_MACHINE, "SystemUsesLightTheme");
    }

    // Treat either key requesting dark as dark mode.
    if (appsUseLight !== null || systemUseLight !== null) {
      return appsUseLight === 0 || systemUseLight === 0;
    }
  } catch (e) {
    // Fall through to standards-based media query.
  }

  // Fallback for non-Windows or unavailable registry signal.
  if (window.matchMedia) {
    var mediaQuery = window.matchMedia("(prefers-color-scheme: dark)");
    if (mediaQuery.media !== "not all") {
      return mediaQuery.matches;
    }
  }

  return false;
};

ThemeManager.setupMediaQueryListener = function() {
    // Listen for OS dark mode changes if supported
    if (window.matchMedia && window.matchMedia("(prefers-color-scheme: dark)").media !== "not all") {
      var darkModeQuery = window.matchMedia("(prefers-color-scheme: dark)");
      darkModeQuery.addListener(function(e) {
        // Apply if following system mode (or no explicit pref).
        if (this.shouldFollowSystem() || !this.hasUserPreference()) {
          this.setTheme(e.matches ? "dark" : "light");
        }
      }.bind(this));
    }
};

ThemeManager.hasUserPreference = function() {
    try {
      var pref = this.getPrefBranch().getCharPref("mode");
      return pref !== "";
    } catch (e) {
      return false;
    }
};

ThemeManager.applyTheme = function() {
  var theme = "light";
    
    // Check user preference first
    try {
      var userPref = this.getPrefBranch().getCharPref("mode");
      if (userPref === "dark" || userPref === "light") {
        theme = userPref;
      } else if (userPref === "system") {
        theme = this.isSystemDarkModePreferred() ? "dark" : "light";
      }
    } catch (e) {
      // Preference doesn't exist, check OS.
      theme = this.isSystemDarkModePreferred() ? "dark" : "light";
    }
    
    this.setTheme(theme);
};

ThemeManager.setTheme = function(theme) {
  var docElement = document.documentElement;
    
    if (theme === "dark") {
      docElement.setAttribute("data-theme", "dark");
    } else {
      docElement.removeAttribute("data-theme");
    }

    try {
      var globalPrefs = this.getGlobalPrefBranch();
      var mode = "system";
      var prefersColorScheme = 3;

      try {
        mode = this.getPrefBranch().getCharPref("mode");
      } catch (e2) {
      }

      if (mode === "dark") {
        prefersColorScheme = 2;
      } else if (mode === "light") {
        prefersColorScheme = 1;
      }

      globalPrefs.setIntPref("browser.display.prefers_color_scheme", prefersColorScheme);
      globalPrefs.setIntPref("ui.color_scheme", theme === "dark" ? 2 : 1);
      globalPrefs.setBoolPref("mozilla.widget.disable-native-theme", false);
    } catch (e) {
      // Ignore pref write failures and keep the chrome theme applied.
    }
};

// Public method to toggle theme
ThemeManager.toggleDarkMode = function() {
  var docElement = document.documentElement;
  var isDark = docElement.getAttribute("data-theme") === "dark";
    
    try {
      this.getPrefBranch().setCharPref("mode", !isDark ? "dark" : "light");
    } catch (e) {
      console.error("Failed to set theme preference:", e);
    }
    
    this.setTheme(!isDark ? "dark" : "light");
};

// Get current theme mode
ThemeManager.getTheme = function() {
    var docElement = document.documentElement;
    return docElement.getAttribute("data-theme") === "dark" ? "dark" : "light";
};

window.ThemeManager = ThemeManager;
this.ThemeManager = ThemeManager;
if (typeof top != "undefined") {
  top.ThemeManager = ThemeManager;
}
window.toggleDarkMode = function() {
  return ThemeManager.toggleDarkMode();
};
this.toggleDarkMode = window.toggleDarkMode;
if (typeof top != "undefined") {
  top.toggleDarkMode = window.toggleDarkMode;
}

// Initialize when document is ready
if (document.readyState === "interactive" || document.readyState === "complete") {
  ThemeManager.init();
} else {
  document.addEventListener("DOMContentLoaded", function() {
    ThemeManager.init();
  });
}
