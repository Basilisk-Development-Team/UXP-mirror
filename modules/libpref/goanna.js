#include ../../netwerk/base/security-prefs.js
#include init/all.js
<<<<<<< HEAD
=======
#ifdef MOZ_DATA_REPORTING
#include ../../toolkit/components/telemetry/datareporting-prefs.js
#endif
#ifdef MOZ_SERVICES_HEALTHREPORT
#if MOZ_WIDGET_TOOLKIT == android
#include ../../mobile/android/chrome/content/healthreport-prefs.js
#else
#include ../../toolkit/components/telemetry/healthreport-prefs.js
#endif
#endif
>>>>>>> parent of b21588597c (Issue #1053 - Part 1c: Remove references to mobile/android targets and paths)
