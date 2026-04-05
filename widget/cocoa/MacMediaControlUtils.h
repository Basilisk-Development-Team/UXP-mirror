/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef mozilla_widget_MacMediaControlUtils_h__
#define mozilla_widget_MacMediaControlUtils_h__

#include "nsIMacMediaControl.h"

#define NS_MAC_MEDIACONTROL_CONTRACTID "@mozilla.org/mac-media-control;1"

class MacMediaControlUtils final : public nsIMacMediaControl {
public:
  NS_DECL_ISUPPORTS
  NS_DECL_NSIMACMEDIACONTROL

  MacMediaControlUtils() = default;

private:
  ~MacMediaControlUtils() = default;
};

#endif // mozilla_widget_MacMediaControlUtils_h__
