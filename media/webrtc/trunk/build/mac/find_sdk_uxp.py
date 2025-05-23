#!/usr/bin/env python
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

from __future__ import print_function
import os
import re
import sys

if sys.platform == 'darwin':
  if len(sys.argv) <= 1:
    print("find_sdk_uxp.py: error: Not enough arguments")
    sys.exit(1)
  else:
    if os.path.isdir(sys.argv[1]):
      SDK_PATH = sys.argv[1]
    else:
      print("find_sdk_uxp.py: error: Specified path does not exist or is not a directory")
      sys.exit(1)

  KNOWN_SDK_VERSIONS = ["10.5", "10.6", "10.7", "10.8", "10.9", "10.10",
                        "10.11", "10.12", "10.13", "10.14", "10.15", "10.16",
                        "11.0", "11.1", "11.2", "11.3", "12.0", "12.1", "12.3",
                        "13.0", "13.1", "13.2", "14.0", "14.2", "14.4", "14.5",
                        "15.0", "15.1", "15.2", "15.4"]

  REGEX = "^MacOSX(\d+\.\d+)\.sdk$"
  SDK_VERSION = re.findall(REGEX, os.path.basename(SDK_PATH))

  if not SDK_VERSION:
    print("find_sdk_uxp.py: error: Could not determin the MacOS X SDK version")
    sys.exit(1)

  if SDK_VERSION[0] in KNOWN_SDK_VERSIONS:
    print(SDK_VERSION[0])
  else:
    print("find_sdk_uxp.py: error: Unknown MacOS X SDK version")

sys.exit(0) 
