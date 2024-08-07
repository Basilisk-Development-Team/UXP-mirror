/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef __FFmpegRuntimeLinker_h__
#define __FFmpegRuntimeLinker_h__

#include "PlatformDecoderModule.h"

namespace mozilla
{

class FFmpegRuntimeLinker
{
public:
  static bool Init();
  static already_AddRefed<PlatformDecoderModule> CreateDecoderModule();
  enum LinkStatus
  {
    LinkStatus_INIT = 0,  // Never been linked.
    LinkStatus_SUCCEEDED, // Found a usable library.
    // The following error statuses are sorted from most to least preferred
    // (i.e., if more than one happens, the top one is chosen.)
    LinkStatus_INVALID_FFMPEG_CANDIDATE, // Found ffmpeg with unexpected contents.
    LinkStatus_INVALID_CANDIDATE, // Found some lib with unexpected contents.
    LinkStatus_NOT_FOUND, // Haven't found any library with an expected name.
  };
  static LinkStatus LinkStatusCode() { return sLinkStatus; }
  static const char* LinkStatusString();
  // Library name to which the sLinkStatus applies, or "" if not applicable.
  static const char* LinkStatusLibraryName() { return sLinkStatusLibraryName; }

private:
  static LinkStatus sLinkStatus;
  static const char* sLinkStatusLibraryName;
};

}

#endif // __FFmpegRuntimeLinker_h__
