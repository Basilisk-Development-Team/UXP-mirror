/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#include "BufferDecoder.h"

#include "nsISupports.h"
#include "MediaResource.h"
#ifdef MOZ_GMP
#include "GMPService.h"
#endif

namespace mozilla {

NS_IMPL_ISUPPORTS0(BufferDecoder)

#ifdef MOZ_GMP
BufferDecoder::BufferDecoder(MediaResource* aResource, GMPCrashHelper* aCrashHelper)
#else
BufferDecoder::BufferDecoder(MediaResource* aResource)
#endif
  : mResource(aResource)
#ifdef MOZ_GMP
  , mCrashHelper(aCrashHelper)
#endif
{
  MOZ_ASSERT(NS_IsMainThread());
  MOZ_COUNT_CTOR(BufferDecoder);
}

BufferDecoder::~BufferDecoder()
{
  // The dtor may run on any thread, we cannot be sure.
  MOZ_COUNT_DTOR(BufferDecoder);
}

void
BufferDecoder::BeginDecoding(TaskQueue* aTaskQueueIdentity)
{
  MOZ_ASSERT(!mTaskQueueIdentity && aTaskQueueIdentity);
  mTaskQueueIdentity = aTaskQueueIdentity;
}

MediaResource*
BufferDecoder::GetResource() const
{
  return mResource;
}

void
BufferDecoder::NotifyDecodedFrames(const FrameStatisticsData& aStats)
{
  // ignore
}

VideoFrameContainer*
BufferDecoder::GetVideoFrameContainer()
{
  // no video frame
  return nullptr;
}

layers::ImageContainer*
BufferDecoder::GetImageContainer()
{
  // no image container
  return nullptr;
}

MediaDecoderOwner*
BufferDecoder::GetOwner() const
{
  // unknown
  return nullptr;
}

#ifdef MOZ_GMP
already_AddRefed<GMPCrashHelper>
BufferDecoder::GetCrashHelper()
{
  return do_AddRef(mCrashHelper);
}
#endif

} // namespace mozilla
