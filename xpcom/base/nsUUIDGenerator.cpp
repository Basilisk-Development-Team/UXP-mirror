/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#if defined(XP_WIN)
#include <stdlib.h>
#include <windows.h>
#include <objbase.h>
#elif defined(XP_MACOSX)
#include <CoreFoundation/CoreFoundation.h>
#else
#include <stdlib.h>
#include "prrng.h"
#endif

#include "nsUUIDGenerator.h"

using namespace mozilla;

NS_IMPL_ISUPPORTS(nsUUIDGenerator, nsIUUIDGenerator)

nsUUIDGenerator::nsUUIDGenerator()
  : mLock("nsUUIDGenerator.mLock")
{
}

nsUUIDGenerator::~nsUUIDGenerator()
{
}

nsresult
nsUUIDGenerator::Init()
{
  // We're a service, so we're guaranteed that Init() is not going
  // to be reentered while we're inside Init().

#if !defined(XP_WIN) && !defined(XP_MACOSX) && !defined(HAVE_ARC4RANDOM)
  /* initialize random number generator using NSPR random noise */
  unsigned int seed;

  size_t bytes = 0;
  while (bytes < sizeof(seed)) {
    size_t nbytes = PR_GetRandomNoise(((unsigned char*)&seed) + bytes,
                                      sizeof(seed) - bytes);
    if (nbytes == 0) {
      return NS_ERROR_FAILURE;
    }
    bytes += nbytes;
  }

  /* Initialize a new RNG state, and immediately switch
   * back to the previous one -- we want to use mState
   * only for our own calls to random().
   */
  mSavedState = initstate(seed, mState, sizeof(mState));
  setstate(mSavedState);

  mRBytes = 4;
#ifdef RAND_MAX
  if ((unsigned long)RAND_MAX < 0xffffffffUL) {
    mRBytes = 3;
  }
  if ((unsigned long)RAND_MAX < 0x00ffffffUL) {
    mRBytes = 2;
  }
  if ((unsigned long)RAND_MAX < 0x0000ffffUL) {
    mRBytes = 1;
  }
  if ((unsigned long)RAND_MAX < 0x000000ffUL) {
    return NS_ERROR_FAILURE;
  }
#endif

#endif /* non XP_WIN and non XP_MACOSX and non ARC4RANDOM */

  return NS_OK;
}

NS_IMETHODIMP
nsUUIDGenerator::GenerateUUID(nsID** aRet)
{
  nsID* id = static_cast<nsID*>(moz_xmalloc(sizeof(nsID)));
  if (!id) {
    return NS_ERROR_OUT_OF_MEMORY;
  }

  nsresult rv = GenerateUUIDInPlace(id);
  if (NS_FAILED(rv)) {
    free(id);
    return rv;
  }

  *aRet = id;
  return rv;
}

NS_IMETHODIMP
nsUUIDGenerator::GenerateUUIDInPlace(nsID* aId)
{
  // The various code in this method is probably not threadsafe, so lock
  // across the whole method.
  MutexAutoLock lock(mLock);

#if defined(XP_WIN)
  HRESULT hr = CoCreateGuid((GUID*)aId);
  if (FAILED(hr)) {
    return NS_ERROR_FAILURE;
  }
#elif defined(XP_MACOSX)
  CFUUIDRef uuid = CFUUIDCreate(kCFAllocatorDefault);
  if (!uuid) {
    return NS_ERROR_FAILURE;
  }

  CFUUIDBytes bytes = CFUUIDGetUUIDBytes(uuid);
  memcpy(aId, &bytes, sizeof(nsID));

  CFRelease(uuid);
#else /* not windows or OS X; generate randomness using random(). */
  /* XXX we should be saving the return of setstate here and switching
   * back to it; instead, we use the value returned when we called
   * initstate, since older glibc's have broken setstate() return values
   */
#ifndef HAVE_ARC4RANDOM
  setstate(mState);
#endif

#if defined(__linux__)
  // Linux does not provide arc4random_buf or arc4random.
  // Use /dev/urandom for secure UUID generation.
  int fd = open("/dev/urandom", O_RDONLY);
  if (fd >= 0) {
    ssize_t n = read(fd, aId, sizeof(nsID));
    close(fd);
    if (n == sizeof(nsID)) {
      return NS_OK;
    }
  }

  // Fallback if /dev/urandom fails
  size_t bytesLeft = sizeof(nsID);
  while (bytesLeft > 0) {
    long rval = random();
    const size_t mRBytes = 4;
    size_t toCopy = std::min(bytesLeft, mRBytes);
    memcpy(reinterpret_cast<char*>(aId) + (sizeof(nsID) - bytesLeft),
           &rval, toCopy);
    bytesLeft -= toCopy;
  }

#else

#ifdef HAVE_ARC4RANDOM_BUF
  arc4random_buf(aId, sizeof(nsID));
#else /* HAVE_ARC4RANDOM_BUF */
  size_t bytesLeft = sizeof(nsID);
  while (bytesLeft > 0) {
#ifdef HAVE_ARC4RANDOM
    long rval = arc4random();
    const size_t mRBytes = 4;
#else
    long rval = random();
    const size_t mRBytes = 4;
#endif
    size_t toCopy = std::min(bytesLeft, mRBytes);
    memcpy(reinterpret_cast<char*>(aId) + (sizeof(nsID) - bytesLeft),
           &rval, toCopy);
    bytesLeft -= toCopy;
  }
#endif /* HAVE_ARC4RANDOM_BUF */

    uint8_t* src = (uint8_t*)&rval;
    // We want to grab the mRBytes least significant bytes of rval, since
    // mRBytes less than sizeof(rval) means the high bytes are 0.
#ifdef IS_BIG_ENDIAN
    src += sizeof(rval) - mRBytes;
#endif
    uint8_t* dst = ((uint8_t*)aId) + (sizeof(nsID) - bytesLeft);
    size_t toWrite = (bytesLeft < mRBytes ? bytesLeft : mRBytes);
    for (size_t i = 0; i < toWrite; i++) {
      dst[i] = src[i];
    }

    bytesLeft -= toWrite;
  }
#endif /* HAVE_ARC4RANDOM_BUF */

  /* Put in the version */
  aId->m2 &= 0x0fff;
  aId->m2 |= 0x4000;

  /* Put in the variant */
  aId->m3[0] &= 0x3f;
  aId->m3[0] |= 0x80;

#ifndef HAVE_ARC4RANDOM
  /* Restore the previous RNG state */
  setstate(mSavedState);
#endif
#endif

  return NS_OK;
}
