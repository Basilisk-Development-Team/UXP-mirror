/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef mozilla_dom_cache_Types_h
#define mozilla_dom_cache_Types_h

#include <stdint.h>
#include "nsCOMPtr.h"
#include "nsIFile.h"
#include "nsString.h"

namespace mozilla {
namespace dom {
namespace cache {

enum Namespace
{
  DEFAULT_NAMESPACE,
  CHROME_ONLY_NAMESPACE,
  NUMBER_OF_NAMESPACES
};
static const Namespace INVALID_NAMESPACE = NUMBER_OF_NAMESPACES;

typedef int64_t CacheId;
static const CacheId INVALID_CACHE_ID = -1;

struct QuotaInfo
{
  nsCOMPtr<nsIFile> mDir;
  nsCString mSuffix;
  nsCString mGroup;
  nsCString mOrigin;
};

} // namespace cache
} // namespace dom
} // namespace mozilla

#endif // mozilla_dom_cache_Types_h
