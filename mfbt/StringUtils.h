/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

/* Utilities for string handling. */

#ifndef mozilla_StringUtils_h
#define mozilla_StringUtils_h

#include <string>
#include <sstream>

namespace mozilla {

/**
 * A convenience function for converting an object to a string representation.
 * Supports any object which can be streamed to an std::ostream.
 */
template<typename T>
std::string
ToString(const T& aValue)
{
  std::ostringstream stream;
  stream << aValue;
  return stream.str();
}

/**
 * Special carveout for strnlen which isn't available for particularly old OS X.
 * This function is available as a standard function everywhere else.
 */

#ifdef __APPLE__
#include <AvailabilityMacros.h> // Might not be needed if already included elsewhere?

/* strnlen() is not available in userspace on Mac OS < 10.7 */
#if !defined(MAC_OS_X_VERSION_10_7) || (MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_7)

#define strnlen mcp_strnlen

static inline size_t mcp_strnlen(const char* string, size_t max_count) {
    const char* p = std::memchr(string, 0, max_count);
    return p ? p - string : max_count;
}

#endif /* < OS X 10.7 */
#endif /* __APPLE__ */

} // namespace mozilla

#endif /* mozilla_StringUtils_h */
