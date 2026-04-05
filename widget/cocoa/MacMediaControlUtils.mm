/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#import <Cocoa/Cocoa.h>
#import <objc/message.h>
#include <dlfcn.h>

#include "MacMediaControlUtils.h"
#include "mozilla/Unused.h"
#include "nsCocoaUtils.h"

// This must be included last:
#include "nsObjCExceptions.h"

NS_IMPL_ISUPPORTS(MacMediaControlUtils, nsIMacMediaControl)

namespace {

static NSMutableDictionary* sNowPlayingInfo = nil;
static bool sTriedLoadMediaPlayerFramework = false;
static void* sMediaPlayerHandle = nullptr;

static void EnsureMediaPlayerFrameworkLoaded();

static NSString*
GetMediaPlayerStringConstant(const char* aSymbolName, NSString* aFallback)
{
  EnsureMediaPlayerFrameworkLoaded();

  if (!sMediaPlayerHandle) {
    sMediaPlayerHandle = dlopen("/System/Library/Frameworks/MediaPlayer.framework/MediaPlayer",
                                RTLD_LAZY | RTLD_NOLOAD);
    if (!sMediaPlayerHandle) {
      sMediaPlayerHandle = dlopen("/System/Library/Frameworks/MediaPlayer.framework/MediaPlayer",
                                  RTLD_LAZY);
    }
  }

  if (!sMediaPlayerHandle) {
    return aFallback;
  }

  NSString* const* key = static_cast<NSString* const*>(dlsym(sMediaPlayerHandle, aSymbolName));
  return (key && *key) ? *key : aFallback;
}

static NSString*
NowPlayingPlaybackRateKey()
{
  static NSString* key = GetMediaPlayerStringConstant("MPNowPlayingInfoPropertyPlaybackRate",
                                                       @"playbackRate");
  return key;
}

static NSString*
NowPlayingElapsedTimeKey()
{
  static NSString* key = GetMediaPlayerStringConstant("MPNowPlayingInfoPropertyElapsedPlaybackTime",
                                                       @"elapsedPlaybackTime");
  return key;
}

static NSString*
MediaItemTitleKey()
{
  static NSString* key = GetMediaPlayerStringConstant("MPMediaItemPropertyTitle",
                                                       @"title");
  return key;
}

static NSString*
MediaItemArtistKey()
{
  static NSString* key = GetMediaPlayerStringConstant("MPMediaItemPropertyArtist",
                                                       @"artist");
  return key;
}

static NSString*
MediaItemAlbumTitleKey()
{
  static NSString* key = GetMediaPlayerStringConstant("MPMediaItemPropertyAlbumTitle",
                                                       @"albumTitle");
  return key;
}

static void
EnsureMediaPlayerFrameworkLoaded()
{
  if (sTriedLoadMediaPlayerFramework) {
    return;
  }
  sTriedLoadMediaPlayerFramework = true;

  NSString* frameworkPath = @"/System/Library/Frameworks/MediaPlayer.framework";
  NSBundle* mediaPlayerBundle = [NSBundle bundleWithPath:frameworkPath];
  if (mediaPlayerBundle) {
    [mediaPlayerBundle load];
  }
}

static id
GetNowPlayingInfoCenter()
{
  EnsureMediaPlayerFrameworkLoaded();

  Class centerClass = NSClassFromString(@"MPNowPlayingInfoCenter");
  if (!centerClass) {
    return nil;
  }

  SEL defaultCenterSel = NSSelectorFromString(@"defaultCenter");
  if (![centerClass respondsToSelector:defaultCenterSel]) {
    return nil;
  }

  return ((id (*)(id, SEL))objc_msgSend)(centerClass, defaultCenterSel);
}

static void
ApplyNowPlayingInfo()
{
  id center = GetNowPlayingInfoCenter();
  if (!center) {
    return;
  }

  SEL setInfoSel = NSSelectorFromString(@"setNowPlayingInfo:");
  if (![center respondsToSelector:setInfoSel]) {
    return;
  }

  NSDictionary* info = (sNowPlayingInfo && [sNowPlayingInfo count] > 0)
                        ? [NSDictionary dictionaryWithDictionary:sNowPlayingInfo]
                        : nil;

  ((void (*)(id, SEL, id))objc_msgSend)(center, setInfoSel, info);
}

static void
EnsureNowPlayingInfo()
{
  if (!sNowPlayingInfo) {
    sNowPlayingInfo = [[NSMutableDictionary alloc] init];
  }
}

} // namespace

NS_IMETHODIMP
MacMediaControlUtils::SetSMTCPlaybackState(bool aPlaying)
{
  NS_OBJC_BEGIN_TRY_ABORT_BLOCK_NSRESULT;

  EnsureNowPlayingInfo();
  [sNowPlayingInfo setObject:[NSNumber numberWithDouble:(aPlaying ? 1.0 : 0.0)]
                      forKey:NowPlayingPlaybackRateKey()];
  [sNowPlayingInfo setObject:[NSNumber numberWithDouble:0.0]
                      forKey:NowPlayingElapsedTimeKey()];

  id center = GetNowPlayingInfoCenter();
  if (center) {
    SEL setPlaybackStateSel = NSSelectorFromString(@"setPlaybackState:");
    if ([center respondsToSelector:setPlaybackStateSel]) {
      // MPNowPlayingPlaybackState: 1 = playing, 2 = paused
      NSInteger state = aPlaying ? 1 : 2;
      ((void (*)(id, SEL, NSInteger))objc_msgSend)(center, setPlaybackStateSel, state);
    }
  }

  ApplyNowPlayingInfo();
  return NS_OK;

  NS_OBJC_END_TRY_ABORT_BLOCK_NSRESULT;
}

NS_IMETHODIMP
MacMediaControlUtils::SetSMTCMetadata(const nsAString& aTitle,
                                      const nsAString& aArtist,
                                      const nsAString& aAlbum)
{
  NS_OBJC_BEGIN_TRY_ABORT_BLOCK_NSRESULT;

  EnsureNowPlayingInfo();

  NSString* title = nsCocoaUtils::ToNSString(aTitle);
  NSString* artist = nsCocoaUtils::ToNSString(aArtist);
  NSString* album = nsCocoaUtils::ToNSString(aAlbum);
  NSString* fallbackTitle = [[NSProcessInfo processInfo] processName];

  if (title && [title length] > 0) {
    [sNowPlayingInfo setObject:title forKey:MediaItemTitleKey()];
  } else if (fallbackTitle && [fallbackTitle length] > 0) {
    [sNowPlayingInfo setObject:fallbackTitle forKey:MediaItemTitleKey()];
  } else {
    [sNowPlayingInfo removeObjectForKey:MediaItemTitleKey()];
  }

  if (artist && [artist length] > 0) {
    [sNowPlayingInfo setObject:artist forKey:MediaItemArtistKey()];
  } else {
    [sNowPlayingInfo removeObjectForKey:MediaItemArtistKey()];
  }

  if (album && [album length] > 0) {
    [sNowPlayingInfo setObject:album forKey:MediaItemAlbumTitleKey()];
  } else {
    [sNowPlayingInfo removeObjectForKey:MediaItemAlbumTitleKey()];
  }

  ApplyNowPlayingInfo();
  return NS_OK;

  NS_OBJC_END_TRY_ABORT_BLOCK_NSRESULT;
}

NS_IMETHODIMP
MacMediaControlUtils::SetSMTCThumbnailURL(const nsAString& aThumbnailURL)
{
  NS_OBJC_BEGIN_TRY_ABORT_BLOCK_NSRESULT;

  // Best effort: Artwork publishing requires MediaPlayer artwork classes that
  // vary by macOS version. Keep this no-op for compatibility.
  Unused << aThumbnailURL;
  return NS_OK;

  NS_OBJC_END_TRY_ABORT_BLOCK_NSRESULT;
}

NS_IMETHODIMP
MacMediaControlUtils::ClearSMTCMetadata()
{
  NS_OBJC_BEGIN_TRY_ABORT_BLOCK_NSRESULT;

  if (sNowPlayingInfo) {
    [sNowPlayingInfo removeAllObjects];
  }
  ApplyNowPlayingInfo();
  return NS_OK;

  NS_OBJC_END_TRY_ABORT_BLOCK_NSRESULT;
}
