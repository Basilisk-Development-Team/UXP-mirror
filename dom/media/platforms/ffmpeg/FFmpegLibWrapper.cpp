/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#include "FFmpegLibWrapper.h"
#include "FFmpegLog.h"
#include "MediaPrefs.h"
#include "mozilla/PodOperations.h"
#include "mozilla/Types.h"
#include "PlatformDecoderModule.h"
#include "prlink.h"

#define AV_LOG_WARNING 24

namespace mozilla
{

FFmpegLibWrapper::FFmpegLibWrapper()
{
  PodZero(this);
}

FFmpegLibWrapper::~FFmpegLibWrapper()
{
  Unlink();
}

FFmpegLibWrapper::LinkResult
FFmpegLibWrapper::Link()
{
  if (!mAVCodecLib || !mAVUtilLib) {
    Unlink();
    return LinkResult::NoProvidedLib;
  }

  avcodec_version =
    (decltype(avcodec_version))PR_FindSymbol(mAVCodecLib, "avcodec_version");
  if (!avcodec_version) {
    Unlink();
    return LinkResult::NoAVCodecVersion;
  }
  uint32_t version = avcodec_version();
  uint32_t macro = (version >> 16) & 0xFFu;
  mVersion = static_cast<int>(macro);
  uint32_t micro = version & 0xFFu;

  enum {
    AV_FUNC_AVUTIL_MASK = 1 << 8,
    AV_FUNC_58 = 1 << 0,
    AV_FUNC_59 = 1 << 1,
    AV_FUNC_60 = 1 << 2,
    AV_FUNC_AVUTIL_58 = AV_FUNC_58 | AV_FUNC_AVUTIL_MASK,
    AV_FUNC_AVUTIL_59 = AV_FUNC_59 | AV_FUNC_AVUTIL_MASK,
    AV_FUNC_AVUTIL_60 = AV_FUNC_60 | AV_FUNC_AVUTIL_MASK,
    AV_FUNC_AVCODEC_ALL = AV_FUNC_58 | AV_FUNC_59 | AV_FUNC_60,
    AV_FUNC_AVUTIL_ALL = AV_FUNC_AVCODEC_ALL | AV_FUNC_AVUTIL_MASK
  };

  switch (macro) {
    case 58:
      version = AV_FUNC_58;
      break;
    case 59:
      version = AV_FUNC_59;
      break;
    case 60:
      version = AV_FUNC_60;
      break;
    default:
      FFMPEG_LOG("Unknown avcodec version: %d", macro);
      Unlink();
      return LinkResult::UnknownFFMpegVersion;
  }

#define AV_FUNC_OPTION(func, ver)                                              \
  if ((ver) & version) {                                                       \
    if (!(func = (decltype(func))PR_FindSymbol(((ver) & AV_FUNC_AVUTIL_MASK) ? mAVUtilLib : mAVCodecLib, #func))) { \
      FFMPEG_LOG("Couldn't load function " # func);                            \
    }                                                                          \
  } else {                                                                     \
    func = (decltype(func))nullptr;                                            \
  }

#define AV_FUNC(func, ver)                                                     \
  AV_FUNC_OPTION(func, ver)                                                    \
  if ((ver) & version && !func) {                                              \
    Unlink();                                                                  \
    return LinkResult::MissingFFMpegFunction;                                  \
  }

  AV_FUNC(av_lockmgr_register, AV_FUNC_58)
  AV_FUNC(avcodec_alloc_context3, AV_FUNC_AVCODEC_ALL)
  AV_FUNC(avcodec_close, AV_FUNC_AVCODEC_ALL)
  AV_FUNC(avcodec_decode_audio4, AV_FUNC_58)
  AV_FUNC(avcodec_decode_video2, AV_FUNC_58)
  AV_FUNC(avcodec_find_decoder, AV_FUNC_AVCODEC_ALL)
  AV_FUNC(avcodec_flush_buffers, AV_FUNC_AVCODEC_ALL)
  AV_FUNC(avcodec_open2, AV_FUNC_AVCODEC_ALL)
  AV_FUNC(avcodec_register_all, AV_FUNC_58)
  AV_FUNC(av_init_packet, AV_FUNC_AVCODEC_ALL)
  AV_FUNC(av_parser_init, AV_FUNC_AVCODEC_ALL)
  AV_FUNC(av_parser_close, AV_FUNC_AVCODEC_ALL)
  AV_FUNC(av_parser_parse2, AV_FUNC_AVCODEC_ALL)
  AV_FUNC(avcodec_send_packet, AV_FUNC_AVCODEC_ALL)
  AV_FUNC(avcodec_receive_frame, AV_FUNC_AVCODEC_ALL)
  AV_FUNC(av_log_set_level, AV_FUNC_AVUTIL_ALL)
  AV_FUNC(av_malloc, AV_FUNC_AVUTIL_ALL)
  AV_FUNC(av_freep, AV_FUNC_AVUTIL_ALL)
  AV_FUNC(av_frame_alloc, AV_FUNC_AVUTIL_ALL)
  AV_FUNC(av_frame_free, AV_FUNC_AVUTIL_ALL)
  AV_FUNC(av_frame_unref, AV_FUNC_AVUTIL_ALL)
  AV_FUNC_OPTION(av_frame_get_colorspace, AV_FUNC_AVUTIL_58)
  AV_FUNC_OPTION(av_frame_get_color_range, AV_FUNC_AVUTIL_58)
  AV_FUNC_OPTION(av_tx_init, AV_FUNC_AVUTIL_ALL)
  AV_FUNC_OPTION(av_tx_uninit, AV_FUNC_AVUTIL_ALL)
#undef AV_FUNC
#undef AV_FUNC_OPTION

  if (avcodec_register_all) {
    avcodec_register_all();
  }
#ifdef DEBUG
  av_log_set_level(AV_LOG_WARNING);
#endif

  return LinkResult::Success;
}

void
FFmpegLibWrapper::Unlink()
{
  if (av_lockmgr_register) {
    // Registering a null lockmgr cause the destruction of libav* global mutexes
    // as the default lockmgr that allocated them will be deregistered.
    // This prevents ASAN and valgrind to report sizeof(pthread_mutex_t) leaks.
    av_lockmgr_register(nullptr);
  }
  if (mAVUtilLib && mAVUtilLib != mAVCodecLib) {
    PR_UnloadLibrary(mAVUtilLib);
  }
  if (mAVCodecLib) {
    PR_UnloadLibrary(mAVCodecLib);
  }
  PodZero(this);
}

} // namespace mozilla
