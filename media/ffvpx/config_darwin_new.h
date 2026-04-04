#pragma once

/*
 * Consolidated macOS ffvpx config.
 *
 * Include defaults first to provide 0-valued fallbacks for missing CONFIG_*
 * symbols, then include generated platform config so enabled features keep
 * their configured values.
 */

#include "defaults_disabled.h"
#include "config_darwin64.h"
#include "config_common.h"

/*
 * UXP macOS build policy: keep ffvpx software-only for H.264 paths.
 * Force these toggles off even if upstream-generated headers enable them.
 */
#undef CONFIG_H264_D3D11VA_HWACCEL
#define CONFIG_H264_D3D11VA_HWACCEL 0
#undef CONFIG_H264_DXVA2_HWACCEL
#define CONFIG_H264_DXVA2_HWACCEL 0
#undef CONFIG_H264_VAAPI_HWACCEL
#define CONFIG_H264_VAAPI_HWACCEL 0
#undef CONFIG_H264_VDA_HWACCEL
#define CONFIG_H264_VDA_HWACCEL 0
#undef CONFIG_H264_VIDEOTOOLBOX_HWACCEL
#define CONFIG_H264_VIDEOTOOLBOX_HWACCEL 0
#undef CONFIG_H264_VDPAU_HWACCEL
#define CONFIG_H264_VDPAU_HWACCEL 0
#undef CONFIG_H264_VDPAU_DECODER
#define CONFIG_H264_VDPAU_DECODER 0

/* Keep transform APIs enabled because avcodec.symbols exports them. */
#undef CONFIG_DCT
#define CONFIG_DCT 1
#undef CONFIG_MDCT
#define CONFIG_MDCT 1
#undef CONFIG_RDFT
#define CONFIG_RDFT 1
