/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#include "mozilla/TaskQueue.h"

#include "FFmpegAudioDecoder.h"
#include "FFmpegLog.h"
#include "TimeUnits.h"

#define MAX_CHANNELS 16

namespace mozilla
{

FFmpegAudioDecoder<LIBAV_VER>::FFmpegAudioDecoder(FFmpegLibWrapper* aLib,
  TaskQueue* aTaskQueue, MediaDataDecoderCallback* aCallback,
  const AudioInfo& aConfig)
  : FFmpegDataDecoder(aLib, aTaskQueue, aCallback, GetCodecId(aConfig.mMimeType))
{
  MOZ_COUNT_CTOR(FFmpegAudioDecoder);
  // Use a new MediaByteBuffer as the object will be modified during initialization.
  if (aConfig.mCodecSpecificConfig && aConfig.mCodecSpecificConfig->Length()) {
    mExtraData = new MediaByteBuffer;
    mExtraData->AppendElements(*aConfig.mCodecSpecificConfig);
  }
}

RefPtr<MediaDataDecoder::InitPromise>
FFmpegAudioDecoder<LIBAV_VER>::Init()
{
  nsresult rv = InitDecoder();

  return rv == NS_OK ? InitPromise::CreateAndResolve(TrackInfo::kAudioTrack, __func__)
                     : InitPromise::CreateAndReject(NS_ERROR_DOM_MEDIA_FATAL_ERR, __func__);
}

void
FFmpegAudioDecoder<LIBAV_VER>::InitCodecContext()
{
  MOZ_ASSERT(mCodecContext);
  // We do not want to set this value to 0 as FFmpeg by default will
  // use the number of cores, which with our mozlibavutil get_cpu_count
  // isn't implemented.
  mCodecContext->thread_count = 1;
  // FFmpeg takes this as a suggestion for what format to use for audio samples.
  mCodecContext->request_sample_fmt = AV_SAMPLE_FMT_FLT;
}

static AlignedAudioBuffer
CopyAndPackAudio(AVFrame* aFrame, uint32_t aNumChannels, uint32_t aNumAFrames)
{
  MOZ_ASSERT(aNumChannels <= MAX_CHANNELS);

  AlignedAudioBuffer audio(aNumChannels * aNumAFrames);
  if (!audio) {
    return audio;
  }

  if (aFrame->format == AV_SAMPLE_FMT_FLT) {
    // Audio data already packed. No need to do anything other than copy it
    // into a buffer we own.
    memcpy(audio.get(), aFrame->data[0],
           aNumChannels * aNumAFrames * sizeof(AudioDataValue));
  } else if (aFrame->format == AV_SAMPLE_FMT_FLTP) {
    // Planar audio data. Pack it into something we can understand.
    AudioDataValue* tmp = audio.get();
    AudioDataValue** data = reinterpret_cast<AudioDataValue**>(aFrame->data);
    for (uint32_t frame = 0; frame < aNumAFrames; frame++) {
      for (uint32_t channel = 0; channel < aNumChannels; channel++) {
        *tmp++ = data[channel][frame];
      }
    }
  } else if (aFrame->format == AV_SAMPLE_FMT_S16) {
    // Audio data already packed. Need to convert from S16 to 32 bits Float
    AudioDataValue* tmp = audio.get();
    int16_t* data = reinterpret_cast<int16_t**>(aFrame->data)[0];
    for (uint32_t frame = 0; frame < aNumAFrames; frame++) {
      for (uint32_t channel = 0; channel < aNumChannels; channel++) {
        *tmp++ = AudioSampleToFloat(*data++);
      }
    }
  } else if (aFrame->format == AV_SAMPLE_FMT_S16P) {
    // Planar audio data. Convert it from S16 to 32 bits float
    // and pack it into something we can understand.
    AudioDataValue* tmp = audio.get();
    int16_t** data = reinterpret_cast<int16_t**>(aFrame->data);
    for (uint32_t frame = 0; frame < aNumAFrames; frame++) {
      for (uint32_t channel = 0; channel < aNumChannels; channel++) {
        *tmp++ = AudioSampleToFloat(data[channel][frame]);
      }
    }
  } else if (aFrame->format == AV_SAMPLE_FMT_S32) {
    // Audio data already packed. Need to convert from S16 to 32 bits Float
    AudioDataValue* tmp = audio.get();
    int32_t* data = reinterpret_cast<int32_t**>(aFrame->data)[0];
    for (uint32_t frame = 0; frame < aNumAFrames; frame++) {
      for (uint32_t channel = 0; channel < aNumChannels; channel++) {
        *tmp++ = AudioSampleToFloat(*data++);
      }
    }
  } else if (aFrame->format == AV_SAMPLE_FMT_S32P) {
    // Planar audio data. Convert it from S32 to 32 bits float
    // and pack it into something we can understand.
    AudioDataValue* tmp = audio.get();
    int32_t** data = reinterpret_cast<int32_t**>(aFrame->data);
    for (uint32_t frame = 0; frame < aNumAFrames; frame++) {
      for (uint32_t channel = 0; channel < aNumChannels; channel++) {
        *tmp++ = AudioSampleToFloat(data[channel][frame]);
      }
    }
  }

  return audio;
}

MediaResult
FFmpegAudioDecoder<LIBAV_VER>::DoDecode(MediaRawData* aSample)
{
  AVPacket packet;
  mLib->av_init_packet(&packet);

  packet.data = const_cast<uint8_t*>(aSample->Data());
  packet.size = aSample->Size();

  if (!PrepareFrame()) {
    return MediaResult(
      NS_ERROR_OUT_OF_MEMORY,
      RESULT_DETAIL("FFmpeg audio decoder failed to allocate frame"));
  }

  int64_t samplePosition = aSample->mOffset;
  media::TimeUnit pts = media::TimeUnit::FromMicroseconds(aSample->mTime);

  while (packet.size > 0) {
    int decoded = false;
    int bytesConsumed = -1;
#if LIBAVCODEC_VERSION_MAJOR == 58
	bytesConsumed =
      mLib->avcodec_decode_audio4(mCodecContext, mFrame, &decoded, &packet);

    if (bytesConsumed < 0) {
      NS_WARNING("FFmpeg audio decoder error.");
      return MediaResult(NS_ERROR_DOM_MEDIA_DECODE_ERR,
                         RESULT_DETAIL("FFmpeg audio error:%d", bytesConsumed));
    }
#else
#define AVRESULT_OK 0
    int ret = mLib->avcodec_send_packet(mCodecContext, &packet);
    switch (ret) {
      case AVRESULT_OK:
        bytesConsumed = packet.size;
        break;
      case AVERROR(EAGAIN):
        break;
      case AVERROR_EOF:
        FFMPEG_LOG("End of stream.");
        return MediaResult(NS_ERROR_DOM_MEDIA_END_OF_STREAM,
                           RESULT_DETAIL("End of stream"));
      default:
        NS_WARNING("FFmpeg audio decoder error.");
        return MediaResult(NS_ERROR_DOM_MEDIA_DECODE_ERR,
                           RESULT_DETAIL("FFmpeg audio error"));
    }

    ret = mLib->avcodec_receive_frame(mCodecContext, mFrame);
    switch (ret) {
      case AVRESULT_OK:
        decoded = true;
        break;
      case AVERROR(EAGAIN):
        break;
      case AVERROR_EOF: {
        FFMPEG_LOG("End of stream.");
        return MediaResult(NS_ERROR_DOM_MEDIA_END_OF_STREAM,
                           RESULT_DETAIL("End of stream"));
      }
    }
#endif

    if (decoded) {
      if (mFrame->format != AV_SAMPLE_FMT_FLT &&
          mFrame->format != AV_SAMPLE_FMT_FLTP &&
          mFrame->format != AV_SAMPLE_FMT_S16 &&
          mFrame->format != AV_SAMPLE_FMT_S16P &&
          mFrame->format != AV_SAMPLE_FMT_S32 &&
          mFrame->format != AV_SAMPLE_FMT_S32P) {
        return MediaResult(
          NS_ERROR_DOM_MEDIA_DECODE_ERR,
          RESULT_DETAIL(
            "FFmpeg audio decoder outputs unsupported audio format"));
      }
      uint32_t numChannels = mCodecContext->channels;
      AudioConfig::ChannelLayout layout(numChannels);
      if (!layout.IsValid()) {
        return MediaResult(
          NS_ERROR_DOM_MEDIA_FATAL_ERR,
          RESULT_DETAIL("Unsupported channel layout:%u", numChannels));
      }

      uint32_t samplingRate = mCodecContext->sample_rate;

      AlignedAudioBuffer audio =
        CopyAndPackAudio(mFrame, numChannels, mFrame->nb_samples);
      if (!audio) {
        return MediaResult(NS_ERROR_OUT_OF_MEMORY, __func__);
      }

      media::TimeUnit duration =
        FramesToTimeUnit(mFrame->nb_samples, samplingRate);
      if (!duration.IsValid()) {
        return MediaResult(
          NS_ERROR_DOM_MEDIA_OVERFLOW_ERR,
          RESULT_DETAIL("Invalid sample duration"));
      }

      RefPtr<AudioData> data = new AudioData(samplePosition,
                                             pts.ToMicroseconds(),
                                             duration.ToMicroseconds(),
                                             mFrame->nb_samples,
                                             Move(audio),
                                             numChannels,
                                             samplingRate);
      mCallback->Output(data);
      pts += duration;
      if (!pts.IsValid()) {
        return MediaResult(
          NS_ERROR_DOM_MEDIA_OVERFLOW_ERR,
          RESULT_DETAIL("Invalid count of accumulated audio samples"));
      }
    }
    // The packet wasn't sent to ffmpeg, another attempt will happen next
    // iteration.
    if (bytesConsumed != -1) {
	  packet.data += bytesConsumed;
	  packet.size -= bytesConsumed;
	  samplePosition += bytesConsumed;
    }
  }
  return NS_OK;
}

void
FFmpegAudioDecoder<LIBAV_VER>::ProcessDrain()
{
  ProcessFlush();
  mCallback->DrainComplete();
}

AVCodecID
FFmpegAudioDecoder<LIBAV_VER>::GetCodecId(const nsACString& aMimeType)
{
  if (aMimeType.EqualsLiteral("audio/mpeg")) {
    return AV_CODEC_ID_MP3;
  }
  if (aMimeType.EqualsLiteral("audio/flac")) {
    return AV_CODEC_ID_FLAC;
  }
  if (aMimeType.EqualsLiteral("audio/mp4a-latm")) {
    return AV_CODEC_ID_AAC;
  }

  return AV_CODEC_ID_NONE;
}

FFmpegAudioDecoder<LIBAV_VER>::~FFmpegAudioDecoder()
{
  MOZ_COUNT_DTOR(FFmpegAudioDecoder);
}

} // namespace mozilla
