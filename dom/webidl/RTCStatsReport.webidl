/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://dev.w3.org/2011/webrtc/editor/webrtc.html#rtcstatsreport-object
 * http://www.w3.org/2011/04/webrtc/wiki/Stats
 */

enum RTCStatsType {
  "inbound-rtp",
  "outbound-rtp",
  "session",
  "track",
  "transport",
  "candidate-pair",
  "local-candidate",
  "remote-candidate"
};

dictionary RTCStats {
  DOMHighResTimeStamp timestamp;
  RTCStatsType type;
  DOMString id;
};

dictionary RTCRTPStreamStats : RTCStats {
  DOMString ssrc;
  DOMString mediaType;
  DOMString remoteId;
  boolean isRemote = false;
  DOMString mediaTrackId;
  DOMString transportId;
  DOMString codecId;

  // Video encoder/decoder measurements (absent for rtcp)
  double bitrateMean;
  double bitrateStdDev;
  double framerateMean;
  double framerateStdDev;
};

dictionary RTCInboundRTPStreamStats : RTCRTPStreamStats {
  unsigned long packetsReceived;
  unsigned long long bytesReceived;
  double jitter;
  unsigned long packetsLost;
  long mozAvSyncDelay;
  long mozJitterBufferDelay;
  long mozRtt;

  // Video decoder measurement (absent in rtcp case)
  unsigned long discardedPackets;
};

dictionary RTCOutboundRTPStreamStats : RTCRTPStreamStats {
  unsigned long packetsSent;
  unsigned long long bytesSent;
  double targetBitrate;  // config encoder bitrate target of this SSRC in bits/s

  // Video encoder measurement (absent in rtcp case)
  unsigned long droppedFrames;
};

dictionary RTCMediaStreamTrackStats : RTCStats {
  DOMString trackIdentifier;      // track.id property
  boolean remoteSource;
  sequence<DOMString> ssrcIds;
  // Stuff that makes sense for video
  unsigned long frameWidth;
  unsigned long frameHeight;
  double framesPerSecond;        // The nominal FPS value
  unsigned long framesSent;
  unsigned long framesReceived;  // Only for remoteSource=true
  unsigned long framesDecoded;
  unsigned long framesDropped;   // See VideoPlaybackQuality.droppedVideoFrames
  unsigned long framesCorrupted; // as above.
  // Stuff that makes sense for audio
  double audioLevel;               // linear, 1.0 = 0 dBov (from RFC 6464).
  // AEC stuff on audio tracks sourced from a microphone where AEC is applied
  double echoReturnLoss;           // in decibels from G.168 (2012) section 3.14
  double echoReturnLossEnhancement; // as above, section 3.15
};

dictionary RTCMediaStreamStats : RTCStats {
  DOMString streamIdentifier;     // stream.id property
  sequence<DOMString> trackIds;   // Note: stats object ids, not track.id
};

dictionary RTCTransportStats: RTCStats {
  unsigned long bytesSent;
  unsigned long bytesReceived;
};

dictionary RTCIceComponentStats : RTCStats {
  DOMString transportId;
  long component;
  unsigned long bytesSent;
  unsigned long bytesReceived;
  boolean activeConnection;
};

enum RTCStatsIceCandidatePairState {
  "frozen",
  "waiting",
  "inprogress",
  "failed",
  "succeeded",
  "cancelled"
};

dictionary RTCIceCandidatePairStats : RTCStats {
  DOMString componentId;
  DOMString localCandidateId;
  DOMString remoteCandidateId;
  RTCStatsIceCandidatePairState state;
  unsigned long long priority;
  boolean readable;
  boolean nominated;
  boolean selected;
};

enum RTCStatsIceCandidateType {
  "host",
  "serverreflexive",
  "peerreflexive",
  "relayed"
};

dictionary RTCIceCandidateStats : RTCStats {
  DOMString componentId;
  DOMString candidateId;
  DOMString ipAddress;
  DOMString transport;
  DOMString mozLocalTransport; // needs standardization
  long portNumber;
  RTCStatsIceCandidateType candidateType;
};

dictionary RTCCodecStats : RTCStats {
  unsigned long payloadType;       // As used in RTP encoding.
  DOMString codec;                 // video/vp8 or equivalent
  unsigned long clockRate;
  unsigned long channels;          // 2=stereo, missing for most other cases.
  DOMString parameters;            // From SDP description line
};

// This is the internal representation of the report in this implementation
// to be received from c++

dictionary RTCStatsReportInternal {
  DOMString                           pcid = "";
  sequence<RTCInboundRTPStreamStats>  inboundRTPStreamStats;
  sequence<RTCOutboundRTPStreamStats> outboundRTPStreamStats;
  sequence<RTCMediaStreamTrackStats>  mediaStreamTrackStats;
  sequence<RTCMediaStreamStats>       mediaStreamStats;
  sequence<RTCTransportStats>         transportStats;
  sequence<RTCIceComponentStats>      iceComponentStats;
  sequence<RTCIceCandidatePairStats>  iceCandidatePairStats;
  sequence<RTCIceCandidateStats>      iceCandidateStats;
  sequence<RTCCodecStats>             codecStats;
  DOMString                           localSdp;
  DOMString                           remoteSdp;
  DOMHighResTimeStamp                 timestamp;
  boolean                             closed; // Is the PC now closed
};

[Pref="media.peerconnection.enabled",
// TODO: Use MapClass here once it's available (Bug 928114)
// MapClass(DOMString, object)
 JSImplementation="@mozilla.org/dom/rtcstatsreport;1"]
interface RTCStatsReport {
  readonly maplike<DOMString, object>;
  [ChromeOnly]
  readonly attribute DOMString mozPcid;
};
