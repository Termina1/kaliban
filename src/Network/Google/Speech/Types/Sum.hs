{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, LambdaCase, NoImplicitPrelude, OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.Speech.Types.Sum
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.Speech.Types.Sum where

import Network.Google.Prelude

-- | V1 error format.
data Xgafv
    = X1
      -- ^ @1@
      -- v1 error format
    | X2
      -- ^ @2@
      -- v2 error format
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable Xgafv

instance FromHttpApiData Xgafv where
    parseQueryParam = \case
        "1" -> Right X1
        "2" -> Right X2
        x -> Left ("Unable to parse Xgafv from: " <> x)

instance ToHttpApiData Xgafv where
    toQueryParam = \case
        X1 -> "1"
        X2 -> "2"

instance FromJSON Xgafv where
    parseJSON = parseJSONText "Xgafv"

instance ToJSON Xgafv where
    toJSON = toJSONText

-- | *Required* Encoding of audio data sent in all \`RecognitionAudio\`
-- messages.
data RecognitionConfigEncoding
    = EncodingUnspecified
      -- ^ @ENCODING_UNSPECIFIED@
      -- Not specified.
    | LINEAR16
      -- ^ @LINEAR16@
      -- Uncompressed 16-bit signed little-endian samples (Linear PCM).
    | Flac
      -- ^ @FLAC@
      -- [\`FLAC\`](https:\/\/xiph.org\/flac\/documentation.html) (Free Lossless
      -- Audio Codec) is the recommended encoding because it is
      -- lossless--therefore recognition is not compromised--and requires only
      -- about half the bandwidth of \`LINEAR16\`. \`FLAC\` stream encoding
      -- supports 16-bit and 24-bit samples, however, not all fields in
      -- \`STREAMINFO\` are supported.
    | Mulaw
      -- ^ @MULAW@
      -- 8-bit samples that compand 14-bit audio samples using G.711
      -- PCMU\/mu-law.
    | Amr
      -- ^ @AMR@
      -- Adaptive Multi-Rate Narrowband codec. \`sample_rate_hertz\` must be
      -- 8000.
    | AmrWb
      -- ^ @AMR_WB@
      -- Adaptive Multi-Rate Wideband codec. \`sample_rate_hertz\` must be 16000.
    | OggOpus
      -- ^ @OGG_OPUS@
      -- Opus encoded audio frames in Ogg container
      -- ([OggOpus](https:\/\/wiki.xiph.org\/OggOpus)). \`sample_rate_hertz\`
      -- must be one of 8000, 12000, 16000, 24000, or 48000.
    | SpeexWithHeaderByte
      -- ^ @SPEEX_WITH_HEADER_BYTE@
      -- Although the use of lossy encodings is not recommended, if a very low
      -- bitrate encoding is required, \`OGG_OPUS\` is highly preferred over
      -- Speex encoding. The [Speex](https:\/\/speex.org\/) encoding supported by
      -- Cloud Speech API has a header byte in each block, as in MIME type
      -- \`audio\/x-speex-with-header-byte\`. It is a variant of the RTP Speex
      -- encoding defined in [RFC 5574](https:\/\/tools.ietf.org\/html\/rfc5574).
      -- The stream is a sequence of blocks, one block per RTP packet. Each block
      -- starts with a byte containing the length of the block, in bytes,
      -- followed by one or more frames of Speex data, padded to an integral
      -- number of bytes (octets) as specified in RFC 5574. In other words, each
      -- RTP header is replaced with a single byte containing the block length.
      -- Only Speex wideband is supported. \`sample_rate_hertz\` must be 16000.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable RecognitionConfigEncoding

instance FromHttpApiData RecognitionConfigEncoding where
    parseQueryParam = \case
        "ENCODING_UNSPECIFIED" -> Right EncodingUnspecified
        "LINEAR16" -> Right LINEAR16
        "FLAC" -> Right Flac
        "MULAW" -> Right Mulaw
        "AMR" -> Right Amr
        "AMR_WB" -> Right AmrWb
        "OGG_OPUS" -> Right OggOpus
        "SPEEX_WITH_HEADER_BYTE" -> Right SpeexWithHeaderByte
        x -> Left ("Unable to parse RecognitionConfigEncoding from: " <> x)

instance ToHttpApiData RecognitionConfigEncoding where
    toQueryParam = \case
        EncodingUnspecified -> "ENCODING_UNSPECIFIED"
        LINEAR16 -> "LINEAR16"
        Flac -> "FLAC"
        Mulaw -> "MULAW"
        Amr -> "AMR"
        AmrWb -> "AMR_WB"
        OggOpus -> "OGG_OPUS"
        SpeexWithHeaderByte -> "SPEEX_WITH_HEADER_BYTE"

instance FromJSON RecognitionConfigEncoding where
    parseJSON = parseJSONText "RecognitionConfigEncoding"

instance ToJSON RecognitionConfigEncoding where
    toJSON = toJSONText
