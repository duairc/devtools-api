{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | The SystemInfo domain defines methods and events for querying low-level system information.
module DevTools.API.SystemInfo.Types{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

where

-- aeson----------------------------------------------------------------------
import           Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as A
                     ( FromJSON, ToJSON, Object, Value (Null)
                     , parseJSON, withText, withArray, withObject
                     , toEncoding, pairs, toJSON, object
                     )


-- base-----------------------------------------------------------------------
import           Control.Applicative ((<|>), (<*>))
import qualified Control.Applicative as P (empty)
import qualified Data.Foldable as P (fold)
import           Data.Function (($))
import           Data.Functor ((<$>))
import qualified Data.Maybe as P (catMaybes)
import qualified Data.Typeable as P (Typeable)
import qualified GHC.Generics as P (Generic)
import qualified Prelude as P


-- deepseq--------------------------------------------------------------------
import qualified Control.DeepSeq as D (NFData)


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Describes a single graphics processor (GPU).
data GPUDevice = GPUDevice
    { -- | PCI ID of the GPU vendor, if available; 0 otherwise.
      vendorId :: !P.Double
      -- | PCI ID of the GPU device, if available; 0 otherwise.
    , deviceId :: !P.Double
      -- | String description of the GPU vendor, if the PCI ID is not available.
    , vendorString :: !T.Text
      -- | String description of the GPU device, if the PCI ID is not available.
    , deviceString :: !T.Text
      -- | String description of the GPU driver vendor.
    , driverVendor :: !T.Text
      -- | String description of the GPU driver version.
    , driverVersion :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GPUDevice where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "GPUDevice" $ \_o -> GPUDevice
            <$> _o .: "vendorId"
            <*> _o .: "deviceId"
            <*> _o .: "vendorString"
            <*> _o .: "deviceString"
            <*> _o .: "driverVendor"
            <*> _o .: "driverVersion"
        ago = A.withArray "GPUDevice" $ \_a -> GPUDevice
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)


------------------------------------------------------------------------------
instance A.ToJSON GPUDevice where
    toEncoding (GPUDevice _0 _1 _2 _3 _4 _5) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "vendorId" .= _0
        , P.pure $ "deviceId" .= _1
        , P.pure $ "vendorString" .= _2
        , P.pure $ "deviceString" .= _3
        , P.pure $ "driverVendor" .= _4
        , P.pure $ "driverVersion" .= _5
        ]
    toJSON (GPUDevice _0 _1 _2 _3 _4 _5) = A.object $ P.catMaybes
        [ P.pure $ "vendorId" .= _0
        , P.pure $ "deviceId" .= _1
        , P.pure $ "vendorString" .= _2
        , P.pure $ "deviceString" .= _3
        , P.pure $ "driverVendor" .= _4
        , P.pure $ "driverVersion" .= _5
        ]


------------------------------------------------------------------------------
instance P.Semigroup GPUDevice where
    GPUDevice _0 _1 _2 _3 _4 _5 <> GPUDevice _ _ _ _ _ _ = GPUDevice _0 _1 _2 _3 _4 _5


------------------------------------------------------------------------------
-- | Describes the width and height dimensions of an entity.
data Size = Size
    { -- | Width in pixels.
      width :: !P.Int
      -- | Height in pixels.
    , height :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Size where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "Size" $ \_o -> Size
            <$> _o .: "width"
            <*> _o .: "height"
        ago = A.withArray "Size" $ \_a -> Size
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON Size where
    toEncoding (Size _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "width" .= _0
        , P.pure $ "height" .= _1
        ]
    toJSON (Size _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "width" .= _0
        , P.pure $ "height" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup Size where
    Size _0 _1 <> Size _ _ = Size _0 _1


------------------------------------------------------------------------------
-- | Describes a supported video decoding profile with its associated minimum and
-- maximum resolutions.
data VideoDecodeAcceleratorCapability = VideoDecodeAcceleratorCapability
    { -- | Video codec profile that is supported, e.g. VP9 Profile 2.
      profile :: !T.Text
      -- | Maximum video dimensions in pixels supported for this |profile|.
    , maxResolution :: !Size
      -- | Minimum video dimensions in pixels supported for this |profile|.
    , minResolution :: !Size
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON VideoDecodeAcceleratorCapability where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "VideoDecodeAcceleratorCapability" $ \_o -> VideoDecodeAcceleratorCapability
            <$> _o .: "profile"
            <*> _o .: "maxResolution"
            <*> _o .: "minResolution"
        ago = A.withArray "VideoDecodeAcceleratorCapability" $ \_a -> VideoDecodeAcceleratorCapability
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON VideoDecodeAcceleratorCapability where
    toEncoding (VideoDecodeAcceleratorCapability _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "profile" .= _0
        , P.pure $ "maxResolution" .= _1
        , P.pure $ "minResolution" .= _2
        ]
    toJSON (VideoDecodeAcceleratorCapability _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "profile" .= _0
        , P.pure $ "maxResolution" .= _1
        , P.pure $ "minResolution" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup VideoDecodeAcceleratorCapability where
    VideoDecodeAcceleratorCapability _0 _1 _2 <> VideoDecodeAcceleratorCapability _ _ _ = VideoDecodeAcceleratorCapability _0 _1 _2


------------------------------------------------------------------------------
-- | Describes a supported video encoding profile with its associated maximum
-- resolution and maximum framerate.
data VideoEncodeAcceleratorCapability = VideoEncodeAcceleratorCapability
    { -- | Video codec profile that is supported, e.g H264 Main.
      profile :: !T.Text
      -- | Maximum video dimensions in pixels supported for this |profile|.
    , maxResolution :: !Size
      -- | Maximum encoding framerate in frames per second supported for this
      -- |profile|, as fraction's numerator and denominator, e.g. 24\/1 fps,
      -- 24000\/1001 fps, etc.
    , maxFramerateNumerator :: !P.Int
    , maxFramerateDenominator :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON VideoEncodeAcceleratorCapability where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "VideoEncodeAcceleratorCapability" $ \_o -> VideoEncodeAcceleratorCapability
            <$> _o .: "profile"
            <*> _o .: "maxResolution"
            <*> _o .: "maxFramerateNumerator"
            <*> _o .: "maxFramerateDenominator"
        ago = A.withArray "VideoEncodeAcceleratorCapability" $ \_a -> VideoEncodeAcceleratorCapability
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON VideoEncodeAcceleratorCapability where
    toEncoding (VideoEncodeAcceleratorCapability _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "profile" .= _0
        , P.pure $ "maxResolution" .= _1
        , P.pure $ "maxFramerateNumerator" .= _2
        , P.pure $ "maxFramerateDenominator" .= _3
        ]
    toJSON (VideoEncodeAcceleratorCapability _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "profile" .= _0
        , P.pure $ "maxResolution" .= _1
        , P.pure $ "maxFramerateNumerator" .= _2
        , P.pure $ "maxFramerateDenominator" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup VideoEncodeAcceleratorCapability where
    VideoEncodeAcceleratorCapability _0 _1 _2 _3 <> VideoEncodeAcceleratorCapability _ _ _ _ = VideoEncodeAcceleratorCapability _0 _1 _2 _3


------------------------------------------------------------------------------
-- | YUV subsampling type of the pixels of a given image.
data SubsamplingFormat
    = Yuv420
    | Yuv422
    | Yuv444
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SubsamplingFormat where
    parseJSON = A.withText "SubsamplingFormat" $ \t -> case t of
        "yuv420" -> P.pure Yuv420
        "yuv422" -> P.pure Yuv422
        "yuv444" -> P.pure Yuv444
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON SubsamplingFormat where
    toJSON Yuv420 = "yuv420"
    toJSON Yuv422 = "yuv422"
    toJSON Yuv444 = "yuv444"


------------------------------------------------------------------------------
-- | Image format of a given image.
data ImageType
    = Jpeg
    | Webp
    | Unknown
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ImageType where
    parseJSON = A.withText "ImageType" $ \t -> case t of
        "jpeg" -> P.pure Jpeg
        "webp" -> P.pure Webp
        "unknown" -> P.pure Unknown
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON ImageType where
    toJSON Jpeg = "jpeg"
    toJSON Webp = "webp"
    toJSON Unknown = "unknown"


------------------------------------------------------------------------------
-- | Describes a supported image decoding profile with its associated minimum and
-- maximum resolutions and subsampling.
data ImageDecodeAcceleratorCapability = ImageDecodeAcceleratorCapability
    { -- | Image coded, e.g. Jpeg.
      imageType :: !ImageType
      -- | Maximum supported dimensions of the image in pixels.
    , maxDimensions :: !Size
      -- | Minimum supported dimensions of the image in pixels.
    , minDimensions :: !Size
      -- | Optional array of supported subsampling formats, e.g. 4:2:0, if known.
    , subsamplings :: ![SubsamplingFormat]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ImageDecodeAcceleratorCapability where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "ImageDecodeAcceleratorCapability" $ \_o -> ImageDecodeAcceleratorCapability
            <$> _o .: "imageType"
            <*> _o .: "maxDimensions"
            <*> _o .: "minDimensions"
            <*> _o .: "subsamplings"
        ago = A.withArray "ImageDecodeAcceleratorCapability" $ \_a -> ImageDecodeAcceleratorCapability
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON ImageDecodeAcceleratorCapability where
    toEncoding (ImageDecodeAcceleratorCapability _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "imageType" .= _0
        , P.pure $ "maxDimensions" .= _1
        , P.pure $ "minDimensions" .= _2
        , P.pure $ "subsamplings" .= _3
        ]
    toJSON (ImageDecodeAcceleratorCapability _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "imageType" .= _0
        , P.pure $ "maxDimensions" .= _1
        , P.pure $ "minDimensions" .= _2
        , P.pure $ "subsamplings" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup ImageDecodeAcceleratorCapability where
    ImageDecodeAcceleratorCapability _0 _1 _2 _3 <> ImageDecodeAcceleratorCapability _ _ _ _ = ImageDecodeAcceleratorCapability _0 _1 _2 _3


------------------------------------------------------------------------------
-- | Provides information about the GPU(s) on the system.
data GPUInfo = GPUInfo
    { -- | The graphics devices on the system. Element 0 is the primary GPU.
      devices :: ![GPUDevice]
      -- | An optional dictionary of additional GPU related attributes.
    , auxAttributes :: !(P.Maybe A.Object)
      -- | An optional dictionary of graphics features and their status.
    , featureStatus :: !(P.Maybe A.Object)
      -- | An optional array of GPU driver bug workarounds.
    , driverBugWorkarounds :: ![T.Text]
      -- | Supported accelerated video decoding capabilities.
    , videoDecoding :: ![VideoDecodeAcceleratorCapability]
      -- | Supported accelerated video encoding capabilities.
    , videoEncoding :: ![VideoEncodeAcceleratorCapability]
      -- | Supported accelerated image decoding capabilities.
    , imageDecoding :: ![ImageDecodeAcceleratorCapability]
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GPUInfo where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "GPUInfo" $ \_o -> GPUInfo
            <$> _o .: "devices"
            <*> _o .:? "auxAttributes"
            <*> _o .:? "featureStatus"
            <*> _o .: "driverBugWorkarounds"
            <*> _o .: "videoDecoding"
            <*> _o .: "videoEncoding"
            <*> _o .: "imageDecoding"
        ago = A.withArray "GPUInfo" $ \_a -> GPUInfo
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.maybe P.empty A.parseJSON (_a !? 6)


------------------------------------------------------------------------------
instance A.ToJSON GPUInfo where
    toEncoding (GPUInfo _0 _1 _2 _3 _4 _5 _6) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "devices" .= _0
        , ("auxAttributes" .=) <$> _1
        , ("featureStatus" .=) <$> _2
        , P.pure $ "driverBugWorkarounds" .= _3
        , P.pure $ "videoDecoding" .= _4
        , P.pure $ "videoEncoding" .= _5
        , P.pure $ "imageDecoding" .= _6
        ]
    toJSON (GPUInfo _0 _1 _2 _3 _4 _5 _6) = A.object $ P.catMaybes
        [ P.pure $ "devices" .= _0
        , ("auxAttributes" .=) <$> _1
        , ("featureStatus" .=) <$> _2
        , P.pure $ "driverBugWorkarounds" .= _3
        , P.pure $ "videoDecoding" .= _4
        , P.pure $ "videoEncoding" .= _5
        , P.pure $ "imageDecoding" .= _6
        ]


------------------------------------------------------------------------------
instance P.Semigroup GPUInfo where
    GPUInfo _0 _1 _2 _3 _4 _5 _6 <> GPUInfo _ __1 __2 _ _ _ _ = GPUInfo _0 (_1 <|> __1) (_2 <|> __2) _3 _4 _5 _6


------------------------------------------------------------------------------
-- | Represents process info.
data ProcessInfo = ProcessInfo
    { -- | Specifies process type.
      type_ :: !T.Text
      -- | Specifies process id.
    , id :: !P.Int
      -- | Specifies cumulative CPU usage in seconds across all threads of the
      -- process since the process start.
    , cpuTime :: !P.Double
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ProcessInfo where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "ProcessInfo" $ \_o -> ProcessInfo
            <$> _o .: "type"
            <*> _o .: "id"
            <*> _o .: "cpuTime"
        ago = A.withArray "ProcessInfo" $ \_a -> ProcessInfo
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON ProcessInfo where
    toEncoding (ProcessInfo _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "type" .= _0
        , P.pure $ "id" .= _1
        , P.pure $ "cpuTime" .= _2
        ]
    toJSON (ProcessInfo _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "type" .= _0
        , P.pure $ "id" .= _1
        , P.pure $ "cpuTime" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup ProcessInfo where
    ProcessInfo _0 _1 _2 <> ProcessInfo _ _ _ = ProcessInfo _0 _1 _2

