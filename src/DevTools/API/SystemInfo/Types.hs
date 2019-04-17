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
        ago = A.withArray "GPUDevice" $ \_a -> GPUDevice
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON GPUDevice where
    toEncoding (GPUDevice _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "vendorId" .= _0
        , P.pure $ "deviceId" .= _1
        , P.pure $ "vendorString" .= _2
        , P.pure $ "deviceString" .= _3
        ]
    toJSON (GPUDevice _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "vendorId" .= _0
        , P.pure $ "deviceId" .= _1
        , P.pure $ "vendorString" .= _2
        , P.pure $ "deviceString" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup GPUDevice where
    GPUDevice _0 _1 _2 _3 <> GPUDevice _ _ _ _ = GPUDevice _0 _1 _2 _3


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
        ago = A.withArray "GPUInfo" $ \_a -> GPUInfo
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON GPUInfo where
    toEncoding (GPUInfo _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "devices" .= _0
        , ("auxAttributes" .=) <$> _1
        , ("featureStatus" .=) <$> _2
        , P.pure $ "driverBugWorkarounds" .= _3
        ]
    toJSON (GPUInfo _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "devices" .= _0
        , ("auxAttributes" .=) <$> _1
        , ("featureStatus" .=) <$> _2
        , P.pure $ "driverBugWorkarounds" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup GPUInfo where
    GPUInfo _0 _1 _2 _3 <> GPUInfo _ __1 __2 _ = GPUInfo _0 (_1 <|> __1) (_2 <|> __2) _3


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

