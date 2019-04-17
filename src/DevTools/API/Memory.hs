{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.Memory{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

    ( module DevTools.API.Memory.Types
    , module DevTools.API.Memory
    ) 
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
import qualified Data.Proxy as P (Proxy (Proxy))
import qualified Data.Typeable as P (Typeable)
import qualified GHC.Generics as P (Generic)
import qualified Prelude as P


-- deepseq--------------------------------------------------------------------
import qualified Control.DeepSeq as D (NFData)


-- devtools-------------------------------------------------------------------
import qualified DevTools.Event as E (Event, Result, name)
import qualified DevTools.Method as M (Method, Result, name)


-- devtools-api---------------------------------------------------------------
import           DevTools.API.Memory.Types


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
data GetDOMCounters = GetDOMCounters
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetDOMCounters where
    parseJSON A.Null = P.pure GetDOMCounters
    parseJSON v = A.withArray "getDOMCounters" go v
        <|> A.withObject "getDOMCounters" go v
      where
        go _ = P.pure GetDOMCounters


------------------------------------------------------------------------------
instance A.ToJSON GetDOMCounters where
    toEncoding GetDOMCounters = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetDOMCounters = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetDOMCounters where
    GetDOMCounters <> GetDOMCounters = GetDOMCounters


------------------------------------------------------------------------------
instance P.Monoid GetDOMCounters where
    mempty = GetDOMCounters


------------------------------------------------------------------------------
data GetDOMCountersResult = GetDOMCountersResult
    { documents :: !P.Int
    , nodes :: !P.Int
    , jsEventListeners :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetDOMCountersResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getDOMCountersResult" $ \_o -> GetDOMCountersResult
            <$> _o .: "documents"
            <*> _o .: "nodes"
            <*> _o .: "jsEventListeners"
        ago = A.withArray "getDOMCountersResult" $ \_a -> GetDOMCountersResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON GetDOMCountersResult where
    toEncoding (GetDOMCountersResult _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "documents" .= _0
        , P.pure $ "nodes" .= _1
        , P.pure $ "jsEventListeners" .= _2
        ]
    toJSON (GetDOMCountersResult _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "documents" .= _0
        , P.pure $ "nodes" .= _1
        , P.pure $ "jsEventListeners" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetDOMCountersResult where
    GetDOMCountersResult _0 _1 _2 <> GetDOMCountersResult _ _ _ = GetDOMCountersResult _0 _1 _2


------------------------------------------------------------------------------
instance M.Method GetDOMCounters where
    type Result GetDOMCounters = GetDOMCountersResult
    name _ = "Memory.getDOMCounters"


------------------------------------------------------------------------------
getDOMCounters
    :: GetDOMCounters
getDOMCounters = GetDOMCounters


------------------------------------------------------------------------------
data PrepareForLeakDetection = PrepareForLeakDetection
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PrepareForLeakDetection where
    parseJSON A.Null = P.pure PrepareForLeakDetection
    parseJSON v = A.withArray "prepareForLeakDetection" go v
        <|> A.withObject "prepareForLeakDetection" go v
      where
        go _ = P.pure PrepareForLeakDetection


------------------------------------------------------------------------------
instance A.ToJSON PrepareForLeakDetection where
    toEncoding PrepareForLeakDetection = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON PrepareForLeakDetection = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup PrepareForLeakDetection where
    PrepareForLeakDetection <> PrepareForLeakDetection = PrepareForLeakDetection


------------------------------------------------------------------------------
instance P.Monoid PrepareForLeakDetection where
    mempty = PrepareForLeakDetection


------------------------------------------------------------------------------
instance M.Method PrepareForLeakDetection where
    type Result PrepareForLeakDetection = ()
    name _ = "Memory.prepareForLeakDetection"


------------------------------------------------------------------------------
prepareForLeakDetection
    :: PrepareForLeakDetection
prepareForLeakDetection = PrepareForLeakDetection


------------------------------------------------------------------------------
-- | Simulate OomIntervention by purging V8 memory.
data ForciblyPurgeJavaScriptMemory = ForciblyPurgeJavaScriptMemory
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ForciblyPurgeJavaScriptMemory where
    parseJSON A.Null = P.pure ForciblyPurgeJavaScriptMemory
    parseJSON v = A.withArray "forciblyPurgeJavaScriptMemory" go v
        <|> A.withObject "forciblyPurgeJavaScriptMemory" go v
      where
        go _ = P.pure ForciblyPurgeJavaScriptMemory


------------------------------------------------------------------------------
instance A.ToJSON ForciblyPurgeJavaScriptMemory where
    toEncoding ForciblyPurgeJavaScriptMemory = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON ForciblyPurgeJavaScriptMemory = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup ForciblyPurgeJavaScriptMemory where
    ForciblyPurgeJavaScriptMemory <> ForciblyPurgeJavaScriptMemory = ForciblyPurgeJavaScriptMemory


------------------------------------------------------------------------------
instance P.Monoid ForciblyPurgeJavaScriptMemory where
    mempty = ForciblyPurgeJavaScriptMemory


------------------------------------------------------------------------------
instance M.Method ForciblyPurgeJavaScriptMemory where
    type Result ForciblyPurgeJavaScriptMemory = ()
    name _ = "Memory.forciblyPurgeJavaScriptMemory"


------------------------------------------------------------------------------
-- | Simulate OomIntervention by purging V8 memory.
forciblyPurgeJavaScriptMemory
    :: ForciblyPurgeJavaScriptMemory
forciblyPurgeJavaScriptMemory = ForciblyPurgeJavaScriptMemory


------------------------------------------------------------------------------
-- | Enable\/disable suppressing memory pressure notifications in all processes.
data SetPressureNotificationsSuppressed = SetPressureNotificationsSuppressed
    { -- | If true, memory pressure notifications will be suppressed.
      suppressed :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetPressureNotificationsSuppressed where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setPressureNotificationsSuppressed" $ \_o -> SetPressureNotificationsSuppressed
            <$> _o .: "suppressed"
        ago = A.withArray "setPressureNotificationsSuppressed" $ \_a -> SetPressureNotificationsSuppressed
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetPressureNotificationsSuppressed where
    toEncoding (SetPressureNotificationsSuppressed _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "suppressed" .= _0
        ]
    toJSON (SetPressureNotificationsSuppressed _0) = A.object $ P.catMaybes
        [ P.pure $ "suppressed" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetPressureNotificationsSuppressed where
    SetPressureNotificationsSuppressed _0 <> SetPressureNotificationsSuppressed _ = SetPressureNotificationsSuppressed _0


------------------------------------------------------------------------------
instance M.Method SetPressureNotificationsSuppressed where
    type Result SetPressureNotificationsSuppressed = ()
    name _ = "Memory.setPressureNotificationsSuppressed"


------------------------------------------------------------------------------
-- | Enable\/disable suppressing memory pressure notifications in all processes.
setPressureNotificationsSuppressed
    :: P.Bool
    -- ^ If true, memory pressure notifications will be suppressed.

    -> SetPressureNotificationsSuppressed
setPressureNotificationsSuppressed _0 = SetPressureNotificationsSuppressed _0


------------------------------------------------------------------------------
-- | Simulate a memory pressure notification in all processes.
data SimulatePressureNotification = SimulatePressureNotification
    { -- | Memory pressure level of the notification.
      level :: !PressureLevel
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SimulatePressureNotification where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "simulatePressureNotification" $ \_o -> SimulatePressureNotification
            <$> _o .: "level"
        ago = A.withArray "simulatePressureNotification" $ \_a -> SimulatePressureNotification
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SimulatePressureNotification where
    toEncoding (SimulatePressureNotification _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "level" .= _0
        ]
    toJSON (SimulatePressureNotification _0) = A.object $ P.catMaybes
        [ P.pure $ "level" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SimulatePressureNotification where
    SimulatePressureNotification _0 <> SimulatePressureNotification _ = SimulatePressureNotification _0


------------------------------------------------------------------------------
instance M.Method SimulatePressureNotification where
    type Result SimulatePressureNotification = ()
    name _ = "Memory.simulatePressureNotification"


------------------------------------------------------------------------------
-- | Simulate a memory pressure notification in all processes.
simulatePressureNotification
    :: PressureLevel
    -- ^ Memory pressure level of the notification.

    -> SimulatePressureNotification
simulatePressureNotification _0 = SimulatePressureNotification _0


------------------------------------------------------------------------------
-- | Start collecting native memory profile.
data StartSampling = StartSampling
    { -- | Average number of bytes between samples.
      samplingInterval :: !(P.Maybe P.Int)
      -- | Do not randomize intervals between samples.
    , suppressRandomness :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StartSampling where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "startSampling" $ \_o -> StartSampling
            <$> _o .:? "samplingInterval"
            <*> _o .:? "suppressRandomness"
        ago = A.withArray "startSampling" $ \_a -> StartSampling
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON StartSampling where
    toEncoding (StartSampling _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ ("samplingInterval" .=) <$> _0
        , ("suppressRandomness" .=) <$> _1
        ]
    toJSON (StartSampling _0 _1) = A.object $ P.catMaybes
        [ ("samplingInterval" .=) <$> _0
        , ("suppressRandomness" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup StartSampling where
    StartSampling _0 _1 <> StartSampling __0 __1 = StartSampling (_0 <|> __0) (_1 <|> __1)


------------------------------------------------------------------------------
instance P.Monoid StartSampling where
    mempty = StartSampling P.empty P.empty


------------------------------------------------------------------------------
instance M.Method StartSampling where
    type Result StartSampling = ()
    name _ = "Memory.startSampling"


------------------------------------------------------------------------------
-- | Start collecting native memory profile.
startSampling
    :: StartSampling
startSampling = StartSampling P.empty P.empty


------------------------------------------------------------------------------
-- | Stop collecting native memory profile.
data StopSampling = StopSampling
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StopSampling where
    parseJSON A.Null = P.pure StopSampling
    parseJSON v = A.withArray "stopSampling" go v
        <|> A.withObject "stopSampling" go v
      where
        go _ = P.pure StopSampling


------------------------------------------------------------------------------
instance A.ToJSON StopSampling where
    toEncoding StopSampling = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON StopSampling = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup StopSampling where
    StopSampling <> StopSampling = StopSampling


------------------------------------------------------------------------------
instance P.Monoid StopSampling where
    mempty = StopSampling


------------------------------------------------------------------------------
instance M.Method StopSampling where
    type Result StopSampling = ()
    name _ = "Memory.stopSampling"


------------------------------------------------------------------------------
-- | Stop collecting native memory profile.
stopSampling
    :: StopSampling
stopSampling = StopSampling


------------------------------------------------------------------------------
-- | Retrieve native memory allocations profile
-- collected since renderer process startup.
data GetAllTimeSamplingProfile = GetAllTimeSamplingProfile
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetAllTimeSamplingProfile where
    parseJSON A.Null = P.pure GetAllTimeSamplingProfile
    parseJSON v = A.withArray "getAllTimeSamplingProfile" go v
        <|> A.withObject "getAllTimeSamplingProfile" go v
      where
        go _ = P.pure GetAllTimeSamplingProfile


------------------------------------------------------------------------------
instance A.ToJSON GetAllTimeSamplingProfile where
    toEncoding GetAllTimeSamplingProfile = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetAllTimeSamplingProfile = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetAllTimeSamplingProfile where
    GetAllTimeSamplingProfile <> GetAllTimeSamplingProfile = GetAllTimeSamplingProfile


------------------------------------------------------------------------------
instance P.Monoid GetAllTimeSamplingProfile where
    mempty = GetAllTimeSamplingProfile


------------------------------------------------------------------------------
-- | Retrieve native memory allocations profile
-- collected since renderer process startup.
data GetAllTimeSamplingProfileResult = GetAllTimeSamplingProfileResult
    { profile :: !SamplingProfile
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetAllTimeSamplingProfileResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getAllTimeSamplingProfileResult" $ \_o -> GetAllTimeSamplingProfileResult
            <$> _o .: "profile"
        ago = A.withArray "getAllTimeSamplingProfileResult" $ \_a -> GetAllTimeSamplingProfileResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetAllTimeSamplingProfileResult where
    toEncoding (GetAllTimeSamplingProfileResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "profile" .= _0
        ]
    toJSON (GetAllTimeSamplingProfileResult _0) = A.object $ P.catMaybes
        [ P.pure $ "profile" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetAllTimeSamplingProfileResult where
    GetAllTimeSamplingProfileResult _0 <> GetAllTimeSamplingProfileResult _ = GetAllTimeSamplingProfileResult _0


------------------------------------------------------------------------------
instance M.Method GetAllTimeSamplingProfile where
    type Result GetAllTimeSamplingProfile = GetAllTimeSamplingProfileResult
    name _ = "Memory.getAllTimeSamplingProfile"


------------------------------------------------------------------------------
-- | Retrieve native memory allocations profile
-- collected since renderer process startup.
getAllTimeSamplingProfile
    :: GetAllTimeSamplingProfile
getAllTimeSamplingProfile = GetAllTimeSamplingProfile


------------------------------------------------------------------------------
-- | Retrieve native memory allocations profile
-- collected since browser process startup.
data GetBrowserSamplingProfile = GetBrowserSamplingProfile
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetBrowserSamplingProfile where
    parseJSON A.Null = P.pure GetBrowserSamplingProfile
    parseJSON v = A.withArray "getBrowserSamplingProfile" go v
        <|> A.withObject "getBrowserSamplingProfile" go v
      where
        go _ = P.pure GetBrowserSamplingProfile


------------------------------------------------------------------------------
instance A.ToJSON GetBrowserSamplingProfile where
    toEncoding GetBrowserSamplingProfile = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetBrowserSamplingProfile = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetBrowserSamplingProfile where
    GetBrowserSamplingProfile <> GetBrowserSamplingProfile = GetBrowserSamplingProfile


------------------------------------------------------------------------------
instance P.Monoid GetBrowserSamplingProfile where
    mempty = GetBrowserSamplingProfile


------------------------------------------------------------------------------
-- | Retrieve native memory allocations profile
-- collected since browser process startup.
data GetBrowserSamplingProfileResult = GetBrowserSamplingProfileResult
    { profile :: !SamplingProfile
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetBrowserSamplingProfileResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getBrowserSamplingProfileResult" $ \_o -> GetBrowserSamplingProfileResult
            <$> _o .: "profile"
        ago = A.withArray "getBrowserSamplingProfileResult" $ \_a -> GetBrowserSamplingProfileResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetBrowserSamplingProfileResult where
    toEncoding (GetBrowserSamplingProfileResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "profile" .= _0
        ]
    toJSON (GetBrowserSamplingProfileResult _0) = A.object $ P.catMaybes
        [ P.pure $ "profile" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetBrowserSamplingProfileResult where
    GetBrowserSamplingProfileResult _0 <> GetBrowserSamplingProfileResult _ = GetBrowserSamplingProfileResult _0


------------------------------------------------------------------------------
instance M.Method GetBrowserSamplingProfile where
    type Result GetBrowserSamplingProfile = GetBrowserSamplingProfileResult
    name _ = "Memory.getBrowserSamplingProfile"


------------------------------------------------------------------------------
-- | Retrieve native memory allocations profile
-- collected since browser process startup.
getBrowserSamplingProfile
    :: GetBrowserSamplingProfile
getBrowserSamplingProfile = GetBrowserSamplingProfile


------------------------------------------------------------------------------
-- | Retrieve native memory allocations profile collected since last
-- @startSampling@ call.
data GetSamplingProfile = GetSamplingProfile
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetSamplingProfile where
    parseJSON A.Null = P.pure GetSamplingProfile
    parseJSON v = A.withArray "getSamplingProfile" go v
        <|> A.withObject "getSamplingProfile" go v
      where
        go _ = P.pure GetSamplingProfile


------------------------------------------------------------------------------
instance A.ToJSON GetSamplingProfile where
    toEncoding GetSamplingProfile = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetSamplingProfile = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetSamplingProfile where
    GetSamplingProfile <> GetSamplingProfile = GetSamplingProfile


------------------------------------------------------------------------------
instance P.Monoid GetSamplingProfile where
    mempty = GetSamplingProfile


------------------------------------------------------------------------------
-- | Retrieve native memory allocations profile collected since last
-- @startSampling@ call.
data GetSamplingProfileResult = GetSamplingProfileResult
    { profile :: !SamplingProfile
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetSamplingProfileResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getSamplingProfileResult" $ \_o -> GetSamplingProfileResult
            <$> _o .: "profile"
        ago = A.withArray "getSamplingProfileResult" $ \_a -> GetSamplingProfileResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetSamplingProfileResult where
    toEncoding (GetSamplingProfileResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "profile" .= _0
        ]
    toJSON (GetSamplingProfileResult _0) = A.object $ P.catMaybes
        [ P.pure $ "profile" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetSamplingProfileResult where
    GetSamplingProfileResult _0 <> GetSamplingProfileResult _ = GetSamplingProfileResult _0


------------------------------------------------------------------------------
instance M.Method GetSamplingProfile where
    type Result GetSamplingProfile = GetSamplingProfileResult
    name _ = "Memory.getSamplingProfile"


------------------------------------------------------------------------------
-- | Retrieve native memory allocations profile collected since last
-- @startSampling@ call.
getSamplingProfile
    :: GetSamplingProfile
getSamplingProfile = GetSamplingProfile

