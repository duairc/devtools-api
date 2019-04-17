{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.Profiler
    ( module DevTools.API.Profiler.Types
    , module DevTools.API.Profiler
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
import qualified DevTools.API.Debugger.Types as Debugger
import           DevTools.API.Profiler.Types


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
data Disable = Disable
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Disable where
    parseJSON A.Null = P.pure Disable
    parseJSON v = A.withArray "disable" go v
        <|> A.withObject "disable" go v
      where
        go _ = P.pure Disable


------------------------------------------------------------------------------
instance A.ToJSON Disable where
    toEncoding Disable = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON Disable = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup Disable where
    Disable <> Disable = Disable


------------------------------------------------------------------------------
instance P.Monoid Disable where
    mempty = Disable


------------------------------------------------------------------------------
instance M.Method Disable where
    type Result Disable = ()
    name _ = "Profiler.disable"


------------------------------------------------------------------------------
disable
    :: Disable
disable = Disable


------------------------------------------------------------------------------
data Enable = Enable
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Enable where
    parseJSON A.Null = P.pure Enable
    parseJSON v = A.withArray "enable" go v
        <|> A.withObject "enable" go v
      where
        go _ = P.pure Enable


------------------------------------------------------------------------------
instance A.ToJSON Enable where
    toEncoding Enable = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON Enable = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup Enable where
    Enable <> Enable = Enable


------------------------------------------------------------------------------
instance P.Monoid Enable where
    mempty = Enable


------------------------------------------------------------------------------
instance M.Method Enable where
    type Result Enable = ()
    name _ = "Profiler.enable"


------------------------------------------------------------------------------
enable
    :: Enable
enable = Enable


------------------------------------------------------------------------------
-- | Collect coverage data for the current isolate. The coverage data may be incomplete due to
-- garbage collection.
data GetBestEffortCoverage = GetBestEffortCoverage
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetBestEffortCoverage where
    parseJSON A.Null = P.pure GetBestEffortCoverage
    parseJSON v = A.withArray "getBestEffortCoverage" go v
        <|> A.withObject "getBestEffortCoverage" go v
      where
        go _ = P.pure GetBestEffortCoverage


------------------------------------------------------------------------------
instance A.ToJSON GetBestEffortCoverage where
    toEncoding GetBestEffortCoverage = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetBestEffortCoverage = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetBestEffortCoverage where
    GetBestEffortCoverage <> GetBestEffortCoverage = GetBestEffortCoverage


------------------------------------------------------------------------------
instance P.Monoid GetBestEffortCoverage where
    mempty = GetBestEffortCoverage


------------------------------------------------------------------------------
-- | Collect coverage data for the current isolate. The coverage data may be incomplete due to
-- garbage collection.
data GetBestEffortCoverageResult = GetBestEffortCoverageResult
    { -- | Coverage data for the current isolate.
      result :: ![ScriptCoverage]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetBestEffortCoverageResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getBestEffortCoverageResult" $ \_o -> GetBestEffortCoverageResult
            <$> _o .: "result"
        ago = A.withArray "getBestEffortCoverageResult" $ \_a -> GetBestEffortCoverageResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetBestEffortCoverageResult where
    toEncoding (GetBestEffortCoverageResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "result" .= _0
        ]
    toJSON (GetBestEffortCoverageResult _0) = A.object $ P.catMaybes
        [ P.pure $ "result" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetBestEffortCoverageResult where
    GetBestEffortCoverageResult _0 <> GetBestEffortCoverageResult _ = GetBestEffortCoverageResult _0


------------------------------------------------------------------------------
instance M.Method GetBestEffortCoverage where
    type Result GetBestEffortCoverage = GetBestEffortCoverageResult
    name _ = "Profiler.getBestEffortCoverage"


------------------------------------------------------------------------------
-- | Collect coverage data for the current isolate. The coverage data may be incomplete due to
-- garbage collection.
getBestEffortCoverage
    :: GetBestEffortCoverage
getBestEffortCoverage = GetBestEffortCoverage


------------------------------------------------------------------------------
-- | Changes CPU profiler sampling interval. Must be called before CPU profiles recording started.
data SetSamplingInterval = SetSamplingInterval
    { -- | New sampling interval in microseconds.
      interval :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetSamplingInterval where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setSamplingInterval" $ \_o -> SetSamplingInterval
            <$> _o .: "interval"
        ago = A.withArray "setSamplingInterval" $ \_a -> SetSamplingInterval
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetSamplingInterval where
    toEncoding (SetSamplingInterval _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "interval" .= _0
        ]
    toJSON (SetSamplingInterval _0) = A.object $ P.catMaybes
        [ P.pure $ "interval" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetSamplingInterval where
    SetSamplingInterval _0 <> SetSamplingInterval _ = SetSamplingInterval _0


------------------------------------------------------------------------------
instance M.Method SetSamplingInterval where
    type Result SetSamplingInterval = ()
    name _ = "Profiler.setSamplingInterval"


------------------------------------------------------------------------------
-- | Changes CPU profiler sampling interval. Must be called before CPU profiles recording started.
setSamplingInterval
    :: P.Int
    -- ^ New sampling interval in microseconds.

    -> SetSamplingInterval
setSamplingInterval _0 = SetSamplingInterval _0


------------------------------------------------------------------------------
data Start = Start
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Start where
    parseJSON A.Null = P.pure Start
    parseJSON v = A.withArray "start" go v
        <|> A.withObject "start" go v
      where
        go _ = P.pure Start


------------------------------------------------------------------------------
instance A.ToJSON Start where
    toEncoding Start = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON Start = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup Start where
    Start <> Start = Start


------------------------------------------------------------------------------
instance P.Monoid Start where
    mempty = Start


------------------------------------------------------------------------------
instance M.Method Start where
    type Result Start = ()
    name _ = "Profiler.start"


------------------------------------------------------------------------------
start
    :: Start
start = Start


------------------------------------------------------------------------------
-- | Enable precise code coverage. Coverage data for JavaScript executed before enabling precise code
-- coverage may be incomplete. Enabling prevents running optimized code and resets execution
-- counters.
data StartPreciseCoverage = StartPreciseCoverage
    { -- | Collect accurate call counts beyond simple 'covered' or 'not covered'.
      callCount :: !(P.Maybe P.Bool)
      -- | Collect block-based coverage.
    , detailed :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StartPreciseCoverage where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "startPreciseCoverage" $ \_o -> StartPreciseCoverage
            <$> _o .:? "callCount"
            <*> _o .:? "detailed"
        ago = A.withArray "startPreciseCoverage" $ \_a -> StartPreciseCoverage
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON StartPreciseCoverage where
    toEncoding (StartPreciseCoverage _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ ("callCount" .=) <$> _0
        , ("detailed" .=) <$> _1
        ]
    toJSON (StartPreciseCoverage _0 _1) = A.object $ P.catMaybes
        [ ("callCount" .=) <$> _0
        , ("detailed" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup StartPreciseCoverage where
    StartPreciseCoverage _0 _1 <> StartPreciseCoverage __0 __1 = StartPreciseCoverage (_0 <|> __0) (_1 <|> __1)


------------------------------------------------------------------------------
instance P.Monoid StartPreciseCoverage where
    mempty = StartPreciseCoverage P.empty P.empty


------------------------------------------------------------------------------
instance M.Method StartPreciseCoverage where
    type Result StartPreciseCoverage = ()
    name _ = "Profiler.startPreciseCoverage"


------------------------------------------------------------------------------
-- | Enable precise code coverage. Coverage data for JavaScript executed before enabling precise code
-- coverage may be incomplete. Enabling prevents running optimized code and resets execution
-- counters.
startPreciseCoverage
    :: StartPreciseCoverage
startPreciseCoverage = StartPreciseCoverage P.empty P.empty


------------------------------------------------------------------------------
-- | Enable type profile.
{-# WARNING StartTypeProfile "This feature is marked as EXPERIMENTAL." #-}
data StartTypeProfile = StartTypeProfile
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StartTypeProfile where
    parseJSON A.Null = P.pure StartTypeProfile
    parseJSON v = A.withArray "startTypeProfile" go v
        <|> A.withObject "startTypeProfile" go v
      where
        go _ = P.pure StartTypeProfile


------------------------------------------------------------------------------
instance A.ToJSON StartTypeProfile where
    toEncoding StartTypeProfile = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON StartTypeProfile = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup StartTypeProfile where
    StartTypeProfile <> StartTypeProfile = StartTypeProfile


------------------------------------------------------------------------------
instance P.Monoid StartTypeProfile where
    mempty = StartTypeProfile


------------------------------------------------------------------------------
instance M.Method StartTypeProfile where
    type Result StartTypeProfile = ()
    name _ = "Profiler.startTypeProfile"


------------------------------------------------------------------------------
-- | Enable type profile.
{-# WARNING startTypeProfile "This feature is marked as EXPERIMENTAL." #-}
startTypeProfile
    :: StartTypeProfile
startTypeProfile = StartTypeProfile


------------------------------------------------------------------------------
data Stop = Stop
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Stop where
    parseJSON A.Null = P.pure Stop
    parseJSON v = A.withArray "stop" go v
        <|> A.withObject "stop" go v
      where
        go _ = P.pure Stop


------------------------------------------------------------------------------
instance A.ToJSON Stop where
    toEncoding Stop = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON Stop = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup Stop where
    Stop <> Stop = Stop


------------------------------------------------------------------------------
instance P.Monoid Stop where
    mempty = Stop


------------------------------------------------------------------------------
data StopResult = StopResult
    { -- | Recorded profile.
      profile :: !Profile
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StopResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "stopResult" $ \_o -> StopResult
            <$> _o .: "profile"
        ago = A.withArray "stopResult" $ \_a -> StopResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON StopResult where
    toEncoding (StopResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "profile" .= _0
        ]
    toJSON (StopResult _0) = A.object $ P.catMaybes
        [ P.pure $ "profile" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup StopResult where
    StopResult _0 <> StopResult _ = StopResult _0


------------------------------------------------------------------------------
instance M.Method Stop where
    type Result Stop = StopResult
    name _ = "Profiler.stop"


------------------------------------------------------------------------------
stop
    :: Stop
stop = Stop


------------------------------------------------------------------------------
-- | Disable precise code coverage. Disabling releases unnecessary execution count records and allows
-- executing optimized code.
data StopPreciseCoverage = StopPreciseCoverage
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StopPreciseCoverage where
    parseJSON A.Null = P.pure StopPreciseCoverage
    parseJSON v = A.withArray "stopPreciseCoverage" go v
        <|> A.withObject "stopPreciseCoverage" go v
      where
        go _ = P.pure StopPreciseCoverage


------------------------------------------------------------------------------
instance A.ToJSON StopPreciseCoverage where
    toEncoding StopPreciseCoverage = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON StopPreciseCoverage = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup StopPreciseCoverage where
    StopPreciseCoverage <> StopPreciseCoverage = StopPreciseCoverage


------------------------------------------------------------------------------
instance P.Monoid StopPreciseCoverage where
    mempty = StopPreciseCoverage


------------------------------------------------------------------------------
instance M.Method StopPreciseCoverage where
    type Result StopPreciseCoverage = ()
    name _ = "Profiler.stopPreciseCoverage"


------------------------------------------------------------------------------
-- | Disable precise code coverage. Disabling releases unnecessary execution count records and allows
-- executing optimized code.
stopPreciseCoverage
    :: StopPreciseCoverage
stopPreciseCoverage = StopPreciseCoverage


------------------------------------------------------------------------------
-- | Disable type profile. Disabling releases type profile data collected so far.
{-# WARNING StopTypeProfile "This feature is marked as EXPERIMENTAL." #-}
data StopTypeProfile = StopTypeProfile
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StopTypeProfile where
    parseJSON A.Null = P.pure StopTypeProfile
    parseJSON v = A.withArray "stopTypeProfile" go v
        <|> A.withObject "stopTypeProfile" go v
      where
        go _ = P.pure StopTypeProfile


------------------------------------------------------------------------------
instance A.ToJSON StopTypeProfile where
    toEncoding StopTypeProfile = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON StopTypeProfile = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup StopTypeProfile where
    StopTypeProfile <> StopTypeProfile = StopTypeProfile


------------------------------------------------------------------------------
instance P.Monoid StopTypeProfile where
    mempty = StopTypeProfile


------------------------------------------------------------------------------
instance M.Method StopTypeProfile where
    type Result StopTypeProfile = ()
    name _ = "Profiler.stopTypeProfile"


------------------------------------------------------------------------------
-- | Disable type profile. Disabling releases type profile data collected so far.
{-# WARNING stopTypeProfile "This feature is marked as EXPERIMENTAL." #-}
stopTypeProfile
    :: StopTypeProfile
stopTypeProfile = StopTypeProfile


------------------------------------------------------------------------------
-- | Collect coverage data for the current isolate, and resets execution counters. Precise code
-- coverage needs to have started.
data TakePreciseCoverage = TakePreciseCoverage
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TakePreciseCoverage where
    parseJSON A.Null = P.pure TakePreciseCoverage
    parseJSON v = A.withArray "takePreciseCoverage" go v
        <|> A.withObject "takePreciseCoverage" go v
      where
        go _ = P.pure TakePreciseCoverage


------------------------------------------------------------------------------
instance A.ToJSON TakePreciseCoverage where
    toEncoding TakePreciseCoverage = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON TakePreciseCoverage = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup TakePreciseCoverage where
    TakePreciseCoverage <> TakePreciseCoverage = TakePreciseCoverage


------------------------------------------------------------------------------
instance P.Monoid TakePreciseCoverage where
    mempty = TakePreciseCoverage


------------------------------------------------------------------------------
-- | Collect coverage data for the current isolate, and resets execution counters. Precise code
-- coverage needs to have started.
data TakePreciseCoverageResult = TakePreciseCoverageResult
    { -- | Coverage data for the current isolate.
      result :: ![ScriptCoverage]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TakePreciseCoverageResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "takePreciseCoverageResult" $ \_o -> TakePreciseCoverageResult
            <$> _o .: "result"
        ago = A.withArray "takePreciseCoverageResult" $ \_a -> TakePreciseCoverageResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON TakePreciseCoverageResult where
    toEncoding (TakePreciseCoverageResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "result" .= _0
        ]
    toJSON (TakePreciseCoverageResult _0) = A.object $ P.catMaybes
        [ P.pure $ "result" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup TakePreciseCoverageResult where
    TakePreciseCoverageResult _0 <> TakePreciseCoverageResult _ = TakePreciseCoverageResult _0


------------------------------------------------------------------------------
instance M.Method TakePreciseCoverage where
    type Result TakePreciseCoverage = TakePreciseCoverageResult
    name _ = "Profiler.takePreciseCoverage"


------------------------------------------------------------------------------
-- | Collect coverage data for the current isolate, and resets execution counters. Precise code
-- coverage needs to have started.
takePreciseCoverage
    :: TakePreciseCoverage
takePreciseCoverage = TakePreciseCoverage


------------------------------------------------------------------------------
-- | Collect type profile.
{-# WARNING TakeTypeProfile "This feature is marked as EXPERIMENTAL." #-}
data TakeTypeProfile = TakeTypeProfile
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TakeTypeProfile where
    parseJSON A.Null = P.pure TakeTypeProfile
    parseJSON v = A.withArray "takeTypeProfile" go v
        <|> A.withObject "takeTypeProfile" go v
      where
        go _ = P.pure TakeTypeProfile


------------------------------------------------------------------------------
instance A.ToJSON TakeTypeProfile where
    toEncoding TakeTypeProfile = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON TakeTypeProfile = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup TakeTypeProfile where
    TakeTypeProfile <> TakeTypeProfile = TakeTypeProfile


------------------------------------------------------------------------------
instance P.Monoid TakeTypeProfile where
    mempty = TakeTypeProfile


------------------------------------------------------------------------------
-- | Collect type profile.
{-# WARNING TakeTypeProfileResult "This feature is marked as EXPERIMENTAL." #-}
data TakeTypeProfileResult = TakeTypeProfileResult
    { -- | Type profile for all scripts since startTypeProfile() was turned on.
      result :: ![ScriptTypeProfile]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TakeTypeProfileResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "takeTypeProfileResult" $ \_o -> TakeTypeProfileResult
            <$> _o .: "result"
        ago = A.withArray "takeTypeProfileResult" $ \_a -> TakeTypeProfileResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON TakeTypeProfileResult where
    toEncoding (TakeTypeProfileResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "result" .= _0
        ]
    toJSON (TakeTypeProfileResult _0) = A.object $ P.catMaybes
        [ P.pure $ "result" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup TakeTypeProfileResult where
    TakeTypeProfileResult _0 <> TakeTypeProfileResult _ = TakeTypeProfileResult _0


------------------------------------------------------------------------------
instance M.Method TakeTypeProfile where
    type Result TakeTypeProfile = TakeTypeProfileResult
    name _ = "Profiler.takeTypeProfile"


------------------------------------------------------------------------------
-- | Collect type profile.
{-# WARNING takeTypeProfile "This feature is marked as EXPERIMENTAL." #-}
takeTypeProfile
    :: TakeTypeProfile
takeTypeProfile = TakeTypeProfile


------------------------------------------------------------------------------
data ConsoleProfileFinished = ConsoleProfileFinished
    { id :: !T.Text
      -- | Location of console.profileEnd().
    , location :: !Debugger.Location
    , profile :: !Profile
      -- | Profile title passed as an argument to console.profile().
    , title :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ConsoleProfileFinished where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "consoleProfileFinished" $ \_o -> ConsoleProfileFinished
            <$> _o .: "id"
            <*> _o .: "location"
            <*> _o .: "profile"
            <*> _o .:? "title"
        ago = A.withArray "consoleProfileFinished" $ \_a -> ConsoleProfileFinished
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON ConsoleProfileFinished where
    toEncoding (ConsoleProfileFinished _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "id" .= _0
        , P.pure $ "location" .= _1
        , P.pure $ "profile" .= _2
        , ("title" .=) <$> _3
        ]
    toJSON (ConsoleProfileFinished _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "id" .= _0
        , P.pure $ "location" .= _1
        , P.pure $ "profile" .= _2
        , ("title" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup ConsoleProfileFinished where
    ConsoleProfileFinished _0 _1 _2 _3 <> ConsoleProfileFinished _ _ _ __3 = ConsoleProfileFinished _0 _1 _2 (_3 <|> __3)


------------------------------------------------------------------------------
instance E.Event ConsoleProfileFinished where
    type Result ConsoleProfileFinished = ConsoleProfileFinished
    name _ = "Profiler.consoleProfileFinished"


------------------------------------------------------------------------------
consoleProfileFinished :: P.Proxy ConsoleProfileFinished
consoleProfileFinished = P.Proxy


------------------------------------------------------------------------------
-- | Sent when new profile recording is started using console.profile() call.
data ConsoleProfileStarted = ConsoleProfileStarted
    { id :: !T.Text
      -- | Location of console.profile().
    , location :: !Debugger.Location
      -- | Profile title passed as an argument to console.profile().
    , title :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ConsoleProfileStarted where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "consoleProfileStarted" $ \_o -> ConsoleProfileStarted
            <$> _o .: "id"
            <*> _o .: "location"
            <*> _o .:? "title"
        ago = A.withArray "consoleProfileStarted" $ \_a -> ConsoleProfileStarted
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON ConsoleProfileStarted where
    toEncoding (ConsoleProfileStarted _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "id" .= _0
        , P.pure $ "location" .= _1
        , ("title" .=) <$> _2
        ]
    toJSON (ConsoleProfileStarted _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "id" .= _0
        , P.pure $ "location" .= _1
        , ("title" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup ConsoleProfileStarted where
    ConsoleProfileStarted _0 _1 _2 <> ConsoleProfileStarted _ _ __2 = ConsoleProfileStarted _0 _1 (_2 <|> __2)


------------------------------------------------------------------------------
instance E.Event ConsoleProfileStarted where
    type Result ConsoleProfileStarted = ConsoleProfileStarted
    name _ = "Profiler.consoleProfileStarted"


------------------------------------------------------------------------------
-- | Sent when new profile recording is started using console.profile() call.
consoleProfileStarted :: P.Proxy ConsoleProfileStarted
consoleProfileStarted = P.Proxy

