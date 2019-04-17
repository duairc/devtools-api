{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Defines events for background web platform features.
module DevTools.API.BackgroundService{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

    ( module DevTools.API.BackgroundService.Types
    , module DevTools.API.BackgroundService
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
import           DevTools.API.BackgroundService.Types


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Enables event updates for the service.
data StartObserving = StartObserving
    { service :: !ServiceName
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StartObserving where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "startObserving" $ \_o -> StartObserving
            <$> _o .: "service"
        ago = A.withArray "startObserving" $ \_a -> StartObserving
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON StartObserving where
    toEncoding (StartObserving _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "service" .= _0
        ]
    toJSON (StartObserving _0) = A.object $ P.catMaybes
        [ P.pure $ "service" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup StartObserving where
    StartObserving _0 <> StartObserving _ = StartObserving _0


------------------------------------------------------------------------------
instance M.Method StartObserving where
    type Result StartObserving = ()
    name _ = "BackgroundService.startObserving"


------------------------------------------------------------------------------
-- | Enables event updates for the service.
startObserving
    :: ServiceName
    -> StartObserving
startObserving _0 = StartObserving _0


------------------------------------------------------------------------------
-- | Disables event updates for the service.
data StopObserving = StopObserving
    { service :: !ServiceName
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StopObserving where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "stopObserving" $ \_o -> StopObserving
            <$> _o .: "service"
        ago = A.withArray "stopObserving" $ \_a -> StopObserving
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON StopObserving where
    toEncoding (StopObserving _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "service" .= _0
        ]
    toJSON (StopObserving _0) = A.object $ P.catMaybes
        [ P.pure $ "service" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup StopObserving where
    StopObserving _0 <> StopObserving _ = StopObserving _0


------------------------------------------------------------------------------
instance M.Method StopObserving where
    type Result StopObserving = ()
    name _ = "BackgroundService.stopObserving"


------------------------------------------------------------------------------
-- | Disables event updates for the service.
stopObserving
    :: ServiceName
    -> StopObserving
stopObserving _0 = StopObserving _0


------------------------------------------------------------------------------
-- | Set the recording state for the service.
data SetRecording = SetRecording
    { shouldRecord :: !P.Bool
    , service :: !ServiceName
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetRecording where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setRecording" $ \_o -> SetRecording
            <$> _o .: "shouldRecord"
            <*> _o .: "service"
        ago = A.withArray "setRecording" $ \_a -> SetRecording
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SetRecording where
    toEncoding (SetRecording _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "shouldRecord" .= _0
        , P.pure $ "service" .= _1
        ]
    toJSON (SetRecording _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "shouldRecord" .= _0
        , P.pure $ "service" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetRecording where
    SetRecording _0 _1 <> SetRecording _ _ = SetRecording _0 _1


------------------------------------------------------------------------------
instance M.Method SetRecording where
    type Result SetRecording = ()
    name _ = "BackgroundService.setRecording"


------------------------------------------------------------------------------
-- | Set the recording state for the service.
setRecording
    :: P.Bool
    -> ServiceName
    -> SetRecording
setRecording _0 _1 = SetRecording _0 _1


------------------------------------------------------------------------------
-- | Clears all stored data for the service.
data ClearEvents = ClearEvents
    { service :: !ServiceName
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ClearEvents where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "clearEvents" $ \_o -> ClearEvents
            <$> _o .: "service"
        ago = A.withArray "clearEvents" $ \_a -> ClearEvents
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON ClearEvents where
    toEncoding (ClearEvents _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "service" .= _0
        ]
    toJSON (ClearEvents _0) = A.object $ P.catMaybes
        [ P.pure $ "service" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup ClearEvents where
    ClearEvents _0 <> ClearEvents _ = ClearEvents _0


------------------------------------------------------------------------------
instance M.Method ClearEvents where
    type Result ClearEvents = ()
    name _ = "BackgroundService.clearEvents"


------------------------------------------------------------------------------
-- | Clears all stored data for the service.
clearEvents
    :: ServiceName
    -> ClearEvents
clearEvents _0 = ClearEvents _0


------------------------------------------------------------------------------
-- | Called when the recording state for the service has been updated.
data RecordingStateChanged = RecordingStateChanged
    { isRecording :: !P.Bool
    , service :: !ServiceName
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RecordingStateChanged where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "recordingStateChanged" $ \_o -> RecordingStateChanged
            <$> _o .: "isRecording"
            <*> _o .: "service"
        ago = A.withArray "recordingStateChanged" $ \_a -> RecordingStateChanged
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON RecordingStateChanged where
    toEncoding (RecordingStateChanged _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "isRecording" .= _0
        , P.pure $ "service" .= _1
        ]
    toJSON (RecordingStateChanged _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "isRecording" .= _0
        , P.pure $ "service" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup RecordingStateChanged where
    RecordingStateChanged _0 _1 <> RecordingStateChanged _ _ = RecordingStateChanged _0 _1


------------------------------------------------------------------------------
instance E.Event RecordingStateChanged where
    type Result RecordingStateChanged = RecordingStateChanged
    name _ = "BackgroundService.recordingStateChanged"


------------------------------------------------------------------------------
-- | Called when the recording state for the service has been updated.
recordingStateChanged :: P.Proxy RecordingStateChanged
recordingStateChanged = P.Proxy


------------------------------------------------------------------------------
-- | Called with all existing backgroundServiceEvents when enabled, and all new
-- events afterwards if enabled and recording.
data BackgroundServiceEventReceived = BackgroundServiceEventReceived
    { backgroundServiceEvent :: !BackgroundServiceEvent
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON BackgroundServiceEventReceived where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "backgroundServiceEventReceived" $ \_o -> BackgroundServiceEventReceived
            <$> _o .: "backgroundServiceEvent"
        ago = A.withArray "backgroundServiceEventReceived" $ \_a -> BackgroundServiceEventReceived
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON BackgroundServiceEventReceived where
    toEncoding (BackgroundServiceEventReceived _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "backgroundServiceEvent" .= _0
        ]
    toJSON (BackgroundServiceEventReceived _0) = A.object $ P.catMaybes
        [ P.pure $ "backgroundServiceEvent" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup BackgroundServiceEventReceived where
    BackgroundServiceEventReceived _0 <> BackgroundServiceEventReceived _ = BackgroundServiceEventReceived _0


------------------------------------------------------------------------------
instance E.Event BackgroundServiceEventReceived where
    type Result BackgroundServiceEventReceived = BackgroundServiceEventReceived
    name _ = "BackgroundService.backgroundServiceEventReceived"


------------------------------------------------------------------------------
-- | Called with all existing backgroundServiceEvents when enabled, and all new
-- events afterwards if enabled and recording.
backgroundServiceEventReceived :: P.Proxy BackgroundServiceEventReceived
backgroundServiceEventReceived = P.Proxy

