{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Defines events for background web platform features.
module DevTools.API.BackgroundService.Types{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

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


-- devtools-api---------------------------------------------------------------
import qualified DevTools.API.Network.Types as Network
import qualified DevTools.API.ServiceWorker.Types as ServiceWorker


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | The Background Service that will be associated with the commands\/events.
-- Every Background Service operates independently, but they share the same
-- API.
data ServiceName
    = BackgroundFetch
    | BackgroundSync
    | PushMessaging
    | Notifications
    | PaymentHandler
    | PeriodicBackgroundSync
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ServiceName where
    parseJSON = A.withText "ServiceName" $ \t -> case t of
        "backgroundFetch" -> P.pure BackgroundFetch
        "backgroundSync" -> P.pure BackgroundSync
        "pushMessaging" -> P.pure PushMessaging
        "notifications" -> P.pure Notifications
        "paymentHandler" -> P.pure PaymentHandler
        "periodicBackgroundSync" -> P.pure PeriodicBackgroundSync
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON ServiceName where
    toJSON BackgroundFetch = "backgroundFetch"
    toJSON BackgroundSync = "backgroundSync"
    toJSON PushMessaging = "pushMessaging"
    toJSON Notifications = "notifications"
    toJSON PaymentHandler = "paymentHandler"
    toJSON PeriodicBackgroundSync = "periodicBackgroundSync"


------------------------------------------------------------------------------
-- | A key-value pair for additional event information to pass along.
data EventMetadata = EventMetadata
    { key :: !T.Text
    , value :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON EventMetadata where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "EventMetadata" $ \_o -> EventMetadata
            <$> _o .: "key"
            <*> _o .: "value"
        ago = A.withArray "EventMetadata" $ \_a -> EventMetadata
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON EventMetadata where
    toEncoding (EventMetadata _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "key" .= _0
        , P.pure $ "value" .= _1
        ]
    toJSON (EventMetadata _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "key" .= _0
        , P.pure $ "value" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup EventMetadata where
    EventMetadata _0 _1 <> EventMetadata _ _ = EventMetadata _0 _1


------------------------------------------------------------------------------
data BackgroundServiceEvent = BackgroundServiceEvent
    { -- | Timestamp of the event (in seconds).
      timestamp :: !Network.TimeSinceEpoch
      -- | The origin this event belongs to.
    , origin :: !T.Text
      -- | The Service Worker ID that initiated the event.
    , serviceWorkerRegistrationId :: !ServiceWorker.RegistrationID
      -- | The Background Service this event belongs to.
    , service :: !ServiceName
      -- | A description of the event.
    , eventName :: !T.Text
      -- | An identifier that groups related events together.
    , instanceId :: !T.Text
      -- | A list of event-specific information.
    , eventMetadata :: ![EventMetadata]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON BackgroundServiceEvent where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "BackgroundServiceEvent" $ \_o -> BackgroundServiceEvent
            <$> _o .: "timestamp"
            <*> _o .: "origin"
            <*> _o .: "serviceWorkerRegistrationId"
            <*> _o .: "service"
            <*> _o .: "eventName"
            <*> _o .: "instanceId"
            <*> _o .: "eventMetadata"
        ago = A.withArray "BackgroundServiceEvent" $ \_a -> BackgroundServiceEvent
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.maybe P.empty A.parseJSON (_a !? 6)


------------------------------------------------------------------------------
instance A.ToJSON BackgroundServiceEvent where
    toEncoding (BackgroundServiceEvent _0 _1 _2 _3 _4 _5 _6) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "timestamp" .= _0
        , P.pure $ "origin" .= _1
        , P.pure $ "serviceWorkerRegistrationId" .= _2
        , P.pure $ "service" .= _3
        , P.pure $ "eventName" .= _4
        , P.pure $ "instanceId" .= _5
        , P.pure $ "eventMetadata" .= _6
        ]
    toJSON (BackgroundServiceEvent _0 _1 _2 _3 _4 _5 _6) = A.object $ P.catMaybes
        [ P.pure $ "timestamp" .= _0
        , P.pure $ "origin" .= _1
        , P.pure $ "serviceWorkerRegistrationId" .= _2
        , P.pure $ "service" .= _3
        , P.pure $ "eventName" .= _4
        , P.pure $ "instanceId" .= _5
        , P.pure $ "eventMetadata" .= _6
        ]


------------------------------------------------------------------------------
instance P.Semigroup BackgroundServiceEvent where
    BackgroundServiceEvent _0 _1 _2 _3 _4 _5 _6 <> BackgroundServiceEvent _ _ _ _ _ _ _ = BackgroundServiceEvent _0 _1 _2 _3 _4 _5 _6

