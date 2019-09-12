{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.ServiceWorker{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

    ( module DevTools.API.ServiceWorker.Types
    , module DevTools.API.ServiceWorker
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
import           DevTools.API.ServiceWorker.Types


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
data DeliverPushMessage = DeliverPushMessage
    { origin :: !T.Text
    , registrationId :: !RegistrationID
    , data_ :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DeliverPushMessage where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "deliverPushMessage" $ \_o -> DeliverPushMessage
            <$> _o .: "origin"
            <*> _o .: "registrationId"
            <*> _o .: "data"
        ago = A.withArray "deliverPushMessage" $ \_a -> DeliverPushMessage
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON DeliverPushMessage where
    toEncoding (DeliverPushMessage _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "origin" .= _0
        , P.pure $ "registrationId" .= _1
        , P.pure $ "data" .= _2
        ]
    toJSON (DeliverPushMessage _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "origin" .= _0
        , P.pure $ "registrationId" .= _1
        , P.pure $ "data" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup DeliverPushMessage where
    DeliverPushMessage _0 _1 _2 <> DeliverPushMessage _ _ _ = DeliverPushMessage _0 _1 _2


------------------------------------------------------------------------------
instance M.Method DeliverPushMessage where
    type Result DeliverPushMessage = ()
    name _ = "ServiceWorker.deliverPushMessage"


------------------------------------------------------------------------------
deliverPushMessage
    :: T.Text
    -> RegistrationID
    -> T.Text
    -> DeliverPushMessage
deliverPushMessage _0 _1 _2 = DeliverPushMessage _0 _1 _2


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
    name _ = "ServiceWorker.disable"


------------------------------------------------------------------------------
disable
    :: Disable
disable = Disable


------------------------------------------------------------------------------
data DispatchSyncEvent = DispatchSyncEvent
    { origin :: !T.Text
    , registrationId :: !RegistrationID
    , tag :: !T.Text
    , lastChance :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DispatchSyncEvent where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "dispatchSyncEvent" $ \_o -> DispatchSyncEvent
            <$> _o .: "origin"
            <*> _o .: "registrationId"
            <*> _o .: "tag"
            <*> _o .: "lastChance"
        ago = A.withArray "dispatchSyncEvent" $ \_a -> DispatchSyncEvent
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON DispatchSyncEvent where
    toEncoding (DispatchSyncEvent _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "origin" .= _0
        , P.pure $ "registrationId" .= _1
        , P.pure $ "tag" .= _2
        , P.pure $ "lastChance" .= _3
        ]
    toJSON (DispatchSyncEvent _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "origin" .= _0
        , P.pure $ "registrationId" .= _1
        , P.pure $ "tag" .= _2
        , P.pure $ "lastChance" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup DispatchSyncEvent where
    DispatchSyncEvent _0 _1 _2 _3 <> DispatchSyncEvent _ _ _ _ = DispatchSyncEvent _0 _1 _2 _3


------------------------------------------------------------------------------
instance M.Method DispatchSyncEvent where
    type Result DispatchSyncEvent = ()
    name _ = "ServiceWorker.dispatchSyncEvent"


------------------------------------------------------------------------------
dispatchSyncEvent
    :: T.Text
    -> RegistrationID
    -> T.Text
    -> P.Bool
    -> DispatchSyncEvent
dispatchSyncEvent _0 _1 _2 _3 = DispatchSyncEvent _0 _1 _2 _3


------------------------------------------------------------------------------
data DispatchPeriodicSyncEvent = DispatchPeriodicSyncEvent
    { origin :: !T.Text
    , registrationId :: !RegistrationID
    , tag :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DispatchPeriodicSyncEvent where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "dispatchPeriodicSyncEvent" $ \_o -> DispatchPeriodicSyncEvent
            <$> _o .: "origin"
            <*> _o .: "registrationId"
            <*> _o .: "tag"
        ago = A.withArray "dispatchPeriodicSyncEvent" $ \_a -> DispatchPeriodicSyncEvent
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON DispatchPeriodicSyncEvent where
    toEncoding (DispatchPeriodicSyncEvent _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "origin" .= _0
        , P.pure $ "registrationId" .= _1
        , P.pure $ "tag" .= _2
        ]
    toJSON (DispatchPeriodicSyncEvent _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "origin" .= _0
        , P.pure $ "registrationId" .= _1
        , P.pure $ "tag" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup DispatchPeriodicSyncEvent where
    DispatchPeriodicSyncEvent _0 _1 _2 <> DispatchPeriodicSyncEvent _ _ _ = DispatchPeriodicSyncEvent _0 _1 _2


------------------------------------------------------------------------------
instance M.Method DispatchPeriodicSyncEvent where
    type Result DispatchPeriodicSyncEvent = ()
    name _ = "ServiceWorker.dispatchPeriodicSyncEvent"


------------------------------------------------------------------------------
dispatchPeriodicSyncEvent
    :: T.Text
    -> RegistrationID
    -> T.Text
    -> DispatchPeriodicSyncEvent
dispatchPeriodicSyncEvent _0 _1 _2 = DispatchPeriodicSyncEvent _0 _1 _2


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
    name _ = "ServiceWorker.enable"


------------------------------------------------------------------------------
enable
    :: Enable
enable = Enable


------------------------------------------------------------------------------
data InspectWorker = InspectWorker
    { versionId :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON InspectWorker where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "inspectWorker" $ \_o -> InspectWorker
            <$> _o .: "versionId"
        ago = A.withArray "inspectWorker" $ \_a -> InspectWorker
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON InspectWorker where
    toEncoding (InspectWorker _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "versionId" .= _0
        ]
    toJSON (InspectWorker _0) = A.object $ P.catMaybes
        [ P.pure $ "versionId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup InspectWorker where
    InspectWorker _0 <> InspectWorker _ = InspectWorker _0


------------------------------------------------------------------------------
instance M.Method InspectWorker where
    type Result InspectWorker = ()
    name _ = "ServiceWorker.inspectWorker"


------------------------------------------------------------------------------
inspectWorker
    :: T.Text
    -> InspectWorker
inspectWorker _0 = InspectWorker _0


------------------------------------------------------------------------------
data SetForceUpdateOnPageLoad = SetForceUpdateOnPageLoad
    { forceUpdateOnPageLoad :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetForceUpdateOnPageLoad where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setForceUpdateOnPageLoad" $ \_o -> SetForceUpdateOnPageLoad
            <$> _o .: "forceUpdateOnPageLoad"
        ago = A.withArray "setForceUpdateOnPageLoad" $ \_a -> SetForceUpdateOnPageLoad
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetForceUpdateOnPageLoad where
    toEncoding (SetForceUpdateOnPageLoad _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "forceUpdateOnPageLoad" .= _0
        ]
    toJSON (SetForceUpdateOnPageLoad _0) = A.object $ P.catMaybes
        [ P.pure $ "forceUpdateOnPageLoad" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetForceUpdateOnPageLoad where
    SetForceUpdateOnPageLoad _0 <> SetForceUpdateOnPageLoad _ = SetForceUpdateOnPageLoad _0


------------------------------------------------------------------------------
instance M.Method SetForceUpdateOnPageLoad where
    type Result SetForceUpdateOnPageLoad = ()
    name _ = "ServiceWorker.setForceUpdateOnPageLoad"


------------------------------------------------------------------------------
setForceUpdateOnPageLoad
    :: P.Bool
    -> SetForceUpdateOnPageLoad
setForceUpdateOnPageLoad _0 = SetForceUpdateOnPageLoad _0


------------------------------------------------------------------------------
data SkipWaiting = SkipWaiting
    { scopeURL :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SkipWaiting where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "skipWaiting" $ \_o -> SkipWaiting
            <$> _o .: "scopeURL"
        ago = A.withArray "skipWaiting" $ \_a -> SkipWaiting
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SkipWaiting where
    toEncoding (SkipWaiting _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "scopeURL" .= _0
        ]
    toJSON (SkipWaiting _0) = A.object $ P.catMaybes
        [ P.pure $ "scopeURL" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SkipWaiting where
    SkipWaiting _0 <> SkipWaiting _ = SkipWaiting _0


------------------------------------------------------------------------------
instance M.Method SkipWaiting where
    type Result SkipWaiting = ()
    name _ = "ServiceWorker.skipWaiting"


------------------------------------------------------------------------------
skipWaiting
    :: T.Text
    -> SkipWaiting
skipWaiting _0 = SkipWaiting _0


------------------------------------------------------------------------------
data StartWorker = StartWorker
    { scopeURL :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StartWorker where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "startWorker" $ \_o -> StartWorker
            <$> _o .: "scopeURL"
        ago = A.withArray "startWorker" $ \_a -> StartWorker
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON StartWorker where
    toEncoding (StartWorker _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "scopeURL" .= _0
        ]
    toJSON (StartWorker _0) = A.object $ P.catMaybes
        [ P.pure $ "scopeURL" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup StartWorker where
    StartWorker _0 <> StartWorker _ = StartWorker _0


------------------------------------------------------------------------------
instance M.Method StartWorker where
    type Result StartWorker = ()
    name _ = "ServiceWorker.startWorker"


------------------------------------------------------------------------------
startWorker
    :: T.Text
    -> StartWorker
startWorker _0 = StartWorker _0


------------------------------------------------------------------------------
data StopAllWorkers = StopAllWorkers
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StopAllWorkers where
    parseJSON A.Null = P.pure StopAllWorkers
    parseJSON v = A.withArray "stopAllWorkers" go v
        <|> A.withObject "stopAllWorkers" go v
      where
        go _ = P.pure StopAllWorkers


------------------------------------------------------------------------------
instance A.ToJSON StopAllWorkers where
    toEncoding StopAllWorkers = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON StopAllWorkers = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup StopAllWorkers where
    StopAllWorkers <> StopAllWorkers = StopAllWorkers


------------------------------------------------------------------------------
instance P.Monoid StopAllWorkers where
    mempty = StopAllWorkers


------------------------------------------------------------------------------
instance M.Method StopAllWorkers where
    type Result StopAllWorkers = ()
    name _ = "ServiceWorker.stopAllWorkers"


------------------------------------------------------------------------------
stopAllWorkers
    :: StopAllWorkers
stopAllWorkers = StopAllWorkers


------------------------------------------------------------------------------
data StopWorker = StopWorker
    { versionId :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StopWorker where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "stopWorker" $ \_o -> StopWorker
            <$> _o .: "versionId"
        ago = A.withArray "stopWorker" $ \_a -> StopWorker
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON StopWorker where
    toEncoding (StopWorker _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "versionId" .= _0
        ]
    toJSON (StopWorker _0) = A.object $ P.catMaybes
        [ P.pure $ "versionId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup StopWorker where
    StopWorker _0 <> StopWorker _ = StopWorker _0


------------------------------------------------------------------------------
instance M.Method StopWorker where
    type Result StopWorker = ()
    name _ = "ServiceWorker.stopWorker"


------------------------------------------------------------------------------
stopWorker
    :: T.Text
    -> StopWorker
stopWorker _0 = StopWorker _0


------------------------------------------------------------------------------
data Unregister = Unregister
    { scopeURL :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Unregister where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "unregister" $ \_o -> Unregister
            <$> _o .: "scopeURL"
        ago = A.withArray "unregister" $ \_a -> Unregister
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON Unregister where
    toEncoding (Unregister _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "scopeURL" .= _0
        ]
    toJSON (Unregister _0) = A.object $ P.catMaybes
        [ P.pure $ "scopeURL" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup Unregister where
    Unregister _0 <> Unregister _ = Unregister _0


------------------------------------------------------------------------------
instance M.Method Unregister where
    type Result Unregister = ()
    name _ = "ServiceWorker.unregister"


------------------------------------------------------------------------------
unregister
    :: T.Text
    -> Unregister
unregister _0 = Unregister _0


------------------------------------------------------------------------------
data UpdateRegistration = UpdateRegistration
    { scopeURL :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON UpdateRegistration where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "updateRegistration" $ \_o -> UpdateRegistration
            <$> _o .: "scopeURL"
        ago = A.withArray "updateRegistration" $ \_a -> UpdateRegistration
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON UpdateRegistration where
    toEncoding (UpdateRegistration _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "scopeURL" .= _0
        ]
    toJSON (UpdateRegistration _0) = A.object $ P.catMaybes
        [ P.pure $ "scopeURL" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup UpdateRegistration where
    UpdateRegistration _0 <> UpdateRegistration _ = UpdateRegistration _0


------------------------------------------------------------------------------
instance M.Method UpdateRegistration where
    type Result UpdateRegistration = ()
    name _ = "ServiceWorker.updateRegistration"


------------------------------------------------------------------------------
updateRegistration
    :: T.Text
    -> UpdateRegistration
updateRegistration _0 = UpdateRegistration _0


------------------------------------------------------------------------------
data WorkerErrorReported = WorkerErrorReported
    { errorMessage :: !ServiceWorkerErrorMessage
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON WorkerErrorReported where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "workerErrorReported" $ \_o -> WorkerErrorReported
            <$> _o .: "errorMessage"
        ago = A.withArray "workerErrorReported" $ \_a -> WorkerErrorReported
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON WorkerErrorReported where
    toEncoding (WorkerErrorReported _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "errorMessage" .= _0
        ]
    toJSON (WorkerErrorReported _0) = A.object $ P.catMaybes
        [ P.pure $ "errorMessage" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup WorkerErrorReported where
    WorkerErrorReported _0 <> WorkerErrorReported _ = WorkerErrorReported _0


------------------------------------------------------------------------------
instance E.Event WorkerErrorReported where
    type Result WorkerErrorReported = WorkerErrorReported
    name _ = "ServiceWorker.workerErrorReported"


------------------------------------------------------------------------------
workerErrorReported :: P.Proxy WorkerErrorReported
workerErrorReported = P.Proxy


------------------------------------------------------------------------------
data WorkerRegistrationUpdated = WorkerRegistrationUpdated
    { registrations :: ![ServiceWorkerRegistration]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON WorkerRegistrationUpdated where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "workerRegistrationUpdated" $ \_o -> WorkerRegistrationUpdated
            <$> _o .: "registrations"
        ago = A.withArray "workerRegistrationUpdated" $ \_a -> WorkerRegistrationUpdated
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON WorkerRegistrationUpdated where
    toEncoding (WorkerRegistrationUpdated _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "registrations" .= _0
        ]
    toJSON (WorkerRegistrationUpdated _0) = A.object $ P.catMaybes
        [ P.pure $ "registrations" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup WorkerRegistrationUpdated where
    WorkerRegistrationUpdated _0 <> WorkerRegistrationUpdated _ = WorkerRegistrationUpdated _0


------------------------------------------------------------------------------
instance E.Event WorkerRegistrationUpdated where
    type Result WorkerRegistrationUpdated = WorkerRegistrationUpdated
    name _ = "ServiceWorker.workerRegistrationUpdated"


------------------------------------------------------------------------------
workerRegistrationUpdated :: P.Proxy WorkerRegistrationUpdated
workerRegistrationUpdated = P.Proxy


------------------------------------------------------------------------------
data WorkerVersionUpdated = WorkerVersionUpdated
    { versions :: ![ServiceWorkerVersion]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON WorkerVersionUpdated where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "workerVersionUpdated" $ \_o -> WorkerVersionUpdated
            <$> _o .: "versions"
        ago = A.withArray "workerVersionUpdated" $ \_a -> WorkerVersionUpdated
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON WorkerVersionUpdated where
    toEncoding (WorkerVersionUpdated _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "versions" .= _0
        ]
    toJSON (WorkerVersionUpdated _0) = A.object $ P.catMaybes
        [ P.pure $ "versions" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup WorkerVersionUpdated where
    WorkerVersionUpdated _0 <> WorkerVersionUpdated _ = WorkerVersionUpdated _0


------------------------------------------------------------------------------
instance E.Event WorkerVersionUpdated where
    type Result WorkerVersionUpdated = WorkerVersionUpdated
    name _ = "ServiceWorker.workerVersionUpdated"


------------------------------------------------------------------------------
workerVersionUpdated :: P.Proxy WorkerVersionUpdated
workerVersionUpdated = P.Proxy

