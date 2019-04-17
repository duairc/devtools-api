{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.ServiceWorker.Types{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

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
import qualified DevTools.API.Target.Types as Target


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
type RegistrationID = T.Text


------------------------------------------------------------------------------
-- | ServiceWorker registration.
data ServiceWorkerRegistration = ServiceWorkerRegistration
    { registrationId :: !RegistrationID
    , scopeURL :: !T.Text
    , isDeleted :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ServiceWorkerRegistration where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "ServiceWorkerRegistration" $ \_o -> ServiceWorkerRegistration
            <$> _o .: "registrationId"
            <*> _o .: "scopeURL"
            <*> _o .: "isDeleted"
        ago = A.withArray "ServiceWorkerRegistration" $ \_a -> ServiceWorkerRegistration
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON ServiceWorkerRegistration where
    toEncoding (ServiceWorkerRegistration _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "registrationId" .= _0
        , P.pure $ "scopeURL" .= _1
        , P.pure $ "isDeleted" .= _2
        ]
    toJSON (ServiceWorkerRegistration _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "registrationId" .= _0
        , P.pure $ "scopeURL" .= _1
        , P.pure $ "isDeleted" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup ServiceWorkerRegistration where
    ServiceWorkerRegistration _0 _1 _2 <> ServiceWorkerRegistration _ _ _ = ServiceWorkerRegistration _0 _1 _2


------------------------------------------------------------------------------
data ServiceWorkerVersionRunningStatus
    = Stopped
    | Starting
    | Running
    | Stopping
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ServiceWorkerVersionRunningStatus where
    parseJSON = A.withText "ServiceWorkerVersionRunningStatus" $ \t -> case t of
        "stopped" -> P.pure Stopped
        "starting" -> P.pure Starting
        "running" -> P.pure Running
        "stopping" -> P.pure Stopping
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON ServiceWorkerVersionRunningStatus where
    toJSON Stopped = "stopped"
    toJSON Starting = "starting"
    toJSON Running = "running"
    toJSON Stopping = "stopping"


------------------------------------------------------------------------------
data ServiceWorkerVersionStatus
    = New
    | Installing
    | Installed
    | Activating
    | Activated
    | Redundant
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ServiceWorkerVersionStatus where
    parseJSON = A.withText "ServiceWorkerVersionStatus" $ \t -> case t of
        "new" -> P.pure New
        "installing" -> P.pure Installing
        "installed" -> P.pure Installed
        "activating" -> P.pure Activating
        "activated" -> P.pure Activated
        "redundant" -> P.pure Redundant
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON ServiceWorkerVersionStatus where
    toJSON New = "new"
    toJSON Installing = "installing"
    toJSON Installed = "installed"
    toJSON Activating = "activating"
    toJSON Activated = "activated"
    toJSON Redundant = "redundant"


------------------------------------------------------------------------------
-- | ServiceWorker version.
data ServiceWorkerVersion = ServiceWorkerVersion
    { versionId :: !T.Text
    , registrationId :: !RegistrationID
    , scriptURL :: !T.Text
    , runningStatus :: !ServiceWorkerVersionRunningStatus
    , status :: !ServiceWorkerVersionStatus
      -- | The Last-Modified header value of the main script.
    , scriptLastModified :: !(P.Maybe P.Double)
      -- | The time at which the response headers of the main script were received from the server.
      -- For cached script it is the last time the cache entry was validated.
    , scriptResponseTime :: !(P.Maybe P.Double)
    , controlledClients :: !(P.Maybe [Target.TargetID])
    , targetId :: !(P.Maybe Target.TargetID)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ServiceWorkerVersion where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "ServiceWorkerVersion" $ \_o -> ServiceWorkerVersion
            <$> _o .: "versionId"
            <*> _o .: "registrationId"
            <*> _o .: "scriptURL"
            <*> _o .: "runningStatus"
            <*> _o .: "status"
            <*> _o .:? "scriptLastModified"
            <*> _o .:? "scriptResponseTime"
            <*> _o .:? "controlledClients"
            <*> _o .:? "targetId"
        ago = A.withArray "ServiceWorkerVersion" $ \_a -> ServiceWorkerVersion
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)
            <*> P.traverse A.parseJSON (_a !? 7)
            <*> P.traverse A.parseJSON (_a !? 8)


------------------------------------------------------------------------------
instance A.ToJSON ServiceWorkerVersion where
    toEncoding (ServiceWorkerVersion _0 _1 _2 _3 _4 _5 _6 _7 _8) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "versionId" .= _0
        , P.pure $ "registrationId" .= _1
        , P.pure $ "scriptURL" .= _2
        , P.pure $ "runningStatus" .= _3
        , P.pure $ "status" .= _4
        , ("scriptLastModified" .=) <$> _5
        , ("scriptResponseTime" .=) <$> _6
        , ("controlledClients" .=) <$> _7
        , ("targetId" .=) <$> _8
        ]
    toJSON (ServiceWorkerVersion _0 _1 _2 _3 _4 _5 _6 _7 _8) = A.object $ P.catMaybes
        [ P.pure $ "versionId" .= _0
        , P.pure $ "registrationId" .= _1
        , P.pure $ "scriptURL" .= _2
        , P.pure $ "runningStatus" .= _3
        , P.pure $ "status" .= _4
        , ("scriptLastModified" .=) <$> _5
        , ("scriptResponseTime" .=) <$> _6
        , ("controlledClients" .=) <$> _7
        , ("targetId" .=) <$> _8
        ]


------------------------------------------------------------------------------
instance P.Semigroup ServiceWorkerVersion where
    ServiceWorkerVersion _0 _1 _2 _3 _4 _5 _6 _7 _8 <> ServiceWorkerVersion _ _ _ _ _ __5 __6 __7 __8 = ServiceWorkerVersion _0 _1 _2 _3 _4 (_5 <|> __5) (_6 <|> __6) (_7 <|> __7) (_8 <|> __8)


------------------------------------------------------------------------------
-- | ServiceWorker error message.
data ServiceWorkerErrorMessage = ServiceWorkerErrorMessage
    { errorMessage :: !T.Text
    , registrationId :: !RegistrationID
    , versionId :: !T.Text
    , sourceURL :: !T.Text
    , lineNumber :: !P.Int
    , columnNumber :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ServiceWorkerErrorMessage where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "ServiceWorkerErrorMessage" $ \_o -> ServiceWorkerErrorMessage
            <$> _o .: "errorMessage"
            <*> _o .: "registrationId"
            <*> _o .: "versionId"
            <*> _o .: "sourceURL"
            <*> _o .: "lineNumber"
            <*> _o .: "columnNumber"
        ago = A.withArray "ServiceWorkerErrorMessage" $ \_a -> ServiceWorkerErrorMessage
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)


------------------------------------------------------------------------------
instance A.ToJSON ServiceWorkerErrorMessage where
    toEncoding (ServiceWorkerErrorMessage _0 _1 _2 _3 _4 _5) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "errorMessage" .= _0
        , P.pure $ "registrationId" .= _1
        , P.pure $ "versionId" .= _2
        , P.pure $ "sourceURL" .= _3
        , P.pure $ "lineNumber" .= _4
        , P.pure $ "columnNumber" .= _5
        ]
    toJSON (ServiceWorkerErrorMessage _0 _1 _2 _3 _4 _5) = A.object $ P.catMaybes
        [ P.pure $ "errorMessage" .= _0
        , P.pure $ "registrationId" .= _1
        , P.pure $ "versionId" .= _2
        , P.pure $ "sourceURL" .= _3
        , P.pure $ "lineNumber" .= _4
        , P.pure $ "columnNumber" .= _5
        ]


------------------------------------------------------------------------------
instance P.Semigroup ServiceWorkerErrorMessage where
    ServiceWorkerErrorMessage _0 _1 _2 _3 _4 _5 <> ServiceWorkerErrorMessage _ _ _ _ _ _ = ServiceWorkerErrorMessage _0 _1 _2 _3 _4 _5

