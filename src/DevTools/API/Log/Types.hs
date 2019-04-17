{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Provides access to log entries.
module DevTools.API.Log.Types
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
import qualified DevTools.API.Runtime.Types as Runtime


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Log entry.
data LogEntry = LogEntry
    { -- | Log entry source.
      source :: !Source
      -- | Log entry severity.
    , level :: !Level
      -- | Logged text.
    , text :: !T.Text
      -- | Timestamp when this entry was added.
    , timestamp :: !Runtime.Timestamp
      -- | URL of the resource if known.
    , url :: !(P.Maybe T.Text)
      -- | Line number in the resource.
    , lineNumber :: !(P.Maybe P.Int)
      -- | JavaScript stack trace.
    , stackTrace :: !(P.Maybe Runtime.StackTrace)
      -- | Identifier of the network request associated with this entry.
    , networkRequestId :: !(P.Maybe Network.RequestId)
      -- | Identifier of the worker associated with this entry.
    , workerId :: !(P.Maybe T.Text)
      -- | Call arguments.
    , args :: !(P.Maybe [Runtime.RemoteObject])
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON LogEntry where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "LogEntry" $ \_o -> LogEntry
            <$> _o .: "source"
            <*> _o .: "level"
            <*> _o .: "text"
            <*> _o .: "timestamp"
            <*> _o .:? "url"
            <*> _o .:? "lineNumber"
            <*> _o .:? "stackTrace"
            <*> _o .:? "networkRequestId"
            <*> _o .:? "workerId"
            <*> _o .:? "args"
        ago = A.withArray "LogEntry" $ \_a -> LogEntry
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)
            <*> P.traverse A.parseJSON (_a !? 7)
            <*> P.traverse A.parseJSON (_a !? 8)
            <*> P.traverse A.parseJSON (_a !? 9)


------------------------------------------------------------------------------
instance A.ToJSON LogEntry where
    toEncoding (LogEntry _0 _1 _2 _3 _4 _5 _6 _7 _8 _9) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "source" .= _0
        , P.pure $ "level" .= _1
        , P.pure $ "text" .= _2
        , P.pure $ "timestamp" .= _3
        , ("url" .=) <$> _4
        , ("lineNumber" .=) <$> _5
        , ("stackTrace" .=) <$> _6
        , ("networkRequestId" .=) <$> _7
        , ("workerId" .=) <$> _8
        , ("args" .=) <$> _9
        ]
    toJSON (LogEntry _0 _1 _2 _3 _4 _5 _6 _7 _8 _9) = A.object $ P.catMaybes
        [ P.pure $ "source" .= _0
        , P.pure $ "level" .= _1
        , P.pure $ "text" .= _2
        , P.pure $ "timestamp" .= _3
        , ("url" .=) <$> _4
        , ("lineNumber" .=) <$> _5
        , ("stackTrace" .=) <$> _6
        , ("networkRequestId" .=) <$> _7
        , ("workerId" .=) <$> _8
        , ("args" .=) <$> _9
        ]


------------------------------------------------------------------------------
instance P.Semigroup LogEntry where
    LogEntry _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 <> LogEntry _ _ _ _ __4 __5 __6 __7 __8 __9 = LogEntry _0 _1 _2 _3 (_4 <|> __4) (_5 <|> __5) (_6 <|> __6) (_7 <|> __7) (_8 <|> __8) (_9 <|> __9)


------------------------------------------------------------------------------
data Source
    = Xml
    | Javascript
    | Network
    | Storage
    | Appcache
    | Rendering
    | Security
    | Deprecation
    | Worker
    | Violation
    | Intervention
    | Recommendation
    | Other
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Source where
    parseJSON = A.withText "Source" $ \t -> case t of
        "xml" -> P.pure Xml
        "javascript" -> P.pure Javascript
        "network" -> P.pure Network
        "storage" -> P.pure Storage
        "appcache" -> P.pure Appcache
        "rendering" -> P.pure Rendering
        "security" -> P.pure Security
        "deprecation" -> P.pure Deprecation
        "worker" -> P.pure Worker
        "violation" -> P.pure Violation
        "intervention" -> P.pure Intervention
        "recommendation" -> P.pure Recommendation
        "other" -> P.pure Other
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Source where
    toJSON Xml = "xml"
    toJSON Javascript = "javascript"
    toJSON Network = "network"
    toJSON Storage = "storage"
    toJSON Appcache = "appcache"
    toJSON Rendering = "rendering"
    toJSON Security = "security"
    toJSON Deprecation = "deprecation"
    toJSON Worker = "worker"
    toJSON Violation = "violation"
    toJSON Intervention = "intervention"
    toJSON Recommendation = "recommendation"
    toJSON Other = "other"


------------------------------------------------------------------------------
data Level
    = Verbose
    | Info
    | Warning
    | Error
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Level where
    parseJSON = A.withText "Level" $ \t -> case t of
        "verbose" -> P.pure Verbose
        "info" -> P.pure Info
        "warning" -> P.pure Warning
        "error" -> P.pure Error
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Level where
    toJSON Verbose = "verbose"
    toJSON Info = "info"
    toJSON Warning = "warning"
    toJSON Error = "error"


------------------------------------------------------------------------------
-- | Violation configuration setting.
data ViolationSetting = ViolationSetting
    { -- | Violation type.
      name :: !Name
      -- | Time threshold to trigger upon.
    , threshold :: !P.Double
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ViolationSetting where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "ViolationSetting" $ \_o -> ViolationSetting
            <$> _o .: "name"
            <*> _o .: "threshold"
        ago = A.withArray "ViolationSetting" $ \_a -> ViolationSetting
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON ViolationSetting where
    toEncoding (ViolationSetting _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "threshold" .= _1
        ]
    toJSON (ViolationSetting _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "threshold" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup ViolationSetting where
    ViolationSetting _0 _1 <> ViolationSetting _ _ = ViolationSetting _0 _1


------------------------------------------------------------------------------
data Name
    = LongTask
    | LongLayout
    | BlockedEvent
    | BlockedParser
    | DiscouragedAPIUse
    | Handler
    | RecurringHandler
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Name where
    parseJSON = A.withText "Name" $ \t -> case t of
        "longTask" -> P.pure LongTask
        "longLayout" -> P.pure LongLayout
        "blockedEvent" -> P.pure BlockedEvent
        "blockedParser" -> P.pure BlockedParser
        "discouragedAPIUse" -> P.pure DiscouragedAPIUse
        "handler" -> P.pure Handler
        "recurringHandler" -> P.pure RecurringHandler
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Name where
    toJSON LongTask = "longTask"
    toJSON LongLayout = "longLayout"
    toJSON BlockedEvent = "blockedEvent"
    toJSON BlockedParser = "blockedParser"
    toJSON DiscouragedAPIUse = "discouragedAPIUse"
    toJSON Handler = "handler"
    toJSON RecurringHandler = "recurringHandler"

