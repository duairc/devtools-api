{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | This domain is deprecated - use Runtime or Log instead.
module DevTools.API.Console.Types{-{-# DEPRECATED "This may be removed in a future release." #-}-}

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
-- | Console message.
data ConsoleMessage = ConsoleMessage
    { -- | Message source.
      source :: !Source
      -- | Message severity.
    , level :: !Level
      -- | Message text.
    , text :: !T.Text
      -- | URL of the message origin.
    , url :: !(P.Maybe T.Text)
      -- | Line number in the resource that generated this message (1-based).
    , line :: !(P.Maybe P.Int)
      -- | Column number in the resource that generated this message (1-based).
    , column :: !(P.Maybe P.Int)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ConsoleMessage where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "ConsoleMessage" $ \_o -> ConsoleMessage
            <$> _o .: "source"
            <*> _o .: "level"
            <*> _o .: "text"
            <*> _o .:? "url"
            <*> _o .:? "line"
            <*> _o .:? "column"
        ago = A.withArray "ConsoleMessage" $ \_a -> ConsoleMessage
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)


------------------------------------------------------------------------------
instance A.ToJSON ConsoleMessage where
    toEncoding (ConsoleMessage _0 _1 _2 _3 _4 _5) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "source" .= _0
        , P.pure $ "level" .= _1
        , P.pure $ "text" .= _2
        , ("url" .=) <$> _3
        , ("line" .=) <$> _4
        , ("column" .=) <$> _5
        ]
    toJSON (ConsoleMessage _0 _1 _2 _3 _4 _5) = A.object $ P.catMaybes
        [ P.pure $ "source" .= _0
        , P.pure $ "level" .= _1
        , P.pure $ "text" .= _2
        , ("url" .=) <$> _3
        , ("line" .=) <$> _4
        , ("column" .=) <$> _5
        ]


------------------------------------------------------------------------------
instance P.Semigroup ConsoleMessage where
    ConsoleMessage _0 _1 _2 _3 _4 _5 <> ConsoleMessage _ _ _ __3 __4 __5 = ConsoleMessage _0 _1 _2 (_3 <|> __3) (_4 <|> __4) (_5 <|> __5)


------------------------------------------------------------------------------
data Source
    = Xml
    | Javascript
    | Network
    | ConsoleApi
    | Storage
    | Appcache
    | Rendering
    | Security
    | Other
    | Deprecation
    | Worker
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
        "console-api" -> P.pure ConsoleApi
        "storage" -> P.pure Storage
        "appcache" -> P.pure Appcache
        "rendering" -> P.pure Rendering
        "security" -> P.pure Security
        "other" -> P.pure Other
        "deprecation" -> P.pure Deprecation
        "worker" -> P.pure Worker
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Source where
    toJSON Xml = "xml"
    toJSON Javascript = "javascript"
    toJSON Network = "network"
    toJSON ConsoleApi = "console-api"
    toJSON Storage = "storage"
    toJSON Appcache = "appcache"
    toJSON Rendering = "rendering"
    toJSON Security = "security"
    toJSON Other = "other"
    toJSON Deprecation = "deprecation"
    toJSON Worker = "worker"


------------------------------------------------------------------------------
data Level
    = Log
    | Warning
    | Error
    | Debug
    | Info
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Level where
    parseJSON = A.withText "Level" $ \t -> case t of
        "log" -> P.pure Log
        "warning" -> P.pure Warning
        "error" -> P.pure Error
        "debug" -> P.pure Debug
        "info" -> P.pure Info
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Level where
    toJSON Log = "log"
    toJSON Warning = "warning"
    toJSON Error = "error"
    toJSON Debug = "debug"
    toJSON Info = "info"

