{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.Tracing.Types{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

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
-- | Configuration for memory dump. Used only when "memory-infra" category is enabled.
type MemoryDumpConfig = A.Object


------------------------------------------------------------------------------
data TraceConfig = TraceConfig
    { -- | Controls how the trace buffer stores data.
      recordMode :: !(P.Maybe RecordMode)
      -- | Turns on JavaScript stack sampling.
    , enableSampling :: !(P.Maybe P.Bool)
      -- | Turns on system tracing.
    , enableSystrace :: !(P.Maybe P.Bool)
      -- | Turns on argument filter.
    , enableArgumentFilter :: !(P.Maybe P.Bool)
      -- | Included category filters.
    , includedCategories :: !(P.Maybe [T.Text])
      -- | Excluded category filters.
    , excludedCategories :: !(P.Maybe [T.Text])
      -- | Configuration to synthesize the delays in tracing.
    , syntheticDelays :: !(P.Maybe [T.Text])
      -- | Configuration for memory dump triggers. Used only when "memory-infra" category is enabled.
    , memoryDumpConfig :: !(P.Maybe MemoryDumpConfig)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TraceConfig where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "TraceConfig" $ \_o -> TraceConfig
            <$> _o .:? "recordMode"
            <*> _o .:? "enableSampling"
            <*> _o .:? "enableSystrace"
            <*> _o .:? "enableArgumentFilter"
            <*> _o .:? "includedCategories"
            <*> _o .:? "excludedCategories"
            <*> _o .:? "syntheticDelays"
            <*> _o .:? "memoryDumpConfig"
        ago = A.withArray "TraceConfig" $ \_a -> TraceConfig
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)
            <*> P.traverse A.parseJSON (_a !? 7)


------------------------------------------------------------------------------
instance A.ToJSON TraceConfig where
    toEncoding (TraceConfig _0 _1 _2 _3 _4 _5 _6 _7) = A.pairs $ P.fold $ P.catMaybes
        [ ("recordMode" .=) <$> _0
        , ("enableSampling" .=) <$> _1
        , ("enableSystrace" .=) <$> _2
        , ("enableArgumentFilter" .=) <$> _3
        , ("includedCategories" .=) <$> _4
        , ("excludedCategories" .=) <$> _5
        , ("syntheticDelays" .=) <$> _6
        , ("memoryDumpConfig" .=) <$> _7
        ]
    toJSON (TraceConfig _0 _1 _2 _3 _4 _5 _6 _7) = A.object $ P.catMaybes
        [ ("recordMode" .=) <$> _0
        , ("enableSampling" .=) <$> _1
        , ("enableSystrace" .=) <$> _2
        , ("enableArgumentFilter" .=) <$> _3
        , ("includedCategories" .=) <$> _4
        , ("excludedCategories" .=) <$> _5
        , ("syntheticDelays" .=) <$> _6
        , ("memoryDumpConfig" .=) <$> _7
        ]


------------------------------------------------------------------------------
instance P.Semigroup TraceConfig where
    TraceConfig _0 _1 _2 _3 _4 _5 _6 _7 <> TraceConfig __0 __1 __2 __3 __4 __5 __6 __7 = TraceConfig (_0 <|> __0) (_1 <|> __1) (_2 <|> __2) (_3 <|> __3) (_4 <|> __4) (_5 <|> __5) (_6 <|> __6) (_7 <|> __7)


------------------------------------------------------------------------------
instance P.Monoid TraceConfig where
    mempty = TraceConfig P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
data RecordMode
    = RecordUntilFull
    | RecordContinuously
    | RecordAsMuchAsPossible
    | EchoToConsole
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RecordMode where
    parseJSON = A.withText "RecordMode" $ \t -> case t of
        "recordUntilFull" -> P.pure RecordUntilFull
        "recordContinuously" -> P.pure RecordContinuously
        "recordAsMuchAsPossible" -> P.pure RecordAsMuchAsPossible
        "echoToConsole" -> P.pure EchoToConsole
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON RecordMode where
    toJSON RecordUntilFull = "recordUntilFull"
    toJSON RecordContinuously = "recordContinuously"
    toJSON RecordAsMuchAsPossible = "recordAsMuchAsPossible"
    toJSON EchoToConsole = "echoToConsole"


------------------------------------------------------------------------------
-- | Data format of a trace. Can be either the legacy JSON format or the
-- protocol buffer format. Note that the JSON format will be deprecated soon.
data StreamFormat
    = Json
    | Proto
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StreamFormat where
    parseJSON = A.withText "StreamFormat" $ \t -> case t of
        "json" -> P.pure Json
        "proto" -> P.pure Proto
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON StreamFormat where
    toJSON Json = "json"
    toJSON Proto = "proto"


------------------------------------------------------------------------------
-- | Compression type to use for traces returned via streams.
data StreamCompression
    = None
    | Gzip
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StreamCompression where
    parseJSON = A.withText "StreamCompression" $ \t -> case t of
        "none" -> P.pure None
        "gzip" -> P.pure Gzip
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON StreamCompression where
    toJSON None = "none"
    toJSON Gzip = "gzip"

