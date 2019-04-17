{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.Memory.Types{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

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
-- | Memory pressure level.
data PressureLevel
    = Moderate
    | Critical
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PressureLevel where
    parseJSON = A.withText "PressureLevel" $ \t -> case t of
        "moderate" -> P.pure Moderate
        "critical" -> P.pure Critical
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON PressureLevel where
    toJSON Moderate = "moderate"
    toJSON Critical = "critical"


------------------------------------------------------------------------------
-- | Heap profile sample.
data SamplingProfileNode = SamplingProfileNode
    { -- | Size of the sampled allocation.
      size :: !P.Double
      -- | Total bytes attributed to this sample.
    , total :: !P.Double
      -- | Execution stack at the point of allocation.
    , stack :: ![T.Text]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SamplingProfileNode where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "SamplingProfileNode" $ \_o -> SamplingProfileNode
            <$> _o .: "size"
            <*> _o .: "total"
            <*> _o .: "stack"
        ago = A.withArray "SamplingProfileNode" $ \_a -> SamplingProfileNode
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON SamplingProfileNode where
    toEncoding (SamplingProfileNode _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "size" .= _0
        , P.pure $ "total" .= _1
        , P.pure $ "stack" .= _2
        ]
    toJSON (SamplingProfileNode _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "size" .= _0
        , P.pure $ "total" .= _1
        , P.pure $ "stack" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup SamplingProfileNode where
    SamplingProfileNode _0 _1 _2 <> SamplingProfileNode _ _ _ = SamplingProfileNode _0 _1 _2


------------------------------------------------------------------------------
-- | Array of heap profile samples.
data SamplingProfile = SamplingProfile
    { samples :: ![SamplingProfileNode]
    , modules :: ![Module]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SamplingProfile where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "SamplingProfile" $ \_o -> SamplingProfile
            <$> _o .: "samples"
            <*> _o .: "modules"
        ago = A.withArray "SamplingProfile" $ \_a -> SamplingProfile
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SamplingProfile where
    toEncoding (SamplingProfile _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "samples" .= _0
        , P.pure $ "modules" .= _1
        ]
    toJSON (SamplingProfile _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "samples" .= _0
        , P.pure $ "modules" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SamplingProfile where
    SamplingProfile _0 _1 <> SamplingProfile _ _ = SamplingProfile _0 _1


------------------------------------------------------------------------------
-- | Executable module information
data Module = Module
    { -- | Name of the module.
      name :: !T.Text
      -- | UUID of the module.
    , uuid :: !T.Text
      -- | Base address where the module is loaded into memory. Encoded as a decimal
      -- or hexadecimal (0x prefixed) string.
    , baseAddress :: !T.Text
      -- | Size of the module in bytes.
    , size :: !P.Double
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Module where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "Module" $ \_o -> Module
            <$> _o .: "name"
            <*> _o .: "uuid"
            <*> _o .: "baseAddress"
            <*> _o .: "size"
        ago = A.withArray "Module" $ \_a -> Module
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON Module where
    toEncoding (Module _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "uuid" .= _1
        , P.pure $ "baseAddress" .= _2
        , P.pure $ "size" .= _3
        ]
    toJSON (Module _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "uuid" .= _1
        , P.pure $ "baseAddress" .= _2
        , P.pure $ "size" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup Module where
    Module _0 _1 _2 _3 <> Module _ _ _ _ = Module _0 _1 _2 _3

