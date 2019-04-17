{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.HeapProfiler.Types{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

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
import qualified DevTools.API.Runtime.Types as Runtime


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Heap snapshot object id.
type HeapSnapshotObjectId = T.Text


------------------------------------------------------------------------------
-- | Sampling Heap Profile node. Holds callsite information, allocation statistics and child nodes.
data SamplingHeapProfileNode = SamplingHeapProfileNode
    { -- | Function location.
      callFrame :: !Runtime.CallFrame
      -- | Allocations size in bytes for the node excluding children.
    , selfSize :: !P.Double
      -- | Node id. Ids are unique across all profiles collected between startSampling and stopSampling.
    , id :: !P.Int
      -- | Child nodes.
    , children :: ![SamplingHeapProfileNode]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SamplingHeapProfileNode where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "SamplingHeapProfileNode" $ \_o -> SamplingHeapProfileNode
            <$> _o .: "callFrame"
            <*> _o .: "selfSize"
            <*> _o .: "id"
            <*> _o .: "children"
        ago = A.withArray "SamplingHeapProfileNode" $ \_a -> SamplingHeapProfileNode
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON SamplingHeapProfileNode where
    toEncoding (SamplingHeapProfileNode _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "callFrame" .= _0
        , P.pure $ "selfSize" .= _1
        , P.pure $ "id" .= _2
        , P.pure $ "children" .= _3
        ]
    toJSON (SamplingHeapProfileNode _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "callFrame" .= _0
        , P.pure $ "selfSize" .= _1
        , P.pure $ "id" .= _2
        , P.pure $ "children" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup SamplingHeapProfileNode where
    SamplingHeapProfileNode _0 _1 _2 _3 <> SamplingHeapProfileNode _ _ _ _ = SamplingHeapProfileNode _0 _1 _2 _3


------------------------------------------------------------------------------
-- | A single sample from a sampling profile.
data SamplingHeapProfileSample = SamplingHeapProfileSample
    { -- | Allocation size in bytes attributed to the sample.
      size :: !P.Double
      -- | Id of the corresponding profile tree node.
    , nodeId :: !P.Int
      -- | Time-ordered sample ordinal number. It is unique across all profiles retrieved
      -- between startSampling and stopSampling.
    , ordinal :: !P.Double
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SamplingHeapProfileSample where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "SamplingHeapProfileSample" $ \_o -> SamplingHeapProfileSample
            <$> _o .: "size"
            <*> _o .: "nodeId"
            <*> _o .: "ordinal"
        ago = A.withArray "SamplingHeapProfileSample" $ \_a -> SamplingHeapProfileSample
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON SamplingHeapProfileSample where
    toEncoding (SamplingHeapProfileSample _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "size" .= _0
        , P.pure $ "nodeId" .= _1
        , P.pure $ "ordinal" .= _2
        ]
    toJSON (SamplingHeapProfileSample _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "size" .= _0
        , P.pure $ "nodeId" .= _1
        , P.pure $ "ordinal" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup SamplingHeapProfileSample where
    SamplingHeapProfileSample _0 _1 _2 <> SamplingHeapProfileSample _ _ _ = SamplingHeapProfileSample _0 _1 _2


------------------------------------------------------------------------------
-- | Sampling profile.
data SamplingHeapProfile = SamplingHeapProfile
    { head :: !SamplingHeapProfileNode
    , samples :: ![SamplingHeapProfileSample]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SamplingHeapProfile where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "SamplingHeapProfile" $ \_o -> SamplingHeapProfile
            <$> _o .: "head"
            <*> _o .: "samples"
        ago = A.withArray "SamplingHeapProfile" $ \_a -> SamplingHeapProfile
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SamplingHeapProfile where
    toEncoding (SamplingHeapProfile _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "head" .= _0
        , P.pure $ "samples" .= _1
        ]
    toJSON (SamplingHeapProfile _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "head" .= _0
        , P.pure $ "samples" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SamplingHeapProfile where
    SamplingHeapProfile _0 _1 <> SamplingHeapProfile _ _ = SamplingHeapProfile _0 _1

