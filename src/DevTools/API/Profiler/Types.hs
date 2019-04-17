{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.Profiler.Types
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
-- | Profile node. Holds callsite information, execution statistics and child nodes.
data ProfileNode = ProfileNode
    { -- | Unique id of the node.
      id :: !P.Int
      -- | Function location.
    , callFrame :: !Runtime.CallFrame
      -- | Number of samples where this node was on top of the call stack.
    , hitCount :: !(P.Maybe P.Int)
      -- | Child node ids.
    , children :: !(P.Maybe [P.Int])
      -- | The reason of being not optimized. The function may be deoptimized or marked as don't
      -- optimize.
    , deoptReason :: !(P.Maybe T.Text)
      -- | An array of source position ticks.
    , positionTicks :: !(P.Maybe [PositionTickInfo])
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ProfileNode where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "ProfileNode" $ \_o -> ProfileNode
            <$> _o .: "id"
            <*> _o .: "callFrame"
            <*> _o .:? "hitCount"
            <*> _o .:? "children"
            <*> _o .:? "deoptReason"
            <*> _o .:? "positionTicks"
        ago = A.withArray "ProfileNode" $ \_a -> ProfileNode
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)


------------------------------------------------------------------------------
instance A.ToJSON ProfileNode where
    toEncoding (ProfileNode _0 _1 _2 _3 _4 _5) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "id" .= _0
        , P.pure $ "callFrame" .= _1
        , ("hitCount" .=) <$> _2
        , ("children" .=) <$> _3
        , ("deoptReason" .=) <$> _4
        , ("positionTicks" .=) <$> _5
        ]
    toJSON (ProfileNode _0 _1 _2 _3 _4 _5) = A.object $ P.catMaybes
        [ P.pure $ "id" .= _0
        , P.pure $ "callFrame" .= _1
        , ("hitCount" .=) <$> _2
        , ("children" .=) <$> _3
        , ("deoptReason" .=) <$> _4
        , ("positionTicks" .=) <$> _5
        ]


------------------------------------------------------------------------------
instance P.Semigroup ProfileNode where
    ProfileNode _0 _1 _2 _3 _4 _5 <> ProfileNode _ _ __2 __3 __4 __5 = ProfileNode _0 _1 (_2 <|> __2) (_3 <|> __3) (_4 <|> __4) (_5 <|> __5)


------------------------------------------------------------------------------
-- | Profile.
data Profile = Profile
    { -- | The list of profile nodes. First item is the root node.
      nodes :: ![ProfileNode]
      -- | Profiling start timestamp in microseconds.
    , startTime :: !P.Double
      -- | Profiling end timestamp in microseconds.
    , endTime :: !P.Double
      -- | Ids of samples top nodes.
    , samples :: !(P.Maybe [P.Int])
      -- | Time intervals between adjacent samples in microseconds. The first delta is relative to the
      -- profile startTime.
    , timeDeltas :: !(P.Maybe [P.Int])
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Profile where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "Profile" $ \_o -> Profile
            <$> _o .: "nodes"
            <*> _o .: "startTime"
            <*> _o .: "endTime"
            <*> _o .:? "samples"
            <*> _o .:? "timeDeltas"
        ago = A.withArray "Profile" $ \_a -> Profile
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON Profile where
    toEncoding (Profile _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodes" .= _0
        , P.pure $ "startTime" .= _1
        , P.pure $ "endTime" .= _2
        , ("samples" .=) <$> _3
        , ("timeDeltas" .=) <$> _4
        ]
    toJSON (Profile _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ P.pure $ "nodes" .= _0
        , P.pure $ "startTime" .= _1
        , P.pure $ "endTime" .= _2
        , ("samples" .=) <$> _3
        , ("timeDeltas" .=) <$> _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup Profile where
    Profile _0 _1 _2 _3 _4 <> Profile _ _ _ __3 __4 = Profile _0 _1 _2 (_3 <|> __3) (_4 <|> __4)


------------------------------------------------------------------------------
-- | Specifies a number of samples attributed to a certain source position.
data PositionTickInfo = PositionTickInfo
    { -- | Source line number (1-based).
      line :: !P.Int
      -- | Number of samples attributed to the source line.
    , ticks :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PositionTickInfo where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "PositionTickInfo" $ \_o -> PositionTickInfo
            <$> _o .: "line"
            <*> _o .: "ticks"
        ago = A.withArray "PositionTickInfo" $ \_a -> PositionTickInfo
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON PositionTickInfo where
    toEncoding (PositionTickInfo _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "line" .= _0
        , P.pure $ "ticks" .= _1
        ]
    toJSON (PositionTickInfo _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "line" .= _0
        , P.pure $ "ticks" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup PositionTickInfo where
    PositionTickInfo _0 _1 <> PositionTickInfo _ _ = PositionTickInfo _0 _1


------------------------------------------------------------------------------
-- | Coverage data for a source range.
data CoverageRange = CoverageRange
    { -- | JavaScript script source offset for the range start.
      startOffset :: !P.Int
      -- | JavaScript script source offset for the range end.
    , endOffset :: !P.Int
      -- | Collected execution count of the source range.
    , count :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CoverageRange where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "CoverageRange" $ \_o -> CoverageRange
            <$> _o .: "startOffset"
            <*> _o .: "endOffset"
            <*> _o .: "count"
        ago = A.withArray "CoverageRange" $ \_a -> CoverageRange
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON CoverageRange where
    toEncoding (CoverageRange _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "startOffset" .= _0
        , P.pure $ "endOffset" .= _1
        , P.pure $ "count" .= _2
        ]
    toJSON (CoverageRange _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "startOffset" .= _0
        , P.pure $ "endOffset" .= _1
        , P.pure $ "count" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup CoverageRange where
    CoverageRange _0 _1 _2 <> CoverageRange _ _ _ = CoverageRange _0 _1 _2


------------------------------------------------------------------------------
-- | Coverage data for a JavaScript function.
data FunctionCoverage = FunctionCoverage
    { -- | JavaScript function name.
      functionName :: !T.Text
      -- | Source ranges inside the function with coverage data.
    , ranges :: ![CoverageRange]
      -- | Whether coverage data for this function has block granularity.
    , isBlockCoverage :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON FunctionCoverage where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "FunctionCoverage" $ \_o -> FunctionCoverage
            <$> _o .: "functionName"
            <*> _o .: "ranges"
            <*> _o .: "isBlockCoverage"
        ago = A.withArray "FunctionCoverage" $ \_a -> FunctionCoverage
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON FunctionCoverage where
    toEncoding (FunctionCoverage _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "functionName" .= _0
        , P.pure $ "ranges" .= _1
        , P.pure $ "isBlockCoverage" .= _2
        ]
    toJSON (FunctionCoverage _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "functionName" .= _0
        , P.pure $ "ranges" .= _1
        , P.pure $ "isBlockCoverage" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup FunctionCoverage where
    FunctionCoverage _0 _1 _2 <> FunctionCoverage _ _ _ = FunctionCoverage _0 _1 _2


------------------------------------------------------------------------------
-- | Coverage data for a JavaScript script.
data ScriptCoverage = ScriptCoverage
    { -- | JavaScript script id.
      scriptId :: !Runtime.ScriptId
      -- | JavaScript script name or url.
    , url :: !T.Text
      -- | Functions contained in the script that has coverage data.
    , functions :: ![FunctionCoverage]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ScriptCoverage where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "ScriptCoverage" $ \_o -> ScriptCoverage
            <$> _o .: "scriptId"
            <*> _o .: "url"
            <*> _o .: "functions"
        ago = A.withArray "ScriptCoverage" $ \_a -> ScriptCoverage
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON ScriptCoverage where
    toEncoding (ScriptCoverage _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "scriptId" .= _0
        , P.pure $ "url" .= _1
        , P.pure $ "functions" .= _2
        ]
    toJSON (ScriptCoverage _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "scriptId" .= _0
        , P.pure $ "url" .= _1
        , P.pure $ "functions" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup ScriptCoverage where
    ScriptCoverage _0 _1 _2 <> ScriptCoverage _ _ _ = ScriptCoverage _0 _1 _2


------------------------------------------------------------------------------
-- | Describes a type collected during runtime.
{-# WARNING TypeObject "This feature is marked as EXPERIMENTAL." #-}
data TypeObject = TypeObject
    { -- | Name of a type collected with type profiling.
      name :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TypeObject where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "TypeObject" $ \_o -> TypeObject
            <$> _o .: "name"
        ago = A.withArray "TypeObject" $ \_a -> TypeObject
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON TypeObject where
    toEncoding (TypeObject _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "name" .= _0
        ]
    toJSON (TypeObject _0) = A.object $ P.catMaybes
        [ P.pure $ "name" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup TypeObject where
    TypeObject _0 <> TypeObject _ = TypeObject _0


------------------------------------------------------------------------------
-- | Source offset and types for a parameter or return value.
{-# WARNING TypeProfileEntry "This feature is marked as EXPERIMENTAL." #-}
data TypeProfileEntry = TypeProfileEntry
    { -- | Source offset of the parameter or end of function for return values.
      offset :: !P.Int
      -- | The types for this parameter or return value.
    , types :: ![TypeObject]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TypeProfileEntry where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "TypeProfileEntry" $ \_o -> TypeProfileEntry
            <$> _o .: "offset"
            <*> _o .: "types"
        ago = A.withArray "TypeProfileEntry" $ \_a -> TypeProfileEntry
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON TypeProfileEntry where
    toEncoding (TypeProfileEntry _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "offset" .= _0
        , P.pure $ "types" .= _1
        ]
    toJSON (TypeProfileEntry _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "offset" .= _0
        , P.pure $ "types" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup TypeProfileEntry where
    TypeProfileEntry _0 _1 <> TypeProfileEntry _ _ = TypeProfileEntry _0 _1


------------------------------------------------------------------------------
-- | Type profile data collected during runtime for a JavaScript script.
{-# WARNING ScriptTypeProfile "This feature is marked as EXPERIMENTAL." #-}
data ScriptTypeProfile = ScriptTypeProfile
    { -- | JavaScript script id.
      scriptId :: !Runtime.ScriptId
      -- | JavaScript script name or url.
    , url :: !T.Text
      -- | Type profile entries for parameters and return values of the functions in the script.
    , entries :: ![TypeProfileEntry]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ScriptTypeProfile where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "ScriptTypeProfile" $ \_o -> ScriptTypeProfile
            <$> _o .: "scriptId"
            <*> _o .: "url"
            <*> _o .: "entries"
        ago = A.withArray "ScriptTypeProfile" $ \_a -> ScriptTypeProfile
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON ScriptTypeProfile where
    toEncoding (ScriptTypeProfile _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "scriptId" .= _0
        , P.pure $ "url" .= _1
        , P.pure $ "entries" .= _2
        ]
    toJSON (ScriptTypeProfile _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "scriptId" .= _0
        , P.pure $ "url" .= _1
        , P.pure $ "entries" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup ScriptTypeProfile where
    ScriptTypeProfile _0 _1 _2 <> ScriptTypeProfile _ _ _ = ScriptTypeProfile _0 _1 _2

