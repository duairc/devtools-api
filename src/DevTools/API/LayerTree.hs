{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.LayerTree{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

    ( module DevTools.API.LayerTree.Types
    , module DevTools.API.LayerTree
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
import qualified DevTools.API.DOM.Types as DOM
import           DevTools.API.LayerTree.Types


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Provides the reasons why the given layer was composited.
data CompositingReasons = CompositingReasons
    { -- | The id of the layer for which we want to get the reasons it was composited.
      layerId :: !LayerId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CompositingReasons where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "compositingReasons" $ \_o -> CompositingReasons
            <$> _o .: "layerId"
        ago = A.withArray "compositingReasons" $ \_a -> CompositingReasons
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON CompositingReasons where
    toEncoding (CompositingReasons _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "layerId" .= _0
        ]
    toJSON (CompositingReasons _0) = A.object $ P.catMaybes
        [ P.pure $ "layerId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup CompositingReasons where
    CompositingReasons _0 <> CompositingReasons _ = CompositingReasons _0


------------------------------------------------------------------------------
-- | Provides the reasons why the given layer was composited.
data CompositingReasonsResult = CompositingReasonsResult
    { -- | A list of strings specifying reasons for the given layer to become composited.
      compositingReasons_ :: ![T.Text]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CompositingReasonsResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "compositingReasonsResult" $ \_o -> CompositingReasonsResult
            <$> _o .: "compositingReasons"
        ago = A.withArray "compositingReasonsResult" $ \_a -> CompositingReasonsResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON CompositingReasonsResult where
    toEncoding (CompositingReasonsResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "compositingReasons" .= _0
        ]
    toJSON (CompositingReasonsResult _0) = A.object $ P.catMaybes
        [ P.pure $ "compositingReasons" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup CompositingReasonsResult where
    CompositingReasonsResult _0 <> CompositingReasonsResult _ = CompositingReasonsResult _0


------------------------------------------------------------------------------
instance M.Method CompositingReasons where
    type Result CompositingReasons = CompositingReasonsResult
    name _ = "LayerTree.compositingReasons"


------------------------------------------------------------------------------
-- | Provides the reasons why the given layer was composited.
compositingReasons
    :: LayerId
    -- ^ The id of the layer for which we want to get the reasons it was composited.

    -> CompositingReasons
compositingReasons _0 = CompositingReasons _0


------------------------------------------------------------------------------
-- | Disables compositing tree inspection.
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
    name _ = "LayerTree.disable"


------------------------------------------------------------------------------
-- | Disables compositing tree inspection.
disable
    :: Disable
disable = Disable


------------------------------------------------------------------------------
-- | Enables compositing tree inspection.
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
    name _ = "LayerTree.enable"


------------------------------------------------------------------------------
-- | Enables compositing tree inspection.
enable
    :: Enable
enable = Enable


------------------------------------------------------------------------------
-- | Returns the snapshot identifier.
data LoadSnapshot = LoadSnapshot
    { -- | An array of tiles composing the snapshot.
      tiles :: ![PictureTile]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON LoadSnapshot where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "loadSnapshot" $ \_o -> LoadSnapshot
            <$> _o .: "tiles"
        ago = A.withArray "loadSnapshot" $ \_a -> LoadSnapshot
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON LoadSnapshot where
    toEncoding (LoadSnapshot _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "tiles" .= _0
        ]
    toJSON (LoadSnapshot _0) = A.object $ P.catMaybes
        [ P.pure $ "tiles" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup LoadSnapshot where
    LoadSnapshot _0 <> LoadSnapshot _ = LoadSnapshot _0


------------------------------------------------------------------------------
-- | Returns the snapshot identifier.
data LoadSnapshotResult = LoadSnapshotResult
    { -- | The id of the snapshot.
      snapshotId :: !SnapshotId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON LoadSnapshotResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "loadSnapshotResult" $ \_o -> LoadSnapshotResult
            <$> _o .: "snapshotId"
        ago = A.withArray "loadSnapshotResult" $ \_a -> LoadSnapshotResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON LoadSnapshotResult where
    toEncoding (LoadSnapshotResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "snapshotId" .= _0
        ]
    toJSON (LoadSnapshotResult _0) = A.object $ P.catMaybes
        [ P.pure $ "snapshotId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup LoadSnapshotResult where
    LoadSnapshotResult _0 <> LoadSnapshotResult _ = LoadSnapshotResult _0


------------------------------------------------------------------------------
instance M.Method LoadSnapshot where
    type Result LoadSnapshot = LoadSnapshotResult
    name _ = "LayerTree.loadSnapshot"


------------------------------------------------------------------------------
-- | Returns the snapshot identifier.
loadSnapshot
    :: [PictureTile]
    -- ^ An array of tiles composing the snapshot.

    -> LoadSnapshot
loadSnapshot _0 = LoadSnapshot _0


------------------------------------------------------------------------------
-- | Returns the layer snapshot identifier.
data MakeSnapshot = MakeSnapshot
    { -- | The id of the layer.
      layerId :: !LayerId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON MakeSnapshot where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "makeSnapshot" $ \_o -> MakeSnapshot
            <$> _o .: "layerId"
        ago = A.withArray "makeSnapshot" $ \_a -> MakeSnapshot
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON MakeSnapshot where
    toEncoding (MakeSnapshot _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "layerId" .= _0
        ]
    toJSON (MakeSnapshot _0) = A.object $ P.catMaybes
        [ P.pure $ "layerId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup MakeSnapshot where
    MakeSnapshot _0 <> MakeSnapshot _ = MakeSnapshot _0


------------------------------------------------------------------------------
-- | Returns the layer snapshot identifier.
data MakeSnapshotResult = MakeSnapshotResult
    { -- | The id of the layer snapshot.
      snapshotId :: !SnapshotId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON MakeSnapshotResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "makeSnapshotResult" $ \_o -> MakeSnapshotResult
            <$> _o .: "snapshotId"
        ago = A.withArray "makeSnapshotResult" $ \_a -> MakeSnapshotResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON MakeSnapshotResult where
    toEncoding (MakeSnapshotResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "snapshotId" .= _0
        ]
    toJSON (MakeSnapshotResult _0) = A.object $ P.catMaybes
        [ P.pure $ "snapshotId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup MakeSnapshotResult where
    MakeSnapshotResult _0 <> MakeSnapshotResult _ = MakeSnapshotResult _0


------------------------------------------------------------------------------
instance M.Method MakeSnapshot where
    type Result MakeSnapshot = MakeSnapshotResult
    name _ = "LayerTree.makeSnapshot"


------------------------------------------------------------------------------
-- | Returns the layer snapshot identifier.
makeSnapshot
    :: LayerId
    -- ^ The id of the layer.

    -> MakeSnapshot
makeSnapshot _0 = MakeSnapshot _0


------------------------------------------------------------------------------
data ProfileSnapshot = ProfileSnapshot
    { -- | The id of the layer snapshot.
      snapshotId :: !SnapshotId
      -- | The maximum number of times to replay the snapshot (1, if not specified).
    , minRepeatCount :: !(P.Maybe P.Int)
      -- | The minimum duration (in seconds) to replay the snapshot.
    , minDuration :: !(P.Maybe P.Double)
      -- | The clip rectangle to apply when replaying the snapshot.
    , clipRect :: !(P.Maybe DOM.Rect)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ProfileSnapshot where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "profileSnapshot" $ \_o -> ProfileSnapshot
            <$> _o .: "snapshotId"
            <*> _o .:? "minRepeatCount"
            <*> _o .:? "minDuration"
            <*> _o .:? "clipRect"
        ago = A.withArray "profileSnapshot" $ \_a -> ProfileSnapshot
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON ProfileSnapshot where
    toEncoding (ProfileSnapshot _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "snapshotId" .= _0
        , ("minRepeatCount" .=) <$> _1
        , ("minDuration" .=) <$> _2
        , ("clipRect" .=) <$> _3
        ]
    toJSON (ProfileSnapshot _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "snapshotId" .= _0
        , ("minRepeatCount" .=) <$> _1
        , ("minDuration" .=) <$> _2
        , ("clipRect" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup ProfileSnapshot where
    ProfileSnapshot _0 _1 _2 _3 <> ProfileSnapshot _ __1 __2 __3 = ProfileSnapshot _0 (_1 <|> __1) (_2 <|> __2) (_3 <|> __3)


------------------------------------------------------------------------------
data ProfileSnapshotResult = ProfileSnapshotResult
    { -- | The array of paint profiles, one per run.
      timings :: ![PaintProfile]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ProfileSnapshotResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "profileSnapshotResult" $ \_o -> ProfileSnapshotResult
            <$> _o .: "timings"
        ago = A.withArray "profileSnapshotResult" $ \_a -> ProfileSnapshotResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON ProfileSnapshotResult where
    toEncoding (ProfileSnapshotResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "timings" .= _0
        ]
    toJSON (ProfileSnapshotResult _0) = A.object $ P.catMaybes
        [ P.pure $ "timings" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup ProfileSnapshotResult where
    ProfileSnapshotResult _0 <> ProfileSnapshotResult _ = ProfileSnapshotResult _0


------------------------------------------------------------------------------
instance M.Method ProfileSnapshot where
    type Result ProfileSnapshot = ProfileSnapshotResult
    name _ = "LayerTree.profileSnapshot"


------------------------------------------------------------------------------
profileSnapshot
    :: SnapshotId
    -- ^ The id of the layer snapshot.

    -> ProfileSnapshot
profileSnapshot _0 = ProfileSnapshot _0 P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Releases layer snapshot captured by the back-end.
data ReleaseSnapshot = ReleaseSnapshot
    { -- | The id of the layer snapshot.
      snapshotId :: !SnapshotId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ReleaseSnapshot where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "releaseSnapshot" $ \_o -> ReleaseSnapshot
            <$> _o .: "snapshotId"
        ago = A.withArray "releaseSnapshot" $ \_a -> ReleaseSnapshot
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON ReleaseSnapshot where
    toEncoding (ReleaseSnapshot _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "snapshotId" .= _0
        ]
    toJSON (ReleaseSnapshot _0) = A.object $ P.catMaybes
        [ P.pure $ "snapshotId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup ReleaseSnapshot where
    ReleaseSnapshot _0 <> ReleaseSnapshot _ = ReleaseSnapshot _0


------------------------------------------------------------------------------
instance M.Method ReleaseSnapshot where
    type Result ReleaseSnapshot = ()
    name _ = "LayerTree.releaseSnapshot"


------------------------------------------------------------------------------
-- | Releases layer snapshot captured by the back-end.
releaseSnapshot
    :: SnapshotId
    -- ^ The id of the layer snapshot.

    -> ReleaseSnapshot
releaseSnapshot _0 = ReleaseSnapshot _0


------------------------------------------------------------------------------
-- | Replays the layer snapshot and returns the resulting bitmap.
data ReplaySnapshot = ReplaySnapshot
    { -- | The id of the layer snapshot.
      snapshotId :: !SnapshotId
      -- | The first step to replay from (replay from the very start if not specified).
    , fromStep :: !(P.Maybe P.Int)
      -- | The last step to replay to (replay till the end if not specified).
    , toStep :: !(P.Maybe P.Int)
      -- | The scale to apply while replaying (defaults to 1).
    , scale :: !(P.Maybe P.Double)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ReplaySnapshot where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "replaySnapshot" $ \_o -> ReplaySnapshot
            <$> _o .: "snapshotId"
            <*> _o .:? "fromStep"
            <*> _o .:? "toStep"
            <*> _o .:? "scale"
        ago = A.withArray "replaySnapshot" $ \_a -> ReplaySnapshot
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON ReplaySnapshot where
    toEncoding (ReplaySnapshot _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "snapshotId" .= _0
        , ("fromStep" .=) <$> _1
        , ("toStep" .=) <$> _2
        , ("scale" .=) <$> _3
        ]
    toJSON (ReplaySnapshot _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "snapshotId" .= _0
        , ("fromStep" .=) <$> _1
        , ("toStep" .=) <$> _2
        , ("scale" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup ReplaySnapshot where
    ReplaySnapshot _0 _1 _2 _3 <> ReplaySnapshot _ __1 __2 __3 = ReplaySnapshot _0 (_1 <|> __1) (_2 <|> __2) (_3 <|> __3)


------------------------------------------------------------------------------
-- | Replays the layer snapshot and returns the resulting bitmap.
data ReplaySnapshotResult = ReplaySnapshotResult
    { -- | A data: URL for resulting image.
      dataURL :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ReplaySnapshotResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "replaySnapshotResult" $ \_o -> ReplaySnapshotResult
            <$> _o .: "dataURL"
        ago = A.withArray "replaySnapshotResult" $ \_a -> ReplaySnapshotResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON ReplaySnapshotResult where
    toEncoding (ReplaySnapshotResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "dataURL" .= _0
        ]
    toJSON (ReplaySnapshotResult _0) = A.object $ P.catMaybes
        [ P.pure $ "dataURL" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup ReplaySnapshotResult where
    ReplaySnapshotResult _0 <> ReplaySnapshotResult _ = ReplaySnapshotResult _0


------------------------------------------------------------------------------
instance M.Method ReplaySnapshot where
    type Result ReplaySnapshot = ReplaySnapshotResult
    name _ = "LayerTree.replaySnapshot"


------------------------------------------------------------------------------
-- | Replays the layer snapshot and returns the resulting bitmap.
replaySnapshot
    :: SnapshotId
    -- ^ The id of the layer snapshot.

    -> ReplaySnapshot
replaySnapshot _0 = ReplaySnapshot _0 P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Replays the layer snapshot and returns canvas log.
data SnapshotCommandLog = SnapshotCommandLog
    { -- | The id of the layer snapshot.
      snapshotId :: !SnapshotId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SnapshotCommandLog where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "snapshotCommandLog" $ \_o -> SnapshotCommandLog
            <$> _o .: "snapshotId"
        ago = A.withArray "snapshotCommandLog" $ \_a -> SnapshotCommandLog
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SnapshotCommandLog where
    toEncoding (SnapshotCommandLog _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "snapshotId" .= _0
        ]
    toJSON (SnapshotCommandLog _0) = A.object $ P.catMaybes
        [ P.pure $ "snapshotId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SnapshotCommandLog where
    SnapshotCommandLog _0 <> SnapshotCommandLog _ = SnapshotCommandLog _0


------------------------------------------------------------------------------
-- | Replays the layer snapshot and returns canvas log.
data SnapshotCommandLogResult = SnapshotCommandLogResult
    { -- | The array of canvas function calls.
      commandLog :: ![A.Object]
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SnapshotCommandLogResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "snapshotCommandLogResult" $ \_o -> SnapshotCommandLogResult
            <$> _o .: "commandLog"
        ago = A.withArray "snapshotCommandLogResult" $ \_a -> SnapshotCommandLogResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SnapshotCommandLogResult where
    toEncoding (SnapshotCommandLogResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "commandLog" .= _0
        ]
    toJSON (SnapshotCommandLogResult _0) = A.object $ P.catMaybes
        [ P.pure $ "commandLog" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SnapshotCommandLogResult where
    SnapshotCommandLogResult _0 <> SnapshotCommandLogResult _ = SnapshotCommandLogResult _0


------------------------------------------------------------------------------
instance M.Method SnapshotCommandLog where
    type Result SnapshotCommandLog = SnapshotCommandLogResult
    name _ = "LayerTree.snapshotCommandLog"


------------------------------------------------------------------------------
-- | Replays the layer snapshot and returns canvas log.
snapshotCommandLog
    :: SnapshotId
    -- ^ The id of the layer snapshot.

    -> SnapshotCommandLog
snapshotCommandLog _0 = SnapshotCommandLog _0


------------------------------------------------------------------------------
data LayerPainted = LayerPainted
    { -- | The id of the painted layer.
      layerId :: !LayerId
      -- | Clip rectangle.
    , clip :: !DOM.Rect
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON LayerPainted where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "layerPainted" $ \_o -> LayerPainted
            <$> _o .: "layerId"
            <*> _o .: "clip"
        ago = A.withArray "layerPainted" $ \_a -> LayerPainted
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON LayerPainted where
    toEncoding (LayerPainted _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "layerId" .= _0
        , P.pure $ "clip" .= _1
        ]
    toJSON (LayerPainted _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "layerId" .= _0
        , P.pure $ "clip" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup LayerPainted where
    LayerPainted _0 _1 <> LayerPainted _ _ = LayerPainted _0 _1


------------------------------------------------------------------------------
instance E.Event LayerPainted where
    type Result LayerPainted = LayerPainted
    name _ = "LayerTree.layerPainted"


------------------------------------------------------------------------------
layerPainted :: P.Proxy LayerPainted
layerPainted = P.Proxy


------------------------------------------------------------------------------
data LayerTreeDidChange = LayerTreeDidChange
    { -- | Layer tree, absent if not in the comspositing mode.
      layers :: !(P.Maybe [Layer])
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON LayerTreeDidChange where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "layerTreeDidChange" $ \_o -> LayerTreeDidChange
            <$> _o .:? "layers"
        ago = A.withArray "layerTreeDidChange" $ \_a -> LayerTreeDidChange
            <$> P.traverse A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON LayerTreeDidChange where
    toEncoding (LayerTreeDidChange _0) = A.pairs $ P.fold $ P.catMaybes
        [ ("layers" .=) <$> _0
        ]
    toJSON (LayerTreeDidChange _0) = A.object $ P.catMaybes
        [ ("layers" .=) <$> _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup LayerTreeDidChange where
    LayerTreeDidChange _0 <> LayerTreeDidChange __0 = LayerTreeDidChange (_0 <|> __0)


------------------------------------------------------------------------------
instance P.Monoid LayerTreeDidChange where
    mempty = LayerTreeDidChange P.empty


------------------------------------------------------------------------------
instance E.Event LayerTreeDidChange where
    type Result LayerTreeDidChange = LayerTreeDidChange
    name _ = "LayerTree.layerTreeDidChange"


------------------------------------------------------------------------------
layerTreeDidChange :: P.Proxy LayerTreeDidChange
layerTreeDidChange = P.Proxy

