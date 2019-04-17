{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.HeapProfiler{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

    ( module DevTools.API.HeapProfiler.Types
    , module DevTools.API.HeapProfiler
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
import           DevTools.API.HeapProfiler.Types
import qualified DevTools.API.Runtime.Types as Runtime


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Enables console to refer to the node with given id via $x (see Command Line API for more details
-- $x functions).
data AddInspectedHeapObject = AddInspectedHeapObject
    { -- | Heap snapshot object id to be accessible by means of $x command line API.
      heapObjectId :: !HeapSnapshotObjectId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AddInspectedHeapObject where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "addInspectedHeapObject" $ \_o -> AddInspectedHeapObject
            <$> _o .: "heapObjectId"
        ago = A.withArray "addInspectedHeapObject" $ \_a -> AddInspectedHeapObject
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON AddInspectedHeapObject where
    toEncoding (AddInspectedHeapObject _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "heapObjectId" .= _0
        ]
    toJSON (AddInspectedHeapObject _0) = A.object $ P.catMaybes
        [ P.pure $ "heapObjectId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup AddInspectedHeapObject where
    AddInspectedHeapObject _0 <> AddInspectedHeapObject _ = AddInspectedHeapObject _0


------------------------------------------------------------------------------
instance M.Method AddInspectedHeapObject where
    type Result AddInspectedHeapObject = ()
    name _ = "HeapProfiler.addInspectedHeapObject"


------------------------------------------------------------------------------
-- | Enables console to refer to the node with given id via $x (see Command Line API for more details
-- $x functions).
addInspectedHeapObject
    :: HeapSnapshotObjectId
    -- ^ Heap snapshot object id to be accessible by means of $x command line API.

    -> AddInspectedHeapObject
addInspectedHeapObject _0 = AddInspectedHeapObject _0


------------------------------------------------------------------------------
data CollectGarbage = CollectGarbage
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CollectGarbage where
    parseJSON A.Null = P.pure CollectGarbage
    parseJSON v = A.withArray "collectGarbage" go v
        <|> A.withObject "collectGarbage" go v
      where
        go _ = P.pure CollectGarbage


------------------------------------------------------------------------------
instance A.ToJSON CollectGarbage where
    toEncoding CollectGarbage = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON CollectGarbage = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup CollectGarbage where
    CollectGarbage <> CollectGarbage = CollectGarbage


------------------------------------------------------------------------------
instance P.Monoid CollectGarbage where
    mempty = CollectGarbage


------------------------------------------------------------------------------
instance M.Method CollectGarbage where
    type Result CollectGarbage = ()
    name _ = "HeapProfiler.collectGarbage"


------------------------------------------------------------------------------
collectGarbage
    :: CollectGarbage
collectGarbage = CollectGarbage


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
    name _ = "HeapProfiler.disable"


------------------------------------------------------------------------------
disable
    :: Disable
disable = Disable


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
    name _ = "HeapProfiler.enable"


------------------------------------------------------------------------------
enable
    :: Enable
enable = Enable


------------------------------------------------------------------------------
data GetHeapObjectId = GetHeapObjectId
    { -- | Identifier of the object to get heap object id for.
      objectId :: !Runtime.RemoteObjectId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetHeapObjectId where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getHeapObjectId" $ \_o -> GetHeapObjectId
            <$> _o .: "objectId"
        ago = A.withArray "getHeapObjectId" $ \_a -> GetHeapObjectId
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetHeapObjectId where
    toEncoding (GetHeapObjectId _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "objectId" .= _0
        ]
    toJSON (GetHeapObjectId _0) = A.object $ P.catMaybes
        [ P.pure $ "objectId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetHeapObjectId where
    GetHeapObjectId _0 <> GetHeapObjectId _ = GetHeapObjectId _0


------------------------------------------------------------------------------
data GetHeapObjectIdResult = GetHeapObjectIdResult
    { -- | Id of the heap snapshot object corresponding to the passed remote object id.
      heapSnapshotObjectId :: !HeapSnapshotObjectId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetHeapObjectIdResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getHeapObjectIdResult" $ \_o -> GetHeapObjectIdResult
            <$> _o .: "heapSnapshotObjectId"
        ago = A.withArray "getHeapObjectIdResult" $ \_a -> GetHeapObjectIdResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetHeapObjectIdResult where
    toEncoding (GetHeapObjectIdResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "heapSnapshotObjectId" .= _0
        ]
    toJSON (GetHeapObjectIdResult _0) = A.object $ P.catMaybes
        [ P.pure $ "heapSnapshotObjectId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetHeapObjectIdResult where
    GetHeapObjectIdResult _0 <> GetHeapObjectIdResult _ = GetHeapObjectIdResult _0


------------------------------------------------------------------------------
instance M.Method GetHeapObjectId where
    type Result GetHeapObjectId = GetHeapObjectIdResult
    name _ = "HeapProfiler.getHeapObjectId"


------------------------------------------------------------------------------
getHeapObjectId
    :: Runtime.RemoteObjectId
    -- ^ Identifier of the object to get heap object id for.

    -> GetHeapObjectId
getHeapObjectId _0 = GetHeapObjectId _0


------------------------------------------------------------------------------
data GetObjectByHeapObjectId = GetObjectByHeapObjectId
    { objectId :: !HeapSnapshotObjectId
      -- | Symbolic group name that can be used to release multiple objects.
    , objectGroup :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetObjectByHeapObjectId where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getObjectByHeapObjectId" $ \_o -> GetObjectByHeapObjectId
            <$> _o .: "objectId"
            <*> _o .:? "objectGroup"
        ago = A.withArray "getObjectByHeapObjectId" $ \_a -> GetObjectByHeapObjectId
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON GetObjectByHeapObjectId where
    toEncoding (GetObjectByHeapObjectId _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "objectId" .= _0
        , ("objectGroup" .=) <$> _1
        ]
    toJSON (GetObjectByHeapObjectId _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "objectId" .= _0
        , ("objectGroup" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetObjectByHeapObjectId where
    GetObjectByHeapObjectId _0 _1 <> GetObjectByHeapObjectId _ __1 = GetObjectByHeapObjectId _0 (_1 <|> __1)


------------------------------------------------------------------------------
data GetObjectByHeapObjectIdResult = GetObjectByHeapObjectIdResult
    { -- | Evaluation result.
      result :: !Runtime.RemoteObject
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetObjectByHeapObjectIdResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getObjectByHeapObjectIdResult" $ \_o -> GetObjectByHeapObjectIdResult
            <$> _o .: "result"
        ago = A.withArray "getObjectByHeapObjectIdResult" $ \_a -> GetObjectByHeapObjectIdResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetObjectByHeapObjectIdResult where
    toEncoding (GetObjectByHeapObjectIdResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "result" .= _0
        ]
    toJSON (GetObjectByHeapObjectIdResult _0) = A.object $ P.catMaybes
        [ P.pure $ "result" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetObjectByHeapObjectIdResult where
    GetObjectByHeapObjectIdResult _0 <> GetObjectByHeapObjectIdResult _ = GetObjectByHeapObjectIdResult _0


------------------------------------------------------------------------------
instance M.Method GetObjectByHeapObjectId where
    type Result GetObjectByHeapObjectId = GetObjectByHeapObjectIdResult
    name _ = "HeapProfiler.getObjectByHeapObjectId"


------------------------------------------------------------------------------
getObjectByHeapObjectId
    :: HeapSnapshotObjectId
    -> GetObjectByHeapObjectId
getObjectByHeapObjectId _0 = GetObjectByHeapObjectId _0 P.empty


------------------------------------------------------------------------------
data GetSamplingProfile = GetSamplingProfile
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetSamplingProfile where
    parseJSON A.Null = P.pure GetSamplingProfile
    parseJSON v = A.withArray "getSamplingProfile" go v
        <|> A.withObject "getSamplingProfile" go v
      where
        go _ = P.pure GetSamplingProfile


------------------------------------------------------------------------------
instance A.ToJSON GetSamplingProfile where
    toEncoding GetSamplingProfile = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetSamplingProfile = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetSamplingProfile where
    GetSamplingProfile <> GetSamplingProfile = GetSamplingProfile


------------------------------------------------------------------------------
instance P.Monoid GetSamplingProfile where
    mempty = GetSamplingProfile


------------------------------------------------------------------------------
data GetSamplingProfileResult = GetSamplingProfileResult
    { -- | Return the sampling profile being collected.
      profile :: !SamplingHeapProfile
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetSamplingProfileResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getSamplingProfileResult" $ \_o -> GetSamplingProfileResult
            <$> _o .: "profile"
        ago = A.withArray "getSamplingProfileResult" $ \_a -> GetSamplingProfileResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetSamplingProfileResult where
    toEncoding (GetSamplingProfileResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "profile" .= _0
        ]
    toJSON (GetSamplingProfileResult _0) = A.object $ P.catMaybes
        [ P.pure $ "profile" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetSamplingProfileResult where
    GetSamplingProfileResult _0 <> GetSamplingProfileResult _ = GetSamplingProfileResult _0


------------------------------------------------------------------------------
instance M.Method GetSamplingProfile where
    type Result GetSamplingProfile = GetSamplingProfileResult
    name _ = "HeapProfiler.getSamplingProfile"


------------------------------------------------------------------------------
getSamplingProfile
    :: GetSamplingProfile
getSamplingProfile = GetSamplingProfile


------------------------------------------------------------------------------
data StartSampling = StartSampling
    { -- | Average sample interval in bytes. Poisson distribution is used for the intervals. The
      -- default value is 32768 bytes.
      samplingInterval :: !(P.Maybe P.Double)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StartSampling where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "startSampling" $ \_o -> StartSampling
            <$> _o .:? "samplingInterval"
        ago = A.withArray "startSampling" $ \_a -> StartSampling
            <$> P.traverse A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON StartSampling where
    toEncoding (StartSampling _0) = A.pairs $ P.fold $ P.catMaybes
        [ ("samplingInterval" .=) <$> _0
        ]
    toJSON (StartSampling _0) = A.object $ P.catMaybes
        [ ("samplingInterval" .=) <$> _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup StartSampling where
    StartSampling _0 <> StartSampling __0 = StartSampling (_0 <|> __0)


------------------------------------------------------------------------------
instance P.Monoid StartSampling where
    mempty = StartSampling P.empty


------------------------------------------------------------------------------
instance M.Method StartSampling where
    type Result StartSampling = ()
    name _ = "HeapProfiler.startSampling"


------------------------------------------------------------------------------
startSampling
    :: StartSampling
startSampling = StartSampling P.empty


------------------------------------------------------------------------------
data StartTrackingHeapObjects = StartTrackingHeapObjects
    { trackAllocations :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StartTrackingHeapObjects where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "startTrackingHeapObjects" $ \_o -> StartTrackingHeapObjects
            <$> _o .:? "trackAllocations"
        ago = A.withArray "startTrackingHeapObjects" $ \_a -> StartTrackingHeapObjects
            <$> P.traverse A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON StartTrackingHeapObjects where
    toEncoding (StartTrackingHeapObjects _0) = A.pairs $ P.fold $ P.catMaybes
        [ ("trackAllocations" .=) <$> _0
        ]
    toJSON (StartTrackingHeapObjects _0) = A.object $ P.catMaybes
        [ ("trackAllocations" .=) <$> _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup StartTrackingHeapObjects where
    StartTrackingHeapObjects _0 <> StartTrackingHeapObjects __0 = StartTrackingHeapObjects (_0 <|> __0)


------------------------------------------------------------------------------
instance P.Monoid StartTrackingHeapObjects where
    mempty = StartTrackingHeapObjects P.empty


------------------------------------------------------------------------------
instance M.Method StartTrackingHeapObjects where
    type Result StartTrackingHeapObjects = ()
    name _ = "HeapProfiler.startTrackingHeapObjects"


------------------------------------------------------------------------------
startTrackingHeapObjects
    :: StartTrackingHeapObjects
startTrackingHeapObjects = StartTrackingHeapObjects P.empty


------------------------------------------------------------------------------
data StopSampling = StopSampling
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StopSampling where
    parseJSON A.Null = P.pure StopSampling
    parseJSON v = A.withArray "stopSampling" go v
        <|> A.withObject "stopSampling" go v
      where
        go _ = P.pure StopSampling


------------------------------------------------------------------------------
instance A.ToJSON StopSampling where
    toEncoding StopSampling = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON StopSampling = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup StopSampling where
    StopSampling <> StopSampling = StopSampling


------------------------------------------------------------------------------
instance P.Monoid StopSampling where
    mempty = StopSampling


------------------------------------------------------------------------------
data StopSamplingResult = StopSamplingResult
    { -- | Recorded sampling heap profile.
      profile :: !SamplingHeapProfile
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StopSamplingResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "stopSamplingResult" $ \_o -> StopSamplingResult
            <$> _o .: "profile"
        ago = A.withArray "stopSamplingResult" $ \_a -> StopSamplingResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON StopSamplingResult where
    toEncoding (StopSamplingResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "profile" .= _0
        ]
    toJSON (StopSamplingResult _0) = A.object $ P.catMaybes
        [ P.pure $ "profile" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup StopSamplingResult where
    StopSamplingResult _0 <> StopSamplingResult _ = StopSamplingResult _0


------------------------------------------------------------------------------
instance M.Method StopSampling where
    type Result StopSampling = StopSamplingResult
    name _ = "HeapProfiler.stopSampling"


------------------------------------------------------------------------------
stopSampling
    :: StopSampling
stopSampling = StopSampling


------------------------------------------------------------------------------
data StopTrackingHeapObjects = StopTrackingHeapObjects
    { -- | If true 'reportHeapSnapshotProgress' events will be generated while snapshot is being taken
      -- when the tracking is stopped.
      reportProgress :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StopTrackingHeapObjects where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "stopTrackingHeapObjects" $ \_o -> StopTrackingHeapObjects
            <$> _o .:? "reportProgress"
        ago = A.withArray "stopTrackingHeapObjects" $ \_a -> StopTrackingHeapObjects
            <$> P.traverse A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON StopTrackingHeapObjects where
    toEncoding (StopTrackingHeapObjects _0) = A.pairs $ P.fold $ P.catMaybes
        [ ("reportProgress" .=) <$> _0
        ]
    toJSON (StopTrackingHeapObjects _0) = A.object $ P.catMaybes
        [ ("reportProgress" .=) <$> _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup StopTrackingHeapObjects where
    StopTrackingHeapObjects _0 <> StopTrackingHeapObjects __0 = StopTrackingHeapObjects (_0 <|> __0)


------------------------------------------------------------------------------
instance P.Monoid StopTrackingHeapObjects where
    mempty = StopTrackingHeapObjects P.empty


------------------------------------------------------------------------------
instance M.Method StopTrackingHeapObjects where
    type Result StopTrackingHeapObjects = ()
    name _ = "HeapProfiler.stopTrackingHeapObjects"


------------------------------------------------------------------------------
stopTrackingHeapObjects
    :: StopTrackingHeapObjects
stopTrackingHeapObjects = StopTrackingHeapObjects P.empty


------------------------------------------------------------------------------
data TakeHeapSnapshot = TakeHeapSnapshot
    { -- | If true 'reportHeapSnapshotProgress' events will be generated while snapshot is being taken.
      reportProgress :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TakeHeapSnapshot where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "takeHeapSnapshot" $ \_o -> TakeHeapSnapshot
            <$> _o .:? "reportProgress"
        ago = A.withArray "takeHeapSnapshot" $ \_a -> TakeHeapSnapshot
            <$> P.traverse A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON TakeHeapSnapshot where
    toEncoding (TakeHeapSnapshot _0) = A.pairs $ P.fold $ P.catMaybes
        [ ("reportProgress" .=) <$> _0
        ]
    toJSON (TakeHeapSnapshot _0) = A.object $ P.catMaybes
        [ ("reportProgress" .=) <$> _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup TakeHeapSnapshot where
    TakeHeapSnapshot _0 <> TakeHeapSnapshot __0 = TakeHeapSnapshot (_0 <|> __0)


------------------------------------------------------------------------------
instance P.Monoid TakeHeapSnapshot where
    mempty = TakeHeapSnapshot P.empty


------------------------------------------------------------------------------
instance M.Method TakeHeapSnapshot where
    type Result TakeHeapSnapshot = ()
    name _ = "HeapProfiler.takeHeapSnapshot"


------------------------------------------------------------------------------
takeHeapSnapshot
    :: TakeHeapSnapshot
takeHeapSnapshot = TakeHeapSnapshot P.empty


------------------------------------------------------------------------------
data AddHeapSnapshotChunk = AddHeapSnapshotChunk
    { chunk :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AddHeapSnapshotChunk where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "addHeapSnapshotChunk" $ \_o -> AddHeapSnapshotChunk
            <$> _o .: "chunk"
        ago = A.withArray "addHeapSnapshotChunk" $ \_a -> AddHeapSnapshotChunk
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON AddHeapSnapshotChunk where
    toEncoding (AddHeapSnapshotChunk _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "chunk" .= _0
        ]
    toJSON (AddHeapSnapshotChunk _0) = A.object $ P.catMaybes
        [ P.pure $ "chunk" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup AddHeapSnapshotChunk where
    AddHeapSnapshotChunk _0 <> AddHeapSnapshotChunk _ = AddHeapSnapshotChunk _0


------------------------------------------------------------------------------
instance E.Event AddHeapSnapshotChunk where
    type Result AddHeapSnapshotChunk = AddHeapSnapshotChunk
    name _ = "HeapProfiler.addHeapSnapshotChunk"


------------------------------------------------------------------------------
addHeapSnapshotChunk :: P.Proxy AddHeapSnapshotChunk
addHeapSnapshotChunk = P.Proxy


------------------------------------------------------------------------------
-- | If heap objects tracking has been started then backend may send update for one or more fragments
data HeapStatsUpdate = HeapStatsUpdate
    { -- | An array of triplets. Each triplet describes a fragment. The first integer is the fragment
      -- index, the second integer is a total count of objects for the fragment, the third integer is
      -- a total size of the objects for the fragment.
      statsUpdate :: ![P.Int]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON HeapStatsUpdate where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "heapStatsUpdate" $ \_o -> HeapStatsUpdate
            <$> _o .: "statsUpdate"
        ago = A.withArray "heapStatsUpdate" $ \_a -> HeapStatsUpdate
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON HeapStatsUpdate where
    toEncoding (HeapStatsUpdate _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "statsUpdate" .= _0
        ]
    toJSON (HeapStatsUpdate _0) = A.object $ P.catMaybes
        [ P.pure $ "statsUpdate" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup HeapStatsUpdate where
    HeapStatsUpdate _0 <> HeapStatsUpdate _ = HeapStatsUpdate _0


------------------------------------------------------------------------------
instance E.Event HeapStatsUpdate where
    type Result HeapStatsUpdate = HeapStatsUpdate
    name _ = "HeapProfiler.heapStatsUpdate"


------------------------------------------------------------------------------
-- | If heap objects tracking has been started then backend may send update for one or more fragments
heapStatsUpdate :: P.Proxy HeapStatsUpdate
heapStatsUpdate = P.Proxy


------------------------------------------------------------------------------
-- | If heap objects tracking has been started then backend regularly sends a current value for last
-- seen object id and corresponding timestamp. If the were changes in the heap since last event
-- then one or more heapStatsUpdate events will be sent before a new lastSeenObjectId event.
data LastSeenObjectId = LastSeenObjectId
    { lastSeenObjectId_ :: !P.Int
    , timestamp :: !P.Double
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON LastSeenObjectId where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "lastSeenObjectId" $ \_o -> LastSeenObjectId
            <$> _o .: "lastSeenObjectId"
            <*> _o .: "timestamp"
        ago = A.withArray "lastSeenObjectId" $ \_a -> LastSeenObjectId
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON LastSeenObjectId where
    toEncoding (LastSeenObjectId _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "lastSeenObjectId" .= _0
        , P.pure $ "timestamp" .= _1
        ]
    toJSON (LastSeenObjectId _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "lastSeenObjectId" .= _0
        , P.pure $ "timestamp" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup LastSeenObjectId where
    LastSeenObjectId _0 _1 <> LastSeenObjectId _ _ = LastSeenObjectId _0 _1


------------------------------------------------------------------------------
instance E.Event LastSeenObjectId where
    type Result LastSeenObjectId = LastSeenObjectId
    name _ = "HeapProfiler.lastSeenObjectId"


------------------------------------------------------------------------------
-- | If heap objects tracking has been started then backend regularly sends a current value for last
-- seen object id and corresponding timestamp. If the were changes in the heap since last event
-- then one or more heapStatsUpdate events will be sent before a new lastSeenObjectId event.
lastSeenObjectId :: P.Proxy LastSeenObjectId
lastSeenObjectId = P.Proxy


------------------------------------------------------------------------------
data ReportHeapSnapshotProgress = ReportHeapSnapshotProgress
    { done :: !P.Int
    , total :: !P.Int
    , finished :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ReportHeapSnapshotProgress where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "reportHeapSnapshotProgress" $ \_o -> ReportHeapSnapshotProgress
            <$> _o .: "done"
            <*> _o .: "total"
            <*> _o .:? "finished"
        ago = A.withArray "reportHeapSnapshotProgress" $ \_a -> ReportHeapSnapshotProgress
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON ReportHeapSnapshotProgress where
    toEncoding (ReportHeapSnapshotProgress _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "done" .= _0
        , P.pure $ "total" .= _1
        , ("finished" .=) <$> _2
        ]
    toJSON (ReportHeapSnapshotProgress _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "done" .= _0
        , P.pure $ "total" .= _1
        , ("finished" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup ReportHeapSnapshotProgress where
    ReportHeapSnapshotProgress _0 _1 _2 <> ReportHeapSnapshotProgress _ _ __2 = ReportHeapSnapshotProgress _0 _1 (_2 <|> __2)


------------------------------------------------------------------------------
instance E.Event ReportHeapSnapshotProgress where
    type Result ReportHeapSnapshotProgress = ReportHeapSnapshotProgress
    name _ = "HeapProfiler.reportHeapSnapshotProgress"


------------------------------------------------------------------------------
reportHeapSnapshotProgress :: P.Proxy ReportHeapSnapshotProgress
reportHeapSnapshotProgress = P.Proxy


------------------------------------------------------------------------------
data ResetProfiles = ResetProfiles
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ResetProfiles where
    parseJSON A.Null = P.pure ResetProfiles
    parseJSON v = A.withArray "resetProfiles" go v
        <|> A.withObject "resetProfiles" go v
      where
        go _ = P.pure ResetProfiles


------------------------------------------------------------------------------
instance A.ToJSON ResetProfiles where
    toEncoding ResetProfiles = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON ResetProfiles = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup ResetProfiles where
    ResetProfiles <> ResetProfiles = ResetProfiles


------------------------------------------------------------------------------
instance P.Monoid ResetProfiles where
    mempty = ResetProfiles


------------------------------------------------------------------------------
instance E.Event ResetProfiles where
    type Result ResetProfiles = ()
    name _ = "HeapProfiler.resetProfiles"


------------------------------------------------------------------------------
resetProfiles :: P.Proxy ResetProfiles
resetProfiles = P.Proxy

