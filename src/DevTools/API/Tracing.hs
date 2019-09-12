{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.Tracing{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

    ( module DevTools.API.Tracing.Types
    , module DevTools.API.Tracing
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
import qualified DevTools.API.IO.Types as IO
import           DevTools.API.Tracing.Types


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Stop trace events collection.
data End = End
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON End where
    parseJSON A.Null = P.pure End
    parseJSON v = A.withArray "end" go v
        <|> A.withObject "end" go v
      where
        go _ = P.pure End


------------------------------------------------------------------------------
instance A.ToJSON End where
    toEncoding End = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON End = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup End where
    End <> End = End


------------------------------------------------------------------------------
instance P.Monoid End where
    mempty = End


------------------------------------------------------------------------------
instance M.Method End where
    type Result End = ()
    name _ = "Tracing.end"


------------------------------------------------------------------------------
-- | Stop trace events collection.
end
    :: End
end = End


------------------------------------------------------------------------------
-- | Gets supported tracing categories.
data GetCategories = GetCategories
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetCategories where
    parseJSON A.Null = P.pure GetCategories
    parseJSON v = A.withArray "getCategories" go v
        <|> A.withObject "getCategories" go v
      where
        go _ = P.pure GetCategories


------------------------------------------------------------------------------
instance A.ToJSON GetCategories where
    toEncoding GetCategories = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetCategories = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetCategories where
    GetCategories <> GetCategories = GetCategories


------------------------------------------------------------------------------
instance P.Monoid GetCategories where
    mempty = GetCategories


------------------------------------------------------------------------------
-- | Gets supported tracing categories.
data GetCategoriesResult = GetCategoriesResult
    { -- | A list of supported tracing categories.
      categories :: ![T.Text]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetCategoriesResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getCategoriesResult" $ \_o -> GetCategoriesResult
            <$> _o .: "categories"
        ago = A.withArray "getCategoriesResult" $ \_a -> GetCategoriesResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetCategoriesResult where
    toEncoding (GetCategoriesResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "categories" .= _0
        ]
    toJSON (GetCategoriesResult _0) = A.object $ P.catMaybes
        [ P.pure $ "categories" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetCategoriesResult where
    GetCategoriesResult _0 <> GetCategoriesResult _ = GetCategoriesResult _0


------------------------------------------------------------------------------
instance M.Method GetCategories where
    type Result GetCategories = GetCategoriesResult
    name _ = "Tracing.getCategories"


------------------------------------------------------------------------------
-- | Gets supported tracing categories.
getCategories
    :: GetCategories
getCategories = GetCategories


------------------------------------------------------------------------------
-- | Record a clock sync marker in the trace.
data RecordClockSyncMarker = RecordClockSyncMarker
    { -- | The ID of this clock sync marker
      syncId :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RecordClockSyncMarker where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "recordClockSyncMarker" $ \_o -> RecordClockSyncMarker
            <$> _o .: "syncId"
        ago = A.withArray "recordClockSyncMarker" $ \_a -> RecordClockSyncMarker
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON RecordClockSyncMarker where
    toEncoding (RecordClockSyncMarker _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "syncId" .= _0
        ]
    toJSON (RecordClockSyncMarker _0) = A.object $ P.catMaybes
        [ P.pure $ "syncId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup RecordClockSyncMarker where
    RecordClockSyncMarker _0 <> RecordClockSyncMarker _ = RecordClockSyncMarker _0


------------------------------------------------------------------------------
instance M.Method RecordClockSyncMarker where
    type Result RecordClockSyncMarker = ()
    name _ = "Tracing.recordClockSyncMarker"


------------------------------------------------------------------------------
-- | Record a clock sync marker in the trace.
recordClockSyncMarker
    :: T.Text
    -- ^ The ID of this clock sync marker

    -> RecordClockSyncMarker
recordClockSyncMarker _0 = RecordClockSyncMarker _0


------------------------------------------------------------------------------
-- | Request a global memory dump.
data RequestMemoryDump = RequestMemoryDump
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RequestMemoryDump where
    parseJSON A.Null = P.pure RequestMemoryDump
    parseJSON v = A.withArray "requestMemoryDump" go v
        <|> A.withObject "requestMemoryDump" go v
      where
        go _ = P.pure RequestMemoryDump


------------------------------------------------------------------------------
instance A.ToJSON RequestMemoryDump where
    toEncoding RequestMemoryDump = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON RequestMemoryDump = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup RequestMemoryDump where
    RequestMemoryDump <> RequestMemoryDump = RequestMemoryDump


------------------------------------------------------------------------------
instance P.Monoid RequestMemoryDump where
    mempty = RequestMemoryDump


------------------------------------------------------------------------------
-- | Request a global memory dump.
data RequestMemoryDumpResult = RequestMemoryDumpResult
    { -- | GUID of the resulting global memory dump.
      dumpGuid :: !T.Text
      -- | True iff the global memory dump succeeded.
    , success :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RequestMemoryDumpResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "requestMemoryDumpResult" $ \_o -> RequestMemoryDumpResult
            <$> _o .: "dumpGuid"
            <*> _o .: "success"
        ago = A.withArray "requestMemoryDumpResult" $ \_a -> RequestMemoryDumpResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON RequestMemoryDumpResult where
    toEncoding (RequestMemoryDumpResult _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "dumpGuid" .= _0
        , P.pure $ "success" .= _1
        ]
    toJSON (RequestMemoryDumpResult _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "dumpGuid" .= _0
        , P.pure $ "success" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup RequestMemoryDumpResult where
    RequestMemoryDumpResult _0 _1 <> RequestMemoryDumpResult _ _ = RequestMemoryDumpResult _0 _1


------------------------------------------------------------------------------
instance M.Method RequestMemoryDump where
    type Result RequestMemoryDump = RequestMemoryDumpResult
    name _ = "Tracing.requestMemoryDump"


------------------------------------------------------------------------------
-- | Request a global memory dump.
requestMemoryDump
    :: RequestMemoryDump
requestMemoryDump = RequestMemoryDump


------------------------------------------------------------------------------
-- | Start trace events collection.
{-# DEPRECATED categories, options "This may be removed in a future release." #-}
data Start = Start
    { -- | Category\/tag filter
      categories :: !(P.Maybe T.Text)
      -- | Tracing options
    , options :: !(P.Maybe T.Text)
      -- | If set, the agent will issue bufferUsage events at this interval, specified in milliseconds
    , bufferUsageReportingInterval :: !(P.Maybe P.Double)
      -- | Whether to report trace events as series of dataCollected events or to save trace to a
      -- stream (defaults to @ReportEvents@).
    , transferMode :: !(P.Maybe TransferMode)
      -- | Trace data format to use. This only applies when using @ReturnAsStream@
      -- transfer mode (defaults to @json@).
    , streamFormat :: !(P.Maybe StreamFormat)
      -- | Compression format to use. This only applies when using @ReturnAsStream@
      -- transfer mode (defaults to @none@)
    , streamCompression :: !(P.Maybe StreamCompression)
    , traceConfig :: !(P.Maybe TraceConfig)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Start where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "start" $ \_o -> Start
            <$> _o .:? "categories"
            <*> _o .:? "options"
            <*> _o .:? "bufferUsageReportingInterval"
            <*> _o .:? "transferMode"
            <*> _o .:? "streamFormat"
            <*> _o .:? "streamCompression"
            <*> _o .:? "traceConfig"
        ago = A.withArray "start" $ \_a -> Start
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)


------------------------------------------------------------------------------
instance A.ToJSON Start where
    toEncoding (Start _0 _1 _2 _3 _4 _5 _6) = A.pairs $ P.fold $ P.catMaybes
        [ ("categories" .=) <$> _0
        , ("options" .=) <$> _1
        , ("bufferUsageReportingInterval" .=) <$> _2
        , ("transferMode" .=) <$> _3
        , ("streamFormat" .=) <$> _4
        , ("streamCompression" .=) <$> _5
        , ("traceConfig" .=) <$> _6
        ]
    toJSON (Start _0 _1 _2 _3 _4 _5 _6) = A.object $ P.catMaybes
        [ ("categories" .=) <$> _0
        , ("options" .=) <$> _1
        , ("bufferUsageReportingInterval" .=) <$> _2
        , ("transferMode" .=) <$> _3
        , ("streamFormat" .=) <$> _4
        , ("streamCompression" .=) <$> _5
        , ("traceConfig" .=) <$> _6
        ]


------------------------------------------------------------------------------
instance P.Semigroup Start where
    Start _0 _1 _2 _3 _4 _5 _6 <> Start __0 __1 __2 __3 __4 __5 __6 = Start (_0 <|> __0) (_1 <|> __1) (_2 <|> __2) (_3 <|> __3) (_4 <|> __4) (_5 <|> __5) (_6 <|> __6)


------------------------------------------------------------------------------
instance P.Monoid Start where
    mempty = Start P.empty P.empty P.empty P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
data TransferMode
    = ReportEvents
    | ReturnAsStream
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TransferMode where
    parseJSON = A.withText "TransferMode" $ \t -> case t of
        "ReportEvents" -> P.pure ReportEvents
        "ReturnAsStream" -> P.pure ReturnAsStream
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON TransferMode where
    toJSON ReportEvents = "ReportEvents"
    toJSON ReturnAsStream = "ReturnAsStream"


------------------------------------------------------------------------------
instance M.Method Start where
    type Result Start = ()
    name _ = "Tracing.start"


------------------------------------------------------------------------------
-- | Start trace events collection.
start
    :: Start
start = Start P.empty P.empty P.empty P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
data BufferUsage = BufferUsage
    { -- | A number in range [0..1] that indicates the used size of event buffer as a fraction of its
      -- total size.
      percentFull :: !(P.Maybe P.Double)
      -- | An approximate number of events in the trace log.
    , eventCount :: !(P.Maybe P.Double)
      -- | A number in range [0..1] that indicates the used size of event buffer as a fraction of its
      -- total size.
    , value :: !(P.Maybe P.Double)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON BufferUsage where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "bufferUsage" $ \_o -> BufferUsage
            <$> _o .:? "percentFull"
            <*> _o .:? "eventCount"
            <*> _o .:? "value"
        ago = A.withArray "bufferUsage" $ \_a -> BufferUsage
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON BufferUsage where
    toEncoding (BufferUsage _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ ("percentFull" .=) <$> _0
        , ("eventCount" .=) <$> _1
        , ("value" .=) <$> _2
        ]
    toJSON (BufferUsage _0 _1 _2) = A.object $ P.catMaybes
        [ ("percentFull" .=) <$> _0
        , ("eventCount" .=) <$> _1
        , ("value" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup BufferUsage where
    BufferUsage _0 _1 _2 <> BufferUsage __0 __1 __2 = BufferUsage (_0 <|> __0) (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
instance P.Monoid BufferUsage where
    mempty = BufferUsage P.empty P.empty P.empty


------------------------------------------------------------------------------
instance E.Event BufferUsage where
    type Result BufferUsage = BufferUsage
    name _ = "Tracing.bufferUsage"


------------------------------------------------------------------------------
bufferUsage :: P.Proxy BufferUsage
bufferUsage = P.Proxy


------------------------------------------------------------------------------
-- | Contains an bucket of collected trace events. When tracing is stopped collected events will be
-- send as a sequence of dataCollected events followed by tracingComplete event.
data DataCollected = DataCollected
    { value :: ![A.Object]
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DataCollected where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "dataCollected" $ \_o -> DataCollected
            <$> _o .: "value"
        ago = A.withArray "dataCollected" $ \_a -> DataCollected
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON DataCollected where
    toEncoding (DataCollected _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "value" .= _0
        ]
    toJSON (DataCollected _0) = A.object $ P.catMaybes
        [ P.pure $ "value" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup DataCollected where
    DataCollected _0 <> DataCollected _ = DataCollected _0


------------------------------------------------------------------------------
instance E.Event DataCollected where
    type Result DataCollected = DataCollected
    name _ = "Tracing.dataCollected"


------------------------------------------------------------------------------
-- | Contains an bucket of collected trace events. When tracing is stopped collected events will be
-- send as a sequence of dataCollected events followed by tracingComplete event.
dataCollected :: P.Proxy DataCollected
dataCollected = P.Proxy


------------------------------------------------------------------------------
-- | Signals that tracing is stopped and there is no trace buffers pending flush, all data were
-- delivered via dataCollected events.
data TracingComplete = TracingComplete
    { -- | Indicates whether some trace data is known to have been lost, e.g. because the trace ring
      -- buffer wrapped around.
      dataLossOccurred :: !P.Bool
      -- | A handle of the stream that holds resulting trace data.
    , stream :: !(P.Maybe IO.StreamHandle)
      -- | Trace data format of returned stream.
    , traceFormat :: !(P.Maybe StreamFormat)
      -- | Compression format of returned stream.
    , streamCompression :: !(P.Maybe StreamCompression)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TracingComplete where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "tracingComplete" $ \_o -> TracingComplete
            <$> _o .: "dataLossOccurred"
            <*> _o .:? "stream"
            <*> _o .:? "traceFormat"
            <*> _o .:? "streamCompression"
        ago = A.withArray "tracingComplete" $ \_a -> TracingComplete
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON TracingComplete where
    toEncoding (TracingComplete _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "dataLossOccurred" .= _0
        , ("stream" .=) <$> _1
        , ("traceFormat" .=) <$> _2
        , ("streamCompression" .=) <$> _3
        ]
    toJSON (TracingComplete _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "dataLossOccurred" .= _0
        , ("stream" .=) <$> _1
        , ("traceFormat" .=) <$> _2
        , ("streamCompression" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup TracingComplete where
    TracingComplete _0 _1 _2 _3 <> TracingComplete _ __1 __2 __3 = TracingComplete _0 (_1 <|> __1) (_2 <|> __2) (_3 <|> __3)


------------------------------------------------------------------------------
instance E.Event TracingComplete where
    type Result TracingComplete = TracingComplete
    name _ = "Tracing.tracingComplete"


------------------------------------------------------------------------------
-- | Signals that tracing is stopped and there is no trace buffers pending flush, all data were
-- delivered via dataCollected events.
tracingComplete :: P.Proxy TracingComplete
tracingComplete = P.Proxy

