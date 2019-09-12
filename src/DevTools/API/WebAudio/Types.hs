{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | This domain allows inspection of Web Audio API.
-- https:\/\/webaudio.github.io\/web-audio-api\/
module DevTools.API.WebAudio.Types{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

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
-- | An unique ID for a graph object (AudioContext, AudioNode, AudioParam) in Web Audio API
type GraphObjectId = T.Text


------------------------------------------------------------------------------
-- | Enum of BaseAudioContext types
data ContextType
    = Realtime
    | Offline
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ContextType where
    parseJSON = A.withText "ContextType" $ \t -> case t of
        "realtime" -> P.pure Realtime
        "offline" -> P.pure Offline
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON ContextType where
    toJSON Realtime = "realtime"
    toJSON Offline = "offline"


------------------------------------------------------------------------------
-- | Enum of AudioContextState from the spec
data ContextState
    = Suspended
    | Running
    | Closed
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ContextState where
    parseJSON = A.withText "ContextState" $ \t -> case t of
        "suspended" -> P.pure Suspended
        "running" -> P.pure Running
        "closed" -> P.pure Closed
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON ContextState where
    toJSON Suspended = "suspended"
    toJSON Running = "running"
    toJSON Closed = "closed"


------------------------------------------------------------------------------
-- | Enum of AudioNode types
type NodeType = T.Text


------------------------------------------------------------------------------
-- | Enum of AudioNode::ChannelCountMode from the spec
data ChannelCountMode
    = ClampedMax
    | Explicit
    | Max
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ChannelCountMode where
    parseJSON = A.withText "ChannelCountMode" $ \t -> case t of
        "clamped-max" -> P.pure ClampedMax
        "explicit" -> P.pure Explicit
        "max" -> P.pure Max
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON ChannelCountMode where
    toJSON ClampedMax = "clamped-max"
    toJSON Explicit = "explicit"
    toJSON Max = "max"


------------------------------------------------------------------------------
-- | Enum of AudioNode::ChannelInterpretation from the spec
data ChannelInterpretation
    = Discrete
    | Speakers
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ChannelInterpretation where
    parseJSON = A.withText "ChannelInterpretation" $ \t -> case t of
        "discrete" -> P.pure Discrete
        "speakers" -> P.pure Speakers
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON ChannelInterpretation where
    toJSON Discrete = "discrete"
    toJSON Speakers = "speakers"


------------------------------------------------------------------------------
-- | Enum of AudioParam types
type ParamType = T.Text


------------------------------------------------------------------------------
-- | Enum of AudioParam::AutomationRate from the spec
data AutomationRate
    = ARate
    | KRate
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AutomationRate where
    parseJSON = A.withText "AutomationRate" $ \t -> case t of
        "a-rate" -> P.pure ARate
        "k-rate" -> P.pure KRate
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON AutomationRate where
    toJSON ARate = "a-rate"
    toJSON KRate = "k-rate"


------------------------------------------------------------------------------
-- | Fields in AudioContext that change in real-time.
data ContextRealtimeData = ContextRealtimeData
    { -- | The current context time in second in BaseAudioContext.
      currentTime :: !P.Double
      -- | The time spent on rendering graph divided by render qunatum duration,
      -- and multiplied by 100. 100 means the audio renderer reached the full
      -- capacity and glitch may occur.
    , renderCapacity :: !P.Double
      -- | A running mean of callback interval.
    , callbackIntervalMean :: !P.Double
      -- | A running variance of callback interval.
    , callbackIntervalVariance :: !P.Double
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ContextRealtimeData where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "ContextRealtimeData" $ \_o -> ContextRealtimeData
            <$> _o .: "currentTime"
            <*> _o .: "renderCapacity"
            <*> _o .: "callbackIntervalMean"
            <*> _o .: "callbackIntervalVariance"
        ago = A.withArray "ContextRealtimeData" $ \_a -> ContextRealtimeData
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON ContextRealtimeData where
    toEncoding (ContextRealtimeData _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "currentTime" .= _0
        , P.pure $ "renderCapacity" .= _1
        , P.pure $ "callbackIntervalMean" .= _2
        , P.pure $ "callbackIntervalVariance" .= _3
        ]
    toJSON (ContextRealtimeData _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "currentTime" .= _0
        , P.pure $ "renderCapacity" .= _1
        , P.pure $ "callbackIntervalMean" .= _2
        , P.pure $ "callbackIntervalVariance" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup ContextRealtimeData where
    ContextRealtimeData _0 _1 _2 _3 <> ContextRealtimeData _ _ _ _ = ContextRealtimeData _0 _1 _2 _3


------------------------------------------------------------------------------
-- | Protocol object for BaseAudioContext
data BaseAudioContext = BaseAudioContext
    { contextId :: !GraphObjectId
    , contextType :: !ContextType
    , contextState :: !ContextState
    , realtimeData :: !(P.Maybe ContextRealtimeData)
      -- | Platform-dependent callback buffer size.
    , callbackBufferSize :: !P.Double
      -- | Number of output channels supported by audio hardware in use.
    , maxOutputChannelCount :: !P.Double
      -- | Context sample rate.
    , sampleRate :: !P.Double
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON BaseAudioContext where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "BaseAudioContext" $ \_o -> BaseAudioContext
            <$> _o .: "contextId"
            <*> _o .: "contextType"
            <*> _o .: "contextState"
            <*> _o .:? "realtimeData"
            <*> _o .: "callbackBufferSize"
            <*> _o .: "maxOutputChannelCount"
            <*> _o .: "sampleRate"
        ago = A.withArray "BaseAudioContext" $ \_a -> BaseAudioContext
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.maybe P.empty A.parseJSON (_a !? 6)


------------------------------------------------------------------------------
instance A.ToJSON BaseAudioContext where
    toEncoding (BaseAudioContext _0 _1 _2 _3 _4 _5 _6) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "contextId" .= _0
        , P.pure $ "contextType" .= _1
        , P.pure $ "contextState" .= _2
        , ("realtimeData" .=) <$> _3
        , P.pure $ "callbackBufferSize" .= _4
        , P.pure $ "maxOutputChannelCount" .= _5
        , P.pure $ "sampleRate" .= _6
        ]
    toJSON (BaseAudioContext _0 _1 _2 _3 _4 _5 _6) = A.object $ P.catMaybes
        [ P.pure $ "contextId" .= _0
        , P.pure $ "contextType" .= _1
        , P.pure $ "contextState" .= _2
        , ("realtimeData" .=) <$> _3
        , P.pure $ "callbackBufferSize" .= _4
        , P.pure $ "maxOutputChannelCount" .= _5
        , P.pure $ "sampleRate" .= _6
        ]


------------------------------------------------------------------------------
instance P.Semigroup BaseAudioContext where
    BaseAudioContext _0 _1 _2 _3 _4 _5 _6 <> BaseAudioContext _ _ _ __3 _ _ _ = BaseAudioContext _0 _1 _2 (_3 <|> __3) _4 _5 _6


------------------------------------------------------------------------------
-- | Protocol object for AudioListner
data AudioListener = AudioListener
    { listenerId :: !GraphObjectId
    , contextId :: !GraphObjectId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AudioListener where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "AudioListener" $ \_o -> AudioListener
            <$> _o .: "listenerId"
            <*> _o .: "contextId"
        ago = A.withArray "AudioListener" $ \_a -> AudioListener
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON AudioListener where
    toEncoding (AudioListener _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "listenerId" .= _0
        , P.pure $ "contextId" .= _1
        ]
    toJSON (AudioListener _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "listenerId" .= _0
        , P.pure $ "contextId" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup AudioListener where
    AudioListener _0 _1 <> AudioListener _ _ = AudioListener _0 _1


------------------------------------------------------------------------------
-- | Protocol object for AudioNode
data AudioNode = AudioNode
    { nodeId :: !GraphObjectId
    , contextId :: !GraphObjectId
    , nodeType :: !NodeType
    , numberOfInputs :: !P.Double
    , numberOfOutputs :: !P.Double
    , channelCount :: !P.Double
    , channelCountMode :: !ChannelCountMode
    , channelInterpretation :: !ChannelInterpretation
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AudioNode where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "AudioNode" $ \_o -> AudioNode
            <$> _o .: "nodeId"
            <*> _o .: "contextId"
            <*> _o .: "nodeType"
            <*> _o .: "numberOfInputs"
            <*> _o .: "numberOfOutputs"
            <*> _o .: "channelCount"
            <*> _o .: "channelCountMode"
            <*> _o .: "channelInterpretation"
        ago = A.withArray "AudioNode" $ \_a -> AudioNode
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.maybe P.empty A.parseJSON (_a !? 6)
            <*> P.maybe P.empty A.parseJSON (_a !? 7)


------------------------------------------------------------------------------
instance A.ToJSON AudioNode where
    toEncoding (AudioNode _0 _1 _2 _3 _4 _5 _6 _7) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "contextId" .= _1
        , P.pure $ "nodeType" .= _2
        , P.pure $ "numberOfInputs" .= _3
        , P.pure $ "numberOfOutputs" .= _4
        , P.pure $ "channelCount" .= _5
        , P.pure $ "channelCountMode" .= _6
        , P.pure $ "channelInterpretation" .= _7
        ]
    toJSON (AudioNode _0 _1 _2 _3 _4 _5 _6 _7) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "contextId" .= _1
        , P.pure $ "nodeType" .= _2
        , P.pure $ "numberOfInputs" .= _3
        , P.pure $ "numberOfOutputs" .= _4
        , P.pure $ "channelCount" .= _5
        , P.pure $ "channelCountMode" .= _6
        , P.pure $ "channelInterpretation" .= _7
        ]


------------------------------------------------------------------------------
instance P.Semigroup AudioNode where
    AudioNode _0 _1 _2 _3 _4 _5 _6 _7 <> AudioNode _ _ _ _ _ _ _ _ = AudioNode _0 _1 _2 _3 _4 _5 _6 _7


------------------------------------------------------------------------------
-- | Protocol object for AudioParam
data AudioParam = AudioParam
    { paramId :: !GraphObjectId
    , nodeId :: !GraphObjectId
    , contextId :: !GraphObjectId
    , paramType :: !ParamType
    , rate :: !AutomationRate
    , defaultValue :: !P.Double
    , minValue :: !P.Double
    , maxValue :: !P.Double
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AudioParam where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "AudioParam" $ \_o -> AudioParam
            <$> _o .: "paramId"
            <*> _o .: "nodeId"
            <*> _o .: "contextId"
            <*> _o .: "paramType"
            <*> _o .: "rate"
            <*> _o .: "defaultValue"
            <*> _o .: "minValue"
            <*> _o .: "maxValue"
        ago = A.withArray "AudioParam" $ \_a -> AudioParam
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.maybe P.empty A.parseJSON (_a !? 6)
            <*> P.maybe P.empty A.parseJSON (_a !? 7)


------------------------------------------------------------------------------
instance A.ToJSON AudioParam where
    toEncoding (AudioParam _0 _1 _2 _3 _4 _5 _6 _7) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "paramId" .= _0
        , P.pure $ "nodeId" .= _1
        , P.pure $ "contextId" .= _2
        , P.pure $ "paramType" .= _3
        , P.pure $ "rate" .= _4
        , P.pure $ "defaultValue" .= _5
        , P.pure $ "minValue" .= _6
        , P.pure $ "maxValue" .= _7
        ]
    toJSON (AudioParam _0 _1 _2 _3 _4 _5 _6 _7) = A.object $ P.catMaybes
        [ P.pure $ "paramId" .= _0
        , P.pure $ "nodeId" .= _1
        , P.pure $ "contextId" .= _2
        , P.pure $ "paramType" .= _3
        , P.pure $ "rate" .= _4
        , P.pure $ "defaultValue" .= _5
        , P.pure $ "minValue" .= _6
        , P.pure $ "maxValue" .= _7
        ]


------------------------------------------------------------------------------
instance P.Semigroup AudioParam where
    AudioParam _0 _1 _2 _3 _4 _5 _6 _7 <> AudioParam _ _ _ _ _ _ _ _ = AudioParam _0 _1 _2 _3 _4 _5 _6 _7

