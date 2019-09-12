{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | This domain allows inspection of Web Audio API.
-- https:\/\/webaudio.github.io\/web-audio-api\/
module DevTools.API.WebAudio{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

    ( module DevTools.API.WebAudio.Types
    , module DevTools.API.WebAudio
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
import           DevTools.API.WebAudio.Types


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Enables the WebAudio domain and starts sending context lifetime events.
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
    name _ = "WebAudio.enable"


------------------------------------------------------------------------------
-- | Enables the WebAudio domain and starts sending context lifetime events.
enable
    :: Enable
enable = Enable


------------------------------------------------------------------------------
-- | Disables the WebAudio domain.
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
    name _ = "WebAudio.disable"


------------------------------------------------------------------------------
-- | Disables the WebAudio domain.
disable
    :: Disable
disable = Disable


------------------------------------------------------------------------------
-- | Fetch the realtime data from the registered contexts.
data GetRealtimeData = GetRealtimeData
    { contextId :: !GraphObjectId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetRealtimeData where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getRealtimeData" $ \_o -> GetRealtimeData
            <$> _o .: "contextId"
        ago = A.withArray "getRealtimeData" $ \_a -> GetRealtimeData
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetRealtimeData where
    toEncoding (GetRealtimeData _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "contextId" .= _0
        ]
    toJSON (GetRealtimeData _0) = A.object $ P.catMaybes
        [ P.pure $ "contextId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetRealtimeData where
    GetRealtimeData _0 <> GetRealtimeData _ = GetRealtimeData _0


------------------------------------------------------------------------------
-- | Fetch the realtime data from the registered contexts.
data GetRealtimeDataResult = GetRealtimeDataResult
    { realtimeData :: !ContextRealtimeData
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetRealtimeDataResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getRealtimeDataResult" $ \_o -> GetRealtimeDataResult
            <$> _o .: "realtimeData"
        ago = A.withArray "getRealtimeDataResult" $ \_a -> GetRealtimeDataResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetRealtimeDataResult where
    toEncoding (GetRealtimeDataResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "realtimeData" .= _0
        ]
    toJSON (GetRealtimeDataResult _0) = A.object $ P.catMaybes
        [ P.pure $ "realtimeData" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetRealtimeDataResult where
    GetRealtimeDataResult _0 <> GetRealtimeDataResult _ = GetRealtimeDataResult _0


------------------------------------------------------------------------------
instance M.Method GetRealtimeData where
    type Result GetRealtimeData = GetRealtimeDataResult
    name _ = "WebAudio.getRealtimeData"


------------------------------------------------------------------------------
-- | Fetch the realtime data from the registered contexts.
getRealtimeData
    :: GraphObjectId
    -> GetRealtimeData
getRealtimeData _0 = GetRealtimeData _0


------------------------------------------------------------------------------
-- | Notifies that a new BaseAudioContext has been created.
data ContextCreated = ContextCreated
    { context :: !BaseAudioContext
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ContextCreated where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "contextCreated" $ \_o -> ContextCreated
            <$> _o .: "context"
        ago = A.withArray "contextCreated" $ \_a -> ContextCreated
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON ContextCreated where
    toEncoding (ContextCreated _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "context" .= _0
        ]
    toJSON (ContextCreated _0) = A.object $ P.catMaybes
        [ P.pure $ "context" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup ContextCreated where
    ContextCreated _0 <> ContextCreated _ = ContextCreated _0


------------------------------------------------------------------------------
instance E.Event ContextCreated where
    type Result ContextCreated = ContextCreated
    name _ = "WebAudio.contextCreated"


------------------------------------------------------------------------------
-- | Notifies that a new BaseAudioContext has been created.
contextCreated :: P.Proxy ContextCreated
contextCreated = P.Proxy


------------------------------------------------------------------------------
-- | Notifies that an existing BaseAudioContext will be destroyed.
data ContextWillBeDestroyed = ContextWillBeDestroyed
    { contextId :: !GraphObjectId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ContextWillBeDestroyed where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "contextWillBeDestroyed" $ \_o -> ContextWillBeDestroyed
            <$> _o .: "contextId"
        ago = A.withArray "contextWillBeDestroyed" $ \_a -> ContextWillBeDestroyed
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON ContextWillBeDestroyed where
    toEncoding (ContextWillBeDestroyed _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "contextId" .= _0
        ]
    toJSON (ContextWillBeDestroyed _0) = A.object $ P.catMaybes
        [ P.pure $ "contextId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup ContextWillBeDestroyed where
    ContextWillBeDestroyed _0 <> ContextWillBeDestroyed _ = ContextWillBeDestroyed _0


------------------------------------------------------------------------------
instance E.Event ContextWillBeDestroyed where
    type Result ContextWillBeDestroyed = ContextWillBeDestroyed
    name _ = "WebAudio.contextWillBeDestroyed"


------------------------------------------------------------------------------
-- | Notifies that an existing BaseAudioContext will be destroyed.
contextWillBeDestroyed :: P.Proxy ContextWillBeDestroyed
contextWillBeDestroyed = P.Proxy


------------------------------------------------------------------------------
-- | Notifies that existing BaseAudioContext has changed some properties (id stays the same)..
data ContextChanged = ContextChanged
    { context :: !BaseAudioContext
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ContextChanged where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "contextChanged" $ \_o -> ContextChanged
            <$> _o .: "context"
        ago = A.withArray "contextChanged" $ \_a -> ContextChanged
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON ContextChanged where
    toEncoding (ContextChanged _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "context" .= _0
        ]
    toJSON (ContextChanged _0) = A.object $ P.catMaybes
        [ P.pure $ "context" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup ContextChanged where
    ContextChanged _0 <> ContextChanged _ = ContextChanged _0


------------------------------------------------------------------------------
instance E.Event ContextChanged where
    type Result ContextChanged = ContextChanged
    name _ = "WebAudio.contextChanged"


------------------------------------------------------------------------------
-- | Notifies that existing BaseAudioContext has changed some properties (id stays the same)..
contextChanged :: P.Proxy ContextChanged
contextChanged = P.Proxy


------------------------------------------------------------------------------
-- | Notifies that the construction of an AudioListener has finished.
data AudioListenerCreated = AudioListenerCreated
    { listener :: !AudioListener
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AudioListenerCreated where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "audioListenerCreated" $ \_o -> AudioListenerCreated
            <$> _o .: "listener"
        ago = A.withArray "audioListenerCreated" $ \_a -> AudioListenerCreated
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON AudioListenerCreated where
    toEncoding (AudioListenerCreated _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "listener" .= _0
        ]
    toJSON (AudioListenerCreated _0) = A.object $ P.catMaybes
        [ P.pure $ "listener" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup AudioListenerCreated where
    AudioListenerCreated _0 <> AudioListenerCreated _ = AudioListenerCreated _0


------------------------------------------------------------------------------
instance E.Event AudioListenerCreated where
    type Result AudioListenerCreated = AudioListenerCreated
    name _ = "WebAudio.audioListenerCreated"


------------------------------------------------------------------------------
-- | Notifies that the construction of an AudioListener has finished.
audioListenerCreated :: P.Proxy AudioListenerCreated
audioListenerCreated = P.Proxy


------------------------------------------------------------------------------
-- | Notifies that a new AudioListener has been created.
data AudioListenerWillBeDestroyed = AudioListenerWillBeDestroyed
    { contextId :: !GraphObjectId
    , listenerId :: !GraphObjectId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AudioListenerWillBeDestroyed where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "audioListenerWillBeDestroyed" $ \_o -> AudioListenerWillBeDestroyed
            <$> _o .: "contextId"
            <*> _o .: "listenerId"
        ago = A.withArray "audioListenerWillBeDestroyed" $ \_a -> AudioListenerWillBeDestroyed
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON AudioListenerWillBeDestroyed where
    toEncoding (AudioListenerWillBeDestroyed _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "contextId" .= _0
        , P.pure $ "listenerId" .= _1
        ]
    toJSON (AudioListenerWillBeDestroyed _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "contextId" .= _0
        , P.pure $ "listenerId" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup AudioListenerWillBeDestroyed where
    AudioListenerWillBeDestroyed _0 _1 <> AudioListenerWillBeDestroyed _ _ = AudioListenerWillBeDestroyed _0 _1


------------------------------------------------------------------------------
instance E.Event AudioListenerWillBeDestroyed where
    type Result AudioListenerWillBeDestroyed = AudioListenerWillBeDestroyed
    name _ = "WebAudio.audioListenerWillBeDestroyed"


------------------------------------------------------------------------------
-- | Notifies that a new AudioListener has been created.
audioListenerWillBeDestroyed :: P.Proxy AudioListenerWillBeDestroyed
audioListenerWillBeDestroyed = P.Proxy


------------------------------------------------------------------------------
-- | Notifies that a new AudioNode has been created.
data AudioNodeCreated = AudioNodeCreated
    { node :: !AudioNode
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AudioNodeCreated where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "audioNodeCreated" $ \_o -> AudioNodeCreated
            <$> _o .: "node"
        ago = A.withArray "audioNodeCreated" $ \_a -> AudioNodeCreated
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON AudioNodeCreated where
    toEncoding (AudioNodeCreated _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "node" .= _0
        ]
    toJSON (AudioNodeCreated _0) = A.object $ P.catMaybes
        [ P.pure $ "node" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup AudioNodeCreated where
    AudioNodeCreated _0 <> AudioNodeCreated _ = AudioNodeCreated _0


------------------------------------------------------------------------------
instance E.Event AudioNodeCreated where
    type Result AudioNodeCreated = AudioNodeCreated
    name _ = "WebAudio.audioNodeCreated"


------------------------------------------------------------------------------
-- | Notifies that a new AudioNode has been created.
audioNodeCreated :: P.Proxy AudioNodeCreated
audioNodeCreated = P.Proxy


------------------------------------------------------------------------------
-- | Notifies that an existing AudioNode has been destroyed.
data AudioNodeWillBeDestroyed = AudioNodeWillBeDestroyed
    { contextId :: !GraphObjectId
    , nodeId :: !GraphObjectId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AudioNodeWillBeDestroyed where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "audioNodeWillBeDestroyed" $ \_o -> AudioNodeWillBeDestroyed
            <$> _o .: "contextId"
            <*> _o .: "nodeId"
        ago = A.withArray "audioNodeWillBeDestroyed" $ \_a -> AudioNodeWillBeDestroyed
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON AudioNodeWillBeDestroyed where
    toEncoding (AudioNodeWillBeDestroyed _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "contextId" .= _0
        , P.pure $ "nodeId" .= _1
        ]
    toJSON (AudioNodeWillBeDestroyed _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "contextId" .= _0
        , P.pure $ "nodeId" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup AudioNodeWillBeDestroyed where
    AudioNodeWillBeDestroyed _0 _1 <> AudioNodeWillBeDestroyed _ _ = AudioNodeWillBeDestroyed _0 _1


------------------------------------------------------------------------------
instance E.Event AudioNodeWillBeDestroyed where
    type Result AudioNodeWillBeDestroyed = AudioNodeWillBeDestroyed
    name _ = "WebAudio.audioNodeWillBeDestroyed"


------------------------------------------------------------------------------
-- | Notifies that an existing AudioNode has been destroyed.
audioNodeWillBeDestroyed :: P.Proxy AudioNodeWillBeDestroyed
audioNodeWillBeDestroyed = P.Proxy


------------------------------------------------------------------------------
-- | Notifies that a new AudioParam has been created.
data AudioParamCreated = AudioParamCreated
    { param :: !AudioParam
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AudioParamCreated where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "audioParamCreated" $ \_o -> AudioParamCreated
            <$> _o .: "param"
        ago = A.withArray "audioParamCreated" $ \_a -> AudioParamCreated
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON AudioParamCreated where
    toEncoding (AudioParamCreated _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "param" .= _0
        ]
    toJSON (AudioParamCreated _0) = A.object $ P.catMaybes
        [ P.pure $ "param" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup AudioParamCreated where
    AudioParamCreated _0 <> AudioParamCreated _ = AudioParamCreated _0


------------------------------------------------------------------------------
instance E.Event AudioParamCreated where
    type Result AudioParamCreated = AudioParamCreated
    name _ = "WebAudio.audioParamCreated"


------------------------------------------------------------------------------
-- | Notifies that a new AudioParam has been created.
audioParamCreated :: P.Proxy AudioParamCreated
audioParamCreated = P.Proxy


------------------------------------------------------------------------------
-- | Notifies that an existing AudioParam has been destroyed.
data AudioParamWillBeDestroyed = AudioParamWillBeDestroyed
    { contextId :: !GraphObjectId
    , nodeId :: !GraphObjectId
    , paramId :: !GraphObjectId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AudioParamWillBeDestroyed where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "audioParamWillBeDestroyed" $ \_o -> AudioParamWillBeDestroyed
            <$> _o .: "contextId"
            <*> _o .: "nodeId"
            <*> _o .: "paramId"
        ago = A.withArray "audioParamWillBeDestroyed" $ \_a -> AudioParamWillBeDestroyed
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON AudioParamWillBeDestroyed where
    toEncoding (AudioParamWillBeDestroyed _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "contextId" .= _0
        , P.pure $ "nodeId" .= _1
        , P.pure $ "paramId" .= _2
        ]
    toJSON (AudioParamWillBeDestroyed _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "contextId" .= _0
        , P.pure $ "nodeId" .= _1
        , P.pure $ "paramId" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup AudioParamWillBeDestroyed where
    AudioParamWillBeDestroyed _0 _1 _2 <> AudioParamWillBeDestroyed _ _ _ = AudioParamWillBeDestroyed _0 _1 _2


------------------------------------------------------------------------------
instance E.Event AudioParamWillBeDestroyed where
    type Result AudioParamWillBeDestroyed = AudioParamWillBeDestroyed
    name _ = "WebAudio.audioParamWillBeDestroyed"


------------------------------------------------------------------------------
-- | Notifies that an existing AudioParam has been destroyed.
audioParamWillBeDestroyed :: P.Proxy AudioParamWillBeDestroyed
audioParamWillBeDestroyed = P.Proxy


------------------------------------------------------------------------------
-- | Notifies that two AudioNodes are connected.
data NodesConnected = NodesConnected
    { contextId :: !GraphObjectId
    , sourceId :: !GraphObjectId
    , destinationId :: !GraphObjectId
    , sourceOutputIndex :: !(P.Maybe P.Double)
    , destinationInputIndex :: !(P.Maybe P.Double)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON NodesConnected where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "nodesConnected" $ \_o -> NodesConnected
            <$> _o .: "contextId"
            <*> _o .: "sourceId"
            <*> _o .: "destinationId"
            <*> _o .:? "sourceOutputIndex"
            <*> _o .:? "destinationInputIndex"
        ago = A.withArray "nodesConnected" $ \_a -> NodesConnected
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON NodesConnected where
    toEncoding (NodesConnected _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "contextId" .= _0
        , P.pure $ "sourceId" .= _1
        , P.pure $ "destinationId" .= _2
        , ("sourceOutputIndex" .=) <$> _3
        , ("destinationInputIndex" .=) <$> _4
        ]
    toJSON (NodesConnected _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ P.pure $ "contextId" .= _0
        , P.pure $ "sourceId" .= _1
        , P.pure $ "destinationId" .= _2
        , ("sourceOutputIndex" .=) <$> _3
        , ("destinationInputIndex" .=) <$> _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup NodesConnected where
    NodesConnected _0 _1 _2 _3 _4 <> NodesConnected _ _ _ __3 __4 = NodesConnected _0 _1 _2 (_3 <|> __3) (_4 <|> __4)


------------------------------------------------------------------------------
instance E.Event NodesConnected where
    type Result NodesConnected = NodesConnected
    name _ = "WebAudio.nodesConnected"


------------------------------------------------------------------------------
-- | Notifies that two AudioNodes are connected.
nodesConnected :: P.Proxy NodesConnected
nodesConnected = P.Proxy


------------------------------------------------------------------------------
-- | Notifies that AudioNodes are disconnected. The destination can be null, and it means all the outgoing connections from the source are disconnected.
data NodesDisconnected = NodesDisconnected
    { contextId :: !GraphObjectId
    , sourceId :: !GraphObjectId
    , destinationId :: !GraphObjectId
    , sourceOutputIndex :: !(P.Maybe P.Double)
    , destinationInputIndex :: !(P.Maybe P.Double)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON NodesDisconnected where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "nodesDisconnected" $ \_o -> NodesDisconnected
            <$> _o .: "contextId"
            <*> _o .: "sourceId"
            <*> _o .: "destinationId"
            <*> _o .:? "sourceOutputIndex"
            <*> _o .:? "destinationInputIndex"
        ago = A.withArray "nodesDisconnected" $ \_a -> NodesDisconnected
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON NodesDisconnected where
    toEncoding (NodesDisconnected _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "contextId" .= _0
        , P.pure $ "sourceId" .= _1
        , P.pure $ "destinationId" .= _2
        , ("sourceOutputIndex" .=) <$> _3
        , ("destinationInputIndex" .=) <$> _4
        ]
    toJSON (NodesDisconnected _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ P.pure $ "contextId" .= _0
        , P.pure $ "sourceId" .= _1
        , P.pure $ "destinationId" .= _2
        , ("sourceOutputIndex" .=) <$> _3
        , ("destinationInputIndex" .=) <$> _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup NodesDisconnected where
    NodesDisconnected _0 _1 _2 _3 _4 <> NodesDisconnected _ _ _ __3 __4 = NodesDisconnected _0 _1 _2 (_3 <|> __3) (_4 <|> __4)


------------------------------------------------------------------------------
instance E.Event NodesDisconnected where
    type Result NodesDisconnected = NodesDisconnected
    name _ = "WebAudio.nodesDisconnected"


------------------------------------------------------------------------------
-- | Notifies that AudioNodes are disconnected. The destination can be null, and it means all the outgoing connections from the source are disconnected.
nodesDisconnected :: P.Proxy NodesDisconnected
nodesDisconnected = P.Proxy


------------------------------------------------------------------------------
-- | Notifies that an AudioNode is connected to an AudioParam.
data NodeParamConnected = NodeParamConnected
    { contextId :: !GraphObjectId
    , sourceId :: !GraphObjectId
    , destinationId :: !GraphObjectId
    , sourceOutputIndex :: !(P.Maybe P.Double)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON NodeParamConnected where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "nodeParamConnected" $ \_o -> NodeParamConnected
            <$> _o .: "contextId"
            <*> _o .: "sourceId"
            <*> _o .: "destinationId"
            <*> _o .:? "sourceOutputIndex"
        ago = A.withArray "nodeParamConnected" $ \_a -> NodeParamConnected
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON NodeParamConnected where
    toEncoding (NodeParamConnected _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "contextId" .= _0
        , P.pure $ "sourceId" .= _1
        , P.pure $ "destinationId" .= _2
        , ("sourceOutputIndex" .=) <$> _3
        ]
    toJSON (NodeParamConnected _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "contextId" .= _0
        , P.pure $ "sourceId" .= _1
        , P.pure $ "destinationId" .= _2
        , ("sourceOutputIndex" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup NodeParamConnected where
    NodeParamConnected _0 _1 _2 _3 <> NodeParamConnected _ _ _ __3 = NodeParamConnected _0 _1 _2 (_3 <|> __3)


------------------------------------------------------------------------------
instance E.Event NodeParamConnected where
    type Result NodeParamConnected = NodeParamConnected
    name _ = "WebAudio.nodeParamConnected"


------------------------------------------------------------------------------
-- | Notifies that an AudioNode is connected to an AudioParam.
nodeParamConnected :: P.Proxy NodeParamConnected
nodeParamConnected = P.Proxy


------------------------------------------------------------------------------
-- | Notifies that an AudioNode is disconnected to an AudioParam.
data NodeParamDisconnected = NodeParamDisconnected
    { contextId :: !GraphObjectId
    , sourceId :: !GraphObjectId
    , destinationId :: !GraphObjectId
    , sourceOutputIndex :: !(P.Maybe P.Double)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON NodeParamDisconnected where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "nodeParamDisconnected" $ \_o -> NodeParamDisconnected
            <$> _o .: "contextId"
            <*> _o .: "sourceId"
            <*> _o .: "destinationId"
            <*> _o .:? "sourceOutputIndex"
        ago = A.withArray "nodeParamDisconnected" $ \_a -> NodeParamDisconnected
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON NodeParamDisconnected where
    toEncoding (NodeParamDisconnected _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "contextId" .= _0
        , P.pure $ "sourceId" .= _1
        , P.pure $ "destinationId" .= _2
        , ("sourceOutputIndex" .=) <$> _3
        ]
    toJSON (NodeParamDisconnected _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "contextId" .= _0
        , P.pure $ "sourceId" .= _1
        , P.pure $ "destinationId" .= _2
        , ("sourceOutputIndex" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup NodeParamDisconnected where
    NodeParamDisconnected _0 _1 _2 _3 <> NodeParamDisconnected _ _ _ __3 = NodeParamDisconnected _0 _1 _2 (_3 <|> __3)


------------------------------------------------------------------------------
instance E.Event NodeParamDisconnected where
    type Result NodeParamDisconnected = NodeParamDisconnected
    name _ = "WebAudio.nodeParamDisconnected"


------------------------------------------------------------------------------
-- | Notifies that an AudioNode is disconnected to an AudioParam.
nodeParamDisconnected :: P.Proxy NodeParamDisconnected
nodeParamDisconnected = P.Proxy

