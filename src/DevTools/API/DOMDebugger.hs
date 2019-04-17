{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | DOM debugging allows setting breakpoints on particular DOM operations and events. JavaScript
-- execution will stop on these operations as if there was a regular breakpoint set.
module DevTools.API.DOMDebugger
    ( module DevTools.API.DOMDebugger.Types
    , module DevTools.API.DOMDebugger
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
import           DevTools.API.DOMDebugger.Types
import qualified DevTools.API.Runtime.Types as Runtime


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Returns event listeners of the given object.
data GetEventListeners = GetEventListeners
    { -- | Identifier of the object to return listeners for.
      objectId :: !Runtime.RemoteObjectId
      -- | The maximum depth at which Node children should be retrieved, defaults to 1. Use -1 for the
      -- entire subtree or provide an integer larger than 0.
    , depth :: !(P.Maybe P.Int)
      -- | Whether or not iframes and shadow roots should be traversed when returning the subtree
      -- (default is false). Reports listeners for all contexts if pierce is enabled.
    , pierce :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetEventListeners where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getEventListeners" $ \_o -> GetEventListeners
            <$> _o .: "objectId"
            <*> _o .:? "depth"
            <*> _o .:? "pierce"
        ago = A.withArray "getEventListeners" $ \_a -> GetEventListeners
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON GetEventListeners where
    toEncoding (GetEventListeners _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "objectId" .= _0
        , ("depth" .=) <$> _1
        , ("pierce" .=) <$> _2
        ]
    toJSON (GetEventListeners _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "objectId" .= _0
        , ("depth" .=) <$> _1
        , ("pierce" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetEventListeners where
    GetEventListeners _0 _1 _2 <> GetEventListeners _ __1 __2 = GetEventListeners _0 (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
-- | Returns event listeners of the given object.
data GetEventListenersResult = GetEventListenersResult
    { -- | Array of relevant listeners.
      listeners :: ![EventListener]
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetEventListenersResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getEventListenersResult" $ \_o -> GetEventListenersResult
            <$> _o .: "listeners"
        ago = A.withArray "getEventListenersResult" $ \_a -> GetEventListenersResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetEventListenersResult where
    toEncoding (GetEventListenersResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "listeners" .= _0
        ]
    toJSON (GetEventListenersResult _0) = A.object $ P.catMaybes
        [ P.pure $ "listeners" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetEventListenersResult where
    GetEventListenersResult _0 <> GetEventListenersResult _ = GetEventListenersResult _0


------------------------------------------------------------------------------
instance M.Method GetEventListeners where
    type Result GetEventListeners = GetEventListenersResult
    name _ = "DOMDebugger.getEventListeners"


------------------------------------------------------------------------------
-- | Returns event listeners of the given object.
getEventListeners
    :: Runtime.RemoteObjectId
    -- ^ Identifier of the object to return listeners for.

    -> GetEventListeners
getEventListeners _0 = GetEventListeners _0 P.empty P.empty


------------------------------------------------------------------------------
-- | Removes DOM breakpoint that was set using @setDOMBreakpoint@.
data RemoveDOMBreakpoint = RemoveDOMBreakpoint
    { -- | Identifier of the node to remove breakpoint from.
      nodeId :: !DOM.NodeId
      -- | Type of the breakpoint to remove.
    , type_ :: !DOMBreakpointType
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RemoveDOMBreakpoint where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "removeDOMBreakpoint" $ \_o -> RemoveDOMBreakpoint
            <$> _o .: "nodeId"
            <*> _o .: "type"
        ago = A.withArray "removeDOMBreakpoint" $ \_a -> RemoveDOMBreakpoint
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON RemoveDOMBreakpoint where
    toEncoding (RemoveDOMBreakpoint _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "type" .= _1
        ]
    toJSON (RemoveDOMBreakpoint _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "type" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup RemoveDOMBreakpoint where
    RemoveDOMBreakpoint _0 _1 <> RemoveDOMBreakpoint _ _ = RemoveDOMBreakpoint _0 _1


------------------------------------------------------------------------------
instance M.Method RemoveDOMBreakpoint where
    type Result RemoveDOMBreakpoint = ()
    name _ = "DOMDebugger.removeDOMBreakpoint"


------------------------------------------------------------------------------
-- | Removes DOM breakpoint that was set using @setDOMBreakpoint@.
removeDOMBreakpoint
    :: DOM.NodeId
    -- ^ Identifier of the node to remove breakpoint from.

    -> DOMBreakpointType
    -- ^ Type of the breakpoint to remove.

    -> RemoveDOMBreakpoint
removeDOMBreakpoint _0 _1 = RemoveDOMBreakpoint _0 _1


------------------------------------------------------------------------------
-- | Removes breakpoint on particular DOM event.
{-# WARNING targetName "This feature is marked as EXPERIMENTAL." #-}
data RemoveEventListenerBreakpoint = RemoveEventListenerBreakpoint
    { -- | Event name.
      eventName :: !T.Text
      -- | EventTarget interface name.
    , targetName :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RemoveEventListenerBreakpoint where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "removeEventListenerBreakpoint" $ \_o -> RemoveEventListenerBreakpoint
            <$> _o .: "eventName"
            <*> _o .:? "targetName"
        ago = A.withArray "removeEventListenerBreakpoint" $ \_a -> RemoveEventListenerBreakpoint
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON RemoveEventListenerBreakpoint where
    toEncoding (RemoveEventListenerBreakpoint _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "eventName" .= _0
        , ("targetName" .=) <$> _1
        ]
    toJSON (RemoveEventListenerBreakpoint _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "eventName" .= _0
        , ("targetName" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup RemoveEventListenerBreakpoint where
    RemoveEventListenerBreakpoint _0 _1 <> RemoveEventListenerBreakpoint _ __1 = RemoveEventListenerBreakpoint _0 (_1 <|> __1)


------------------------------------------------------------------------------
instance M.Method RemoveEventListenerBreakpoint where
    type Result RemoveEventListenerBreakpoint = ()
    name _ = "DOMDebugger.removeEventListenerBreakpoint"


------------------------------------------------------------------------------
-- | Removes breakpoint on particular DOM event.
removeEventListenerBreakpoint
    :: T.Text
    -- ^ Event name.

    -> RemoveEventListenerBreakpoint
removeEventListenerBreakpoint _0 = RemoveEventListenerBreakpoint _0 P.empty


------------------------------------------------------------------------------
-- | Removes breakpoint on particular native event.
{-# WARNING RemoveInstrumentationBreakpoint "This feature is marked as EXPERIMENTAL." #-}
data RemoveInstrumentationBreakpoint = RemoveInstrumentationBreakpoint
    { -- | Instrumentation name to stop on.
      eventName :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RemoveInstrumentationBreakpoint where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "removeInstrumentationBreakpoint" $ \_o -> RemoveInstrumentationBreakpoint
            <$> _o .: "eventName"
        ago = A.withArray "removeInstrumentationBreakpoint" $ \_a -> RemoveInstrumentationBreakpoint
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON RemoveInstrumentationBreakpoint where
    toEncoding (RemoveInstrumentationBreakpoint _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "eventName" .= _0
        ]
    toJSON (RemoveInstrumentationBreakpoint _0) = A.object $ P.catMaybes
        [ P.pure $ "eventName" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup RemoveInstrumentationBreakpoint where
    RemoveInstrumentationBreakpoint _0 <> RemoveInstrumentationBreakpoint _ = RemoveInstrumentationBreakpoint _0


------------------------------------------------------------------------------
instance M.Method RemoveInstrumentationBreakpoint where
    type Result RemoveInstrumentationBreakpoint = ()
    name _ = "DOMDebugger.removeInstrumentationBreakpoint"


------------------------------------------------------------------------------
-- | Removes breakpoint on particular native event.
{-# WARNING removeInstrumentationBreakpoint "This feature is marked as EXPERIMENTAL." #-}
removeInstrumentationBreakpoint
    :: T.Text
    -- ^ Instrumentation name to stop on.

    -> RemoveInstrumentationBreakpoint
removeInstrumentationBreakpoint _0 = RemoveInstrumentationBreakpoint _0


------------------------------------------------------------------------------
-- | Removes breakpoint from XMLHttpRequest.
data RemoveXHRBreakpoint = RemoveXHRBreakpoint
    { -- | Resource URL substring.
      url :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RemoveXHRBreakpoint where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "removeXHRBreakpoint" $ \_o -> RemoveXHRBreakpoint
            <$> _o .: "url"
        ago = A.withArray "removeXHRBreakpoint" $ \_a -> RemoveXHRBreakpoint
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON RemoveXHRBreakpoint where
    toEncoding (RemoveXHRBreakpoint _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "url" .= _0
        ]
    toJSON (RemoveXHRBreakpoint _0) = A.object $ P.catMaybes
        [ P.pure $ "url" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup RemoveXHRBreakpoint where
    RemoveXHRBreakpoint _0 <> RemoveXHRBreakpoint _ = RemoveXHRBreakpoint _0


------------------------------------------------------------------------------
instance M.Method RemoveXHRBreakpoint where
    type Result RemoveXHRBreakpoint = ()
    name _ = "DOMDebugger.removeXHRBreakpoint"


------------------------------------------------------------------------------
-- | Removes breakpoint from XMLHttpRequest.
removeXHRBreakpoint
    :: T.Text
    -- ^ Resource URL substring.

    -> RemoveXHRBreakpoint
removeXHRBreakpoint _0 = RemoveXHRBreakpoint _0


------------------------------------------------------------------------------
-- | Sets breakpoint on particular operation with DOM.
data SetDOMBreakpoint = SetDOMBreakpoint
    { -- | Identifier of the node to set breakpoint on.
      nodeId :: !DOM.NodeId
      -- | Type of the operation to stop upon.
    , type_ :: !DOMBreakpointType
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetDOMBreakpoint where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setDOMBreakpoint" $ \_o -> SetDOMBreakpoint
            <$> _o .: "nodeId"
            <*> _o .: "type"
        ago = A.withArray "setDOMBreakpoint" $ \_a -> SetDOMBreakpoint
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SetDOMBreakpoint where
    toEncoding (SetDOMBreakpoint _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "type" .= _1
        ]
    toJSON (SetDOMBreakpoint _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "type" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetDOMBreakpoint where
    SetDOMBreakpoint _0 _1 <> SetDOMBreakpoint _ _ = SetDOMBreakpoint _0 _1


------------------------------------------------------------------------------
instance M.Method SetDOMBreakpoint where
    type Result SetDOMBreakpoint = ()
    name _ = "DOMDebugger.setDOMBreakpoint"


------------------------------------------------------------------------------
-- | Sets breakpoint on particular operation with DOM.
setDOMBreakpoint
    :: DOM.NodeId
    -- ^ Identifier of the node to set breakpoint on.

    -> DOMBreakpointType
    -- ^ Type of the operation to stop upon.

    -> SetDOMBreakpoint
setDOMBreakpoint _0 _1 = SetDOMBreakpoint _0 _1


------------------------------------------------------------------------------
-- | Sets breakpoint on particular DOM event.
{-{-# WARNING targetName "This feature is marked as EXPERIMENTAL." #-}-}
data SetEventListenerBreakpoint = SetEventListenerBreakpoint
    { -- | DOM Event name to stop on (any DOM event will do).
      eventName :: !T.Text
      -- | EventTarget interface name to stop on. If equal to @"*"@ or not provided, will stop on any
      -- EventTarget.
    , targetName :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetEventListenerBreakpoint where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setEventListenerBreakpoint" $ \_o -> SetEventListenerBreakpoint
            <$> _o .: "eventName"
            <*> _o .:? "targetName"
        ago = A.withArray "setEventListenerBreakpoint" $ \_a -> SetEventListenerBreakpoint
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SetEventListenerBreakpoint where
    toEncoding (SetEventListenerBreakpoint _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "eventName" .= _0
        , ("targetName" .=) <$> _1
        ]
    toJSON (SetEventListenerBreakpoint _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "eventName" .= _0
        , ("targetName" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetEventListenerBreakpoint where
    SetEventListenerBreakpoint _0 _1 <> SetEventListenerBreakpoint _ __1 = SetEventListenerBreakpoint _0 (_1 <|> __1)


------------------------------------------------------------------------------
instance M.Method SetEventListenerBreakpoint where
    type Result SetEventListenerBreakpoint = ()
    name _ = "DOMDebugger.setEventListenerBreakpoint"


------------------------------------------------------------------------------
-- | Sets breakpoint on particular DOM event.
setEventListenerBreakpoint
    :: T.Text
    -- ^ DOM Event name to stop on (any DOM event will do).

    -> SetEventListenerBreakpoint
setEventListenerBreakpoint _0 = SetEventListenerBreakpoint _0 P.empty


------------------------------------------------------------------------------
-- | Sets breakpoint on particular native event.
{-# WARNING SetInstrumentationBreakpoint "This feature is marked as EXPERIMENTAL." #-}
data SetInstrumentationBreakpoint = SetInstrumentationBreakpoint
    { -- | Instrumentation name to stop on.
      eventName :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetInstrumentationBreakpoint where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setInstrumentationBreakpoint" $ \_o -> SetInstrumentationBreakpoint
            <$> _o .: "eventName"
        ago = A.withArray "setInstrumentationBreakpoint" $ \_a -> SetInstrumentationBreakpoint
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetInstrumentationBreakpoint where
    toEncoding (SetInstrumentationBreakpoint _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "eventName" .= _0
        ]
    toJSON (SetInstrumentationBreakpoint _0) = A.object $ P.catMaybes
        [ P.pure $ "eventName" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetInstrumentationBreakpoint where
    SetInstrumentationBreakpoint _0 <> SetInstrumentationBreakpoint _ = SetInstrumentationBreakpoint _0


------------------------------------------------------------------------------
instance M.Method SetInstrumentationBreakpoint where
    type Result SetInstrumentationBreakpoint = ()
    name _ = "DOMDebugger.setInstrumentationBreakpoint"


------------------------------------------------------------------------------
-- | Sets breakpoint on particular native event.
{-# WARNING setInstrumentationBreakpoint "This feature is marked as EXPERIMENTAL." #-}
setInstrumentationBreakpoint
    :: T.Text
    -- ^ Instrumentation name to stop on.

    -> SetInstrumentationBreakpoint
setInstrumentationBreakpoint _0 = SetInstrumentationBreakpoint _0


------------------------------------------------------------------------------
-- | Sets breakpoint on XMLHttpRequest.
data SetXHRBreakpoint = SetXHRBreakpoint
    { -- | Resource URL substring. All XHRs having this substring in the URL will get stopped upon.
      url :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetXHRBreakpoint where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setXHRBreakpoint" $ \_o -> SetXHRBreakpoint
            <$> _o .: "url"
        ago = A.withArray "setXHRBreakpoint" $ \_a -> SetXHRBreakpoint
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetXHRBreakpoint where
    toEncoding (SetXHRBreakpoint _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "url" .= _0
        ]
    toJSON (SetXHRBreakpoint _0) = A.object $ P.catMaybes
        [ P.pure $ "url" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetXHRBreakpoint where
    SetXHRBreakpoint _0 <> SetXHRBreakpoint _ = SetXHRBreakpoint _0


------------------------------------------------------------------------------
instance M.Method SetXHRBreakpoint where
    type Result SetXHRBreakpoint = ()
    name _ = "DOMDebugger.setXHRBreakpoint"


------------------------------------------------------------------------------
-- | Sets breakpoint on XMLHttpRequest.
setXHRBreakpoint
    :: T.Text
    -- ^ Resource URL substring. All XHRs having this substring in the URL will get stopped upon.

    -> SetXHRBreakpoint
setXHRBreakpoint _0 = SetXHRBreakpoint _0

