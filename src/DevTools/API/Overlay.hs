{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | This domain provides various functionality related to drawing atop the inspected page.
module DevTools.API.Overlay{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

    ( module DevTools.API.Overlay.Types
    , module DevTools.API.Overlay
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
import           DevTools.API.Overlay.Types
import qualified DevTools.API.Page.Types as Page
import qualified DevTools.API.Runtime.Types as Runtime


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Disables domain notifications.
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
    name _ = "Overlay.disable"


------------------------------------------------------------------------------
-- | Disables domain notifications.
disable
    :: Disable
disable = Disable


------------------------------------------------------------------------------
-- | Enables domain notifications.
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
    name _ = "Overlay.enable"


------------------------------------------------------------------------------
-- | Enables domain notifications.
enable
    :: Enable
enable = Enable


------------------------------------------------------------------------------
-- | For testing.
data GetHighlightObjectForTest = GetHighlightObjectForTest
    { -- | Id of the node to get highlight object for.
      nodeId :: !DOM.NodeId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetHighlightObjectForTest where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getHighlightObjectForTest" $ \_o -> GetHighlightObjectForTest
            <$> _o .: "nodeId"
        ago = A.withArray "getHighlightObjectForTest" $ \_a -> GetHighlightObjectForTest
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetHighlightObjectForTest where
    toEncoding (GetHighlightObjectForTest _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]
    toJSON (GetHighlightObjectForTest _0) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetHighlightObjectForTest where
    GetHighlightObjectForTest _0 <> GetHighlightObjectForTest _ = GetHighlightObjectForTest _0


------------------------------------------------------------------------------
-- | For testing.
data GetHighlightObjectForTestResult = GetHighlightObjectForTestResult
    { -- | Highlight data for the node.
      highlight :: !A.Object
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetHighlightObjectForTestResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getHighlightObjectForTestResult" $ \_o -> GetHighlightObjectForTestResult
            <$> _o .: "highlight"
        ago = A.withArray "getHighlightObjectForTestResult" $ \_a -> GetHighlightObjectForTestResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetHighlightObjectForTestResult where
    toEncoding (GetHighlightObjectForTestResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "highlight" .= _0
        ]
    toJSON (GetHighlightObjectForTestResult _0) = A.object $ P.catMaybes
        [ P.pure $ "highlight" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetHighlightObjectForTestResult where
    GetHighlightObjectForTestResult _0 <> GetHighlightObjectForTestResult _ = GetHighlightObjectForTestResult _0


------------------------------------------------------------------------------
instance M.Method GetHighlightObjectForTest where
    type Result GetHighlightObjectForTest = GetHighlightObjectForTestResult
    name _ = "Overlay.getHighlightObjectForTest"


------------------------------------------------------------------------------
-- | For testing.
getHighlightObjectForTest
    :: DOM.NodeId
    -- ^ Id of the node to get highlight object for.

    -> GetHighlightObjectForTest
getHighlightObjectForTest _0 = GetHighlightObjectForTest _0


------------------------------------------------------------------------------
-- | Hides any highlight.
data HideHighlight = HideHighlight
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON HideHighlight where
    parseJSON A.Null = P.pure HideHighlight
    parseJSON v = A.withArray "hideHighlight" go v
        <|> A.withObject "hideHighlight" go v
      where
        go _ = P.pure HideHighlight


------------------------------------------------------------------------------
instance A.ToJSON HideHighlight where
    toEncoding HideHighlight = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON HideHighlight = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup HideHighlight where
    HideHighlight <> HideHighlight = HideHighlight


------------------------------------------------------------------------------
instance P.Monoid HideHighlight where
    mempty = HideHighlight


------------------------------------------------------------------------------
instance M.Method HideHighlight where
    type Result HideHighlight = ()
    name _ = "Overlay.hideHighlight"


------------------------------------------------------------------------------
-- | Hides any highlight.
hideHighlight
    :: HideHighlight
hideHighlight = HideHighlight


------------------------------------------------------------------------------
-- | Highlights owner element of the frame with given id.
data HighlightFrame = HighlightFrame
    { -- | Identifier of the frame to highlight.
      frameId :: !Page.FrameId
      -- | The content box highlight fill color (default: transparent).
    , contentColor :: !(P.Maybe DOM.RGBA)
      -- | The content box highlight outline color (default: transparent).
    , contentOutlineColor :: !(P.Maybe DOM.RGBA)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON HighlightFrame where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "highlightFrame" $ \_o -> HighlightFrame
            <$> _o .: "frameId"
            <*> _o .:? "contentColor"
            <*> _o .:? "contentOutlineColor"
        ago = A.withArray "highlightFrame" $ \_a -> HighlightFrame
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON HighlightFrame where
    toEncoding (HighlightFrame _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        , ("contentColor" .=) <$> _1
        , ("contentOutlineColor" .=) <$> _2
        ]
    toJSON (HighlightFrame _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        , ("contentColor" .=) <$> _1
        , ("contentOutlineColor" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup HighlightFrame where
    HighlightFrame _0 _1 _2 <> HighlightFrame _ __1 __2 = HighlightFrame _0 (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
instance M.Method HighlightFrame where
    type Result HighlightFrame = ()
    name _ = "Overlay.highlightFrame"


------------------------------------------------------------------------------
-- | Highlights owner element of the frame with given id.
highlightFrame
    :: Page.FrameId
    -- ^ Identifier of the frame to highlight.

    -> HighlightFrame
highlightFrame _0 = HighlightFrame _0 P.empty P.empty


------------------------------------------------------------------------------
-- | Highlights DOM node with given id or with the given JavaScript object wrapper. Either nodeId or
-- objectId must be specified.
data HighlightNode = HighlightNode
    { -- | A descriptor for the highlight appearance.
      highlightConfig :: !HighlightConfig
      -- | Identifier of the node to highlight.
    , nodeId :: !(P.Maybe DOM.NodeId)
      -- | Identifier of the backend node to highlight.
    , backendNodeId :: !(P.Maybe DOM.BackendNodeId)
      -- | JavaScript object id of the node to be highlighted.
    , objectId :: !(P.Maybe Runtime.RemoteObjectId)
      -- | Selectors to highlight relevant nodes.
    , selector :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON HighlightNode where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "highlightNode" $ \_o -> HighlightNode
            <$> _o .: "highlightConfig"
            <*> _o .:? "nodeId"
            <*> _o .:? "backendNodeId"
            <*> _o .:? "objectId"
            <*> _o .:? "selector"
        ago = A.withArray "highlightNode" $ \_a -> HighlightNode
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON HighlightNode where
    toEncoding (HighlightNode _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "highlightConfig" .= _0
        , ("nodeId" .=) <$> _1
        , ("backendNodeId" .=) <$> _2
        , ("objectId" .=) <$> _3
        , ("selector" .=) <$> _4
        ]
    toJSON (HighlightNode _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ P.pure $ "highlightConfig" .= _0
        , ("nodeId" .=) <$> _1
        , ("backendNodeId" .=) <$> _2
        , ("objectId" .=) <$> _3
        , ("selector" .=) <$> _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup HighlightNode where
    HighlightNode _0 _1 _2 _3 _4 <> HighlightNode _ __1 __2 __3 __4 = HighlightNode _0 (_1 <|> __1) (_2 <|> __2) (_3 <|> __3) (_4 <|> __4)


------------------------------------------------------------------------------
instance M.Method HighlightNode where
    type Result HighlightNode = ()
    name _ = "Overlay.highlightNode"


------------------------------------------------------------------------------
-- | Highlights DOM node with given id or with the given JavaScript object wrapper. Either nodeId or
-- objectId must be specified.
highlightNode
    :: HighlightConfig
    -- ^ A descriptor for the highlight appearance.

    -> HighlightNode
highlightNode _0 = HighlightNode _0 P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Highlights given quad. Coordinates are absolute with respect to the main frame viewport.
data HighlightQuad = HighlightQuad
    { -- | Quad to highlight
      quad :: !DOM.Quad
      -- | The highlight fill color (default: transparent).
    , color :: !(P.Maybe DOM.RGBA)
      -- | The highlight outline color (default: transparent).
    , outlineColor :: !(P.Maybe DOM.RGBA)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON HighlightQuad where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "highlightQuad" $ \_o -> HighlightQuad
            <$> _o .: "quad"
            <*> _o .:? "color"
            <*> _o .:? "outlineColor"
        ago = A.withArray "highlightQuad" $ \_a -> HighlightQuad
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON HighlightQuad where
    toEncoding (HighlightQuad _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "quad" .= _0
        , ("color" .=) <$> _1
        , ("outlineColor" .=) <$> _2
        ]
    toJSON (HighlightQuad _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "quad" .= _0
        , ("color" .=) <$> _1
        , ("outlineColor" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup HighlightQuad where
    HighlightQuad _0 _1 _2 <> HighlightQuad _ __1 __2 = HighlightQuad _0 (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
instance M.Method HighlightQuad where
    type Result HighlightQuad = ()
    name _ = "Overlay.highlightQuad"


------------------------------------------------------------------------------
-- | Highlights given quad. Coordinates are absolute with respect to the main frame viewport.
highlightQuad
    :: DOM.Quad
    -- ^ Quad to highlight

    -> HighlightQuad
highlightQuad _0 = HighlightQuad _0 P.empty P.empty


------------------------------------------------------------------------------
-- | Highlights given rectangle. Coordinates are absolute with respect to the main frame viewport.
data HighlightRect = HighlightRect
    { -- | X coordinate
      x :: !P.Int
      -- | Y coordinate
    , y :: !P.Int
      -- | Rectangle width
    , width :: !P.Int
      -- | Rectangle height
    , height :: !P.Int
      -- | The highlight fill color (default: transparent).
    , color :: !(P.Maybe DOM.RGBA)
      -- | The highlight outline color (default: transparent).
    , outlineColor :: !(P.Maybe DOM.RGBA)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON HighlightRect where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "highlightRect" $ \_o -> HighlightRect
            <$> _o .: "x"
            <*> _o .: "y"
            <*> _o .: "width"
            <*> _o .: "height"
            <*> _o .:? "color"
            <*> _o .:? "outlineColor"
        ago = A.withArray "highlightRect" $ \_a -> HighlightRect
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)


------------------------------------------------------------------------------
instance A.ToJSON HighlightRect where
    toEncoding (HighlightRect _0 _1 _2 _3 _4 _5) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "x" .= _0
        , P.pure $ "y" .= _1
        , P.pure $ "width" .= _2
        , P.pure $ "height" .= _3
        , ("color" .=) <$> _4
        , ("outlineColor" .=) <$> _5
        ]
    toJSON (HighlightRect _0 _1 _2 _3 _4 _5) = A.object $ P.catMaybes
        [ P.pure $ "x" .= _0
        , P.pure $ "y" .= _1
        , P.pure $ "width" .= _2
        , P.pure $ "height" .= _3
        , ("color" .=) <$> _4
        , ("outlineColor" .=) <$> _5
        ]


------------------------------------------------------------------------------
instance P.Semigroup HighlightRect where
    HighlightRect _0 _1 _2 _3 _4 _5 <> HighlightRect _ _ _ _ __4 __5 = HighlightRect _0 _1 _2 _3 (_4 <|> __4) (_5 <|> __5)


------------------------------------------------------------------------------
instance M.Method HighlightRect where
    type Result HighlightRect = ()
    name _ = "Overlay.highlightRect"


------------------------------------------------------------------------------
-- | Highlights given rectangle. Coordinates are absolute with respect to the main frame viewport.
highlightRect
    :: P.Int
    -- ^ X coordinate

    -> P.Int
    -- ^ Y coordinate

    -> P.Int
    -- ^ Rectangle width

    -> P.Int
    -- ^ Rectangle height

    -> HighlightRect
highlightRect _0 _1 _2 _3 = HighlightRect _0 _1 _2 _3 P.empty P.empty


------------------------------------------------------------------------------
-- | Enters the 'inspect' mode. In this mode, elements that user is hovering over are highlighted.
-- Backend then generates 'inspectNodeRequested' event upon element selection.
data SetInspectMode = SetInspectMode
    { -- | Set an inspection mode.
      mode :: !InspectMode
      -- | A descriptor for the highlight appearance of hovered-over nodes. May be omitted if @enabled
      -- == false@.
    , highlightConfig :: !(P.Maybe HighlightConfig)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetInspectMode where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setInspectMode" $ \_o -> SetInspectMode
            <$> _o .: "mode"
            <*> _o .:? "highlightConfig"
        ago = A.withArray "setInspectMode" $ \_a -> SetInspectMode
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SetInspectMode where
    toEncoding (SetInspectMode _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "mode" .= _0
        , ("highlightConfig" .=) <$> _1
        ]
    toJSON (SetInspectMode _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "mode" .= _0
        , ("highlightConfig" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetInspectMode where
    SetInspectMode _0 _1 <> SetInspectMode _ __1 = SetInspectMode _0 (_1 <|> __1)


------------------------------------------------------------------------------
instance M.Method SetInspectMode where
    type Result SetInspectMode = ()
    name _ = "Overlay.setInspectMode"


------------------------------------------------------------------------------
-- | Enters the 'inspect' mode. In this mode, elements that user is hovering over are highlighted.
-- Backend then generates 'inspectNodeRequested' event upon element selection.
setInspectMode
    :: InspectMode
    -- ^ Set an inspection mode.

    -> SetInspectMode
setInspectMode _0 = SetInspectMode _0 P.empty


------------------------------------------------------------------------------
-- | Highlights owner element of all frames detected to be ads.
data SetShowAdHighlights = SetShowAdHighlights
    { -- | True for showing ad highlights
      show :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetShowAdHighlights where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setShowAdHighlights" $ \_o -> SetShowAdHighlights
            <$> _o .: "show"
        ago = A.withArray "setShowAdHighlights" $ \_a -> SetShowAdHighlights
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetShowAdHighlights where
    toEncoding (SetShowAdHighlights _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "show" .= _0
        ]
    toJSON (SetShowAdHighlights _0) = A.object $ P.catMaybes
        [ P.pure $ "show" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetShowAdHighlights where
    SetShowAdHighlights _0 <> SetShowAdHighlights _ = SetShowAdHighlights _0


------------------------------------------------------------------------------
instance M.Method SetShowAdHighlights where
    type Result SetShowAdHighlights = ()
    name _ = "Overlay.setShowAdHighlights"


------------------------------------------------------------------------------
-- | Highlights owner element of all frames detected to be ads.
setShowAdHighlights
    :: P.Bool
    -- ^ True for showing ad highlights

    -> SetShowAdHighlights
setShowAdHighlights _0 = SetShowAdHighlights _0


------------------------------------------------------------------------------
data SetPausedInDebuggerMessage = SetPausedInDebuggerMessage
    { -- | The message to display, also triggers resume and step over controls.
      message :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetPausedInDebuggerMessage where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setPausedInDebuggerMessage" $ \_o -> SetPausedInDebuggerMessage
            <$> _o .:? "message"
        ago = A.withArray "setPausedInDebuggerMessage" $ \_a -> SetPausedInDebuggerMessage
            <$> P.traverse A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetPausedInDebuggerMessage where
    toEncoding (SetPausedInDebuggerMessage _0) = A.pairs $ P.fold $ P.catMaybes
        [ ("message" .=) <$> _0
        ]
    toJSON (SetPausedInDebuggerMessage _0) = A.object $ P.catMaybes
        [ ("message" .=) <$> _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetPausedInDebuggerMessage where
    SetPausedInDebuggerMessage _0 <> SetPausedInDebuggerMessage __0 = SetPausedInDebuggerMessage (_0 <|> __0)


------------------------------------------------------------------------------
instance P.Monoid SetPausedInDebuggerMessage where
    mempty = SetPausedInDebuggerMessage P.empty


------------------------------------------------------------------------------
instance M.Method SetPausedInDebuggerMessage where
    type Result SetPausedInDebuggerMessage = ()
    name _ = "Overlay.setPausedInDebuggerMessage"


------------------------------------------------------------------------------
setPausedInDebuggerMessage
    :: SetPausedInDebuggerMessage
setPausedInDebuggerMessage = SetPausedInDebuggerMessage P.empty


------------------------------------------------------------------------------
-- | Requests that backend shows debug borders on layers
data SetShowDebugBorders = SetShowDebugBorders
    { -- | True for showing debug borders
      show :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetShowDebugBorders where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setShowDebugBorders" $ \_o -> SetShowDebugBorders
            <$> _o .: "show"
        ago = A.withArray "setShowDebugBorders" $ \_a -> SetShowDebugBorders
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetShowDebugBorders where
    toEncoding (SetShowDebugBorders _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "show" .= _0
        ]
    toJSON (SetShowDebugBorders _0) = A.object $ P.catMaybes
        [ P.pure $ "show" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetShowDebugBorders where
    SetShowDebugBorders _0 <> SetShowDebugBorders _ = SetShowDebugBorders _0


------------------------------------------------------------------------------
instance M.Method SetShowDebugBorders where
    type Result SetShowDebugBorders = ()
    name _ = "Overlay.setShowDebugBorders"


------------------------------------------------------------------------------
-- | Requests that backend shows debug borders on layers
setShowDebugBorders
    :: P.Bool
    -- ^ True for showing debug borders

    -> SetShowDebugBorders
setShowDebugBorders _0 = SetShowDebugBorders _0


------------------------------------------------------------------------------
-- | Requests that backend shows the FPS counter
data SetShowFPSCounter = SetShowFPSCounter
    { -- | True for showing the FPS counter
      show :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetShowFPSCounter where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setShowFPSCounter" $ \_o -> SetShowFPSCounter
            <$> _o .: "show"
        ago = A.withArray "setShowFPSCounter" $ \_a -> SetShowFPSCounter
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetShowFPSCounter where
    toEncoding (SetShowFPSCounter _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "show" .= _0
        ]
    toJSON (SetShowFPSCounter _0) = A.object $ P.catMaybes
        [ P.pure $ "show" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetShowFPSCounter where
    SetShowFPSCounter _0 <> SetShowFPSCounter _ = SetShowFPSCounter _0


------------------------------------------------------------------------------
instance M.Method SetShowFPSCounter where
    type Result SetShowFPSCounter = ()
    name _ = "Overlay.setShowFPSCounter"


------------------------------------------------------------------------------
-- | Requests that backend shows the FPS counter
setShowFPSCounter
    :: P.Bool
    -- ^ True for showing the FPS counter

    -> SetShowFPSCounter
setShowFPSCounter _0 = SetShowFPSCounter _0


------------------------------------------------------------------------------
-- | Requests that backend shows paint rectangles
data SetShowPaintRects = SetShowPaintRects
    { -- | True for showing paint rectangles
      result :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetShowPaintRects where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setShowPaintRects" $ \_o -> SetShowPaintRects
            <$> _o .: "result"
        ago = A.withArray "setShowPaintRects" $ \_a -> SetShowPaintRects
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetShowPaintRects where
    toEncoding (SetShowPaintRects _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "result" .= _0
        ]
    toJSON (SetShowPaintRects _0) = A.object $ P.catMaybes
        [ P.pure $ "result" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetShowPaintRects where
    SetShowPaintRects _0 <> SetShowPaintRects _ = SetShowPaintRects _0


------------------------------------------------------------------------------
instance M.Method SetShowPaintRects where
    type Result SetShowPaintRects = ()
    name _ = "Overlay.setShowPaintRects"


------------------------------------------------------------------------------
-- | Requests that backend shows paint rectangles
setShowPaintRects
    :: P.Bool
    -- ^ True for showing paint rectangles

    -> SetShowPaintRects
setShowPaintRects _0 = SetShowPaintRects _0


------------------------------------------------------------------------------
-- | Requests that backend shows scroll bottleneck rects
data SetShowScrollBottleneckRects = SetShowScrollBottleneckRects
    { -- | True for showing scroll bottleneck rects
      show :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetShowScrollBottleneckRects where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setShowScrollBottleneckRects" $ \_o -> SetShowScrollBottleneckRects
            <$> _o .: "show"
        ago = A.withArray "setShowScrollBottleneckRects" $ \_a -> SetShowScrollBottleneckRects
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetShowScrollBottleneckRects where
    toEncoding (SetShowScrollBottleneckRects _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "show" .= _0
        ]
    toJSON (SetShowScrollBottleneckRects _0) = A.object $ P.catMaybes
        [ P.pure $ "show" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetShowScrollBottleneckRects where
    SetShowScrollBottleneckRects _0 <> SetShowScrollBottleneckRects _ = SetShowScrollBottleneckRects _0


------------------------------------------------------------------------------
instance M.Method SetShowScrollBottleneckRects where
    type Result SetShowScrollBottleneckRects = ()
    name _ = "Overlay.setShowScrollBottleneckRects"


------------------------------------------------------------------------------
-- | Requests that backend shows scroll bottleneck rects
setShowScrollBottleneckRects
    :: P.Bool
    -- ^ True for showing scroll bottleneck rects

    -> SetShowScrollBottleneckRects
setShowScrollBottleneckRects _0 = SetShowScrollBottleneckRects _0


------------------------------------------------------------------------------
-- | Requests that backend shows hit-test borders on layers
data SetShowHitTestBorders = SetShowHitTestBorders
    { -- | True for showing hit-test borders
      show :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetShowHitTestBorders where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setShowHitTestBorders" $ \_o -> SetShowHitTestBorders
            <$> _o .: "show"
        ago = A.withArray "setShowHitTestBorders" $ \_a -> SetShowHitTestBorders
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetShowHitTestBorders where
    toEncoding (SetShowHitTestBorders _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "show" .= _0
        ]
    toJSON (SetShowHitTestBorders _0) = A.object $ P.catMaybes
        [ P.pure $ "show" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetShowHitTestBorders where
    SetShowHitTestBorders _0 <> SetShowHitTestBorders _ = SetShowHitTestBorders _0


------------------------------------------------------------------------------
instance M.Method SetShowHitTestBorders where
    type Result SetShowHitTestBorders = ()
    name _ = "Overlay.setShowHitTestBorders"


------------------------------------------------------------------------------
-- | Requests that backend shows hit-test borders on layers
setShowHitTestBorders
    :: P.Bool
    -- ^ True for showing hit-test borders

    -> SetShowHitTestBorders
setShowHitTestBorders _0 = SetShowHitTestBorders _0


------------------------------------------------------------------------------
-- | Paints viewport size upon main frame resize.
data SetShowViewportSizeOnResize = SetShowViewportSizeOnResize
    { -- | Whether to paint size or not.
      show :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetShowViewportSizeOnResize where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setShowViewportSizeOnResize" $ \_o -> SetShowViewportSizeOnResize
            <$> _o .: "show"
        ago = A.withArray "setShowViewportSizeOnResize" $ \_a -> SetShowViewportSizeOnResize
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetShowViewportSizeOnResize where
    toEncoding (SetShowViewportSizeOnResize _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "show" .= _0
        ]
    toJSON (SetShowViewportSizeOnResize _0) = A.object $ P.catMaybes
        [ P.pure $ "show" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetShowViewportSizeOnResize where
    SetShowViewportSizeOnResize _0 <> SetShowViewportSizeOnResize _ = SetShowViewportSizeOnResize _0


------------------------------------------------------------------------------
instance M.Method SetShowViewportSizeOnResize where
    type Result SetShowViewportSizeOnResize = ()
    name _ = "Overlay.setShowViewportSizeOnResize"


------------------------------------------------------------------------------
-- | Paints viewport size upon main frame resize.
setShowViewportSizeOnResize
    :: P.Bool
    -- ^ Whether to paint size or not.

    -> SetShowViewportSizeOnResize
setShowViewportSizeOnResize _0 = SetShowViewportSizeOnResize _0


------------------------------------------------------------------------------
-- | Fired when the node should be inspected. This happens after call to @setInspectMode@ or when
-- user manually inspects an element.
data InspectNodeRequested = InspectNodeRequested
    { -- | Id of the node to inspect.
      backendNodeId :: !DOM.BackendNodeId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON InspectNodeRequested where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "inspectNodeRequested" $ \_o -> InspectNodeRequested
            <$> _o .: "backendNodeId"
        ago = A.withArray "inspectNodeRequested" $ \_a -> InspectNodeRequested
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON InspectNodeRequested where
    toEncoding (InspectNodeRequested _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "backendNodeId" .= _0
        ]
    toJSON (InspectNodeRequested _0) = A.object $ P.catMaybes
        [ P.pure $ "backendNodeId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup InspectNodeRequested where
    InspectNodeRequested _0 <> InspectNodeRequested _ = InspectNodeRequested _0


------------------------------------------------------------------------------
instance E.Event InspectNodeRequested where
    type Result InspectNodeRequested = InspectNodeRequested
    name _ = "Overlay.inspectNodeRequested"


------------------------------------------------------------------------------
-- | Fired when the node should be inspected. This happens after call to @setInspectMode@ or when
-- user manually inspects an element.
inspectNodeRequested :: P.Proxy InspectNodeRequested
inspectNodeRequested = P.Proxy


------------------------------------------------------------------------------
-- | Fired when the node should be highlighted. This happens after call to @setInspectMode@.
data NodeHighlightRequested = NodeHighlightRequested
    { nodeId :: !DOM.NodeId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON NodeHighlightRequested where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "nodeHighlightRequested" $ \_o -> NodeHighlightRequested
            <$> _o .: "nodeId"
        ago = A.withArray "nodeHighlightRequested" $ \_a -> NodeHighlightRequested
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON NodeHighlightRequested where
    toEncoding (NodeHighlightRequested _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]
    toJSON (NodeHighlightRequested _0) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup NodeHighlightRequested where
    NodeHighlightRequested _0 <> NodeHighlightRequested _ = NodeHighlightRequested _0


------------------------------------------------------------------------------
instance E.Event NodeHighlightRequested where
    type Result NodeHighlightRequested = NodeHighlightRequested
    name _ = "Overlay.nodeHighlightRequested"


------------------------------------------------------------------------------
-- | Fired when the node should be highlighted. This happens after call to @setInspectMode@.
nodeHighlightRequested :: P.Proxy NodeHighlightRequested
nodeHighlightRequested = P.Proxy


------------------------------------------------------------------------------
-- | Fired when user asks to capture screenshot of some area on the page.
data ScreenshotRequested = ScreenshotRequested
    { -- | Viewport to capture, in device independent pixels (dip).
      viewport :: !Page.Viewport
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ScreenshotRequested where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "screenshotRequested" $ \_o -> ScreenshotRequested
            <$> _o .: "viewport"
        ago = A.withArray "screenshotRequested" $ \_a -> ScreenshotRequested
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON ScreenshotRequested where
    toEncoding (ScreenshotRequested _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "viewport" .= _0
        ]
    toJSON (ScreenshotRequested _0) = A.object $ P.catMaybes
        [ P.pure $ "viewport" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup ScreenshotRequested where
    ScreenshotRequested _0 <> ScreenshotRequested _ = ScreenshotRequested _0


------------------------------------------------------------------------------
instance E.Event ScreenshotRequested where
    type Result ScreenshotRequested = ScreenshotRequested
    name _ = "Overlay.screenshotRequested"


------------------------------------------------------------------------------
-- | Fired when user asks to capture screenshot of some area on the page.
screenshotRequested :: P.Proxy ScreenshotRequested
screenshotRequested = P.Proxy


------------------------------------------------------------------------------
-- | Fired when user cancels the inspect mode.
data InspectModeCanceled = InspectModeCanceled
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON InspectModeCanceled where
    parseJSON A.Null = P.pure InspectModeCanceled
    parseJSON v = A.withArray "inspectModeCanceled" go v
        <|> A.withObject "inspectModeCanceled" go v
      where
        go _ = P.pure InspectModeCanceled


------------------------------------------------------------------------------
instance A.ToJSON InspectModeCanceled where
    toEncoding InspectModeCanceled = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON InspectModeCanceled = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup InspectModeCanceled where
    InspectModeCanceled <> InspectModeCanceled = InspectModeCanceled


------------------------------------------------------------------------------
instance P.Monoid InspectModeCanceled where
    mempty = InspectModeCanceled


------------------------------------------------------------------------------
instance E.Event InspectModeCanceled where
    type Result InspectModeCanceled = ()
    name _ = "Overlay.inspectModeCanceled"


------------------------------------------------------------------------------
-- | Fired when user cancels the inspect mode.
inspectModeCanceled :: P.Proxy InspectModeCanceled
inspectModeCanceled = P.Proxy

