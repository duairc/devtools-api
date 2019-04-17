{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | This domain exposes DOM read\/write operations. Each DOM Node is represented with its mirror object
-- that has an @id@. This @id@ can be used to get additional information on the Node, resolve it into
-- the JavaScript object wrapper, etc. It is important that client receives DOM events only for the
-- nodes that are known to the client. Backend keeps track of the nodes that were sent to the client
-- and never sends the same node twice. It is client's responsibility to collect information about
-- the nodes that were sent to the client.<p>Note that @iframe@ owner elements will return
-- corresponding document elements as their child nodes.<\/p>
module DevTools.API.DOM.Types
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
import qualified DevTools.API.Page.Types as Page


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Unique DOM node identifier.
type NodeId = P.Int


------------------------------------------------------------------------------
-- | Unique DOM node identifier used to reference a node that may not have been pushed to the
-- front-end.
type BackendNodeId = P.Int


------------------------------------------------------------------------------
-- | Backend node with a friendly name.
data BackendNode = BackendNode
    { -- | @Node@'s nodeType.
      nodeType :: !P.Int
      -- | @Node@'s nodeName.
    , nodeName :: !T.Text
    , backendNodeId :: !BackendNodeId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON BackendNode where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "BackendNode" $ \_o -> BackendNode
            <$> _o .: "nodeType"
            <*> _o .: "nodeName"
            <*> _o .: "backendNodeId"
        ago = A.withArray "BackendNode" $ \_a -> BackendNode
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON BackendNode where
    toEncoding (BackendNode _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeType" .= _0
        , P.pure $ "nodeName" .= _1
        , P.pure $ "backendNodeId" .= _2
        ]
    toJSON (BackendNode _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "nodeType" .= _0
        , P.pure $ "nodeName" .= _1
        , P.pure $ "backendNodeId" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup BackendNode where
    BackendNode _0 _1 _2 <> BackendNode _ _ _ = BackendNode _0 _1 _2


------------------------------------------------------------------------------
-- | Pseudo element type.
data PseudoType
    = FirstLine
    | FirstLetter
    | Before
    | After
    | Backdrop
    | Selection
    | FirstLineInherited
    | Scrollbar
    | ScrollbarThumb
    | ScrollbarButton
    | ScrollbarTrack
    | ScrollbarTrackPiece
    | ScrollbarCorner
    | Resizer
    | InputListButton
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PseudoType where
    parseJSON = A.withText "PseudoType" $ \t -> case t of
        "first-line" -> P.pure FirstLine
        "first-letter" -> P.pure FirstLetter
        "before" -> P.pure Before
        "after" -> P.pure After
        "backdrop" -> P.pure Backdrop
        "selection" -> P.pure Selection
        "first-line-inherited" -> P.pure FirstLineInherited
        "scrollbar" -> P.pure Scrollbar
        "scrollbar-thumb" -> P.pure ScrollbarThumb
        "scrollbar-button" -> P.pure ScrollbarButton
        "scrollbar-track" -> P.pure ScrollbarTrack
        "scrollbar-track-piece" -> P.pure ScrollbarTrackPiece
        "scrollbar-corner" -> P.pure ScrollbarCorner
        "resizer" -> P.pure Resizer
        "input-list-button" -> P.pure InputListButton
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON PseudoType where
    toJSON FirstLine = "first-line"
    toJSON FirstLetter = "first-letter"
    toJSON Before = "before"
    toJSON After = "after"
    toJSON Backdrop = "backdrop"
    toJSON Selection = "selection"
    toJSON FirstLineInherited = "first-line-inherited"
    toJSON Scrollbar = "scrollbar"
    toJSON ScrollbarThumb = "scrollbar-thumb"
    toJSON ScrollbarButton = "scrollbar-button"
    toJSON ScrollbarTrack = "scrollbar-track"
    toJSON ScrollbarTrackPiece = "scrollbar-track-piece"
    toJSON ScrollbarCorner = "scrollbar-corner"
    toJSON Resizer = "resizer"
    toJSON InputListButton = "input-list-button"


------------------------------------------------------------------------------
-- | Shadow root type.
data ShadowRootType
    = UserAgent
    | Open
    | Closed
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ShadowRootType where
    parseJSON = A.withText "ShadowRootType" $ \t -> case t of
        "user-agent" -> P.pure UserAgent
        "open" -> P.pure Open
        "closed" -> P.pure Closed
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON ShadowRootType where
    toJSON UserAgent = "user-agent"
    toJSON Open = "open"
    toJSON Closed = "closed"


------------------------------------------------------------------------------
-- | DOM interaction is implemented in terms of mirror objects that represent the actual DOM nodes.
-- DOMNode is a base node mirror type.
data Node = Node
    { -- | Node identifier that is passed into the rest of the DOM messages as the @nodeId@. Backend
      -- will only push node with given @id@ once. It is aware of all requested nodes and will only
      -- fire DOM events for nodes known to the client.
      nodeId :: !NodeId
      -- | The id of the parent node if any.
    , parentId :: !(P.Maybe NodeId)
      -- | The BackendNodeId for this node.
    , backendNodeId :: !BackendNodeId
      -- | @Node@'s nodeType.
    , nodeType :: !P.Int
      -- | @Node@'s nodeName.
    , nodeName :: !T.Text
      -- | @Node@'s localName.
    , localName :: !T.Text
      -- | @Node@'s nodeValue.
    , nodeValue :: !T.Text
      -- | Child count for @Container@ nodes.
    , childNodeCount :: !(P.Maybe P.Int)
      -- | Child nodes of this node when requested with children.
    , children :: !(P.Maybe [Node])
      -- | Attributes of the @Element@ node in the form of flat array @[name1, value1, name2, value2]@.
    , attributes :: !(P.Maybe [T.Text])
      -- | Document URL that @Document@ or @FrameOwner@ node points to.
    , documentURL :: !(P.Maybe T.Text)
      -- | Base URL that @Document@ or @FrameOwner@ node uses for URL completion.
    , baseURL :: !(P.Maybe T.Text)
      -- | @DocumentType@'s publicId.
    , publicId :: !(P.Maybe T.Text)
      -- | @DocumentType@'s systemId.
    , systemId :: !(P.Maybe T.Text)
      -- | @DocumentType@'s internalSubset.
    , internalSubset :: !(P.Maybe T.Text)
      -- | @Document@'s XML version in case of XML documents.
    , xmlVersion :: !(P.Maybe T.Text)
      -- | @Attr@'s name.
    , name :: !(P.Maybe T.Text)
      -- | @Attr@'s value.
    , value :: !(P.Maybe T.Text)
      -- | Pseudo element type for this node.
    , pseudoType :: !(P.Maybe PseudoType)
      -- | Shadow root type.
    , shadowRootType :: !(P.Maybe ShadowRootType)
      -- | Frame ID for frame owner elements.
    , frameId :: !(P.Maybe Page.FrameId)
      -- | Content document for frame owner elements.
    , contentDocument :: !(P.Maybe Node)
      -- | Shadow root list for given element host.
    , shadowRoots :: !(P.Maybe [Node])
      -- | Content document fragment for template elements.
    , templateContent :: !(P.Maybe Node)
      -- | Pseudo elements associated with this node.
    , pseudoElements :: !(P.Maybe [Node])
      -- | Import document for the HTMLImport links.
    , importedDocument :: !(P.Maybe Node)
      -- | Distributed nodes for given insertion point.
    , distributedNodes :: !(P.Maybe [BackendNode])
      -- | Whether the node is SVG.
    , isSVG :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Node where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "Node" $ \_o -> Node
            <$> _o .: "nodeId"
            <*> _o .:? "parentId"
            <*> _o .: "backendNodeId"
            <*> _o .: "nodeType"
            <*> _o .: "nodeName"
            <*> _o .: "localName"
            <*> _o .: "nodeValue"
            <*> _o .:? "childNodeCount"
            <*> _o .:? "children"
            <*> _o .:? "attributes"
            <*> _o .:? "documentURL"
            <*> _o .:? "baseURL"
            <*> _o .:? "publicId"
            <*> _o .:? "systemId"
            <*> _o .:? "internalSubset"
            <*> _o .:? "xmlVersion"
            <*> _o .:? "name"
            <*> _o .:? "value"
            <*> _o .:? "pseudoType"
            <*> _o .:? "shadowRootType"
            <*> _o .:? "frameId"
            <*> _o .:? "contentDocument"
            <*> _o .:? "shadowRoots"
            <*> _o .:? "templateContent"
            <*> _o .:? "pseudoElements"
            <*> _o .:? "importedDocument"
            <*> _o .:? "distributedNodes"
            <*> _o .:? "isSVG"
        ago = A.withArray "Node" $ \_a -> Node
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.maybe P.empty A.parseJSON (_a !? 6)
            <*> P.traverse A.parseJSON (_a !? 7)
            <*> P.traverse A.parseJSON (_a !? 8)
            <*> P.traverse A.parseJSON (_a !? 9)
            <*> P.traverse A.parseJSON (_a !? 10)
            <*> P.traverse A.parseJSON (_a !? 11)
            <*> P.traverse A.parseJSON (_a !? 12)
            <*> P.traverse A.parseJSON (_a !? 13)
            <*> P.traverse A.parseJSON (_a !? 14)
            <*> P.traverse A.parseJSON (_a !? 15)
            <*> P.traverse A.parseJSON (_a !? 16)
            <*> P.traverse A.parseJSON (_a !? 17)
            <*> P.traverse A.parseJSON (_a !? 18)
            <*> P.traverse A.parseJSON (_a !? 19)
            <*> P.traverse A.parseJSON (_a !? 20)
            <*> P.traverse A.parseJSON (_a !? 21)
            <*> P.traverse A.parseJSON (_a !? 22)
            <*> P.traverse A.parseJSON (_a !? 23)
            <*> P.traverse A.parseJSON (_a !? 24)
            <*> P.traverse A.parseJSON (_a !? 25)
            <*> P.traverse A.parseJSON (_a !? 26)
            <*> P.traverse A.parseJSON (_a !? 27)


------------------------------------------------------------------------------
instance A.ToJSON Node where
    toEncoding (Node _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 _15 _16 _17 _18 _19 _20 _21 _22 _23 _24 _25 _26 _27) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , ("parentId" .=) <$> _1
        , P.pure $ "backendNodeId" .= _2
        , P.pure $ "nodeType" .= _3
        , P.pure $ "nodeName" .= _4
        , P.pure $ "localName" .= _5
        , P.pure $ "nodeValue" .= _6
        , ("childNodeCount" .=) <$> _7
        , ("children" .=) <$> _8
        , ("attributes" .=) <$> _9
        , ("documentURL" .=) <$> _10
        , ("baseURL" .=) <$> _11
        , ("publicId" .=) <$> _12
        , ("systemId" .=) <$> _13
        , ("internalSubset" .=) <$> _14
        , ("xmlVersion" .=) <$> _15
        , ("name" .=) <$> _16
        , ("value" .=) <$> _17
        , ("pseudoType" .=) <$> _18
        , ("shadowRootType" .=) <$> _19
        , ("frameId" .=) <$> _20
        , ("contentDocument" .=) <$> _21
        , ("shadowRoots" .=) <$> _22
        , ("templateContent" .=) <$> _23
        , ("pseudoElements" .=) <$> _24
        , ("importedDocument" .=) <$> _25
        , ("distributedNodes" .=) <$> _26
        , ("isSVG" .=) <$> _27
        ]
    toJSON (Node _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 _15 _16 _17 _18 _19 _20 _21 _22 _23 _24 _25 _26 _27) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , ("parentId" .=) <$> _1
        , P.pure $ "backendNodeId" .= _2
        , P.pure $ "nodeType" .= _3
        , P.pure $ "nodeName" .= _4
        , P.pure $ "localName" .= _5
        , P.pure $ "nodeValue" .= _6
        , ("childNodeCount" .=) <$> _7
        , ("children" .=) <$> _8
        , ("attributes" .=) <$> _9
        , ("documentURL" .=) <$> _10
        , ("baseURL" .=) <$> _11
        , ("publicId" .=) <$> _12
        , ("systemId" .=) <$> _13
        , ("internalSubset" .=) <$> _14
        , ("xmlVersion" .=) <$> _15
        , ("name" .=) <$> _16
        , ("value" .=) <$> _17
        , ("pseudoType" .=) <$> _18
        , ("shadowRootType" .=) <$> _19
        , ("frameId" .=) <$> _20
        , ("contentDocument" .=) <$> _21
        , ("shadowRoots" .=) <$> _22
        , ("templateContent" .=) <$> _23
        , ("pseudoElements" .=) <$> _24
        , ("importedDocument" .=) <$> _25
        , ("distributedNodes" .=) <$> _26
        , ("isSVG" .=) <$> _27
        ]


------------------------------------------------------------------------------
instance P.Semigroup Node where
    Node _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 _15 _16 _17 _18 _19 _20 _21 _22 _23 _24 _25 _26 _27 <> Node _ __1 _ _ _ _ _ __7 __8 __9 __10 __11 __12 __13 __14 __15 __16 __17 __18 __19 __20 __21 __22 __23 __24 __25 __26 __27 = Node _0 (_1 <|> __1) _2 _3 _4 _5 _6 (_7 <|> __7) (_8 <|> __8) (_9 <|> __9) (_10 <|> __10) (_11 <|> __11) (_12 <|> __12) (_13 <|> __13) (_14 <|> __14) (_15 <|> __15) (_16 <|> __16) (_17 <|> __17) (_18 <|> __18) (_19 <|> __19) (_20 <|> __20) (_21 <|> __21) (_22 <|> __22) (_23 <|> __23) (_24 <|> __24) (_25 <|> __25) (_26 <|> __26) (_27 <|> __27)


------------------------------------------------------------------------------
-- | A structure holding an RGBA color.
data RGBA = RGBA
    { -- | The red component, in the [0-255] range.
      r :: !P.Int
      -- | The green component, in the [0-255] range.
    , g :: !P.Int
      -- | The blue component, in the [0-255] range.
    , b :: !P.Int
      -- | The alpha component, in the [0-1] range (default: 1).
    , a :: !(P.Maybe P.Double)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RGBA where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "RGBA" $ \_o -> RGBA
            <$> _o .: "r"
            <*> _o .: "g"
            <*> _o .: "b"
            <*> _o .:? "a"
        ago = A.withArray "RGBA" $ \_a -> RGBA
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON RGBA where
    toEncoding (RGBA _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "r" .= _0
        , P.pure $ "g" .= _1
        , P.pure $ "b" .= _2
        , ("a" .=) <$> _3
        ]
    toJSON (RGBA _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "r" .= _0
        , P.pure $ "g" .= _1
        , P.pure $ "b" .= _2
        , ("a" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup RGBA where
    RGBA _0 _1 _2 _3 <> RGBA _ _ _ __3 = RGBA _0 _1 _2 (_3 <|> __3)


------------------------------------------------------------------------------
-- | An array of quad vertices, x immediately followed by y for each point, points clock-wise.
type Quad = [P.Double]


------------------------------------------------------------------------------
-- | Box model.
data BoxModel = BoxModel
    { -- | Content box
      content :: !Quad
      -- | Padding box
    , padding :: !Quad
      -- | Border box
    , border :: !Quad
      -- | Margin box
    , margin :: !Quad
      -- | Node width
    , width :: !P.Int
      -- | Node height
    , height :: !P.Int
      -- | Shape outside coordinates
    , shapeOutside :: !(P.Maybe ShapeOutsideInfo)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON BoxModel where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "BoxModel" $ \_o -> BoxModel
            <$> _o .: "content"
            <*> _o .: "padding"
            <*> _o .: "border"
            <*> _o .: "margin"
            <*> _o .: "width"
            <*> _o .: "height"
            <*> _o .:? "shapeOutside"
        ago = A.withArray "BoxModel" $ \_a -> BoxModel
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)


------------------------------------------------------------------------------
instance A.ToJSON BoxModel where
    toEncoding (BoxModel _0 _1 _2 _3 _4 _5 _6) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "content" .= _0
        , P.pure $ "padding" .= _1
        , P.pure $ "border" .= _2
        , P.pure $ "margin" .= _3
        , P.pure $ "width" .= _4
        , P.pure $ "height" .= _5
        , ("shapeOutside" .=) <$> _6
        ]
    toJSON (BoxModel _0 _1 _2 _3 _4 _5 _6) = A.object $ P.catMaybes
        [ P.pure $ "content" .= _0
        , P.pure $ "padding" .= _1
        , P.pure $ "border" .= _2
        , P.pure $ "margin" .= _3
        , P.pure $ "width" .= _4
        , P.pure $ "height" .= _5
        , ("shapeOutside" .=) <$> _6
        ]


------------------------------------------------------------------------------
instance P.Semigroup BoxModel where
    BoxModel _0 _1 _2 _3 _4 _5 _6 <> BoxModel _ _ _ _ _ _ __6 = BoxModel _0 _1 _2 _3 _4 _5 (_6 <|> __6)


------------------------------------------------------------------------------
-- | CSS Shape Outside details.
data ShapeOutsideInfo = ShapeOutsideInfo
    { -- | Shape bounds
      bounds :: !Quad
      -- | Shape coordinate details
    , shape :: ![A.Value]
      -- | Margin shape bounds
    , marginShape :: ![A.Value]
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ShapeOutsideInfo where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "ShapeOutsideInfo" $ \_o -> ShapeOutsideInfo
            <$> _o .: "bounds"
            <*> _o .: "shape"
            <*> _o .: "marginShape"
        ago = A.withArray "ShapeOutsideInfo" $ \_a -> ShapeOutsideInfo
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON ShapeOutsideInfo where
    toEncoding (ShapeOutsideInfo _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "bounds" .= _0
        , P.pure $ "shape" .= _1
        , P.pure $ "marginShape" .= _2
        ]
    toJSON (ShapeOutsideInfo _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "bounds" .= _0
        , P.pure $ "shape" .= _1
        , P.pure $ "marginShape" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup ShapeOutsideInfo where
    ShapeOutsideInfo _0 _1 _2 <> ShapeOutsideInfo _ _ _ = ShapeOutsideInfo _0 _1 _2


------------------------------------------------------------------------------
-- | Rectangle.
data Rect = Rect
    { -- | X coordinate
      x :: !P.Double
      -- | Y coordinate
    , y :: !P.Double
      -- | Rectangle width
    , width :: !P.Double
      -- | Rectangle height
    , height :: !P.Double
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Rect where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "Rect" $ \_o -> Rect
            <$> _o .: "x"
            <*> _o .: "y"
            <*> _o .: "width"
            <*> _o .: "height"
        ago = A.withArray "Rect" $ \_a -> Rect
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON Rect where
    toEncoding (Rect _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "x" .= _0
        , P.pure $ "y" .= _1
        , P.pure $ "width" .= _2
        , P.pure $ "height" .= _3
        ]
    toJSON (Rect _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "x" .= _0
        , P.pure $ "y" .= _1
        , P.pure $ "width" .= _2
        , P.pure $ "height" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup Rect where
    Rect _0 _1 _2 _3 <> Rect _ _ _ _ = Rect _0 _1 _2 _3

