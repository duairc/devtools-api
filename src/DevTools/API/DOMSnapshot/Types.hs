{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | This domain facilitates obtaining document snapshots with DOM, layout, and style information.
module DevTools.API.DOMSnapshot.Types{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

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
import qualified DevTools.API.DOM.Types as DOM
import qualified DevTools.API.DOMDebugger.Types as DOMDebugger
import qualified DevTools.API.Page.Types as Page


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | A Node in the DOM tree.
data DOMNode = DOMNode
    { -- | @Node@'s nodeType.
      nodeType :: !P.Int
      -- | @Node@'s nodeName.
    , nodeName :: !T.Text
      -- | @Node@'s nodeValue.
    , nodeValue :: !T.Text
      -- | Only set for textarea elements, contains the text value.
    , textValue :: !(P.Maybe T.Text)
      -- | Only set for input elements, contains the input's associated text value.
    , inputValue :: !(P.Maybe T.Text)
      -- | Only set for radio and checkbox input elements, indicates if the element has been checked
    , inputChecked :: !(P.Maybe P.Bool)
      -- | Only set for option elements, indicates if the element has been selected
    , optionSelected :: !(P.Maybe P.Bool)
      -- | @Node@'s id, corresponds to DOM.Node.backendNodeId.
    , backendNodeId :: !DOM.BackendNodeId
      -- | The indexes of the node's child nodes in the @domNodes@ array returned by @getSnapshot@, if
      -- any.
    , childNodeIndexes :: !(P.Maybe [P.Int])
      -- | Attributes of an @Element@ node.
    , attributes :: !(P.Maybe [NameValue])
      -- | Indexes of pseudo elements associated with this node in the @domNodes@ array returned by
      -- @getSnapshot@, if any.
    , pseudoElementIndexes :: !(P.Maybe [P.Int])
      -- | The index of the node's related layout tree node in the @layoutTreeNodes@ array returned by
      -- @getSnapshot@, if any.
    , layoutNodeIndex :: !(P.Maybe P.Int)
      -- | Document URL that @Document@ or @FrameOwner@ node points to.
    , documentURL :: !(P.Maybe T.Text)
      -- | Base URL that @Document@ or @FrameOwner@ node uses for URL completion.
    , baseURL :: !(P.Maybe T.Text)
      -- | Only set for documents, contains the document's content language.
    , contentLanguage :: !(P.Maybe T.Text)
      -- | Only set for documents, contains the document's character set encoding.
    , documentEncoding :: !(P.Maybe T.Text)
      -- | @DocumentType@ node's publicId.
    , publicId :: !(P.Maybe T.Text)
      -- | @DocumentType@ node's systemId.
    , systemId :: !(P.Maybe T.Text)
      -- | Frame ID for frame owner elements and also for the document node.
    , frameId :: !(P.Maybe Page.FrameId)
      -- | The index of a frame owner element's content document in the @domNodes@ array returned by
      -- @getSnapshot@, if any.
    , contentDocumentIndex :: !(P.Maybe P.Int)
      -- | Type of a pseudo element node.
    , pseudoType :: !(P.Maybe DOM.PseudoType)
      -- | Shadow root type.
    , shadowRootType :: !(P.Maybe DOM.ShadowRootType)
      -- | Whether this DOM node responds to mouse clicks. This includes nodes that have had click
      -- event listeners attached via JavaScript as well as anchor tags that naturally navigate when
      -- clicked.
    , isClickable :: !(P.Maybe P.Bool)
      -- | Details of the node's event listeners, if any.
    , eventListeners :: !(P.Maybe [DOMDebugger.EventListener])
      -- | The selected url for nodes with a srcset attribute.
    , currentSourceURL :: !(P.Maybe T.Text)
      -- | The url of the script (if any) that generates this node.
    , originURL :: !(P.Maybe T.Text)
      -- | Scroll offsets, set when this node is a Document.
    , scrollOffsetX :: !(P.Maybe P.Double)
    , scrollOffsetY :: !(P.Maybe P.Double)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DOMNode where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "DOMNode" $ \_o -> DOMNode
            <$> _o .: "nodeType"
            <*> _o .: "nodeName"
            <*> _o .: "nodeValue"
            <*> _o .:? "textValue"
            <*> _o .:? "inputValue"
            <*> _o .:? "inputChecked"
            <*> _o .:? "optionSelected"
            <*> _o .: "backendNodeId"
            <*> _o .:? "childNodeIndexes"
            <*> _o .:? "attributes"
            <*> _o .:? "pseudoElementIndexes"
            <*> _o .:? "layoutNodeIndex"
            <*> _o .:? "documentURL"
            <*> _o .:? "baseURL"
            <*> _o .:? "contentLanguage"
            <*> _o .:? "documentEncoding"
            <*> _o .:? "publicId"
            <*> _o .:? "systemId"
            <*> _o .:? "frameId"
            <*> _o .:? "contentDocumentIndex"
            <*> _o .:? "pseudoType"
            <*> _o .:? "shadowRootType"
            <*> _o .:? "isClickable"
            <*> _o .:? "eventListeners"
            <*> _o .:? "currentSourceURL"
            <*> _o .:? "originURL"
            <*> _o .:? "scrollOffsetX"
            <*> _o .:? "scrollOffsetY"
        ago = A.withArray "DOMNode" $ \_a -> DOMNode
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)
            <*> P.maybe P.empty A.parseJSON (_a !? 7)
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
instance A.ToJSON DOMNode where
    toEncoding (DOMNode _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 _15 _16 _17 _18 _19 _20 _21 _22 _23 _24 _25 _26 _27) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeType" .= _0
        , P.pure $ "nodeName" .= _1
        , P.pure $ "nodeValue" .= _2
        , ("textValue" .=) <$> _3
        , ("inputValue" .=) <$> _4
        , ("inputChecked" .=) <$> _5
        , ("optionSelected" .=) <$> _6
        , P.pure $ "backendNodeId" .= _7
        , ("childNodeIndexes" .=) <$> _8
        , ("attributes" .=) <$> _9
        , ("pseudoElementIndexes" .=) <$> _10
        , ("layoutNodeIndex" .=) <$> _11
        , ("documentURL" .=) <$> _12
        , ("baseURL" .=) <$> _13
        , ("contentLanguage" .=) <$> _14
        , ("documentEncoding" .=) <$> _15
        , ("publicId" .=) <$> _16
        , ("systemId" .=) <$> _17
        , ("frameId" .=) <$> _18
        , ("contentDocumentIndex" .=) <$> _19
        , ("pseudoType" .=) <$> _20
        , ("shadowRootType" .=) <$> _21
        , ("isClickable" .=) <$> _22
        , ("eventListeners" .=) <$> _23
        , ("currentSourceURL" .=) <$> _24
        , ("originURL" .=) <$> _25
        , ("scrollOffsetX" .=) <$> _26
        , ("scrollOffsetY" .=) <$> _27
        ]
    toJSON (DOMNode _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 _15 _16 _17 _18 _19 _20 _21 _22 _23 _24 _25 _26 _27) = A.object $ P.catMaybes
        [ P.pure $ "nodeType" .= _0
        , P.pure $ "nodeName" .= _1
        , P.pure $ "nodeValue" .= _2
        , ("textValue" .=) <$> _3
        , ("inputValue" .=) <$> _4
        , ("inputChecked" .=) <$> _5
        , ("optionSelected" .=) <$> _6
        , P.pure $ "backendNodeId" .= _7
        , ("childNodeIndexes" .=) <$> _8
        , ("attributes" .=) <$> _9
        , ("pseudoElementIndexes" .=) <$> _10
        , ("layoutNodeIndex" .=) <$> _11
        , ("documentURL" .=) <$> _12
        , ("baseURL" .=) <$> _13
        , ("contentLanguage" .=) <$> _14
        , ("documentEncoding" .=) <$> _15
        , ("publicId" .=) <$> _16
        , ("systemId" .=) <$> _17
        , ("frameId" .=) <$> _18
        , ("contentDocumentIndex" .=) <$> _19
        , ("pseudoType" .=) <$> _20
        , ("shadowRootType" .=) <$> _21
        , ("isClickable" .=) <$> _22
        , ("eventListeners" .=) <$> _23
        , ("currentSourceURL" .=) <$> _24
        , ("originURL" .=) <$> _25
        , ("scrollOffsetX" .=) <$> _26
        , ("scrollOffsetY" .=) <$> _27
        ]


------------------------------------------------------------------------------
instance P.Semigroup DOMNode where
    DOMNode _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 _15 _16 _17 _18 _19 _20 _21 _22 _23 _24 _25 _26 _27 <> DOMNode _ _ _ __3 __4 __5 __6 _ __8 __9 __10 __11 __12 __13 __14 __15 __16 __17 __18 __19 __20 __21 __22 __23 __24 __25 __26 __27 = DOMNode _0 _1 _2 (_3 <|> __3) (_4 <|> __4) (_5 <|> __5) (_6 <|> __6) _7 (_8 <|> __8) (_9 <|> __9) (_10 <|> __10) (_11 <|> __11) (_12 <|> __12) (_13 <|> __13) (_14 <|> __14) (_15 <|> __15) (_16 <|> __16) (_17 <|> __17) (_18 <|> __18) (_19 <|> __19) (_20 <|> __20) (_21 <|> __21) (_22 <|> __22) (_23 <|> __23) (_24 <|> __24) (_25 <|> __25) (_26 <|> __26) (_27 <|> __27)


------------------------------------------------------------------------------
-- | Details of post layout rendered text positions. The exact layout should not be regarded as
-- stable and may change between versions.
data InlineTextBox = InlineTextBox
    { -- | The bounding box in document coordinates. Note that scroll offset of the document is ignored.
      boundingBox :: !DOM.Rect
      -- | The starting index in characters, for this post layout textbox substring. Characters that
      -- would be represented as a surrogate pair in UTF-16 have length 2.
    , startCharacterIndex :: !P.Int
      -- | The number of characters in this post layout textbox substring. Characters that would be
      -- represented as a surrogate pair in UTF-16 have length 2.
    , numCharacters :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON InlineTextBox where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "InlineTextBox" $ \_o -> InlineTextBox
            <$> _o .: "boundingBox"
            <*> _o .: "startCharacterIndex"
            <*> _o .: "numCharacters"
        ago = A.withArray "InlineTextBox" $ \_a -> InlineTextBox
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON InlineTextBox where
    toEncoding (InlineTextBox _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "boundingBox" .= _0
        , P.pure $ "startCharacterIndex" .= _1
        , P.pure $ "numCharacters" .= _2
        ]
    toJSON (InlineTextBox _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "boundingBox" .= _0
        , P.pure $ "startCharacterIndex" .= _1
        , P.pure $ "numCharacters" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup InlineTextBox where
    InlineTextBox _0 _1 _2 <> InlineTextBox _ _ _ = InlineTextBox _0 _1 _2


------------------------------------------------------------------------------
-- | Details of an element in the DOM tree with a LayoutObject.
data LayoutTreeNode = LayoutTreeNode
    { -- | The index of the related DOM node in the @domNodes@ array returned by @getSnapshot@.
      domNodeIndex :: !P.Int
      -- | The bounding box in document coordinates. Note that scroll offset of the document is ignored.
    , boundingBox :: !DOM.Rect
      -- | Contents of the LayoutText, if any.
    , layoutText :: !(P.Maybe T.Text)
      -- | The post-layout inline text nodes, if any.
    , inlineTextNodes :: !(P.Maybe [InlineTextBox])
      -- | Index into the @computedStyles@ array returned by @getSnapshot@.
    , styleIndex :: !(P.Maybe P.Int)
      -- | Global paint order index, which is determined by the stacking order of the nodes. Nodes
      -- that are painted together will have the same index. Only provided if includePaintOrder in
      -- getSnapshot was true.
    , paintOrder :: !(P.Maybe P.Int)
      -- | Set to true to indicate the element begins a new stacking context.
    , isStackingContext :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON LayoutTreeNode where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "LayoutTreeNode" $ \_o -> LayoutTreeNode
            <$> _o .: "domNodeIndex"
            <*> _o .: "boundingBox"
            <*> _o .:? "layoutText"
            <*> _o .:? "inlineTextNodes"
            <*> _o .:? "styleIndex"
            <*> _o .:? "paintOrder"
            <*> _o .:? "isStackingContext"
        ago = A.withArray "LayoutTreeNode" $ \_a -> LayoutTreeNode
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)


------------------------------------------------------------------------------
instance A.ToJSON LayoutTreeNode where
    toEncoding (LayoutTreeNode _0 _1 _2 _3 _4 _5 _6) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "domNodeIndex" .= _0
        , P.pure $ "boundingBox" .= _1
        , ("layoutText" .=) <$> _2
        , ("inlineTextNodes" .=) <$> _3
        , ("styleIndex" .=) <$> _4
        , ("paintOrder" .=) <$> _5
        , ("isStackingContext" .=) <$> _6
        ]
    toJSON (LayoutTreeNode _0 _1 _2 _3 _4 _5 _6) = A.object $ P.catMaybes
        [ P.pure $ "domNodeIndex" .= _0
        , P.pure $ "boundingBox" .= _1
        , ("layoutText" .=) <$> _2
        , ("inlineTextNodes" .=) <$> _3
        , ("styleIndex" .=) <$> _4
        , ("paintOrder" .=) <$> _5
        , ("isStackingContext" .=) <$> _6
        ]


------------------------------------------------------------------------------
instance P.Semigroup LayoutTreeNode where
    LayoutTreeNode _0 _1 _2 _3 _4 _5 _6 <> LayoutTreeNode _ _ __2 __3 __4 __5 __6 = LayoutTreeNode _0 _1 (_2 <|> __2) (_3 <|> __3) (_4 <|> __4) (_5 <|> __5) (_6 <|> __6)


------------------------------------------------------------------------------
-- | A subset of the full ComputedStyle as defined by the request whitelist.
data ComputedStyle = ComputedStyle
    { -- | Name\/value pairs of computed style properties.
      properties :: ![NameValue]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ComputedStyle where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "ComputedStyle" $ \_o -> ComputedStyle
            <$> _o .: "properties"
        ago = A.withArray "ComputedStyle" $ \_a -> ComputedStyle
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON ComputedStyle where
    toEncoding (ComputedStyle _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "properties" .= _0
        ]
    toJSON (ComputedStyle _0) = A.object $ P.catMaybes
        [ P.pure $ "properties" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup ComputedStyle where
    ComputedStyle _0 <> ComputedStyle _ = ComputedStyle _0


------------------------------------------------------------------------------
-- | A name\/value pair.
data NameValue = NameValue
    { -- | Attribute\/property name.
      name :: !T.Text
      -- | Attribute\/property value.
    , value :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON NameValue where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "NameValue" $ \_o -> NameValue
            <$> _o .: "name"
            <*> _o .: "value"
        ago = A.withArray "NameValue" $ \_a -> NameValue
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON NameValue where
    toEncoding (NameValue _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "value" .= _1
        ]
    toJSON (NameValue _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "value" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup NameValue where
    NameValue _0 _1 <> NameValue _ _ = NameValue _0 _1


------------------------------------------------------------------------------
-- | Index of the string in the strings table.
type StringIndex = P.Int


------------------------------------------------------------------------------
-- | Index of the string in the strings table.
type ArrayOfStrings = [StringIndex]


------------------------------------------------------------------------------
-- | Data that is only present on rare nodes.
data RareStringData = RareStringData
    { index :: ![P.Int]
    , value :: ![StringIndex]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RareStringData where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "RareStringData" $ \_o -> RareStringData
            <$> _o .: "index"
            <*> _o .: "value"
        ago = A.withArray "RareStringData" $ \_a -> RareStringData
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON RareStringData where
    toEncoding (RareStringData _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "index" .= _0
        , P.pure $ "value" .= _1
        ]
    toJSON (RareStringData _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "index" .= _0
        , P.pure $ "value" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup RareStringData where
    RareStringData _0 _1 <> RareStringData _ _ = RareStringData _0 _1


------------------------------------------------------------------------------
data RareBooleanData = RareBooleanData
    { index :: ![P.Int]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RareBooleanData where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "RareBooleanData" $ \_o -> RareBooleanData
            <$> _o .: "index"
        ago = A.withArray "RareBooleanData" $ \_a -> RareBooleanData
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON RareBooleanData where
    toEncoding (RareBooleanData _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "index" .= _0
        ]
    toJSON (RareBooleanData _0) = A.object $ P.catMaybes
        [ P.pure $ "index" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup RareBooleanData where
    RareBooleanData _0 <> RareBooleanData _ = RareBooleanData _0


------------------------------------------------------------------------------
data RareIntegerData = RareIntegerData
    { index :: ![P.Int]
    , value :: ![P.Int]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RareIntegerData where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "RareIntegerData" $ \_o -> RareIntegerData
            <$> _o .: "index"
            <*> _o .: "value"
        ago = A.withArray "RareIntegerData" $ \_a -> RareIntegerData
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON RareIntegerData where
    toEncoding (RareIntegerData _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "index" .= _0
        , P.pure $ "value" .= _1
        ]
    toJSON (RareIntegerData _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "index" .= _0
        , P.pure $ "value" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup RareIntegerData where
    RareIntegerData _0 _1 <> RareIntegerData _ _ = RareIntegerData _0 _1


------------------------------------------------------------------------------
type Rectangle = [P.Double]


------------------------------------------------------------------------------
-- | Document snapshot.
data DocumentSnapshot = DocumentSnapshot
    { -- | Document URL that @Document@ or @FrameOwner@ node points to.
      documentURL :: !StringIndex
      -- | Base URL that @Document@ or @FrameOwner@ node uses for URL completion.
    , baseURL :: !StringIndex
      -- | Contains the document's content language.
    , contentLanguage :: !StringIndex
      -- | Contains the document's character set encoding.
    , encodingName :: !StringIndex
      -- | @DocumentType@ node's publicId.
    , publicId :: !StringIndex
      -- | @DocumentType@ node's systemId.
    , systemId :: !StringIndex
      -- | Frame ID for frame owner elements and also for the document node.
    , frameId :: !StringIndex
      -- | A table with dom nodes.
    , nodes :: !NodeTreeSnapshot
      -- | The nodes in the layout tree.
    , layout :: !LayoutTreeSnapshot
      -- | The post-layout inline text nodes.
    , textBoxes :: !TextBoxSnapshot
      -- | Scroll offsets.
    , scrollOffsetX :: !(P.Maybe P.Double)
    , scrollOffsetY :: !(P.Maybe P.Double)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DocumentSnapshot where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "DocumentSnapshot" $ \_o -> DocumentSnapshot
            <$> _o .: "documentURL"
            <*> _o .: "baseURL"
            <*> _o .: "contentLanguage"
            <*> _o .: "encodingName"
            <*> _o .: "publicId"
            <*> _o .: "systemId"
            <*> _o .: "frameId"
            <*> _o .: "nodes"
            <*> _o .: "layout"
            <*> _o .: "textBoxes"
            <*> _o .:? "scrollOffsetX"
            <*> _o .:? "scrollOffsetY"
        ago = A.withArray "DocumentSnapshot" $ \_a -> DocumentSnapshot
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.maybe P.empty A.parseJSON (_a !? 6)
            <*> P.maybe P.empty A.parseJSON (_a !? 7)
            <*> P.maybe P.empty A.parseJSON (_a !? 8)
            <*> P.maybe P.empty A.parseJSON (_a !? 9)
            <*> P.traverse A.parseJSON (_a !? 10)
            <*> P.traverse A.parseJSON (_a !? 11)


------------------------------------------------------------------------------
instance A.ToJSON DocumentSnapshot where
    toEncoding (DocumentSnapshot _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "documentURL" .= _0
        , P.pure $ "baseURL" .= _1
        , P.pure $ "contentLanguage" .= _2
        , P.pure $ "encodingName" .= _3
        , P.pure $ "publicId" .= _4
        , P.pure $ "systemId" .= _5
        , P.pure $ "frameId" .= _6
        , P.pure $ "nodes" .= _7
        , P.pure $ "layout" .= _8
        , P.pure $ "textBoxes" .= _9
        , ("scrollOffsetX" .=) <$> _10
        , ("scrollOffsetY" .=) <$> _11
        ]
    toJSON (DocumentSnapshot _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11) = A.object $ P.catMaybes
        [ P.pure $ "documentURL" .= _0
        , P.pure $ "baseURL" .= _1
        , P.pure $ "contentLanguage" .= _2
        , P.pure $ "encodingName" .= _3
        , P.pure $ "publicId" .= _4
        , P.pure $ "systemId" .= _5
        , P.pure $ "frameId" .= _6
        , P.pure $ "nodes" .= _7
        , P.pure $ "layout" .= _8
        , P.pure $ "textBoxes" .= _9
        , ("scrollOffsetX" .=) <$> _10
        , ("scrollOffsetY" .=) <$> _11
        ]


------------------------------------------------------------------------------
instance P.Semigroup DocumentSnapshot where
    DocumentSnapshot _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 <> DocumentSnapshot _ _ _ _ _ _ _ _ _ _ __10 __11 = DocumentSnapshot _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 (_10 <|> __10) (_11 <|> __11)


------------------------------------------------------------------------------
-- | Table containing nodes.
data NodeTreeSnapshot = NodeTreeSnapshot
    { -- | Parent node index.
      parentIndex :: !(P.Maybe [P.Int])
      -- | @Node@'s nodeType.
    , nodeType :: !(P.Maybe [P.Int])
      -- | @Node@'s nodeName.
    , nodeName :: !(P.Maybe [StringIndex])
      -- | @Node@'s nodeValue.
    , nodeValue :: !(P.Maybe [StringIndex])
      -- | @Node@'s id, corresponds to DOM.Node.backendNodeId.
    , backendNodeId :: !(P.Maybe [DOM.BackendNodeId])
      -- | Attributes of an @Element@ node. Flatten name, value pairs.
    , attributes :: !(P.Maybe [ArrayOfStrings])
      -- | Only set for textarea elements, contains the text value.
    , textValue :: !(P.Maybe RareStringData)
      -- | Only set for input elements, contains the input's associated text value.
    , inputValue :: !(P.Maybe RareStringData)
      -- | Only set for radio and checkbox input elements, indicates if the element has been checked
    , inputChecked :: !(P.Maybe RareBooleanData)
      -- | Only set for option elements, indicates if the element has been selected
    , optionSelected :: !(P.Maybe RareBooleanData)
      -- | The index of the document in the list of the snapshot documents.
    , contentDocumentIndex :: !(P.Maybe RareIntegerData)
      -- | Type of a pseudo element node.
    , pseudoType :: !(P.Maybe RareStringData)
      -- | Whether this DOM node responds to mouse clicks. This includes nodes that have had click
      -- event listeners attached via JavaScript as well as anchor tags that naturally navigate when
      -- clicked.
    , isClickable :: !(P.Maybe RareBooleanData)
      -- | The selected url for nodes with a srcset attribute.
    , currentSourceURL :: !(P.Maybe RareStringData)
      -- | The url of the script (if any) that generates this node.
    , originURL :: !(P.Maybe RareStringData)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON NodeTreeSnapshot where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "NodeTreeSnapshot" $ \_o -> NodeTreeSnapshot
            <$> _o .:? "parentIndex"
            <*> _o .:? "nodeType"
            <*> _o .:? "nodeName"
            <*> _o .:? "nodeValue"
            <*> _o .:? "backendNodeId"
            <*> _o .:? "attributes"
            <*> _o .:? "textValue"
            <*> _o .:? "inputValue"
            <*> _o .:? "inputChecked"
            <*> _o .:? "optionSelected"
            <*> _o .:? "contentDocumentIndex"
            <*> _o .:? "pseudoType"
            <*> _o .:? "isClickable"
            <*> _o .:? "currentSourceURL"
            <*> _o .:? "originURL"
        ago = A.withArray "NodeTreeSnapshot" $ \_a -> NodeTreeSnapshot
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)
            <*> P.traverse A.parseJSON (_a !? 7)
            <*> P.traverse A.parseJSON (_a !? 8)
            <*> P.traverse A.parseJSON (_a !? 9)
            <*> P.traverse A.parseJSON (_a !? 10)
            <*> P.traverse A.parseJSON (_a !? 11)
            <*> P.traverse A.parseJSON (_a !? 12)
            <*> P.traverse A.parseJSON (_a !? 13)
            <*> P.traverse A.parseJSON (_a !? 14)


------------------------------------------------------------------------------
instance A.ToJSON NodeTreeSnapshot where
    toEncoding (NodeTreeSnapshot _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14) = A.pairs $ P.fold $ P.catMaybes
        [ ("parentIndex" .=) <$> _0
        , ("nodeType" .=) <$> _1
        , ("nodeName" .=) <$> _2
        , ("nodeValue" .=) <$> _3
        , ("backendNodeId" .=) <$> _4
        , ("attributes" .=) <$> _5
        , ("textValue" .=) <$> _6
        , ("inputValue" .=) <$> _7
        , ("inputChecked" .=) <$> _8
        , ("optionSelected" .=) <$> _9
        , ("contentDocumentIndex" .=) <$> _10
        , ("pseudoType" .=) <$> _11
        , ("isClickable" .=) <$> _12
        , ("currentSourceURL" .=) <$> _13
        , ("originURL" .=) <$> _14
        ]
    toJSON (NodeTreeSnapshot _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14) = A.object $ P.catMaybes
        [ ("parentIndex" .=) <$> _0
        , ("nodeType" .=) <$> _1
        , ("nodeName" .=) <$> _2
        , ("nodeValue" .=) <$> _3
        , ("backendNodeId" .=) <$> _4
        , ("attributes" .=) <$> _5
        , ("textValue" .=) <$> _6
        , ("inputValue" .=) <$> _7
        , ("inputChecked" .=) <$> _8
        , ("optionSelected" .=) <$> _9
        , ("contentDocumentIndex" .=) <$> _10
        , ("pseudoType" .=) <$> _11
        , ("isClickable" .=) <$> _12
        , ("currentSourceURL" .=) <$> _13
        , ("originURL" .=) <$> _14
        ]


------------------------------------------------------------------------------
instance P.Semigroup NodeTreeSnapshot where
    NodeTreeSnapshot _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 <> NodeTreeSnapshot __0 __1 __2 __3 __4 __5 __6 __7 __8 __9 __10 __11 __12 __13 __14 = NodeTreeSnapshot (_0 <|> __0) (_1 <|> __1) (_2 <|> __2) (_3 <|> __3) (_4 <|> __4) (_5 <|> __5) (_6 <|> __6) (_7 <|> __7) (_8 <|> __8) (_9 <|> __9) (_10 <|> __10) (_11 <|> __11) (_12 <|> __12) (_13 <|> __13) (_14 <|> __14)


------------------------------------------------------------------------------
instance P.Monoid NodeTreeSnapshot where
    mempty = NodeTreeSnapshot P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Details of an element in the DOM tree with a LayoutObject.
data LayoutTreeSnapshot = LayoutTreeSnapshot
    { -- | The index of the related DOM node in the @domNodes@ array returned by @getSnapshot@.
      nodeIndex :: ![P.Int]
      -- | Index into the @computedStyles@ array returned by @captureSnapshot@.
    , styles :: ![ArrayOfStrings]
      -- | The absolute position bounding box.
    , bounds :: ![Rectangle]
      -- | Contents of the LayoutText, if any.
    , text :: ![StringIndex]
      -- | Stacking context information.
    , stackingContexts :: !RareBooleanData
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON LayoutTreeSnapshot where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "LayoutTreeSnapshot" $ \_o -> LayoutTreeSnapshot
            <$> _o .: "nodeIndex"
            <*> _o .: "styles"
            <*> _o .: "bounds"
            <*> _o .: "text"
            <*> _o .: "stackingContexts"
        ago = A.withArray "LayoutTreeSnapshot" $ \_a -> LayoutTreeSnapshot
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON LayoutTreeSnapshot where
    toEncoding (LayoutTreeSnapshot _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeIndex" .= _0
        , P.pure $ "styles" .= _1
        , P.pure $ "bounds" .= _2
        , P.pure $ "text" .= _3
        , P.pure $ "stackingContexts" .= _4
        ]
    toJSON (LayoutTreeSnapshot _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ P.pure $ "nodeIndex" .= _0
        , P.pure $ "styles" .= _1
        , P.pure $ "bounds" .= _2
        , P.pure $ "text" .= _3
        , P.pure $ "stackingContexts" .= _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup LayoutTreeSnapshot where
    LayoutTreeSnapshot _0 _1 _2 _3 _4 <> LayoutTreeSnapshot _ _ _ _ _ = LayoutTreeSnapshot _0 _1 _2 _3 _4


------------------------------------------------------------------------------
-- | Details of post layout rendered text positions. The exact layout should not be regarded as
-- stable and may change between versions.
data TextBoxSnapshot = TextBoxSnapshot
    { -- | Intex of th elayout tree node that owns this box collection.
      layoutIndex :: ![P.Int]
      -- | The absolute position bounding box.
    , bounds :: ![Rectangle]
      -- | The starting index in characters, for this post layout textbox substring. Characters that
      -- would be represented as a surrogate pair in UTF-16 have length 2.
    , start :: ![P.Int]
      -- | The number of characters in this post layout textbox substring. Characters that would be
      -- represented as a surrogate pair in UTF-16 have length 2.
    , length :: ![P.Int]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TextBoxSnapshot where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "TextBoxSnapshot" $ \_o -> TextBoxSnapshot
            <$> _o .: "layoutIndex"
            <*> _o .: "bounds"
            <*> _o .: "start"
            <*> _o .: "length"
        ago = A.withArray "TextBoxSnapshot" $ \_a -> TextBoxSnapshot
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON TextBoxSnapshot where
    toEncoding (TextBoxSnapshot _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "layoutIndex" .= _0
        , P.pure $ "bounds" .= _1
        , P.pure $ "start" .= _2
        , P.pure $ "length" .= _3
        ]
    toJSON (TextBoxSnapshot _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "layoutIndex" .= _0
        , P.pure $ "bounds" .= _1
        , P.pure $ "start" .= _2
        , P.pure $ "length" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup TextBoxSnapshot where
    TextBoxSnapshot _0 _1 _2 _3 <> TextBoxSnapshot _ _ _ _ = TextBoxSnapshot _0 _1 _2 _3

