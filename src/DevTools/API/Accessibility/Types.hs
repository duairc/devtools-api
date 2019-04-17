{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.Accessibility.Types{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

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


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Unique accessibility node identifier.
type AXNodeId = T.Text


------------------------------------------------------------------------------
-- | Enum of possible property types.
data AXValueType
    = Boolean
    | Tristate
    | BooleanOrUndefined
    | Idref
    | IdrefList
    | Integer
    | Node
    | NodeList
    | Number
    | String
    | ComputedString
    | Token
    | TokenList
    | DomRelation
    | Role
    | InternalRole
    | ValueUndefined
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AXValueType where
    parseJSON = A.withText "AXValueType" $ \t -> case t of
        "boolean" -> P.pure Boolean
        "tristate" -> P.pure Tristate
        "booleanOrUndefined" -> P.pure BooleanOrUndefined
        "idref" -> P.pure Idref
        "idrefList" -> P.pure IdrefList
        "integer" -> P.pure Integer
        "node" -> P.pure Node
        "nodeList" -> P.pure NodeList
        "number" -> P.pure Number
        "string" -> P.pure String
        "computedString" -> P.pure ComputedString
        "token" -> P.pure Token
        "tokenList" -> P.pure TokenList
        "domRelation" -> P.pure DomRelation
        "role" -> P.pure Role
        "internalRole" -> P.pure InternalRole
        "valueUndefined" -> P.pure ValueUndefined
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON AXValueType where
    toJSON Boolean = "boolean"
    toJSON Tristate = "tristate"
    toJSON BooleanOrUndefined = "booleanOrUndefined"
    toJSON Idref = "idref"
    toJSON IdrefList = "idrefList"
    toJSON Integer = "integer"
    toJSON Node = "node"
    toJSON NodeList = "nodeList"
    toJSON Number = "number"
    toJSON String = "string"
    toJSON ComputedString = "computedString"
    toJSON Token = "token"
    toJSON TokenList = "tokenList"
    toJSON DomRelation = "domRelation"
    toJSON Role = "role"
    toJSON InternalRole = "internalRole"
    toJSON ValueUndefined = "valueUndefined"


------------------------------------------------------------------------------
-- | Enum of possible property sources.
data AXValueSourceType
    = Attribute
    | Implicit
    | Style
    | Contents
    | Placeholder
    | RelatedElement
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AXValueSourceType where
    parseJSON = A.withText "AXValueSourceType" $ \t -> case t of
        "attribute" -> P.pure Attribute
        "implicit" -> P.pure Implicit
        "style" -> P.pure Style
        "contents" -> P.pure Contents
        "placeholder" -> P.pure Placeholder
        "relatedElement" -> P.pure RelatedElement
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON AXValueSourceType where
    toJSON Attribute = "attribute"
    toJSON Implicit = "implicit"
    toJSON Style = "style"
    toJSON Contents = "contents"
    toJSON Placeholder = "placeholder"
    toJSON RelatedElement = "relatedElement"


------------------------------------------------------------------------------
-- | Enum of possible native property sources (as a subtype of a particular AXValueSourceType).
data AXValueNativeSourceType
    = Figcaption
    | Label
    | Labelfor
    | Labelwrapped
    | Legend
    | Tablecaption
    | Title
    | Other
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AXValueNativeSourceType where
    parseJSON = A.withText "AXValueNativeSourceType" $ \t -> case t of
        "figcaption" -> P.pure Figcaption
        "label" -> P.pure Label
        "labelfor" -> P.pure Labelfor
        "labelwrapped" -> P.pure Labelwrapped
        "legend" -> P.pure Legend
        "tablecaption" -> P.pure Tablecaption
        "title" -> P.pure Title
        "other" -> P.pure Other
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON AXValueNativeSourceType where
    toJSON Figcaption = "figcaption"
    toJSON Label = "label"
    toJSON Labelfor = "labelfor"
    toJSON Labelwrapped = "labelwrapped"
    toJSON Legend = "legend"
    toJSON Tablecaption = "tablecaption"
    toJSON Title = "title"
    toJSON Other = "other"


------------------------------------------------------------------------------
-- | A single source for a computed AX property.
data AXValueSource = AXValueSource
    { -- | What type of source this is.
      type_ :: !AXValueSourceType
      -- | The value of this property source.
    , value :: !(P.Maybe AXValue)
      -- | The name of the relevant attribute, if any.
    , attribute :: !(P.Maybe T.Text)
      -- | The value of the relevant attribute, if any.
    , attributeValue :: !(P.Maybe AXValue)
      -- | Whether this source is superseded by a higher priority source.
    , superseded :: !(P.Maybe P.Bool)
      -- | The native markup source for this value, e.g. a <label> element.
    , nativeSource :: !(P.Maybe AXValueNativeSourceType)
      -- | The value, such as a node or node list, of the native source.
    , nativeSourceValue :: !(P.Maybe AXValue)
      -- | Whether the value for this property is invalid.
    , invalid :: !(P.Maybe P.Bool)
      -- | Reason for the value being invalid, if it is.
    , invalidReason :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AXValueSource where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "AXValueSource" $ \_o -> AXValueSource
            <$> _o .: "type"
            <*> _o .:? "value"
            <*> _o .:? "attribute"
            <*> _o .:? "attributeValue"
            <*> _o .:? "superseded"
            <*> _o .:? "nativeSource"
            <*> _o .:? "nativeSourceValue"
            <*> _o .:? "invalid"
            <*> _o .:? "invalidReason"
        ago = A.withArray "AXValueSource" $ \_a -> AXValueSource
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)
            <*> P.traverse A.parseJSON (_a !? 7)
            <*> P.traverse A.parseJSON (_a !? 8)


------------------------------------------------------------------------------
instance A.ToJSON AXValueSource where
    toEncoding (AXValueSource _0 _1 _2 _3 _4 _5 _6 _7 _8) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "type" .= _0
        , ("value" .=) <$> _1
        , ("attribute" .=) <$> _2
        , ("attributeValue" .=) <$> _3
        , ("superseded" .=) <$> _4
        , ("nativeSource" .=) <$> _5
        , ("nativeSourceValue" .=) <$> _6
        , ("invalid" .=) <$> _7
        , ("invalidReason" .=) <$> _8
        ]
    toJSON (AXValueSource _0 _1 _2 _3 _4 _5 _6 _7 _8) = A.object $ P.catMaybes
        [ P.pure $ "type" .= _0
        , ("value" .=) <$> _1
        , ("attribute" .=) <$> _2
        , ("attributeValue" .=) <$> _3
        , ("superseded" .=) <$> _4
        , ("nativeSource" .=) <$> _5
        , ("nativeSourceValue" .=) <$> _6
        , ("invalid" .=) <$> _7
        , ("invalidReason" .=) <$> _8
        ]


------------------------------------------------------------------------------
instance P.Semigroup AXValueSource where
    AXValueSource _0 _1 _2 _3 _4 _5 _6 _7 _8 <> AXValueSource _ __1 __2 __3 __4 __5 __6 __7 __8 = AXValueSource _0 (_1 <|> __1) (_2 <|> __2) (_3 <|> __3) (_4 <|> __4) (_5 <|> __5) (_6 <|> __6) (_7 <|> __7) (_8 <|> __8)


------------------------------------------------------------------------------
data AXRelatedNode = AXRelatedNode
    { -- | The BackendNodeId of the related DOM node.
      backendDOMNodeId :: !DOM.BackendNodeId
      -- | The IDRef value provided, if any.
    , idref :: !(P.Maybe T.Text)
      -- | The text alternative of this node in the current context.
    , text :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AXRelatedNode where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "AXRelatedNode" $ \_o -> AXRelatedNode
            <$> _o .: "backendDOMNodeId"
            <*> _o .:? "idref"
            <*> _o .:? "text"
        ago = A.withArray "AXRelatedNode" $ \_a -> AXRelatedNode
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON AXRelatedNode where
    toEncoding (AXRelatedNode _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "backendDOMNodeId" .= _0
        , ("idref" .=) <$> _1
        , ("text" .=) <$> _2
        ]
    toJSON (AXRelatedNode _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "backendDOMNodeId" .= _0
        , ("idref" .=) <$> _1
        , ("text" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup AXRelatedNode where
    AXRelatedNode _0 _1 _2 <> AXRelatedNode _ __1 __2 = AXRelatedNode _0 (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
data AXProperty = AXProperty
    { -- | The name of this property.
      name :: !AXPropertyName
      -- | The value of this property.
    , value :: !AXValue
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AXProperty where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "AXProperty" $ \_o -> AXProperty
            <$> _o .: "name"
            <*> _o .: "value"
        ago = A.withArray "AXProperty" $ \_a -> AXProperty
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON AXProperty where
    toEncoding (AXProperty _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "value" .= _1
        ]
    toJSON (AXProperty _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "value" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup AXProperty where
    AXProperty _0 _1 <> AXProperty _ _ = AXProperty _0 _1


------------------------------------------------------------------------------
-- | A single computed AX property.
data AXValue = AXValue
    { -- | The type of this value.
      type_ :: !AXValueType
      -- | The computed value of this property.
    , value :: !(P.Maybe A.Value)
      -- | One or more related nodes, if applicable.
    , relatedNodes :: !(P.Maybe [AXRelatedNode])
      -- | The sources which contributed to the computation of this property.
    , sources :: !(P.Maybe [AXValueSource])
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AXValue where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "AXValue" $ \_o -> AXValue
            <$> _o .: "type"
            <*> _o .:? "value"
            <*> _o .:? "relatedNodes"
            <*> _o .:? "sources"
        ago = A.withArray "AXValue" $ \_a -> AXValue
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON AXValue where
    toEncoding (AXValue _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "type" .= _0
        , ("value" .=) <$> _1
        , ("relatedNodes" .=) <$> _2
        , ("sources" .=) <$> _3
        ]
    toJSON (AXValue _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "type" .= _0
        , ("value" .=) <$> _1
        , ("relatedNodes" .=) <$> _2
        , ("sources" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup AXValue where
    AXValue _0 _1 _2 _3 <> AXValue _ __1 __2 __3 = AXValue _0 (_1 <|> __1) (_2 <|> __2) (_3 <|> __3)


------------------------------------------------------------------------------
-- | Values of AXProperty name:
-- - from 'busy' to 'roledescription': states which apply to every AX node
-- - from 'live' to 'root': attributes which apply to nodes in live regions
-- - from 'autocomplete' to 'valuetext': attributes which apply to widgets
-- - from 'checked' to 'selected': states which apply to widgets
-- - from 'activedescendant' to 'owns' - relationships between elements other than parent\/child\/sibling.
data AXPropertyName
    = Busy
    | Disabled
    | Editable
    | Focusable
    | Focused
    | Hidden
    | HiddenRoot
    | Invalid
    | Keyshortcuts
    | Settable
    | Roledescription
    | Live
    | Atomic
    | Relevant
    | Root
    | Autocomplete
    | HasPopup
    | Level
    | Multiselectable
    | Orientation
    | Multiline
    | Readonly
    | Required
    | Valuemin
    | Valuemax
    | Valuetext
    | Checked
    | Expanded
    | Modal
    | Pressed
    | Selected
    | Activedescendant
    | Controls
    | Describedby
    | Details
    | Errormessage
    | Flowto
    | Labelledby
    | Owns
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AXPropertyName where
    parseJSON = A.withText "AXPropertyName" $ \t -> case t of
        "busy" -> P.pure Busy
        "disabled" -> P.pure Disabled
        "editable" -> P.pure Editable
        "focusable" -> P.pure Focusable
        "focused" -> P.pure Focused
        "hidden" -> P.pure Hidden
        "hiddenRoot" -> P.pure HiddenRoot
        "invalid" -> P.pure Invalid
        "keyshortcuts" -> P.pure Keyshortcuts
        "settable" -> P.pure Settable
        "roledescription" -> P.pure Roledescription
        "live" -> P.pure Live
        "atomic" -> P.pure Atomic
        "relevant" -> P.pure Relevant
        "root" -> P.pure Root
        "autocomplete" -> P.pure Autocomplete
        "hasPopup" -> P.pure HasPopup
        "level" -> P.pure Level
        "multiselectable" -> P.pure Multiselectable
        "orientation" -> P.pure Orientation
        "multiline" -> P.pure Multiline
        "readonly" -> P.pure Readonly
        "required" -> P.pure Required
        "valuemin" -> P.pure Valuemin
        "valuemax" -> P.pure Valuemax
        "valuetext" -> P.pure Valuetext
        "checked" -> P.pure Checked
        "expanded" -> P.pure Expanded
        "modal" -> P.pure Modal
        "pressed" -> P.pure Pressed
        "selected" -> P.pure Selected
        "activedescendant" -> P.pure Activedescendant
        "controls" -> P.pure Controls
        "describedby" -> P.pure Describedby
        "details" -> P.pure Details
        "errormessage" -> P.pure Errormessage
        "flowto" -> P.pure Flowto
        "labelledby" -> P.pure Labelledby
        "owns" -> P.pure Owns
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON AXPropertyName where
    toJSON Busy = "busy"
    toJSON Disabled = "disabled"
    toJSON Editable = "editable"
    toJSON Focusable = "focusable"
    toJSON Focused = "focused"
    toJSON Hidden = "hidden"
    toJSON HiddenRoot = "hiddenRoot"
    toJSON Invalid = "invalid"
    toJSON Keyshortcuts = "keyshortcuts"
    toJSON Settable = "settable"
    toJSON Roledescription = "roledescription"
    toJSON Live = "live"
    toJSON Atomic = "atomic"
    toJSON Relevant = "relevant"
    toJSON Root = "root"
    toJSON Autocomplete = "autocomplete"
    toJSON HasPopup = "hasPopup"
    toJSON Level = "level"
    toJSON Multiselectable = "multiselectable"
    toJSON Orientation = "orientation"
    toJSON Multiline = "multiline"
    toJSON Readonly = "readonly"
    toJSON Required = "required"
    toJSON Valuemin = "valuemin"
    toJSON Valuemax = "valuemax"
    toJSON Valuetext = "valuetext"
    toJSON Checked = "checked"
    toJSON Expanded = "expanded"
    toJSON Modal = "modal"
    toJSON Pressed = "pressed"
    toJSON Selected = "selected"
    toJSON Activedescendant = "activedescendant"
    toJSON Controls = "controls"
    toJSON Describedby = "describedby"
    toJSON Details = "details"
    toJSON Errormessage = "errormessage"
    toJSON Flowto = "flowto"
    toJSON Labelledby = "labelledby"
    toJSON Owns = "owns"


------------------------------------------------------------------------------
-- | A node in the accessibility tree.
data AXNode = AXNode
    { -- | Unique identifier for this node.
      nodeId :: !AXNodeId
      -- | Whether this node is ignored for accessibility
    , ignored :: !P.Bool
      -- | Collection of reasons why this node is hidden.
    , ignoredReasons :: !(P.Maybe [AXProperty])
      -- | This @Node@'s role, whether explicit or implicit.
    , role :: !(P.Maybe AXValue)
      -- | The accessible name for this @Node@.
    , name :: !(P.Maybe AXValue)
      -- | The accessible description for this @Node@.
    , description :: !(P.Maybe AXValue)
      -- | The value for this @Node@.
    , value :: !(P.Maybe AXValue)
      -- | All other properties
    , properties :: !(P.Maybe [AXProperty])
      -- | IDs for each of this node's child nodes.
    , childIds :: !(P.Maybe [AXNodeId])
      -- | The backend ID for the associated DOM node, if any.
    , backendDOMNodeId :: !(P.Maybe DOM.BackendNodeId)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AXNode where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "AXNode" $ \_o -> AXNode
            <$> _o .: "nodeId"
            <*> _o .: "ignored"
            <*> _o .:? "ignoredReasons"
            <*> _o .:? "role"
            <*> _o .:? "name"
            <*> _o .:? "description"
            <*> _o .:? "value"
            <*> _o .:? "properties"
            <*> _o .:? "childIds"
            <*> _o .:? "backendDOMNodeId"
        ago = A.withArray "AXNode" $ \_a -> AXNode
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)
            <*> P.traverse A.parseJSON (_a !? 7)
            <*> P.traverse A.parseJSON (_a !? 8)
            <*> P.traverse A.parseJSON (_a !? 9)


------------------------------------------------------------------------------
instance A.ToJSON AXNode where
    toEncoding (AXNode _0 _1 _2 _3 _4 _5 _6 _7 _8 _9) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "ignored" .= _1
        , ("ignoredReasons" .=) <$> _2
        , ("role" .=) <$> _3
        , ("name" .=) <$> _4
        , ("description" .=) <$> _5
        , ("value" .=) <$> _6
        , ("properties" .=) <$> _7
        , ("childIds" .=) <$> _8
        , ("backendDOMNodeId" .=) <$> _9
        ]
    toJSON (AXNode _0 _1 _2 _3 _4 _5 _6 _7 _8 _9) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "ignored" .= _1
        , ("ignoredReasons" .=) <$> _2
        , ("role" .=) <$> _3
        , ("name" .=) <$> _4
        , ("description" .=) <$> _5
        , ("value" .=) <$> _6
        , ("properties" .=) <$> _7
        , ("childIds" .=) <$> _8
        , ("backendDOMNodeId" .=) <$> _9
        ]


------------------------------------------------------------------------------
instance P.Semigroup AXNode where
    AXNode _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 <> AXNode _ _ __2 __3 __4 __5 __6 __7 __8 __9 = AXNode _0 _1 (_2 <|> __2) (_3 <|> __3) (_4 <|> __4) (_5 <|> __5) (_6 <|> __6) (_7 <|> __7) (_8 <|> __8) (_9 <|> __9)

