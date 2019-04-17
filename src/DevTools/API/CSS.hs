{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | This domain exposes CSS read\/write operations. All CSS objects (stylesheets, rules, and styles)
-- have an associated @id@ used in subsequent operations on the related object. Each object type has
-- a specific @id@ structure, and those are not interchangeable between objects of different kinds.
-- CSS objects can be loaded using the @get*ForNode()@ calls (which accept a DOM node id). A client
-- can also keep track of stylesheets via the @styleSheetAdded@\/@styleSheetRemoved@ events and
-- subsequently load the required stylesheet contents using the @getStyleSheet[Text]()@ methods.
module DevTools.API.CSS{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

    ( module DevTools.API.CSS.Types
    , module DevTools.API.CSS
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
import           DevTools.API.CSS.Types
import qualified DevTools.API.DOM.Types as DOM
import qualified DevTools.API.Page.Types as Page


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Inserts a new rule with the given @ruleText@ in a stylesheet with given @styleSheetId@, at the
-- position specified by @location@.
data AddRule = AddRule
    { -- | The css style sheet identifier where a new rule should be inserted.
      styleSheetId :: !StyleSheetId
      -- | The text of a new rule.
    , ruleText :: !T.Text
      -- | Text position of a new rule in the target style sheet.
    , location :: !SourceRange
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AddRule where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "addRule" $ \_o -> AddRule
            <$> _o .: "styleSheetId"
            <*> _o .: "ruleText"
            <*> _o .: "location"
        ago = A.withArray "addRule" $ \_a -> AddRule
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON AddRule where
    toEncoding (AddRule _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "styleSheetId" .= _0
        , P.pure $ "ruleText" .= _1
        , P.pure $ "location" .= _2
        ]
    toJSON (AddRule _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "styleSheetId" .= _0
        , P.pure $ "ruleText" .= _1
        , P.pure $ "location" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup AddRule where
    AddRule _0 _1 _2 <> AddRule _ _ _ = AddRule _0 _1 _2


------------------------------------------------------------------------------
-- | Inserts a new rule with the given @ruleText@ in a stylesheet with given @styleSheetId@, at the
-- position specified by @location@.
data AddRuleResult = AddRuleResult
    { -- | The newly created rule.
      rule :: !CSSRule
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AddRuleResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "addRuleResult" $ \_o -> AddRuleResult
            <$> _o .: "rule"
        ago = A.withArray "addRuleResult" $ \_a -> AddRuleResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON AddRuleResult where
    toEncoding (AddRuleResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "rule" .= _0
        ]
    toJSON (AddRuleResult _0) = A.object $ P.catMaybes
        [ P.pure $ "rule" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup AddRuleResult where
    AddRuleResult _0 <> AddRuleResult _ = AddRuleResult _0


------------------------------------------------------------------------------
instance M.Method AddRule where
    type Result AddRule = AddRuleResult
    name _ = "CSS.addRule"


------------------------------------------------------------------------------
-- | Inserts a new rule with the given @ruleText@ in a stylesheet with given @styleSheetId@, at the
-- position specified by @location@.
addRule
    :: StyleSheetId
    -- ^ The css style sheet identifier where a new rule should be inserted.

    -> T.Text
    -- ^ The text of a new rule.

    -> SourceRange
    -- ^ Text position of a new rule in the target style sheet.

    -> AddRule
addRule _0 _1 _2 = AddRule _0 _1 _2


------------------------------------------------------------------------------
-- | Returns all class names from specified stylesheet.
data CollectClassNames = CollectClassNames
    { styleSheetId :: !StyleSheetId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CollectClassNames where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "collectClassNames" $ \_o -> CollectClassNames
            <$> _o .: "styleSheetId"
        ago = A.withArray "collectClassNames" $ \_a -> CollectClassNames
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON CollectClassNames where
    toEncoding (CollectClassNames _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "styleSheetId" .= _0
        ]
    toJSON (CollectClassNames _0) = A.object $ P.catMaybes
        [ P.pure $ "styleSheetId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup CollectClassNames where
    CollectClassNames _0 <> CollectClassNames _ = CollectClassNames _0


------------------------------------------------------------------------------
-- | Returns all class names from specified stylesheet.
data CollectClassNamesResult = CollectClassNamesResult
    { -- | Class name list.
      classNames :: ![T.Text]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CollectClassNamesResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "collectClassNamesResult" $ \_o -> CollectClassNamesResult
            <$> _o .: "classNames"
        ago = A.withArray "collectClassNamesResult" $ \_a -> CollectClassNamesResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON CollectClassNamesResult where
    toEncoding (CollectClassNamesResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "classNames" .= _0
        ]
    toJSON (CollectClassNamesResult _0) = A.object $ P.catMaybes
        [ P.pure $ "classNames" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup CollectClassNamesResult where
    CollectClassNamesResult _0 <> CollectClassNamesResult _ = CollectClassNamesResult _0


------------------------------------------------------------------------------
instance M.Method CollectClassNames where
    type Result CollectClassNames = CollectClassNamesResult
    name _ = "CSS.collectClassNames"


------------------------------------------------------------------------------
-- | Returns all class names from specified stylesheet.
collectClassNames
    :: StyleSheetId
    -> CollectClassNames
collectClassNames _0 = CollectClassNames _0


------------------------------------------------------------------------------
-- | Creates a new special "via-inspector" stylesheet in the frame with given @frameId@.
data CreateStyleSheet = CreateStyleSheet
    { -- | Identifier of the frame where "via-inspector" stylesheet should be created.
      frameId :: !Page.FrameId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CreateStyleSheet where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "createStyleSheet" $ \_o -> CreateStyleSheet
            <$> _o .: "frameId"
        ago = A.withArray "createStyleSheet" $ \_a -> CreateStyleSheet
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON CreateStyleSheet where
    toEncoding (CreateStyleSheet _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        ]
    toJSON (CreateStyleSheet _0) = A.object $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup CreateStyleSheet where
    CreateStyleSheet _0 <> CreateStyleSheet _ = CreateStyleSheet _0


------------------------------------------------------------------------------
-- | Creates a new special "via-inspector" stylesheet in the frame with given @frameId@.
data CreateStyleSheetResult = CreateStyleSheetResult
    { -- | Identifier of the created "via-inspector" stylesheet.
      styleSheetId :: !StyleSheetId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CreateStyleSheetResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "createStyleSheetResult" $ \_o -> CreateStyleSheetResult
            <$> _o .: "styleSheetId"
        ago = A.withArray "createStyleSheetResult" $ \_a -> CreateStyleSheetResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON CreateStyleSheetResult where
    toEncoding (CreateStyleSheetResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "styleSheetId" .= _0
        ]
    toJSON (CreateStyleSheetResult _0) = A.object $ P.catMaybes
        [ P.pure $ "styleSheetId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup CreateStyleSheetResult where
    CreateStyleSheetResult _0 <> CreateStyleSheetResult _ = CreateStyleSheetResult _0


------------------------------------------------------------------------------
instance M.Method CreateStyleSheet where
    type Result CreateStyleSheet = CreateStyleSheetResult
    name _ = "CSS.createStyleSheet"


------------------------------------------------------------------------------
-- | Creates a new special "via-inspector" stylesheet in the frame with given @frameId@.
createStyleSheet
    :: Page.FrameId
    -- ^ Identifier of the frame where "via-inspector" stylesheet should be created.

    -> CreateStyleSheet
createStyleSheet _0 = CreateStyleSheet _0


------------------------------------------------------------------------------
-- | Disables the CSS agent for the given page.
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
    name _ = "CSS.disable"


------------------------------------------------------------------------------
-- | Disables the CSS agent for the given page.
disable
    :: Disable
disable = Disable


------------------------------------------------------------------------------
-- | Enables the CSS agent for the given page. Clients should not assume that the CSS agent has been
-- enabled until the result of this command is received.
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
    name _ = "CSS.enable"


------------------------------------------------------------------------------
-- | Enables the CSS agent for the given page. Clients should not assume that the CSS agent has been
-- enabled until the result of this command is received.
enable
    :: Enable
enable = Enable


------------------------------------------------------------------------------
-- | Ensures that the given node will have specified pseudo-classes whenever its style is computed by
-- the browser.
data ForcePseudoState = ForcePseudoState
    { -- | The element id for which to force the pseudo state.
      nodeId :: !DOM.NodeId
      -- | Element pseudo classes to force when computing the element's style.
    , forcedPseudoClasses :: ![T.Text]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ForcePseudoState where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "forcePseudoState" $ \_o -> ForcePseudoState
            <$> _o .: "nodeId"
            <*> _o .: "forcedPseudoClasses"
        ago = A.withArray "forcePseudoState" $ \_a -> ForcePseudoState
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON ForcePseudoState where
    toEncoding (ForcePseudoState _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "forcedPseudoClasses" .= _1
        ]
    toJSON (ForcePseudoState _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "forcedPseudoClasses" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup ForcePseudoState where
    ForcePseudoState _0 _1 <> ForcePseudoState _ _ = ForcePseudoState _0 _1


------------------------------------------------------------------------------
instance M.Method ForcePseudoState where
    type Result ForcePseudoState = ()
    name _ = "CSS.forcePseudoState"


------------------------------------------------------------------------------
-- | Ensures that the given node will have specified pseudo-classes whenever its style is computed by
-- the browser.
forcePseudoState
    :: DOM.NodeId
    -- ^ The element id for which to force the pseudo state.

    -> [T.Text]
    -- ^ Element pseudo classes to force when computing the element's style.

    -> ForcePseudoState
forcePseudoState _0 _1 = ForcePseudoState _0 _1


------------------------------------------------------------------------------
data GetBackgroundColors = GetBackgroundColors
    { -- | Id of the node to get background colors for.
      nodeId :: !DOM.NodeId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetBackgroundColors where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getBackgroundColors" $ \_o -> GetBackgroundColors
            <$> _o .: "nodeId"
        ago = A.withArray "getBackgroundColors" $ \_a -> GetBackgroundColors
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetBackgroundColors where
    toEncoding (GetBackgroundColors _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]
    toJSON (GetBackgroundColors _0) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetBackgroundColors where
    GetBackgroundColors _0 <> GetBackgroundColors _ = GetBackgroundColors _0


------------------------------------------------------------------------------
data GetBackgroundColorsResult = GetBackgroundColorsResult
    { -- | The range of background colors behind this element, if it contains any visible text. If no
      -- visible text is present, this will be undefined. In the case of a flat background color,
      -- this will consist of simply that color. In the case of a gradient, this will consist of each
      -- of the color stops. For anything more complicated, this will be an empty array. Images will
      -- be ignored (as if the image had failed to load).
      backgroundColors :: !(P.Maybe [T.Text])
      -- | The computed font size for this node, as a CSS computed value string (e.g. '12px').
    , computedFontSize :: !(P.Maybe T.Text)
      -- | The computed font weight for this node, as a CSS computed value string (e.g. 'normal' or
      -- '100').
    , computedFontWeight :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetBackgroundColorsResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getBackgroundColorsResult" $ \_o -> GetBackgroundColorsResult
            <$> _o .:? "backgroundColors"
            <*> _o .:? "computedFontSize"
            <*> _o .:? "computedFontWeight"
        ago = A.withArray "getBackgroundColorsResult" $ \_a -> GetBackgroundColorsResult
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON GetBackgroundColorsResult where
    toEncoding (GetBackgroundColorsResult _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ ("backgroundColors" .=) <$> _0
        , ("computedFontSize" .=) <$> _1
        , ("computedFontWeight" .=) <$> _2
        ]
    toJSON (GetBackgroundColorsResult _0 _1 _2) = A.object $ P.catMaybes
        [ ("backgroundColors" .=) <$> _0
        , ("computedFontSize" .=) <$> _1
        , ("computedFontWeight" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetBackgroundColorsResult where
    GetBackgroundColorsResult _0 _1 _2 <> GetBackgroundColorsResult __0 __1 __2 = GetBackgroundColorsResult (_0 <|> __0) (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
instance P.Monoid GetBackgroundColorsResult where
    mempty = GetBackgroundColorsResult P.empty P.empty P.empty


------------------------------------------------------------------------------
instance M.Method GetBackgroundColors where
    type Result GetBackgroundColors = GetBackgroundColorsResult
    name _ = "CSS.getBackgroundColors"


------------------------------------------------------------------------------
getBackgroundColors
    :: DOM.NodeId
    -- ^ Id of the node to get background colors for.

    -> GetBackgroundColors
getBackgroundColors _0 = GetBackgroundColors _0


------------------------------------------------------------------------------
-- | Returns the computed style for a DOM node identified by @nodeId@.
data GetComputedStyleForNode = GetComputedStyleForNode
    { nodeId :: !DOM.NodeId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetComputedStyleForNode where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getComputedStyleForNode" $ \_o -> GetComputedStyleForNode
            <$> _o .: "nodeId"
        ago = A.withArray "getComputedStyleForNode" $ \_a -> GetComputedStyleForNode
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetComputedStyleForNode where
    toEncoding (GetComputedStyleForNode _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]
    toJSON (GetComputedStyleForNode _0) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetComputedStyleForNode where
    GetComputedStyleForNode _0 <> GetComputedStyleForNode _ = GetComputedStyleForNode _0


------------------------------------------------------------------------------
-- | Returns the computed style for a DOM node identified by @nodeId@.
data GetComputedStyleForNodeResult = GetComputedStyleForNodeResult
    { -- | Computed style for the specified DOM node.
      computedStyle :: ![CSSComputedStyleProperty]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetComputedStyleForNodeResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getComputedStyleForNodeResult" $ \_o -> GetComputedStyleForNodeResult
            <$> _o .: "computedStyle"
        ago = A.withArray "getComputedStyleForNodeResult" $ \_a -> GetComputedStyleForNodeResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetComputedStyleForNodeResult where
    toEncoding (GetComputedStyleForNodeResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "computedStyle" .= _0
        ]
    toJSON (GetComputedStyleForNodeResult _0) = A.object $ P.catMaybes
        [ P.pure $ "computedStyle" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetComputedStyleForNodeResult where
    GetComputedStyleForNodeResult _0 <> GetComputedStyleForNodeResult _ = GetComputedStyleForNodeResult _0


------------------------------------------------------------------------------
instance M.Method GetComputedStyleForNode where
    type Result GetComputedStyleForNode = GetComputedStyleForNodeResult
    name _ = "CSS.getComputedStyleForNode"


------------------------------------------------------------------------------
-- | Returns the computed style for a DOM node identified by @nodeId@.
getComputedStyleForNode
    :: DOM.NodeId
    -> GetComputedStyleForNode
getComputedStyleForNode _0 = GetComputedStyleForNode _0


------------------------------------------------------------------------------
-- | Returns the styles defined inline (explicitly in the "style" attribute and implicitly, using DOM
-- attributes) for a DOM node identified by @nodeId@.
data GetInlineStylesForNode = GetInlineStylesForNode
    { nodeId :: !DOM.NodeId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetInlineStylesForNode where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getInlineStylesForNode" $ \_o -> GetInlineStylesForNode
            <$> _o .: "nodeId"
        ago = A.withArray "getInlineStylesForNode" $ \_a -> GetInlineStylesForNode
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetInlineStylesForNode where
    toEncoding (GetInlineStylesForNode _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]
    toJSON (GetInlineStylesForNode _0) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetInlineStylesForNode where
    GetInlineStylesForNode _0 <> GetInlineStylesForNode _ = GetInlineStylesForNode _0


------------------------------------------------------------------------------
-- | Returns the styles defined inline (explicitly in the "style" attribute and implicitly, using DOM
-- attributes) for a DOM node identified by @nodeId@.
data GetInlineStylesForNodeResult = GetInlineStylesForNodeResult
    { -- | Inline style for the specified DOM node.
      inlineStyle :: !(P.Maybe CSSStyle)
      -- | Attribute-defined element style (e.g. resulting from "width=20 height=100%").
    , attributesStyle :: !(P.Maybe CSSStyle)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetInlineStylesForNodeResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getInlineStylesForNodeResult" $ \_o -> GetInlineStylesForNodeResult
            <$> _o .:? "inlineStyle"
            <*> _o .:? "attributesStyle"
        ago = A.withArray "getInlineStylesForNodeResult" $ \_a -> GetInlineStylesForNodeResult
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON GetInlineStylesForNodeResult where
    toEncoding (GetInlineStylesForNodeResult _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ ("inlineStyle" .=) <$> _0
        , ("attributesStyle" .=) <$> _1
        ]
    toJSON (GetInlineStylesForNodeResult _0 _1) = A.object $ P.catMaybes
        [ ("inlineStyle" .=) <$> _0
        , ("attributesStyle" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetInlineStylesForNodeResult where
    GetInlineStylesForNodeResult _0 _1 <> GetInlineStylesForNodeResult __0 __1 = GetInlineStylesForNodeResult (_0 <|> __0) (_1 <|> __1)


------------------------------------------------------------------------------
instance P.Monoid GetInlineStylesForNodeResult where
    mempty = GetInlineStylesForNodeResult P.empty P.empty


------------------------------------------------------------------------------
instance M.Method GetInlineStylesForNode where
    type Result GetInlineStylesForNode = GetInlineStylesForNodeResult
    name _ = "CSS.getInlineStylesForNode"


------------------------------------------------------------------------------
-- | Returns the styles defined inline (explicitly in the "style" attribute and implicitly, using DOM
-- attributes) for a DOM node identified by @nodeId@.
getInlineStylesForNode
    :: DOM.NodeId
    -> GetInlineStylesForNode
getInlineStylesForNode _0 = GetInlineStylesForNode _0


------------------------------------------------------------------------------
-- | Returns requested styles for a DOM node identified by @nodeId@.
data GetMatchedStylesForNode = GetMatchedStylesForNode
    { nodeId :: !DOM.NodeId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetMatchedStylesForNode where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getMatchedStylesForNode" $ \_o -> GetMatchedStylesForNode
            <$> _o .: "nodeId"
        ago = A.withArray "getMatchedStylesForNode" $ \_a -> GetMatchedStylesForNode
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetMatchedStylesForNode where
    toEncoding (GetMatchedStylesForNode _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]
    toJSON (GetMatchedStylesForNode _0) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetMatchedStylesForNode where
    GetMatchedStylesForNode _0 <> GetMatchedStylesForNode _ = GetMatchedStylesForNode _0


------------------------------------------------------------------------------
-- | Returns requested styles for a DOM node identified by @nodeId@.
data GetMatchedStylesForNodeResult = GetMatchedStylesForNodeResult
    { -- | Inline style for the specified DOM node.
      inlineStyle :: !(P.Maybe CSSStyle)
      -- | Attribute-defined element style (e.g. resulting from "width=20 height=100%").
    , attributesStyle :: !(P.Maybe CSSStyle)
      -- | CSS rules matching this node, from all applicable stylesheets.
    , matchedCSSRules :: !(P.Maybe [RuleMatch])
      -- | Pseudo style matches for this node.
    , pseudoElements :: !(P.Maybe [PseudoElementMatches])
      -- | A chain of inherited styles (from the immediate node parent up to the DOM tree root).
    , inherited :: !(P.Maybe [InheritedStyleEntry])
      -- | A list of CSS keyframed animations matching this node.
    , cssKeyframesRules :: !(P.Maybe [CSSKeyframesRule])
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetMatchedStylesForNodeResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getMatchedStylesForNodeResult" $ \_o -> GetMatchedStylesForNodeResult
            <$> _o .:? "inlineStyle"
            <*> _o .:? "attributesStyle"
            <*> _o .:? "matchedCSSRules"
            <*> _o .:? "pseudoElements"
            <*> _o .:? "inherited"
            <*> _o .:? "cssKeyframesRules"
        ago = A.withArray "getMatchedStylesForNodeResult" $ \_a -> GetMatchedStylesForNodeResult
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)


------------------------------------------------------------------------------
instance A.ToJSON GetMatchedStylesForNodeResult where
    toEncoding (GetMatchedStylesForNodeResult _0 _1 _2 _3 _4 _5) = A.pairs $ P.fold $ P.catMaybes
        [ ("inlineStyle" .=) <$> _0
        , ("attributesStyle" .=) <$> _1
        , ("matchedCSSRules" .=) <$> _2
        , ("pseudoElements" .=) <$> _3
        , ("inherited" .=) <$> _4
        , ("cssKeyframesRules" .=) <$> _5
        ]
    toJSON (GetMatchedStylesForNodeResult _0 _1 _2 _3 _4 _5) = A.object $ P.catMaybes
        [ ("inlineStyle" .=) <$> _0
        , ("attributesStyle" .=) <$> _1
        , ("matchedCSSRules" .=) <$> _2
        , ("pseudoElements" .=) <$> _3
        , ("inherited" .=) <$> _4
        , ("cssKeyframesRules" .=) <$> _5
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetMatchedStylesForNodeResult where
    GetMatchedStylesForNodeResult _0 _1 _2 _3 _4 _5 <> GetMatchedStylesForNodeResult __0 __1 __2 __3 __4 __5 = GetMatchedStylesForNodeResult (_0 <|> __0) (_1 <|> __1) (_2 <|> __2) (_3 <|> __3) (_4 <|> __4) (_5 <|> __5)


------------------------------------------------------------------------------
instance P.Monoid GetMatchedStylesForNodeResult where
    mempty = GetMatchedStylesForNodeResult P.empty P.empty P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
instance M.Method GetMatchedStylesForNode where
    type Result GetMatchedStylesForNode = GetMatchedStylesForNodeResult
    name _ = "CSS.getMatchedStylesForNode"


------------------------------------------------------------------------------
-- | Returns requested styles for a DOM node identified by @nodeId@.
getMatchedStylesForNode
    :: DOM.NodeId
    -> GetMatchedStylesForNode
getMatchedStylesForNode _0 = GetMatchedStylesForNode _0


------------------------------------------------------------------------------
-- | Returns all media queries parsed by the rendering engine.
data GetMediaQueries = GetMediaQueries
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetMediaQueries where
    parseJSON A.Null = P.pure GetMediaQueries
    parseJSON v = A.withArray "getMediaQueries" go v
        <|> A.withObject "getMediaQueries" go v
      where
        go _ = P.pure GetMediaQueries


------------------------------------------------------------------------------
instance A.ToJSON GetMediaQueries where
    toEncoding GetMediaQueries = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetMediaQueries = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetMediaQueries where
    GetMediaQueries <> GetMediaQueries = GetMediaQueries


------------------------------------------------------------------------------
instance P.Monoid GetMediaQueries where
    mempty = GetMediaQueries


------------------------------------------------------------------------------
-- | Returns all media queries parsed by the rendering engine.
data GetMediaQueriesResult = GetMediaQueriesResult
    { medias :: ![CSSMedia]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetMediaQueriesResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getMediaQueriesResult" $ \_o -> GetMediaQueriesResult
            <$> _o .: "medias"
        ago = A.withArray "getMediaQueriesResult" $ \_a -> GetMediaQueriesResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetMediaQueriesResult where
    toEncoding (GetMediaQueriesResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "medias" .= _0
        ]
    toJSON (GetMediaQueriesResult _0) = A.object $ P.catMaybes
        [ P.pure $ "medias" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetMediaQueriesResult where
    GetMediaQueriesResult _0 <> GetMediaQueriesResult _ = GetMediaQueriesResult _0


------------------------------------------------------------------------------
instance M.Method GetMediaQueries where
    type Result GetMediaQueries = GetMediaQueriesResult
    name _ = "CSS.getMediaQueries"


------------------------------------------------------------------------------
-- | Returns all media queries parsed by the rendering engine.
getMediaQueries
    :: GetMediaQueries
getMediaQueries = GetMediaQueries


------------------------------------------------------------------------------
-- | Requests information about platform fonts which we used to render child TextNodes in the given
-- node.
data GetPlatformFontsForNode = GetPlatformFontsForNode
    { nodeId :: !DOM.NodeId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetPlatformFontsForNode where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getPlatformFontsForNode" $ \_o -> GetPlatformFontsForNode
            <$> _o .: "nodeId"
        ago = A.withArray "getPlatformFontsForNode" $ \_a -> GetPlatformFontsForNode
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetPlatformFontsForNode where
    toEncoding (GetPlatformFontsForNode _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]
    toJSON (GetPlatformFontsForNode _0) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetPlatformFontsForNode where
    GetPlatformFontsForNode _0 <> GetPlatformFontsForNode _ = GetPlatformFontsForNode _0


------------------------------------------------------------------------------
-- | Requests information about platform fonts which we used to render child TextNodes in the given
-- node.
data GetPlatformFontsForNodeResult = GetPlatformFontsForNodeResult
    { -- | Usage statistics for every employed platform font.
      fonts :: ![PlatformFontUsage]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetPlatformFontsForNodeResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getPlatformFontsForNodeResult" $ \_o -> GetPlatformFontsForNodeResult
            <$> _o .: "fonts"
        ago = A.withArray "getPlatformFontsForNodeResult" $ \_a -> GetPlatformFontsForNodeResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetPlatformFontsForNodeResult where
    toEncoding (GetPlatformFontsForNodeResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "fonts" .= _0
        ]
    toJSON (GetPlatformFontsForNodeResult _0) = A.object $ P.catMaybes
        [ P.pure $ "fonts" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetPlatformFontsForNodeResult where
    GetPlatformFontsForNodeResult _0 <> GetPlatformFontsForNodeResult _ = GetPlatformFontsForNodeResult _0


------------------------------------------------------------------------------
instance M.Method GetPlatformFontsForNode where
    type Result GetPlatformFontsForNode = GetPlatformFontsForNodeResult
    name _ = "CSS.getPlatformFontsForNode"


------------------------------------------------------------------------------
-- | Requests information about platform fonts which we used to render child TextNodes in the given
-- node.
getPlatformFontsForNode
    :: DOM.NodeId
    -> GetPlatformFontsForNode
getPlatformFontsForNode _0 = GetPlatformFontsForNode _0


------------------------------------------------------------------------------
-- | Returns the current textual content for a stylesheet.
data GetStyleSheetText = GetStyleSheetText
    { styleSheetId :: !StyleSheetId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetStyleSheetText where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getStyleSheetText" $ \_o -> GetStyleSheetText
            <$> _o .: "styleSheetId"
        ago = A.withArray "getStyleSheetText" $ \_a -> GetStyleSheetText
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetStyleSheetText where
    toEncoding (GetStyleSheetText _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "styleSheetId" .= _0
        ]
    toJSON (GetStyleSheetText _0) = A.object $ P.catMaybes
        [ P.pure $ "styleSheetId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetStyleSheetText where
    GetStyleSheetText _0 <> GetStyleSheetText _ = GetStyleSheetText _0


------------------------------------------------------------------------------
-- | Returns the current textual content for a stylesheet.
data GetStyleSheetTextResult = GetStyleSheetTextResult
    { -- | The stylesheet text.
      text :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetStyleSheetTextResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getStyleSheetTextResult" $ \_o -> GetStyleSheetTextResult
            <$> _o .: "text"
        ago = A.withArray "getStyleSheetTextResult" $ \_a -> GetStyleSheetTextResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetStyleSheetTextResult where
    toEncoding (GetStyleSheetTextResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "text" .= _0
        ]
    toJSON (GetStyleSheetTextResult _0) = A.object $ P.catMaybes
        [ P.pure $ "text" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetStyleSheetTextResult where
    GetStyleSheetTextResult _0 <> GetStyleSheetTextResult _ = GetStyleSheetTextResult _0


------------------------------------------------------------------------------
instance M.Method GetStyleSheetText where
    type Result GetStyleSheetText = GetStyleSheetTextResult
    name _ = "CSS.getStyleSheetText"


------------------------------------------------------------------------------
-- | Returns the current textual content for a stylesheet.
getStyleSheetText
    :: StyleSheetId
    -> GetStyleSheetText
getStyleSheetText _0 = GetStyleSheetText _0


------------------------------------------------------------------------------
-- | Find a rule with the given active property for the given node and set the new value for this
-- property
data SetEffectivePropertyValueForNode = SetEffectivePropertyValueForNode
    { -- | The element id for which to set property.
      nodeId :: !DOM.NodeId
    , propertyName :: !T.Text
    , value :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetEffectivePropertyValueForNode where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setEffectivePropertyValueForNode" $ \_o -> SetEffectivePropertyValueForNode
            <$> _o .: "nodeId"
            <*> _o .: "propertyName"
            <*> _o .: "value"
        ago = A.withArray "setEffectivePropertyValueForNode" $ \_a -> SetEffectivePropertyValueForNode
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON SetEffectivePropertyValueForNode where
    toEncoding (SetEffectivePropertyValueForNode _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "propertyName" .= _1
        , P.pure $ "value" .= _2
        ]
    toJSON (SetEffectivePropertyValueForNode _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "propertyName" .= _1
        , P.pure $ "value" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetEffectivePropertyValueForNode where
    SetEffectivePropertyValueForNode _0 _1 _2 <> SetEffectivePropertyValueForNode _ _ _ = SetEffectivePropertyValueForNode _0 _1 _2


------------------------------------------------------------------------------
instance M.Method SetEffectivePropertyValueForNode where
    type Result SetEffectivePropertyValueForNode = ()
    name _ = "CSS.setEffectivePropertyValueForNode"


------------------------------------------------------------------------------
-- | Find a rule with the given active property for the given node and set the new value for this
-- property
setEffectivePropertyValueForNode
    :: DOM.NodeId
    -- ^ The element id for which to set property.

    -> T.Text
    -> T.Text
    -> SetEffectivePropertyValueForNode
setEffectivePropertyValueForNode _0 _1 _2 = SetEffectivePropertyValueForNode _0 _1 _2


------------------------------------------------------------------------------
-- | Modifies the keyframe rule key text.
data SetKeyframeKey = SetKeyframeKey
    { styleSheetId :: !StyleSheetId
    , range :: !SourceRange
    , keyText :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetKeyframeKey where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setKeyframeKey" $ \_o -> SetKeyframeKey
            <$> _o .: "styleSheetId"
            <*> _o .: "range"
            <*> _o .: "keyText"
        ago = A.withArray "setKeyframeKey" $ \_a -> SetKeyframeKey
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON SetKeyframeKey where
    toEncoding (SetKeyframeKey _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "styleSheetId" .= _0
        , P.pure $ "range" .= _1
        , P.pure $ "keyText" .= _2
        ]
    toJSON (SetKeyframeKey _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "styleSheetId" .= _0
        , P.pure $ "range" .= _1
        , P.pure $ "keyText" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetKeyframeKey where
    SetKeyframeKey _0 _1 _2 <> SetKeyframeKey _ _ _ = SetKeyframeKey _0 _1 _2


------------------------------------------------------------------------------
-- | Modifies the keyframe rule key text.
data SetKeyframeKeyResult = SetKeyframeKeyResult
    { -- | The resulting key text after modification.
      keyText :: !Value
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetKeyframeKeyResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setKeyframeKeyResult" $ \_o -> SetKeyframeKeyResult
            <$> _o .: "keyText"
        ago = A.withArray "setKeyframeKeyResult" $ \_a -> SetKeyframeKeyResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetKeyframeKeyResult where
    toEncoding (SetKeyframeKeyResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "keyText" .= _0
        ]
    toJSON (SetKeyframeKeyResult _0) = A.object $ P.catMaybes
        [ P.pure $ "keyText" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetKeyframeKeyResult where
    SetKeyframeKeyResult _0 <> SetKeyframeKeyResult _ = SetKeyframeKeyResult _0


------------------------------------------------------------------------------
instance M.Method SetKeyframeKey where
    type Result SetKeyframeKey = SetKeyframeKeyResult
    name _ = "CSS.setKeyframeKey"


------------------------------------------------------------------------------
-- | Modifies the keyframe rule key text.
setKeyframeKey
    :: StyleSheetId
    -> SourceRange
    -> T.Text
    -> SetKeyframeKey
setKeyframeKey _0 _1 _2 = SetKeyframeKey _0 _1 _2


------------------------------------------------------------------------------
-- | Modifies the rule selector.
data SetMediaText = SetMediaText
    { styleSheetId :: !StyleSheetId
    , range :: !SourceRange
    , text :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetMediaText where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setMediaText" $ \_o -> SetMediaText
            <$> _o .: "styleSheetId"
            <*> _o .: "range"
            <*> _o .: "text"
        ago = A.withArray "setMediaText" $ \_a -> SetMediaText
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON SetMediaText where
    toEncoding (SetMediaText _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "styleSheetId" .= _0
        , P.pure $ "range" .= _1
        , P.pure $ "text" .= _2
        ]
    toJSON (SetMediaText _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "styleSheetId" .= _0
        , P.pure $ "range" .= _1
        , P.pure $ "text" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetMediaText where
    SetMediaText _0 _1 _2 <> SetMediaText _ _ _ = SetMediaText _0 _1 _2


------------------------------------------------------------------------------
-- | Modifies the rule selector.
data SetMediaTextResult = SetMediaTextResult
    { -- | The resulting CSS media rule after modification.
      media :: !CSSMedia
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetMediaTextResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setMediaTextResult" $ \_o -> SetMediaTextResult
            <$> _o .: "media"
        ago = A.withArray "setMediaTextResult" $ \_a -> SetMediaTextResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetMediaTextResult where
    toEncoding (SetMediaTextResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "media" .= _0
        ]
    toJSON (SetMediaTextResult _0) = A.object $ P.catMaybes
        [ P.pure $ "media" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetMediaTextResult where
    SetMediaTextResult _0 <> SetMediaTextResult _ = SetMediaTextResult _0


------------------------------------------------------------------------------
instance M.Method SetMediaText where
    type Result SetMediaText = SetMediaTextResult
    name _ = "CSS.setMediaText"


------------------------------------------------------------------------------
-- | Modifies the rule selector.
setMediaText
    :: StyleSheetId
    -> SourceRange
    -> T.Text
    -> SetMediaText
setMediaText _0 _1 _2 = SetMediaText _0 _1 _2


------------------------------------------------------------------------------
-- | Modifies the rule selector.
data SetRuleSelector = SetRuleSelector
    { styleSheetId :: !StyleSheetId
    , range :: !SourceRange
    , selector :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetRuleSelector where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setRuleSelector" $ \_o -> SetRuleSelector
            <$> _o .: "styleSheetId"
            <*> _o .: "range"
            <*> _o .: "selector"
        ago = A.withArray "setRuleSelector" $ \_a -> SetRuleSelector
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON SetRuleSelector where
    toEncoding (SetRuleSelector _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "styleSheetId" .= _0
        , P.pure $ "range" .= _1
        , P.pure $ "selector" .= _2
        ]
    toJSON (SetRuleSelector _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "styleSheetId" .= _0
        , P.pure $ "range" .= _1
        , P.pure $ "selector" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetRuleSelector where
    SetRuleSelector _0 _1 _2 <> SetRuleSelector _ _ _ = SetRuleSelector _0 _1 _2


------------------------------------------------------------------------------
-- | Modifies the rule selector.
data SetRuleSelectorResult = SetRuleSelectorResult
    { -- | The resulting selector list after modification.
      selectorList :: !SelectorList
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetRuleSelectorResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setRuleSelectorResult" $ \_o -> SetRuleSelectorResult
            <$> _o .: "selectorList"
        ago = A.withArray "setRuleSelectorResult" $ \_a -> SetRuleSelectorResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetRuleSelectorResult where
    toEncoding (SetRuleSelectorResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "selectorList" .= _0
        ]
    toJSON (SetRuleSelectorResult _0) = A.object $ P.catMaybes
        [ P.pure $ "selectorList" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetRuleSelectorResult where
    SetRuleSelectorResult _0 <> SetRuleSelectorResult _ = SetRuleSelectorResult _0


------------------------------------------------------------------------------
instance M.Method SetRuleSelector where
    type Result SetRuleSelector = SetRuleSelectorResult
    name _ = "CSS.setRuleSelector"


------------------------------------------------------------------------------
-- | Modifies the rule selector.
setRuleSelector
    :: StyleSheetId
    -> SourceRange
    -> T.Text
    -> SetRuleSelector
setRuleSelector _0 _1 _2 = SetRuleSelector _0 _1 _2


------------------------------------------------------------------------------
-- | Sets the new stylesheet text.
data SetStyleSheetText = SetStyleSheetText
    { styleSheetId :: !StyleSheetId
    , text :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetStyleSheetText where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setStyleSheetText" $ \_o -> SetStyleSheetText
            <$> _o .: "styleSheetId"
            <*> _o .: "text"
        ago = A.withArray "setStyleSheetText" $ \_a -> SetStyleSheetText
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SetStyleSheetText where
    toEncoding (SetStyleSheetText _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "styleSheetId" .= _0
        , P.pure $ "text" .= _1
        ]
    toJSON (SetStyleSheetText _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "styleSheetId" .= _0
        , P.pure $ "text" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetStyleSheetText where
    SetStyleSheetText _0 _1 <> SetStyleSheetText _ _ = SetStyleSheetText _0 _1


------------------------------------------------------------------------------
-- | Sets the new stylesheet text.
data SetStyleSheetTextResult = SetStyleSheetTextResult
    { -- | URL of source map associated with script (if any).
      sourceMapURL :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetStyleSheetTextResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setStyleSheetTextResult" $ \_o -> SetStyleSheetTextResult
            <$> _o .:? "sourceMapURL"
        ago = A.withArray "setStyleSheetTextResult" $ \_a -> SetStyleSheetTextResult
            <$> P.traverse A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetStyleSheetTextResult where
    toEncoding (SetStyleSheetTextResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ ("sourceMapURL" .=) <$> _0
        ]
    toJSON (SetStyleSheetTextResult _0) = A.object $ P.catMaybes
        [ ("sourceMapURL" .=) <$> _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetStyleSheetTextResult where
    SetStyleSheetTextResult _0 <> SetStyleSheetTextResult __0 = SetStyleSheetTextResult (_0 <|> __0)


------------------------------------------------------------------------------
instance P.Monoid SetStyleSheetTextResult where
    mempty = SetStyleSheetTextResult P.empty


------------------------------------------------------------------------------
instance M.Method SetStyleSheetText where
    type Result SetStyleSheetText = SetStyleSheetTextResult
    name _ = "CSS.setStyleSheetText"


------------------------------------------------------------------------------
-- | Sets the new stylesheet text.
setStyleSheetText
    :: StyleSheetId
    -> T.Text
    -> SetStyleSheetText
setStyleSheetText _0 _1 = SetStyleSheetText _0 _1


------------------------------------------------------------------------------
-- | Applies specified style edits one after another in the given order.
data SetStyleTexts = SetStyleTexts
    { edits :: ![StyleDeclarationEdit]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetStyleTexts where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setStyleTexts" $ \_o -> SetStyleTexts
            <$> _o .: "edits"
        ago = A.withArray "setStyleTexts" $ \_a -> SetStyleTexts
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetStyleTexts where
    toEncoding (SetStyleTexts _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "edits" .= _0
        ]
    toJSON (SetStyleTexts _0) = A.object $ P.catMaybes
        [ P.pure $ "edits" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetStyleTexts where
    SetStyleTexts _0 <> SetStyleTexts _ = SetStyleTexts _0


------------------------------------------------------------------------------
-- | Applies specified style edits one after another in the given order.
data SetStyleTextsResult = SetStyleTextsResult
    { -- | The resulting styles after modification.
      styles :: ![CSSStyle]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetStyleTextsResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setStyleTextsResult" $ \_o -> SetStyleTextsResult
            <$> _o .: "styles"
        ago = A.withArray "setStyleTextsResult" $ \_a -> SetStyleTextsResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetStyleTextsResult where
    toEncoding (SetStyleTextsResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "styles" .= _0
        ]
    toJSON (SetStyleTextsResult _0) = A.object $ P.catMaybes
        [ P.pure $ "styles" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetStyleTextsResult where
    SetStyleTextsResult _0 <> SetStyleTextsResult _ = SetStyleTextsResult _0


------------------------------------------------------------------------------
instance M.Method SetStyleTexts where
    type Result SetStyleTexts = SetStyleTextsResult
    name _ = "CSS.setStyleTexts"


------------------------------------------------------------------------------
-- | Applies specified style edits one after another in the given order.
setStyleTexts
    :: [StyleDeclarationEdit]
    -> SetStyleTexts
setStyleTexts _0 = SetStyleTexts _0


------------------------------------------------------------------------------
-- | Enables the selector recording.
data StartRuleUsageTracking = StartRuleUsageTracking
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StartRuleUsageTracking where
    parseJSON A.Null = P.pure StartRuleUsageTracking
    parseJSON v = A.withArray "startRuleUsageTracking" go v
        <|> A.withObject "startRuleUsageTracking" go v
      where
        go _ = P.pure StartRuleUsageTracking


------------------------------------------------------------------------------
instance A.ToJSON StartRuleUsageTracking where
    toEncoding StartRuleUsageTracking = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON StartRuleUsageTracking = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup StartRuleUsageTracking where
    StartRuleUsageTracking <> StartRuleUsageTracking = StartRuleUsageTracking


------------------------------------------------------------------------------
instance P.Monoid StartRuleUsageTracking where
    mempty = StartRuleUsageTracking


------------------------------------------------------------------------------
instance M.Method StartRuleUsageTracking where
    type Result StartRuleUsageTracking = ()
    name _ = "CSS.startRuleUsageTracking"


------------------------------------------------------------------------------
-- | Enables the selector recording.
startRuleUsageTracking
    :: StartRuleUsageTracking
startRuleUsageTracking = StartRuleUsageTracking


------------------------------------------------------------------------------
-- | Stop tracking rule usage and return the list of rules that were used since last call to
-- @takeCoverageDelta@ (or since start of coverage instrumentation)
data StopRuleUsageTracking = StopRuleUsageTracking
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StopRuleUsageTracking where
    parseJSON A.Null = P.pure StopRuleUsageTracking
    parseJSON v = A.withArray "stopRuleUsageTracking" go v
        <|> A.withObject "stopRuleUsageTracking" go v
      where
        go _ = P.pure StopRuleUsageTracking


------------------------------------------------------------------------------
instance A.ToJSON StopRuleUsageTracking where
    toEncoding StopRuleUsageTracking = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON StopRuleUsageTracking = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup StopRuleUsageTracking where
    StopRuleUsageTracking <> StopRuleUsageTracking = StopRuleUsageTracking


------------------------------------------------------------------------------
instance P.Monoid StopRuleUsageTracking where
    mempty = StopRuleUsageTracking


------------------------------------------------------------------------------
-- | Stop tracking rule usage and return the list of rules that were used since last call to
-- @takeCoverageDelta@ (or since start of coverage instrumentation)
data StopRuleUsageTrackingResult = StopRuleUsageTrackingResult
    { ruleUsage :: ![RuleUsage]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StopRuleUsageTrackingResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "stopRuleUsageTrackingResult" $ \_o -> StopRuleUsageTrackingResult
            <$> _o .: "ruleUsage"
        ago = A.withArray "stopRuleUsageTrackingResult" $ \_a -> StopRuleUsageTrackingResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON StopRuleUsageTrackingResult where
    toEncoding (StopRuleUsageTrackingResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "ruleUsage" .= _0
        ]
    toJSON (StopRuleUsageTrackingResult _0) = A.object $ P.catMaybes
        [ P.pure $ "ruleUsage" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup StopRuleUsageTrackingResult where
    StopRuleUsageTrackingResult _0 <> StopRuleUsageTrackingResult _ = StopRuleUsageTrackingResult _0


------------------------------------------------------------------------------
instance M.Method StopRuleUsageTracking where
    type Result StopRuleUsageTracking = StopRuleUsageTrackingResult
    name _ = "CSS.stopRuleUsageTracking"


------------------------------------------------------------------------------
-- | Stop tracking rule usage and return the list of rules that were used since last call to
-- @takeCoverageDelta@ (or since start of coverage instrumentation)
stopRuleUsageTracking
    :: StopRuleUsageTracking
stopRuleUsageTracking = StopRuleUsageTracking


------------------------------------------------------------------------------
-- | Obtain list of rules that became used since last call to this method (or since start of coverage
-- instrumentation)
data TakeCoverageDelta = TakeCoverageDelta
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TakeCoverageDelta where
    parseJSON A.Null = P.pure TakeCoverageDelta
    parseJSON v = A.withArray "takeCoverageDelta" go v
        <|> A.withObject "takeCoverageDelta" go v
      where
        go _ = P.pure TakeCoverageDelta


------------------------------------------------------------------------------
instance A.ToJSON TakeCoverageDelta where
    toEncoding TakeCoverageDelta = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON TakeCoverageDelta = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup TakeCoverageDelta where
    TakeCoverageDelta <> TakeCoverageDelta = TakeCoverageDelta


------------------------------------------------------------------------------
instance P.Monoid TakeCoverageDelta where
    mempty = TakeCoverageDelta


------------------------------------------------------------------------------
-- | Obtain list of rules that became used since last call to this method (or since start of coverage
-- instrumentation)
data TakeCoverageDeltaResult = TakeCoverageDeltaResult
    { coverage :: ![RuleUsage]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TakeCoverageDeltaResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "takeCoverageDeltaResult" $ \_o -> TakeCoverageDeltaResult
            <$> _o .: "coverage"
        ago = A.withArray "takeCoverageDeltaResult" $ \_a -> TakeCoverageDeltaResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON TakeCoverageDeltaResult where
    toEncoding (TakeCoverageDeltaResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "coverage" .= _0
        ]
    toJSON (TakeCoverageDeltaResult _0) = A.object $ P.catMaybes
        [ P.pure $ "coverage" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup TakeCoverageDeltaResult where
    TakeCoverageDeltaResult _0 <> TakeCoverageDeltaResult _ = TakeCoverageDeltaResult _0


------------------------------------------------------------------------------
instance M.Method TakeCoverageDelta where
    type Result TakeCoverageDelta = TakeCoverageDeltaResult
    name _ = "CSS.takeCoverageDelta"


------------------------------------------------------------------------------
-- | Obtain list of rules that became used since last call to this method (or since start of coverage
-- instrumentation)
takeCoverageDelta
    :: TakeCoverageDelta
takeCoverageDelta = TakeCoverageDelta


------------------------------------------------------------------------------
-- | Fires whenever a web font is updated.  A non-empty font parameter indicates a successfully loaded
-- web font
data FontsUpdated = FontsUpdated
    { -- | The web font that has loaded.
      font :: !(P.Maybe FontFace)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON FontsUpdated where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "fontsUpdated" $ \_o -> FontsUpdated
            <$> _o .:? "font"
        ago = A.withArray "fontsUpdated" $ \_a -> FontsUpdated
            <$> P.traverse A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON FontsUpdated where
    toEncoding (FontsUpdated _0) = A.pairs $ P.fold $ P.catMaybes
        [ ("font" .=) <$> _0
        ]
    toJSON (FontsUpdated _0) = A.object $ P.catMaybes
        [ ("font" .=) <$> _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup FontsUpdated where
    FontsUpdated _0 <> FontsUpdated __0 = FontsUpdated (_0 <|> __0)


------------------------------------------------------------------------------
instance P.Monoid FontsUpdated where
    mempty = FontsUpdated P.empty


------------------------------------------------------------------------------
instance E.Event FontsUpdated where
    type Result FontsUpdated = FontsUpdated
    name _ = "CSS.fontsUpdated"


------------------------------------------------------------------------------
-- | Fires whenever a web font is updated.  A non-empty font parameter indicates a successfully loaded
-- web font
fontsUpdated :: P.Proxy FontsUpdated
fontsUpdated = P.Proxy


------------------------------------------------------------------------------
-- | Fires whenever a MediaQuery result changes (for example, after a browser window has been
-- resized.) The current implementation considers only viewport-dependent media features.
data MediaQueryResultChanged = MediaQueryResultChanged
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON MediaQueryResultChanged where
    parseJSON A.Null = P.pure MediaQueryResultChanged
    parseJSON v = A.withArray "mediaQueryResultChanged" go v
        <|> A.withObject "mediaQueryResultChanged" go v
      where
        go _ = P.pure MediaQueryResultChanged


------------------------------------------------------------------------------
instance A.ToJSON MediaQueryResultChanged where
    toEncoding MediaQueryResultChanged = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON MediaQueryResultChanged = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup MediaQueryResultChanged where
    MediaQueryResultChanged <> MediaQueryResultChanged = MediaQueryResultChanged


------------------------------------------------------------------------------
instance P.Monoid MediaQueryResultChanged where
    mempty = MediaQueryResultChanged


------------------------------------------------------------------------------
instance E.Event MediaQueryResultChanged where
    type Result MediaQueryResultChanged = ()
    name _ = "CSS.mediaQueryResultChanged"


------------------------------------------------------------------------------
-- | Fires whenever a MediaQuery result changes (for example, after a browser window has been
-- resized.) The current implementation considers only viewport-dependent media features.
mediaQueryResultChanged :: P.Proxy MediaQueryResultChanged
mediaQueryResultChanged = P.Proxy


------------------------------------------------------------------------------
-- | Fired whenever an active document stylesheet is added.
data StyleSheetAdded = StyleSheetAdded
    { -- | Added stylesheet metainfo.
      header :: !CSSStyleSheetHeader
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StyleSheetAdded where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "styleSheetAdded" $ \_o -> StyleSheetAdded
            <$> _o .: "header"
        ago = A.withArray "styleSheetAdded" $ \_a -> StyleSheetAdded
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON StyleSheetAdded where
    toEncoding (StyleSheetAdded _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "header" .= _0
        ]
    toJSON (StyleSheetAdded _0) = A.object $ P.catMaybes
        [ P.pure $ "header" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup StyleSheetAdded where
    StyleSheetAdded _0 <> StyleSheetAdded _ = StyleSheetAdded _0


------------------------------------------------------------------------------
instance E.Event StyleSheetAdded where
    type Result StyleSheetAdded = StyleSheetAdded
    name _ = "CSS.styleSheetAdded"


------------------------------------------------------------------------------
-- | Fired whenever an active document stylesheet is added.
styleSheetAdded :: P.Proxy StyleSheetAdded
styleSheetAdded = P.Proxy


------------------------------------------------------------------------------
-- | Fired whenever a stylesheet is changed as a result of the client operation.
data StyleSheetChanged = StyleSheetChanged
    { styleSheetId :: !StyleSheetId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StyleSheetChanged where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "styleSheetChanged" $ \_o -> StyleSheetChanged
            <$> _o .: "styleSheetId"
        ago = A.withArray "styleSheetChanged" $ \_a -> StyleSheetChanged
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON StyleSheetChanged where
    toEncoding (StyleSheetChanged _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "styleSheetId" .= _0
        ]
    toJSON (StyleSheetChanged _0) = A.object $ P.catMaybes
        [ P.pure $ "styleSheetId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup StyleSheetChanged where
    StyleSheetChanged _0 <> StyleSheetChanged _ = StyleSheetChanged _0


------------------------------------------------------------------------------
instance E.Event StyleSheetChanged where
    type Result StyleSheetChanged = StyleSheetChanged
    name _ = "CSS.styleSheetChanged"


------------------------------------------------------------------------------
-- | Fired whenever a stylesheet is changed as a result of the client operation.
styleSheetChanged :: P.Proxy StyleSheetChanged
styleSheetChanged = P.Proxy


------------------------------------------------------------------------------
-- | Fired whenever an active document stylesheet is removed.
data StyleSheetRemoved = StyleSheetRemoved
    { -- | Identifier of the removed stylesheet.
      styleSheetId :: !StyleSheetId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StyleSheetRemoved where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "styleSheetRemoved" $ \_o -> StyleSheetRemoved
            <$> _o .: "styleSheetId"
        ago = A.withArray "styleSheetRemoved" $ \_a -> StyleSheetRemoved
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON StyleSheetRemoved where
    toEncoding (StyleSheetRemoved _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "styleSheetId" .= _0
        ]
    toJSON (StyleSheetRemoved _0) = A.object $ P.catMaybes
        [ P.pure $ "styleSheetId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup StyleSheetRemoved where
    StyleSheetRemoved _0 <> StyleSheetRemoved _ = StyleSheetRemoved _0


------------------------------------------------------------------------------
instance E.Event StyleSheetRemoved where
    type Result StyleSheetRemoved = StyleSheetRemoved
    name _ = "CSS.styleSheetRemoved"


------------------------------------------------------------------------------
-- | Fired whenever an active document stylesheet is removed.
styleSheetRemoved :: P.Proxy StyleSheetRemoved
styleSheetRemoved = P.Proxy

