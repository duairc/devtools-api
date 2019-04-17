{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | This domain exposes CSS read\/write operations. All CSS objects (stylesheets, rules, and styles)
-- have an associated @id@ used in subsequent operations on the related object. Each object type has
-- a specific @id@ structure, and those are not interchangeable between objects of different kinds.
-- CSS objects can be loaded using the @get*ForNode()@ calls (which accept a DOM node id). A client
-- can also keep track of stylesheets via the @styleSheetAdded@\/@styleSheetRemoved@ events and
-- subsequently load the required stylesheet contents using the @getStyleSheet[Text]()@ methods.
module DevTools.API.CSS.Types{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

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
import qualified DevTools.API.Page.Types as Page


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
type StyleSheetId = T.Text


------------------------------------------------------------------------------
-- | Stylesheet type: "injected" for stylesheets injected via extension, "user-agent" for user-agent
-- stylesheets, "inspector" for stylesheets created by the inspector (i.e. those holding the "via
-- inspector" rules), "regular" for regular stylesheets.
data StyleSheetOrigin
    = Injected
    | UserAgent
    | Inspector
    | Regular
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StyleSheetOrigin where
    parseJSON = A.withText "StyleSheetOrigin" $ \t -> case t of
        "injected" -> P.pure Injected
        "user-agent" -> P.pure UserAgent
        "inspector" -> P.pure Inspector
        "regular" -> P.pure Regular
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON StyleSheetOrigin where
    toJSON Injected = "injected"
    toJSON UserAgent = "user-agent"
    toJSON Inspector = "inspector"
    toJSON Regular = "regular"


------------------------------------------------------------------------------
-- | CSS rule collection for a single pseudo style.
data PseudoElementMatches = PseudoElementMatches
    { -- | Pseudo element type.
      pseudoType :: !DOM.PseudoType
      -- | Matches of CSS rules applicable to the pseudo style.
    , matches :: ![RuleMatch]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PseudoElementMatches where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "PseudoElementMatches" $ \_o -> PseudoElementMatches
            <$> _o .: "pseudoType"
            <*> _o .: "matches"
        ago = A.withArray "PseudoElementMatches" $ \_a -> PseudoElementMatches
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON PseudoElementMatches where
    toEncoding (PseudoElementMatches _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "pseudoType" .= _0
        , P.pure $ "matches" .= _1
        ]
    toJSON (PseudoElementMatches _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "pseudoType" .= _0
        , P.pure $ "matches" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup PseudoElementMatches where
    PseudoElementMatches _0 _1 <> PseudoElementMatches _ _ = PseudoElementMatches _0 _1


------------------------------------------------------------------------------
-- | Inherited CSS rule collection from ancestor node.
data InheritedStyleEntry = InheritedStyleEntry
    { -- | The ancestor node's inline style, if any, in the style inheritance chain.
      inlineStyle :: !(P.Maybe CSSStyle)
      -- | Matches of CSS rules matching the ancestor node in the style inheritance chain.
    , matchedCSSRules :: ![RuleMatch]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON InheritedStyleEntry where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "InheritedStyleEntry" $ \_o -> InheritedStyleEntry
            <$> _o .:? "inlineStyle"
            <*> _o .: "matchedCSSRules"
        ago = A.withArray "InheritedStyleEntry" $ \_a -> InheritedStyleEntry
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON InheritedStyleEntry where
    toEncoding (InheritedStyleEntry _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ ("inlineStyle" .=) <$> _0
        , P.pure $ "matchedCSSRules" .= _1
        ]
    toJSON (InheritedStyleEntry _0 _1) = A.object $ P.catMaybes
        [ ("inlineStyle" .=) <$> _0
        , P.pure $ "matchedCSSRules" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup InheritedStyleEntry where
    InheritedStyleEntry _0 _1 <> InheritedStyleEntry __0 _ = InheritedStyleEntry (_0 <|> __0) _1


------------------------------------------------------------------------------
-- | Match data for a CSS rule.
data RuleMatch = RuleMatch
    { -- | CSS rule in the match.
      rule :: !CSSRule
      -- | Matching selector indices in the rule's selectorList selectors (0-based).
    , matchingSelectors :: ![P.Int]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RuleMatch where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "RuleMatch" $ \_o -> RuleMatch
            <$> _o .: "rule"
            <*> _o .: "matchingSelectors"
        ago = A.withArray "RuleMatch" $ \_a -> RuleMatch
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON RuleMatch where
    toEncoding (RuleMatch _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "rule" .= _0
        , P.pure $ "matchingSelectors" .= _1
        ]
    toJSON (RuleMatch _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "rule" .= _0
        , P.pure $ "matchingSelectors" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup RuleMatch where
    RuleMatch _0 _1 <> RuleMatch _ _ = RuleMatch _0 _1


------------------------------------------------------------------------------
-- | Data for a simple selector (these are delimited by commas in a selector list).
data Value = Value
    { -- | Value text.
      text :: !T.Text
      -- | Value range in the underlying resource (if available).
    , range :: !(P.Maybe SourceRange)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Value where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "Value" $ \_o -> Value
            <$> _o .: "text"
            <*> _o .:? "range"
        ago = A.withArray "Value" $ \_a -> Value
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON Value where
    toEncoding (Value _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "text" .= _0
        , ("range" .=) <$> _1
        ]
    toJSON (Value _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "text" .= _0
        , ("range" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup Value where
    Value _0 _1 <> Value _ __1 = Value _0 (_1 <|> __1)


------------------------------------------------------------------------------
-- | Selector list data.
data SelectorList = SelectorList
    { -- | Selectors in the list.
      selectors :: ![Value]
      -- | Rule selector text.
    , text :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SelectorList where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "SelectorList" $ \_o -> SelectorList
            <$> _o .: "selectors"
            <*> _o .: "text"
        ago = A.withArray "SelectorList" $ \_a -> SelectorList
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SelectorList where
    toEncoding (SelectorList _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "selectors" .= _0
        , P.pure $ "text" .= _1
        ]
    toJSON (SelectorList _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "selectors" .= _0
        , P.pure $ "text" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SelectorList where
    SelectorList _0 _1 <> SelectorList _ _ = SelectorList _0 _1


------------------------------------------------------------------------------
-- | CSS stylesheet metainformation.
data CSSStyleSheetHeader = CSSStyleSheetHeader
    { -- | The stylesheet identifier.
      styleSheetId :: !StyleSheetId
      -- | Owner frame identifier.
    , frameId :: !Page.FrameId
      -- | Stylesheet resource URL.
    , sourceURL :: !T.Text
      -- | URL of source map associated with the stylesheet (if any).
    , sourceMapURL :: !(P.Maybe T.Text)
      -- | Stylesheet origin.
    , origin :: !StyleSheetOrigin
      -- | Stylesheet title.
    , title :: !T.Text
      -- | The backend id for the owner node of the stylesheet.
    , ownerNode :: !(P.Maybe DOM.BackendNodeId)
      -- | Denotes whether the stylesheet is disabled.
    , disabled :: !P.Bool
      -- | Whether the sourceURL field value comes from the sourceURL comment.
    , hasSourceURL :: !(P.Maybe P.Bool)
      -- | Whether this stylesheet is created for STYLE tag by parser. This flag is not set for
      -- document.written STYLE tags.
    , isInline :: !P.Bool
      -- | Line offset of the stylesheet within the resource (zero based).
    , startLine :: !P.Double
      -- | Column offset of the stylesheet within the resource (zero based).
    , startColumn :: !P.Double
      -- | Size of the content (in characters).
    , length :: !P.Double
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CSSStyleSheetHeader where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "CSSStyleSheetHeader" $ \_o -> CSSStyleSheetHeader
            <$> _o .: "styleSheetId"
            <*> _o .: "frameId"
            <*> _o .: "sourceURL"
            <*> _o .:? "sourceMapURL"
            <*> _o .: "origin"
            <*> _o .: "title"
            <*> _o .:? "ownerNode"
            <*> _o .: "disabled"
            <*> _o .:? "hasSourceURL"
            <*> _o .: "isInline"
            <*> _o .: "startLine"
            <*> _o .: "startColumn"
            <*> _o .: "length"
        ago = A.withArray "CSSStyleSheetHeader" $ \_a -> CSSStyleSheetHeader
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)
            <*> P.maybe P.empty A.parseJSON (_a !? 7)
            <*> P.traverse A.parseJSON (_a !? 8)
            <*> P.maybe P.empty A.parseJSON (_a !? 9)
            <*> P.maybe P.empty A.parseJSON (_a !? 10)
            <*> P.maybe P.empty A.parseJSON (_a !? 11)
            <*> P.maybe P.empty A.parseJSON (_a !? 12)


------------------------------------------------------------------------------
instance A.ToJSON CSSStyleSheetHeader where
    toEncoding (CSSStyleSheetHeader _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "styleSheetId" .= _0
        , P.pure $ "frameId" .= _1
        , P.pure $ "sourceURL" .= _2
        , ("sourceMapURL" .=) <$> _3
        , P.pure $ "origin" .= _4
        , P.pure $ "title" .= _5
        , ("ownerNode" .=) <$> _6
        , P.pure $ "disabled" .= _7
        , ("hasSourceURL" .=) <$> _8
        , P.pure $ "isInline" .= _9
        , P.pure $ "startLine" .= _10
        , P.pure $ "startColumn" .= _11
        , P.pure $ "length" .= _12
        ]
    toJSON (CSSStyleSheetHeader _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12) = A.object $ P.catMaybes
        [ P.pure $ "styleSheetId" .= _0
        , P.pure $ "frameId" .= _1
        , P.pure $ "sourceURL" .= _2
        , ("sourceMapURL" .=) <$> _3
        , P.pure $ "origin" .= _4
        , P.pure $ "title" .= _5
        , ("ownerNode" .=) <$> _6
        , P.pure $ "disabled" .= _7
        , ("hasSourceURL" .=) <$> _8
        , P.pure $ "isInline" .= _9
        , P.pure $ "startLine" .= _10
        , P.pure $ "startColumn" .= _11
        , P.pure $ "length" .= _12
        ]


------------------------------------------------------------------------------
instance P.Semigroup CSSStyleSheetHeader where
    CSSStyleSheetHeader _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 <> CSSStyleSheetHeader _ _ _ __3 _ _ __6 _ __8 _ _ _ _ = CSSStyleSheetHeader _0 _1 _2 (_3 <|> __3) _4 _5 (_6 <|> __6) _7 (_8 <|> __8) _9 _10 _11 _12


------------------------------------------------------------------------------
-- | CSS rule representation.
data CSSRule = CSSRule
    { -- | The css style sheet identifier (absent for user agent stylesheet and user-specified
      -- stylesheet rules) this rule came from.
      styleSheetId :: !(P.Maybe StyleSheetId)
      -- | Rule selector data.
    , selectorList :: !SelectorList
      -- | Parent stylesheet's origin.
    , origin :: !StyleSheetOrigin
      -- | Associated style declaration.
    , style :: !CSSStyle
      -- | Media list array (for rules involving media queries). The array enumerates media queries
      -- starting with the innermost one, going outwards.
    , media :: !(P.Maybe [CSSMedia])
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CSSRule where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "CSSRule" $ \_o -> CSSRule
            <$> _o .:? "styleSheetId"
            <*> _o .: "selectorList"
            <*> _o .: "origin"
            <*> _o .: "style"
            <*> _o .:? "media"
        ago = A.withArray "CSSRule" $ \_a -> CSSRule
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON CSSRule where
    toEncoding (CSSRule _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ ("styleSheetId" .=) <$> _0
        , P.pure $ "selectorList" .= _1
        , P.pure $ "origin" .= _2
        , P.pure $ "style" .= _3
        , ("media" .=) <$> _4
        ]
    toJSON (CSSRule _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ ("styleSheetId" .=) <$> _0
        , P.pure $ "selectorList" .= _1
        , P.pure $ "origin" .= _2
        , P.pure $ "style" .= _3
        , ("media" .=) <$> _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup CSSRule where
    CSSRule _0 _1 _2 _3 _4 <> CSSRule __0 _ _ _ __4 = CSSRule (_0 <|> __0) _1 _2 _3 (_4 <|> __4)


------------------------------------------------------------------------------
-- | CSS coverage information.
data RuleUsage = RuleUsage
    { -- | The css style sheet identifier (absent for user agent stylesheet and user-specified
      -- stylesheet rules) this rule came from.
      styleSheetId :: !StyleSheetId
      -- | Offset of the start of the rule (including selector) from the beginning of the stylesheet.
    , startOffset :: !P.Double
      -- | Offset of the end of the rule body from the beginning of the stylesheet.
    , endOffset :: !P.Double
      -- | Indicates whether the rule was actually used by some element in the page.
    , used :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RuleUsage where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "RuleUsage" $ \_o -> RuleUsage
            <$> _o .: "styleSheetId"
            <*> _o .: "startOffset"
            <*> _o .: "endOffset"
            <*> _o .: "used"
        ago = A.withArray "RuleUsage" $ \_a -> RuleUsage
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON RuleUsage where
    toEncoding (RuleUsage _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "styleSheetId" .= _0
        , P.pure $ "startOffset" .= _1
        , P.pure $ "endOffset" .= _2
        , P.pure $ "used" .= _3
        ]
    toJSON (RuleUsage _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "styleSheetId" .= _0
        , P.pure $ "startOffset" .= _1
        , P.pure $ "endOffset" .= _2
        , P.pure $ "used" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup RuleUsage where
    RuleUsage _0 _1 _2 _3 <> RuleUsage _ _ _ _ = RuleUsage _0 _1 _2 _3


------------------------------------------------------------------------------
-- | Text range within a resource. All numbers are zero-based.
data SourceRange = SourceRange
    { -- | Start line of range.
      startLine :: !P.Int
      -- | Start column of range (inclusive).
    , startColumn :: !P.Int
      -- | End line of range
    , endLine :: !P.Int
      -- | End column of range (exclusive).
    , endColumn :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SourceRange where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "SourceRange" $ \_o -> SourceRange
            <$> _o .: "startLine"
            <*> _o .: "startColumn"
            <*> _o .: "endLine"
            <*> _o .: "endColumn"
        ago = A.withArray "SourceRange" $ \_a -> SourceRange
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON SourceRange where
    toEncoding (SourceRange _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "startLine" .= _0
        , P.pure $ "startColumn" .= _1
        , P.pure $ "endLine" .= _2
        , P.pure $ "endColumn" .= _3
        ]
    toJSON (SourceRange _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "startLine" .= _0
        , P.pure $ "startColumn" .= _1
        , P.pure $ "endLine" .= _2
        , P.pure $ "endColumn" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup SourceRange where
    SourceRange _0 _1 _2 _3 <> SourceRange _ _ _ _ = SourceRange _0 _1 _2 _3


------------------------------------------------------------------------------
data ShorthandEntry = ShorthandEntry
    { -- | Shorthand name.
      name :: !T.Text
      -- | Shorthand value.
    , value :: !T.Text
      -- | Whether the property has "!important" annotation (implies @false@ if absent).
    , important :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ShorthandEntry where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "ShorthandEntry" $ \_o -> ShorthandEntry
            <$> _o .: "name"
            <*> _o .: "value"
            <*> _o .:? "important"
        ago = A.withArray "ShorthandEntry" $ \_a -> ShorthandEntry
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON ShorthandEntry where
    toEncoding (ShorthandEntry _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "value" .= _1
        , ("important" .=) <$> _2
        ]
    toJSON (ShorthandEntry _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "value" .= _1
        , ("important" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup ShorthandEntry where
    ShorthandEntry _0 _1 _2 <> ShorthandEntry _ _ __2 = ShorthandEntry _0 _1 (_2 <|> __2)


------------------------------------------------------------------------------
data CSSComputedStyleProperty = CSSComputedStyleProperty
    { -- | Computed style property name.
      name :: !T.Text
      -- | Computed style property value.
    , value :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CSSComputedStyleProperty where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "CSSComputedStyleProperty" $ \_o -> CSSComputedStyleProperty
            <$> _o .: "name"
            <*> _o .: "value"
        ago = A.withArray "CSSComputedStyleProperty" $ \_a -> CSSComputedStyleProperty
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON CSSComputedStyleProperty where
    toEncoding (CSSComputedStyleProperty _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "value" .= _1
        ]
    toJSON (CSSComputedStyleProperty _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "value" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup CSSComputedStyleProperty where
    CSSComputedStyleProperty _0 _1 <> CSSComputedStyleProperty _ _ = CSSComputedStyleProperty _0 _1


------------------------------------------------------------------------------
-- | CSS style representation.
data CSSStyle = CSSStyle
    { -- | The css style sheet identifier (absent for user agent stylesheet and user-specified
      -- stylesheet rules) this rule came from.
      styleSheetId :: !(P.Maybe StyleSheetId)
      -- | CSS properties in the style.
    , cssProperties :: ![CSSProperty]
      -- | Computed values for all shorthands found in the style.
    , shorthandEntries :: ![ShorthandEntry]
      -- | Style declaration text (if available).
    , cssText :: !(P.Maybe T.Text)
      -- | Style declaration range in the enclosing stylesheet (if available).
    , range :: !(P.Maybe SourceRange)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CSSStyle where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "CSSStyle" $ \_o -> CSSStyle
            <$> _o .:? "styleSheetId"
            <*> _o .: "cssProperties"
            <*> _o .: "shorthandEntries"
            <*> _o .:? "cssText"
            <*> _o .:? "range"
        ago = A.withArray "CSSStyle" $ \_a -> CSSStyle
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON CSSStyle where
    toEncoding (CSSStyle _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ ("styleSheetId" .=) <$> _0
        , P.pure $ "cssProperties" .= _1
        , P.pure $ "shorthandEntries" .= _2
        , ("cssText" .=) <$> _3
        , ("range" .=) <$> _4
        ]
    toJSON (CSSStyle _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ ("styleSheetId" .=) <$> _0
        , P.pure $ "cssProperties" .= _1
        , P.pure $ "shorthandEntries" .= _2
        , ("cssText" .=) <$> _3
        , ("range" .=) <$> _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup CSSStyle where
    CSSStyle _0 _1 _2 _3 _4 <> CSSStyle __0 _ _ __3 __4 = CSSStyle (_0 <|> __0) _1 _2 (_3 <|> __3) (_4 <|> __4)


------------------------------------------------------------------------------
-- | CSS property declaration data.
data CSSProperty = CSSProperty
    { -- | The property name.
      name :: !T.Text
      -- | The property value.
    , value :: !T.Text
      -- | Whether the property has "!important" annotation (implies @false@ if absent).
    , important :: !(P.Maybe P.Bool)
      -- | Whether the property is implicit (implies @false@ if absent).
    , implicit :: !(P.Maybe P.Bool)
      -- | The full property text as specified in the style.
    , text :: !(P.Maybe T.Text)
      -- | Whether the property is understood by the browser (implies @true@ if absent).
    , parsedOk :: !(P.Maybe P.Bool)
      -- | Whether the property is disabled by the user (present for source-based properties only).
    , disabled :: !(P.Maybe P.Bool)
      -- | The entire property range in the enclosing style declaration (if available).
    , range :: !(P.Maybe SourceRange)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CSSProperty where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "CSSProperty" $ \_o -> CSSProperty
            <$> _o .: "name"
            <*> _o .: "value"
            <*> _o .:? "important"
            <*> _o .:? "implicit"
            <*> _o .:? "text"
            <*> _o .:? "parsedOk"
            <*> _o .:? "disabled"
            <*> _o .:? "range"
        ago = A.withArray "CSSProperty" $ \_a -> CSSProperty
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)
            <*> P.traverse A.parseJSON (_a !? 7)


------------------------------------------------------------------------------
instance A.ToJSON CSSProperty where
    toEncoding (CSSProperty _0 _1 _2 _3 _4 _5 _6 _7) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "value" .= _1
        , ("important" .=) <$> _2
        , ("implicit" .=) <$> _3
        , ("text" .=) <$> _4
        , ("parsedOk" .=) <$> _5
        , ("disabled" .=) <$> _6
        , ("range" .=) <$> _7
        ]
    toJSON (CSSProperty _0 _1 _2 _3 _4 _5 _6 _7) = A.object $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "value" .= _1
        , ("important" .=) <$> _2
        , ("implicit" .=) <$> _3
        , ("text" .=) <$> _4
        , ("parsedOk" .=) <$> _5
        , ("disabled" .=) <$> _6
        , ("range" .=) <$> _7
        ]


------------------------------------------------------------------------------
instance P.Semigroup CSSProperty where
    CSSProperty _0 _1 _2 _3 _4 _5 _6 _7 <> CSSProperty _ _ __2 __3 __4 __5 __6 __7 = CSSProperty _0 _1 (_2 <|> __2) (_3 <|> __3) (_4 <|> __4) (_5 <|> __5) (_6 <|> __6) (_7 <|> __7)


------------------------------------------------------------------------------
-- | CSS media rule descriptor.
data CSSMedia = CSSMedia
    { -- | Media query text.
      text :: !T.Text
      -- | Source of the media query: "mediaRule" if specified by a @media rule, "importRule" if
      -- specified by an @import rule, "linkedSheet" if specified by a "media" attribute in a linked
      -- stylesheet's LINK tag, "inlineSheet" if specified by a "media" attribute in an inline
      -- stylesheet's STYLE tag.
    , source :: !Source
      -- | URL of the document containing the media query description.
    , sourceURL :: !(P.Maybe T.Text)
      -- | The associated rule (@media or @import) header range in the enclosing stylesheet (if
      -- available).
    , range :: !(P.Maybe SourceRange)
      -- | Identifier of the stylesheet containing this object (if exists).
    , styleSheetId :: !(P.Maybe StyleSheetId)
      -- | Array of media queries.
    , mediaList :: !(P.Maybe [MediaQuery])
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CSSMedia where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "CSSMedia" $ \_o -> CSSMedia
            <$> _o .: "text"
            <*> _o .: "source"
            <*> _o .:? "sourceURL"
            <*> _o .:? "range"
            <*> _o .:? "styleSheetId"
            <*> _o .:? "mediaList"
        ago = A.withArray "CSSMedia" $ \_a -> CSSMedia
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)


------------------------------------------------------------------------------
instance A.ToJSON CSSMedia where
    toEncoding (CSSMedia _0 _1 _2 _3 _4 _5) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "text" .= _0
        , P.pure $ "source" .= _1
        , ("sourceURL" .=) <$> _2
        , ("range" .=) <$> _3
        , ("styleSheetId" .=) <$> _4
        , ("mediaList" .=) <$> _5
        ]
    toJSON (CSSMedia _0 _1 _2 _3 _4 _5) = A.object $ P.catMaybes
        [ P.pure $ "text" .= _0
        , P.pure $ "source" .= _1
        , ("sourceURL" .=) <$> _2
        , ("range" .=) <$> _3
        , ("styleSheetId" .=) <$> _4
        , ("mediaList" .=) <$> _5
        ]


------------------------------------------------------------------------------
instance P.Semigroup CSSMedia where
    CSSMedia _0 _1 _2 _3 _4 _5 <> CSSMedia _ _ __2 __3 __4 __5 = CSSMedia _0 _1 (_2 <|> __2) (_3 <|> __3) (_4 <|> __4) (_5 <|> __5)


------------------------------------------------------------------------------
data Source
    = MediaRule
    | ImportRule
    | LinkedSheet
    | InlineSheet
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Source where
    parseJSON = A.withText "Source" $ \t -> case t of
        "mediaRule" -> P.pure MediaRule
        "importRule" -> P.pure ImportRule
        "linkedSheet" -> P.pure LinkedSheet
        "inlineSheet" -> P.pure InlineSheet
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Source where
    toJSON MediaRule = "mediaRule"
    toJSON ImportRule = "importRule"
    toJSON LinkedSheet = "linkedSheet"
    toJSON InlineSheet = "inlineSheet"


------------------------------------------------------------------------------
-- | Media query descriptor.
data MediaQuery = MediaQuery
    { -- | Array of media query expressions.
      expressions :: ![MediaQueryExpression]
      -- | Whether the media query condition is satisfied.
    , active :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON MediaQuery where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "MediaQuery" $ \_o -> MediaQuery
            <$> _o .: "expressions"
            <*> _o .: "active"
        ago = A.withArray "MediaQuery" $ \_a -> MediaQuery
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON MediaQuery where
    toEncoding (MediaQuery _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "expressions" .= _0
        , P.pure $ "active" .= _1
        ]
    toJSON (MediaQuery _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "expressions" .= _0
        , P.pure $ "active" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup MediaQuery where
    MediaQuery _0 _1 <> MediaQuery _ _ = MediaQuery _0 _1


------------------------------------------------------------------------------
-- | Media query expression descriptor.
data MediaQueryExpression = MediaQueryExpression
    { -- | Media query expression value.
      value :: !P.Double
      -- | Media query expression units.
    , unit :: !T.Text
      -- | Media query expression feature.
    , feature :: !T.Text
      -- | The associated range of the value text in the enclosing stylesheet (if available).
    , valueRange :: !(P.Maybe SourceRange)
      -- | Computed length of media query expression (if applicable).
    , computedLength :: !(P.Maybe P.Double)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON MediaQueryExpression where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "MediaQueryExpression" $ \_o -> MediaQueryExpression
            <$> _o .: "value"
            <*> _o .: "unit"
            <*> _o .: "feature"
            <*> _o .:? "valueRange"
            <*> _o .:? "computedLength"
        ago = A.withArray "MediaQueryExpression" $ \_a -> MediaQueryExpression
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON MediaQueryExpression where
    toEncoding (MediaQueryExpression _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "value" .= _0
        , P.pure $ "unit" .= _1
        , P.pure $ "feature" .= _2
        , ("valueRange" .=) <$> _3
        , ("computedLength" .=) <$> _4
        ]
    toJSON (MediaQueryExpression _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ P.pure $ "value" .= _0
        , P.pure $ "unit" .= _1
        , P.pure $ "feature" .= _2
        , ("valueRange" .=) <$> _3
        , ("computedLength" .=) <$> _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup MediaQueryExpression where
    MediaQueryExpression _0 _1 _2 _3 _4 <> MediaQueryExpression _ _ _ __3 __4 = MediaQueryExpression _0 _1 _2 (_3 <|> __3) (_4 <|> __4)


------------------------------------------------------------------------------
-- | Information about amount of glyphs that were rendered with given font.
data PlatformFontUsage = PlatformFontUsage
    { -- | Font's family name reported by platform.
      familyName :: !T.Text
      -- | Indicates if the font was downloaded or resolved locally.
    , isCustomFont :: !P.Bool
      -- | Amount of glyphs that were rendered with this font.
    , glyphCount :: !P.Double
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PlatformFontUsage where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "PlatformFontUsage" $ \_o -> PlatformFontUsage
            <$> _o .: "familyName"
            <*> _o .: "isCustomFont"
            <*> _o .: "glyphCount"
        ago = A.withArray "PlatformFontUsage" $ \_a -> PlatformFontUsage
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON PlatformFontUsage where
    toEncoding (PlatformFontUsage _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "familyName" .= _0
        , P.pure $ "isCustomFont" .= _1
        , P.pure $ "glyphCount" .= _2
        ]
    toJSON (PlatformFontUsage _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "familyName" .= _0
        , P.pure $ "isCustomFont" .= _1
        , P.pure $ "glyphCount" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup PlatformFontUsage where
    PlatformFontUsage _0 _1 _2 <> PlatformFontUsage _ _ _ = PlatformFontUsage _0 _1 _2


------------------------------------------------------------------------------
-- | Properties of a web font: https:\/\/www.w3.org\/TR\/2008\/REC-CSS2-20080411\/fonts.html#font-descriptions
data FontFace = FontFace
    { -- | The font-family.
      fontFamily :: !T.Text
      -- | The font-style.
    , fontStyle :: !T.Text
      -- | The font-variant.
    , fontVariant :: !T.Text
      -- | The font-weight.
    , fontWeight :: !T.Text
      -- | The font-stretch.
    , fontStretch :: !T.Text
      -- | The unicode-range.
    , unicodeRange :: !T.Text
      -- | The src.
    , src :: !T.Text
      -- | The resolved platform font family
    , platformFontFamily :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON FontFace where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "FontFace" $ \_o -> FontFace
            <$> _o .: "fontFamily"
            <*> _o .: "fontStyle"
            <*> _o .: "fontVariant"
            <*> _o .: "fontWeight"
            <*> _o .: "fontStretch"
            <*> _o .: "unicodeRange"
            <*> _o .: "src"
            <*> _o .: "platformFontFamily"
        ago = A.withArray "FontFace" $ \_a -> FontFace
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.maybe P.empty A.parseJSON (_a !? 6)
            <*> P.maybe P.empty A.parseJSON (_a !? 7)


------------------------------------------------------------------------------
instance A.ToJSON FontFace where
    toEncoding (FontFace _0 _1 _2 _3 _4 _5 _6 _7) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "fontFamily" .= _0
        , P.pure $ "fontStyle" .= _1
        , P.pure $ "fontVariant" .= _2
        , P.pure $ "fontWeight" .= _3
        , P.pure $ "fontStretch" .= _4
        , P.pure $ "unicodeRange" .= _5
        , P.pure $ "src" .= _6
        , P.pure $ "platformFontFamily" .= _7
        ]
    toJSON (FontFace _0 _1 _2 _3 _4 _5 _6 _7) = A.object $ P.catMaybes
        [ P.pure $ "fontFamily" .= _0
        , P.pure $ "fontStyle" .= _1
        , P.pure $ "fontVariant" .= _2
        , P.pure $ "fontWeight" .= _3
        , P.pure $ "fontStretch" .= _4
        , P.pure $ "unicodeRange" .= _5
        , P.pure $ "src" .= _6
        , P.pure $ "platformFontFamily" .= _7
        ]


------------------------------------------------------------------------------
instance P.Semigroup FontFace where
    FontFace _0 _1 _2 _3 _4 _5 _6 _7 <> FontFace _ _ _ _ _ _ _ _ = FontFace _0 _1 _2 _3 _4 _5 _6 _7


------------------------------------------------------------------------------
-- | CSS keyframes rule representation.
data CSSKeyframesRule = CSSKeyframesRule
    { -- | Animation name.
      animationName :: !Value
      -- | List of keyframes.
    , keyframes :: ![CSSKeyframeRule]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CSSKeyframesRule where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "CSSKeyframesRule" $ \_o -> CSSKeyframesRule
            <$> _o .: "animationName"
            <*> _o .: "keyframes"
        ago = A.withArray "CSSKeyframesRule" $ \_a -> CSSKeyframesRule
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON CSSKeyframesRule where
    toEncoding (CSSKeyframesRule _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "animationName" .= _0
        , P.pure $ "keyframes" .= _1
        ]
    toJSON (CSSKeyframesRule _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "animationName" .= _0
        , P.pure $ "keyframes" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup CSSKeyframesRule where
    CSSKeyframesRule _0 _1 <> CSSKeyframesRule _ _ = CSSKeyframesRule _0 _1


------------------------------------------------------------------------------
-- | CSS keyframe rule representation.
data CSSKeyframeRule = CSSKeyframeRule
    { -- | The css style sheet identifier (absent for user agent stylesheet and user-specified
      -- stylesheet rules) this rule came from.
      styleSheetId :: !(P.Maybe StyleSheetId)
      -- | Parent stylesheet's origin.
    , origin :: !StyleSheetOrigin
      -- | Associated key text.
    , keyText :: !Value
      -- | Associated style declaration.
    , style :: !CSSStyle
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CSSKeyframeRule where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "CSSKeyframeRule" $ \_o -> CSSKeyframeRule
            <$> _o .:? "styleSheetId"
            <*> _o .: "origin"
            <*> _o .: "keyText"
            <*> _o .: "style"
        ago = A.withArray "CSSKeyframeRule" $ \_a -> CSSKeyframeRule
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON CSSKeyframeRule where
    toEncoding (CSSKeyframeRule _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ ("styleSheetId" .=) <$> _0
        , P.pure $ "origin" .= _1
        , P.pure $ "keyText" .= _2
        , P.pure $ "style" .= _3
        ]
    toJSON (CSSKeyframeRule _0 _1 _2 _3) = A.object $ P.catMaybes
        [ ("styleSheetId" .=) <$> _0
        , P.pure $ "origin" .= _1
        , P.pure $ "keyText" .= _2
        , P.pure $ "style" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup CSSKeyframeRule where
    CSSKeyframeRule _0 _1 _2 _3 <> CSSKeyframeRule __0 _ _ _ = CSSKeyframeRule (_0 <|> __0) _1 _2 _3


------------------------------------------------------------------------------
-- | A descriptor of operation to mutate style declaration text.
data StyleDeclarationEdit = StyleDeclarationEdit
    { -- | The css style sheet identifier.
      styleSheetId :: !StyleSheetId
      -- | The range of the style text in the enclosing stylesheet.
    , range :: !SourceRange
      -- | New style text.
    , text :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StyleDeclarationEdit where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "StyleDeclarationEdit" $ \_o -> StyleDeclarationEdit
            <$> _o .: "styleSheetId"
            <*> _o .: "range"
            <*> _o .: "text"
        ago = A.withArray "StyleDeclarationEdit" $ \_a -> StyleDeclarationEdit
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON StyleDeclarationEdit where
    toEncoding (StyleDeclarationEdit _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "styleSheetId" .= _0
        , P.pure $ "range" .= _1
        , P.pure $ "text" .= _2
        ]
    toJSON (StyleDeclarationEdit _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "styleSheetId" .= _0
        , P.pure $ "range" .= _1
        , P.pure $ "text" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup StyleDeclarationEdit where
    StyleDeclarationEdit _0 _1 _2 <> StyleDeclarationEdit _ _ _ = StyleDeclarationEdit _0 _1 _2

