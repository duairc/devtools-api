{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | This domain provides various functionality related to drawing atop the inspected page.
module DevTools.API.Overlay.Types{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

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
-- | Configuration data for the highlighting of page elements.
data HighlightConfig = HighlightConfig
    { -- | Whether the node info tooltip should be shown (default: false).
      showInfo :: !(P.Maybe P.Bool)
      -- | Whether the node styles in the tooltip (default: false).
    , showStyles :: !(P.Maybe P.Bool)
      -- | Whether the rulers should be shown (default: false).
    , showRulers :: !(P.Maybe P.Bool)
      -- | Whether the extension lines from node to the rulers should be shown (default: false).
    , showExtensionLines :: !(P.Maybe P.Bool)
      -- | The content box highlight fill color (default: transparent).
    , contentColor :: !(P.Maybe DOM.RGBA)
      -- | The padding highlight fill color (default: transparent).
    , paddingColor :: !(P.Maybe DOM.RGBA)
      -- | The border highlight fill color (default: transparent).
    , borderColor :: !(P.Maybe DOM.RGBA)
      -- | The margin highlight fill color (default: transparent).
    , marginColor :: !(P.Maybe DOM.RGBA)
      -- | The event target element highlight fill color (default: transparent).
    , eventTargetColor :: !(P.Maybe DOM.RGBA)
      -- | The shape outside fill color (default: transparent).
    , shapeColor :: !(P.Maybe DOM.RGBA)
      -- | The shape margin fill color (default: transparent).
    , shapeMarginColor :: !(P.Maybe DOM.RGBA)
      -- | The grid layout color (default: transparent).
    , cssGridColor :: !(P.Maybe DOM.RGBA)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON HighlightConfig where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "HighlightConfig" $ \_o -> HighlightConfig
            <$> _o .:? "showInfo"
            <*> _o .:? "showStyles"
            <*> _o .:? "showRulers"
            <*> _o .:? "showExtensionLines"
            <*> _o .:? "contentColor"
            <*> _o .:? "paddingColor"
            <*> _o .:? "borderColor"
            <*> _o .:? "marginColor"
            <*> _o .:? "eventTargetColor"
            <*> _o .:? "shapeColor"
            <*> _o .:? "shapeMarginColor"
            <*> _o .:? "cssGridColor"
        ago = A.withArray "HighlightConfig" $ \_a -> HighlightConfig
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


------------------------------------------------------------------------------
instance A.ToJSON HighlightConfig where
    toEncoding (HighlightConfig _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11) = A.pairs $ P.fold $ P.catMaybes
        [ ("showInfo" .=) <$> _0
        , ("showStyles" .=) <$> _1
        , ("showRulers" .=) <$> _2
        , ("showExtensionLines" .=) <$> _3
        , ("contentColor" .=) <$> _4
        , ("paddingColor" .=) <$> _5
        , ("borderColor" .=) <$> _6
        , ("marginColor" .=) <$> _7
        , ("eventTargetColor" .=) <$> _8
        , ("shapeColor" .=) <$> _9
        , ("shapeMarginColor" .=) <$> _10
        , ("cssGridColor" .=) <$> _11
        ]
    toJSON (HighlightConfig _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11) = A.object $ P.catMaybes
        [ ("showInfo" .=) <$> _0
        , ("showStyles" .=) <$> _1
        , ("showRulers" .=) <$> _2
        , ("showExtensionLines" .=) <$> _3
        , ("contentColor" .=) <$> _4
        , ("paddingColor" .=) <$> _5
        , ("borderColor" .=) <$> _6
        , ("marginColor" .=) <$> _7
        , ("eventTargetColor" .=) <$> _8
        , ("shapeColor" .=) <$> _9
        , ("shapeMarginColor" .=) <$> _10
        , ("cssGridColor" .=) <$> _11
        ]


------------------------------------------------------------------------------
instance P.Semigroup HighlightConfig where
    HighlightConfig _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 <> HighlightConfig __0 __1 __2 __3 __4 __5 __6 __7 __8 __9 __10 __11 = HighlightConfig (_0 <|> __0) (_1 <|> __1) (_2 <|> __2) (_3 <|> __3) (_4 <|> __4) (_5 <|> __5) (_6 <|> __6) (_7 <|> __7) (_8 <|> __8) (_9 <|> __9) (_10 <|> __10) (_11 <|> __11)


------------------------------------------------------------------------------
instance P.Monoid HighlightConfig where
    mempty = HighlightConfig P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
data InspectMode
    = SearchForNode
    | SearchForUAShadowDOM
    | CaptureAreaScreenshot
    | ShowDistances
    | None
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON InspectMode where
    parseJSON = A.withText "InspectMode" $ \t -> case t of
        "searchForNode" -> P.pure SearchForNode
        "searchForUAShadowDOM" -> P.pure SearchForUAShadowDOM
        "captureAreaScreenshot" -> P.pure CaptureAreaScreenshot
        "showDistances" -> P.pure ShowDistances
        "none" -> P.pure None
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON InspectMode where
    toJSON SearchForNode = "searchForNode"
    toJSON SearchForUAShadowDOM = "searchForUAShadowDOM"
    toJSON CaptureAreaScreenshot = "captureAreaScreenshot"
    toJSON ShowDistances = "showDistances"
    toJSON None = "none"

