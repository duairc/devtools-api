{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.LayerTree.Types{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

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
-- | Unique Layer identifier.
type LayerId = T.Text


------------------------------------------------------------------------------
-- | Unique snapshot identifier.
type SnapshotId = T.Text


------------------------------------------------------------------------------
-- | Rectangle where scrolling happens on the main thread.
data ScrollRect = ScrollRect
    { -- | Rectangle itself.
      rect :: !DOM.Rect
      -- | Reason for rectangle to force scrolling on the main thread
    , type_ :: !Type
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ScrollRect where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "ScrollRect" $ \_o -> ScrollRect
            <$> _o .: "rect"
            <*> _o .: "type"
        ago = A.withArray "ScrollRect" $ \_a -> ScrollRect
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON ScrollRect where
    toEncoding (ScrollRect _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "rect" .= _0
        , P.pure $ "type" .= _1
        ]
    toJSON (ScrollRect _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "rect" .= _0
        , P.pure $ "type" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup ScrollRect where
    ScrollRect _0 _1 <> ScrollRect _ _ = ScrollRect _0 _1


------------------------------------------------------------------------------
data Type
    = RepaintsOnScroll
    | TouchEventHandler
    | WheelEventHandler
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Type where
    parseJSON = A.withText "Type" $ \t -> case t of
        "RepaintsOnScroll" -> P.pure RepaintsOnScroll
        "TouchEventHandler" -> P.pure TouchEventHandler
        "WheelEventHandler" -> P.pure WheelEventHandler
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Type where
    toJSON RepaintsOnScroll = "RepaintsOnScroll"
    toJSON TouchEventHandler = "TouchEventHandler"
    toJSON WheelEventHandler = "WheelEventHandler"


------------------------------------------------------------------------------
-- | Sticky position constraints.
data StickyPositionConstraint = StickyPositionConstraint
    { -- | Layout rectangle of the sticky element before being shifted
      stickyBoxRect :: !DOM.Rect
      -- | Layout rectangle of the containing block of the sticky element
    , containingBlockRect :: !DOM.Rect
      -- | The nearest sticky layer that shifts the sticky box
    , nearestLayerShiftingStickyBox :: !(P.Maybe LayerId)
      -- | The nearest sticky layer that shifts the containing block
    , nearestLayerShiftingContainingBlock :: !(P.Maybe LayerId)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StickyPositionConstraint where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "StickyPositionConstraint" $ \_o -> StickyPositionConstraint
            <$> _o .: "stickyBoxRect"
            <*> _o .: "containingBlockRect"
            <*> _o .:? "nearestLayerShiftingStickyBox"
            <*> _o .:? "nearestLayerShiftingContainingBlock"
        ago = A.withArray "StickyPositionConstraint" $ \_a -> StickyPositionConstraint
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON StickyPositionConstraint where
    toEncoding (StickyPositionConstraint _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "stickyBoxRect" .= _0
        , P.pure $ "containingBlockRect" .= _1
        , ("nearestLayerShiftingStickyBox" .=) <$> _2
        , ("nearestLayerShiftingContainingBlock" .=) <$> _3
        ]
    toJSON (StickyPositionConstraint _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "stickyBoxRect" .= _0
        , P.pure $ "containingBlockRect" .= _1
        , ("nearestLayerShiftingStickyBox" .=) <$> _2
        , ("nearestLayerShiftingContainingBlock" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup StickyPositionConstraint where
    StickyPositionConstraint _0 _1 _2 _3 <> StickyPositionConstraint _ _ __2 __3 = StickyPositionConstraint _0 _1 (_2 <|> __2) (_3 <|> __3)


------------------------------------------------------------------------------
-- | Serialized fragment of layer picture along with its offset within the layer.
data PictureTile = PictureTile
    { -- | Offset from owning layer left boundary
      x :: !P.Double
      -- | Offset from owning layer top boundary
    , y :: !P.Double
      -- | Base64-encoded snapshot data.
    , picture :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PictureTile where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "PictureTile" $ \_o -> PictureTile
            <$> _o .: "x"
            <*> _o .: "y"
            <*> _o .: "picture"
        ago = A.withArray "PictureTile" $ \_a -> PictureTile
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON PictureTile where
    toEncoding (PictureTile _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "x" .= _0
        , P.pure $ "y" .= _1
        , P.pure $ "picture" .= _2
        ]
    toJSON (PictureTile _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "x" .= _0
        , P.pure $ "y" .= _1
        , P.pure $ "picture" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup PictureTile where
    PictureTile _0 _1 _2 <> PictureTile _ _ _ = PictureTile _0 _1 _2


------------------------------------------------------------------------------
-- | Information about a compositing layer.
data Layer = Layer
    { -- | The unique id for this layer.
      layerId :: !LayerId
      -- | The id of parent (not present for root).
    , parentLayerId :: !(P.Maybe LayerId)
      -- | The backend id for the node associated with this layer.
    , backendNodeId :: !(P.Maybe DOM.BackendNodeId)
      -- | Offset from parent layer, X coordinate.
    , offsetX :: !P.Double
      -- | Offset from parent layer, Y coordinate.
    , offsetY :: !P.Double
      -- | Layer width.
    , width :: !P.Double
      -- | Layer height.
    , height :: !P.Double
      -- | Transformation matrix for layer, default is identity matrix
    , transform :: !(P.Maybe [P.Double])
      -- | Transform anchor point X, absent if no transform specified
    , anchorX :: !(P.Maybe P.Double)
      -- | Transform anchor point Y, absent if no transform specified
    , anchorY :: !(P.Maybe P.Double)
      -- | Transform anchor point Z, absent if no transform specified
    , anchorZ :: !(P.Maybe P.Double)
      -- | Indicates how many time this layer has painted.
    , paintCount :: !P.Int
      -- | Indicates whether this layer hosts any content, rather than being used for
      -- transform\/scrolling purposes only.
    , drawsContent :: !P.Bool
      -- | Set if layer is not visible.
    , invisible :: !(P.Maybe P.Bool)
      -- | Rectangles scrolling on main thread only.
    , scrollRects :: !(P.Maybe [ScrollRect])
      -- | Sticky position constraint information
    , stickyPositionConstraint :: !(P.Maybe StickyPositionConstraint)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Layer where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "Layer" $ \_o -> Layer
            <$> _o .: "layerId"
            <*> _o .:? "parentLayerId"
            <*> _o .:? "backendNodeId"
            <*> _o .: "offsetX"
            <*> _o .: "offsetY"
            <*> _o .: "width"
            <*> _o .: "height"
            <*> _o .:? "transform"
            <*> _o .:? "anchorX"
            <*> _o .:? "anchorY"
            <*> _o .:? "anchorZ"
            <*> _o .: "paintCount"
            <*> _o .: "drawsContent"
            <*> _o .:? "invisible"
            <*> _o .:? "scrollRects"
            <*> _o .:? "stickyPositionConstraint"
        ago = A.withArray "Layer" $ \_a -> Layer
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.maybe P.empty A.parseJSON (_a !? 6)
            <*> P.traverse A.parseJSON (_a !? 7)
            <*> P.traverse A.parseJSON (_a !? 8)
            <*> P.traverse A.parseJSON (_a !? 9)
            <*> P.traverse A.parseJSON (_a !? 10)
            <*> P.maybe P.empty A.parseJSON (_a !? 11)
            <*> P.maybe P.empty A.parseJSON (_a !? 12)
            <*> P.traverse A.parseJSON (_a !? 13)
            <*> P.traverse A.parseJSON (_a !? 14)
            <*> P.traverse A.parseJSON (_a !? 15)


------------------------------------------------------------------------------
instance A.ToJSON Layer where
    toEncoding (Layer _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 _15) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "layerId" .= _0
        , ("parentLayerId" .=) <$> _1
        , ("backendNodeId" .=) <$> _2
        , P.pure $ "offsetX" .= _3
        , P.pure $ "offsetY" .= _4
        , P.pure $ "width" .= _5
        , P.pure $ "height" .= _6
        , ("transform" .=) <$> _7
        , ("anchorX" .=) <$> _8
        , ("anchorY" .=) <$> _9
        , ("anchorZ" .=) <$> _10
        , P.pure $ "paintCount" .= _11
        , P.pure $ "drawsContent" .= _12
        , ("invisible" .=) <$> _13
        , ("scrollRects" .=) <$> _14
        , ("stickyPositionConstraint" .=) <$> _15
        ]
    toJSON (Layer _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 _15) = A.object $ P.catMaybes
        [ P.pure $ "layerId" .= _0
        , ("parentLayerId" .=) <$> _1
        , ("backendNodeId" .=) <$> _2
        , P.pure $ "offsetX" .= _3
        , P.pure $ "offsetY" .= _4
        , P.pure $ "width" .= _5
        , P.pure $ "height" .= _6
        , ("transform" .=) <$> _7
        , ("anchorX" .=) <$> _8
        , ("anchorY" .=) <$> _9
        , ("anchorZ" .=) <$> _10
        , P.pure $ "paintCount" .= _11
        , P.pure $ "drawsContent" .= _12
        , ("invisible" .=) <$> _13
        , ("scrollRects" .=) <$> _14
        , ("stickyPositionConstraint" .=) <$> _15
        ]


------------------------------------------------------------------------------
instance P.Semigroup Layer where
    Layer _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 _15 <> Layer _ __1 __2 _ _ _ _ __7 __8 __9 __10 _ _ __13 __14 __15 = Layer _0 (_1 <|> __1) (_2 <|> __2) _3 _4 _5 _6 (_7 <|> __7) (_8 <|> __8) (_9 <|> __9) (_10 <|> __10) _11 _12 (_13 <|> __13) (_14 <|> __14) (_15 <|> __15)


------------------------------------------------------------------------------
-- | Array of timings, one per paint step.
type PaintProfile = [P.Double]

