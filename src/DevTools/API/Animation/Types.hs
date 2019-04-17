{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.Animation.Types{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

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
-- | Animation instance.
data Animation = Animation
    { -- | @Animation@'s id.
      id :: !T.Text
      -- | @Animation@'s name.
    , name :: !T.Text
      -- | @Animation@'s internal paused state.
    , pausedState :: !P.Bool
      -- | @Animation@'s play state.
    , playState :: !T.Text
      -- | @Animation@'s playback rate.
    , playbackRate :: !P.Double
      -- | @Animation@'s start time.
    , startTime :: !P.Double
      -- | @Animation@'s current time.
    , currentTime :: !P.Double
      -- | Animation type of @Animation@.
    , type_ :: !Type
      -- | @Animation@'s source animation node.
    , source :: !(P.Maybe AnimationEffect)
      -- | A unique ID for @Animation@ representing the sources that triggered this CSS
      -- animation\/transition.
    , cssId :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Animation where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "Animation" $ \_o -> Animation
            <$> _o .: "id"
            <*> _o .: "name"
            <*> _o .: "pausedState"
            <*> _o .: "playState"
            <*> _o .: "playbackRate"
            <*> _o .: "startTime"
            <*> _o .: "currentTime"
            <*> _o .: "type"
            <*> _o .:? "source"
            <*> _o .:? "cssId"
        ago = A.withArray "Animation" $ \_a -> Animation
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.maybe P.empty A.parseJSON (_a !? 6)
            <*> P.maybe P.empty A.parseJSON (_a !? 7)
            <*> P.traverse A.parseJSON (_a !? 8)
            <*> P.traverse A.parseJSON (_a !? 9)


------------------------------------------------------------------------------
instance A.ToJSON Animation where
    toEncoding (Animation _0 _1 _2 _3 _4 _5 _6 _7 _8 _9) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "id" .= _0
        , P.pure $ "name" .= _1
        , P.pure $ "pausedState" .= _2
        , P.pure $ "playState" .= _3
        , P.pure $ "playbackRate" .= _4
        , P.pure $ "startTime" .= _5
        , P.pure $ "currentTime" .= _6
        , P.pure $ "type" .= _7
        , ("source" .=) <$> _8
        , ("cssId" .=) <$> _9
        ]
    toJSON (Animation _0 _1 _2 _3 _4 _5 _6 _7 _8 _9) = A.object $ P.catMaybes
        [ P.pure $ "id" .= _0
        , P.pure $ "name" .= _1
        , P.pure $ "pausedState" .= _2
        , P.pure $ "playState" .= _3
        , P.pure $ "playbackRate" .= _4
        , P.pure $ "startTime" .= _5
        , P.pure $ "currentTime" .= _6
        , P.pure $ "type" .= _7
        , ("source" .=) <$> _8
        , ("cssId" .=) <$> _9
        ]


------------------------------------------------------------------------------
instance P.Semigroup Animation where
    Animation _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 <> Animation _ _ _ _ _ _ _ _ __8 __9 = Animation _0 _1 _2 _3 _4 _5 _6 _7 (_8 <|> __8) (_9 <|> __9)


------------------------------------------------------------------------------
data Type
    = CSSTransition
    | CSSAnimation
    | WebAnimation
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Type where
    parseJSON = A.withText "Type" $ \t -> case t of
        "CSSTransition" -> P.pure CSSTransition
        "CSSAnimation" -> P.pure CSSAnimation
        "WebAnimation" -> P.pure WebAnimation
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Type where
    toJSON CSSTransition = "CSSTransition"
    toJSON CSSAnimation = "CSSAnimation"
    toJSON WebAnimation = "WebAnimation"


------------------------------------------------------------------------------
-- | AnimationEffect instance
data AnimationEffect = AnimationEffect
    { -- | @AnimationEffect@'s delay.
      delay :: !P.Double
      -- | @AnimationEffect@'s end delay.
    , endDelay :: !P.Double
      -- | @AnimationEffect@'s iteration start.
    , iterationStart :: !P.Double
      -- | @AnimationEffect@'s iterations.
    , iterations :: !P.Double
      -- | @AnimationEffect@'s iteration duration.
    , duration :: !P.Double
      -- | @AnimationEffect@'s playback direction.
    , direction :: !T.Text
      -- | @AnimationEffect@'s fill mode.
    , fill :: !T.Text
      -- | @AnimationEffect@'s target node.
    , backendNodeId :: !(P.Maybe DOM.BackendNodeId)
      -- | @AnimationEffect@'s keyframes.
    , keyframesRule :: !(P.Maybe KeyframesRule)
      -- | @AnimationEffect@'s timing function.
    , easing :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AnimationEffect where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "AnimationEffect" $ \_o -> AnimationEffect
            <$> _o .: "delay"
            <*> _o .: "endDelay"
            <*> _o .: "iterationStart"
            <*> _o .: "iterations"
            <*> _o .: "duration"
            <*> _o .: "direction"
            <*> _o .: "fill"
            <*> _o .:? "backendNodeId"
            <*> _o .:? "keyframesRule"
            <*> _o .: "easing"
        ago = A.withArray "AnimationEffect" $ \_a -> AnimationEffect
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.maybe P.empty A.parseJSON (_a !? 6)
            <*> P.traverse A.parseJSON (_a !? 7)
            <*> P.traverse A.parseJSON (_a !? 8)
            <*> P.maybe P.empty A.parseJSON (_a !? 9)


------------------------------------------------------------------------------
instance A.ToJSON AnimationEffect where
    toEncoding (AnimationEffect _0 _1 _2 _3 _4 _5 _6 _7 _8 _9) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "delay" .= _0
        , P.pure $ "endDelay" .= _1
        , P.pure $ "iterationStart" .= _2
        , P.pure $ "iterations" .= _3
        , P.pure $ "duration" .= _4
        , P.pure $ "direction" .= _5
        , P.pure $ "fill" .= _6
        , ("backendNodeId" .=) <$> _7
        , ("keyframesRule" .=) <$> _8
        , P.pure $ "easing" .= _9
        ]
    toJSON (AnimationEffect _0 _1 _2 _3 _4 _5 _6 _7 _8 _9) = A.object $ P.catMaybes
        [ P.pure $ "delay" .= _0
        , P.pure $ "endDelay" .= _1
        , P.pure $ "iterationStart" .= _2
        , P.pure $ "iterations" .= _3
        , P.pure $ "duration" .= _4
        , P.pure $ "direction" .= _5
        , P.pure $ "fill" .= _6
        , ("backendNodeId" .=) <$> _7
        , ("keyframesRule" .=) <$> _8
        , P.pure $ "easing" .= _9
        ]


------------------------------------------------------------------------------
instance P.Semigroup AnimationEffect where
    AnimationEffect _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 <> AnimationEffect _ _ _ _ _ _ _ __7 __8 _ = AnimationEffect _0 _1 _2 _3 _4 _5 _6 (_7 <|> __7) (_8 <|> __8) _9


------------------------------------------------------------------------------
-- | Keyframes Rule
data KeyframesRule = KeyframesRule
    { -- | CSS keyframed animation's name.
      name :: !(P.Maybe T.Text)
      -- | List of animation keyframes.
    , keyframes :: ![KeyframeStyle]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON KeyframesRule where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "KeyframesRule" $ \_o -> KeyframesRule
            <$> _o .:? "name"
            <*> _o .: "keyframes"
        ago = A.withArray "KeyframesRule" $ \_a -> KeyframesRule
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON KeyframesRule where
    toEncoding (KeyframesRule _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ ("name" .=) <$> _0
        , P.pure $ "keyframes" .= _1
        ]
    toJSON (KeyframesRule _0 _1) = A.object $ P.catMaybes
        [ ("name" .=) <$> _0
        , P.pure $ "keyframes" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup KeyframesRule where
    KeyframesRule _0 _1 <> KeyframesRule __0 _ = KeyframesRule (_0 <|> __0) _1


------------------------------------------------------------------------------
-- | Keyframe Style
data KeyframeStyle = KeyframeStyle
    { -- | Keyframe's time offset.
      offset :: !T.Text
      -- | @AnimationEffect@'s timing function.
    , easing :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON KeyframeStyle where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "KeyframeStyle" $ \_o -> KeyframeStyle
            <$> _o .: "offset"
            <*> _o .: "easing"
        ago = A.withArray "KeyframeStyle" $ \_a -> KeyframeStyle
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON KeyframeStyle where
    toEncoding (KeyframeStyle _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "offset" .= _0
        , P.pure $ "easing" .= _1
        ]
    toJSON (KeyframeStyle _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "offset" .= _0
        , P.pure $ "easing" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup KeyframeStyle where
    KeyframeStyle _0 _1 <> KeyframeStyle _ _ = KeyframeStyle _0 _1

