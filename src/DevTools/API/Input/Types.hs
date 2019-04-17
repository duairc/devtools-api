{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.Input.Types
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


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
data TouchPoint = TouchPoint
    { -- | X coordinate of the event relative to the main frame's viewport in CSS pixels.
      x :: !P.Double
      -- | Y coordinate of the event relative to the main frame's viewport in CSS pixels. 0 refers to
      -- the top of the viewport and Y increases as it proceeds towards the bottom of the viewport.
    , y :: !P.Double
      -- | X radius of the touch area (default: 1.0).
    , radiusX :: !(P.Maybe P.Double)
      -- | Y radius of the touch area (default: 1.0).
    , radiusY :: !(P.Maybe P.Double)
      -- | Rotation angle (default: 0.0).
    , rotationAngle :: !(P.Maybe P.Double)
      -- | Force (default: 1.0).
    , force :: !(P.Maybe P.Double)
      -- | Identifier used to track touch sources between events, must be unique within an event.
    , id :: !(P.Maybe P.Double)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TouchPoint where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "TouchPoint" $ \_o -> TouchPoint
            <$> _o .: "x"
            <*> _o .: "y"
            <*> _o .:? "radiusX"
            <*> _o .:? "radiusY"
            <*> _o .:? "rotationAngle"
            <*> _o .:? "force"
            <*> _o .:? "id"
        ago = A.withArray "TouchPoint" $ \_a -> TouchPoint
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)


------------------------------------------------------------------------------
instance A.ToJSON TouchPoint where
    toEncoding (TouchPoint _0 _1 _2 _3 _4 _5 _6) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "x" .= _0
        , P.pure $ "y" .= _1
        , ("radiusX" .=) <$> _2
        , ("radiusY" .=) <$> _3
        , ("rotationAngle" .=) <$> _4
        , ("force" .=) <$> _5
        , ("id" .=) <$> _6
        ]
    toJSON (TouchPoint _0 _1 _2 _3 _4 _5 _6) = A.object $ P.catMaybes
        [ P.pure $ "x" .= _0
        , P.pure $ "y" .= _1
        , ("radiusX" .=) <$> _2
        , ("radiusY" .=) <$> _3
        , ("rotationAngle" .=) <$> _4
        , ("force" .=) <$> _5
        , ("id" .=) <$> _6
        ]


------------------------------------------------------------------------------
instance P.Semigroup TouchPoint where
    TouchPoint _0 _1 _2 _3 _4 _5 _6 <> TouchPoint _ _ __2 __3 __4 __5 __6 = TouchPoint _0 _1 (_2 <|> __2) (_3 <|> __3) (_4 <|> __4) (_5 <|> __5) (_6 <|> __6)


------------------------------------------------------------------------------
{-# WARNING GestureSourceType "This feature is marked as EXPERIMENTAL." #-}
data GestureSourceType
    = Default
    | Touch
    | Mouse
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GestureSourceType where
    parseJSON = A.withText "GestureSourceType" $ \t -> case t of
        "default" -> P.pure Default
        "touch" -> P.pure Touch
        "mouse" -> P.pure Mouse
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON GestureSourceType where
    toJSON Default = "default"
    toJSON Touch = "touch"
    toJSON Mouse = "mouse"


------------------------------------------------------------------------------
-- | UTC time in seconds, counted from January 1, 1970.
type TimeSinceEpoch = P.Double

