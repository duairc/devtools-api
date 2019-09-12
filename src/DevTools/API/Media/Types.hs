{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | This domain allows detailed inspection of media elements
module DevTools.API.Media.Types{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

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
-- | Players will get an ID that is unique within the agent context.
type PlayerId = T.Text


------------------------------------------------------------------------------
type Timestamp = P.Double


------------------------------------------------------------------------------
-- | Player Property type
data PlayerProperty = PlayerProperty
    { name :: !T.Text
    , value :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PlayerProperty where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "PlayerProperty" $ \_o -> PlayerProperty
            <$> _o .: "name"
            <*> _o .:? "value"
        ago = A.withArray "PlayerProperty" $ \_a -> PlayerProperty
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON PlayerProperty where
    toEncoding (PlayerProperty _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "name" .= _0
        , ("value" .=) <$> _1
        ]
    toJSON (PlayerProperty _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "name" .= _0
        , ("value" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup PlayerProperty where
    PlayerProperty _0 _1 <> PlayerProperty _ __1 = PlayerProperty _0 (_1 <|> __1)


------------------------------------------------------------------------------
-- | Break out events into different types
data PlayerEventType
    = PlaybackEvent
    | SystemEvent
    | MessageEvent
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PlayerEventType where
    parseJSON = A.withText "PlayerEventType" $ \t -> case t of
        "playbackEvent" -> P.pure PlaybackEvent
        "systemEvent" -> P.pure SystemEvent
        "messageEvent" -> P.pure MessageEvent
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON PlayerEventType where
    toJSON PlaybackEvent = "playbackEvent"
    toJSON SystemEvent = "systemEvent"
    toJSON MessageEvent = "messageEvent"


------------------------------------------------------------------------------
data PlayerEvent = PlayerEvent
    { type_ :: !PlayerEventType
      -- | Events are timestamped relative to the start of the player creation
      -- not relative to the start of playback.
    , timestamp :: !Timestamp
    , name :: !T.Text
    , value :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PlayerEvent where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "PlayerEvent" $ \_o -> PlayerEvent
            <$> _o .: "type"
            <*> _o .: "timestamp"
            <*> _o .: "name"
            <*> _o .: "value"
        ago = A.withArray "PlayerEvent" $ \_a -> PlayerEvent
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON PlayerEvent where
    toEncoding (PlayerEvent _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "type" .= _0
        , P.pure $ "timestamp" .= _1
        , P.pure $ "name" .= _2
        , P.pure $ "value" .= _3
        ]
    toJSON (PlayerEvent _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "type" .= _0
        , P.pure $ "timestamp" .= _1
        , P.pure $ "name" .= _2
        , P.pure $ "value" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup PlayerEvent where
    PlayerEvent _0 _1 _2 _3 <> PlayerEvent _ _ _ _ = PlayerEvent _0 _1 _2 _3

