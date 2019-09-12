{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | This domain allows detailed inspection of media elements
module DevTools.API.Media{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

    ( module DevTools.API.Media.Types
    , module DevTools.API.Media
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
import           DevTools.API.Media.Types


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Enables the Media domain
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
    name _ = "Media.enable"


------------------------------------------------------------------------------
-- | Enables the Media domain
enable
    :: Enable
enable = Enable


------------------------------------------------------------------------------
-- | Disables the Media domain.
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
    name _ = "Media.disable"


------------------------------------------------------------------------------
-- | Disables the Media domain.
disable
    :: Disable
disable = Disable


------------------------------------------------------------------------------
-- | This can be called multiple times, and can be used to set \/ override \/
-- remove player properties. A null propValue indicates removal.
data PlayerPropertiesChanged = PlayerPropertiesChanged
    { playerId :: !PlayerId
    , properties :: ![PlayerProperty]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PlayerPropertiesChanged where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "playerPropertiesChanged" $ \_o -> PlayerPropertiesChanged
            <$> _o .: "playerId"
            <*> _o .: "properties"
        ago = A.withArray "playerPropertiesChanged" $ \_a -> PlayerPropertiesChanged
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON PlayerPropertiesChanged where
    toEncoding (PlayerPropertiesChanged _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "playerId" .= _0
        , P.pure $ "properties" .= _1
        ]
    toJSON (PlayerPropertiesChanged _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "playerId" .= _0
        , P.pure $ "properties" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup PlayerPropertiesChanged where
    PlayerPropertiesChanged _0 _1 <> PlayerPropertiesChanged _ _ = PlayerPropertiesChanged _0 _1


------------------------------------------------------------------------------
instance E.Event PlayerPropertiesChanged where
    type Result PlayerPropertiesChanged = PlayerPropertiesChanged
    name _ = "Media.playerPropertiesChanged"


------------------------------------------------------------------------------
-- | This can be called multiple times, and can be used to set \/ override \/
-- remove player properties. A null propValue indicates removal.
playerPropertiesChanged :: P.Proxy PlayerPropertiesChanged
playerPropertiesChanged = P.Proxy


------------------------------------------------------------------------------
-- | Send events as a list, allowing them to be batched on the browser for less
-- congestion. If batched, events must ALWAYS be in chronological order.
data PlayerEventsAdded = PlayerEventsAdded
    { playerId :: !PlayerId
    , events :: ![PlayerEvent]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PlayerEventsAdded where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "playerEventsAdded" $ \_o -> PlayerEventsAdded
            <$> _o .: "playerId"
            <*> _o .: "events"
        ago = A.withArray "playerEventsAdded" $ \_a -> PlayerEventsAdded
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON PlayerEventsAdded where
    toEncoding (PlayerEventsAdded _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "playerId" .= _0
        , P.pure $ "events" .= _1
        ]
    toJSON (PlayerEventsAdded _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "playerId" .= _0
        , P.pure $ "events" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup PlayerEventsAdded where
    PlayerEventsAdded _0 _1 <> PlayerEventsAdded _ _ = PlayerEventsAdded _0 _1


------------------------------------------------------------------------------
instance E.Event PlayerEventsAdded where
    type Result PlayerEventsAdded = PlayerEventsAdded
    name _ = "Media.playerEventsAdded"


------------------------------------------------------------------------------
-- | Send events as a list, allowing them to be batched on the browser for less
-- congestion. If batched, events must ALWAYS be in chronological order.
playerEventsAdded :: P.Proxy PlayerEventsAdded
playerEventsAdded = P.Proxy


------------------------------------------------------------------------------
-- | Called whenever a player is created, or when a new agent joins and recieves
-- a list of active players. If an agent is restored, it will recieve the full
-- list of player ids and all events again.
data PlayersCreated = PlayersCreated
    { players :: ![PlayerId]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PlayersCreated where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "playersCreated" $ \_o -> PlayersCreated
            <$> _o .: "players"
        ago = A.withArray "playersCreated" $ \_a -> PlayersCreated
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON PlayersCreated where
    toEncoding (PlayersCreated _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "players" .= _0
        ]
    toJSON (PlayersCreated _0) = A.object $ P.catMaybes
        [ P.pure $ "players" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup PlayersCreated where
    PlayersCreated _0 <> PlayersCreated _ = PlayersCreated _0


------------------------------------------------------------------------------
instance E.Event PlayersCreated where
    type Result PlayersCreated = PlayersCreated
    name _ = "Media.playersCreated"


------------------------------------------------------------------------------
-- | Called whenever a player is created, or when a new agent joins and recieves
-- a list of active players. If an agent is restored, it will recieve the full
-- list of player ids and all events again.
playersCreated :: P.Proxy PlayersCreated
playersCreated = P.Proxy

