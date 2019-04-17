{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | This domain is deprecated - use Runtime or Log instead.
module DevTools.API.Console{-{-# DEPRECATED "This may be removed in a future release." #-}-}

    ( module DevTools.API.Console.Types
    , module DevTools.API.Console
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
import           DevTools.API.Console.Types


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Does nothing.
data ClearMessages = ClearMessages
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ClearMessages where
    parseJSON A.Null = P.pure ClearMessages
    parseJSON v = A.withArray "clearMessages" go v
        <|> A.withObject "clearMessages" go v
      where
        go _ = P.pure ClearMessages


------------------------------------------------------------------------------
instance A.ToJSON ClearMessages where
    toEncoding ClearMessages = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON ClearMessages = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup ClearMessages where
    ClearMessages <> ClearMessages = ClearMessages


------------------------------------------------------------------------------
instance P.Monoid ClearMessages where
    mempty = ClearMessages


------------------------------------------------------------------------------
instance M.Method ClearMessages where
    type Result ClearMessages = ()
    name _ = "Console.clearMessages"


------------------------------------------------------------------------------
-- | Does nothing.
clearMessages
    :: ClearMessages
clearMessages = ClearMessages


------------------------------------------------------------------------------
-- | Disables console domain, prevents further console messages from being reported to the client.
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
    name _ = "Console.disable"


------------------------------------------------------------------------------
-- | Disables console domain, prevents further console messages from being reported to the client.
disable
    :: Disable
disable = Disable


------------------------------------------------------------------------------
-- | Enables console domain, sends the messages collected so far to the client by means of the
-- @messageAdded@ notification.
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
    name _ = "Console.enable"


------------------------------------------------------------------------------
-- | Enables console domain, sends the messages collected so far to the client by means of the
-- @messageAdded@ notification.
enable
    :: Enable
enable = Enable


------------------------------------------------------------------------------
-- | Issued when new console message is added.
data MessageAdded = MessageAdded
    { -- | Console message that has been added.
      message :: !ConsoleMessage
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON MessageAdded where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "messageAdded" $ \_o -> MessageAdded
            <$> _o .: "message"
        ago = A.withArray "messageAdded" $ \_a -> MessageAdded
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON MessageAdded where
    toEncoding (MessageAdded _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "message" .= _0
        ]
    toJSON (MessageAdded _0) = A.object $ P.catMaybes
        [ P.pure $ "message" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup MessageAdded where
    MessageAdded _0 <> MessageAdded _ = MessageAdded _0


------------------------------------------------------------------------------
instance E.Event MessageAdded where
    type Result MessageAdded = MessageAdded
    name _ = "Console.messageAdded"


------------------------------------------------------------------------------
-- | Issued when new console message is added.
messageAdded :: P.Proxy MessageAdded
messageAdded = P.Proxy

