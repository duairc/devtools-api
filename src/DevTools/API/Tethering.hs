{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | The Tethering domain defines methods and events for browser port binding.
module DevTools.API.Tethering{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

    ( module DevTools.API.Tethering.Types
    , module DevTools.API.Tethering
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
import           DevTools.API.Tethering.Types


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Request browser port binding.
data Bind = Bind
    { -- | Port number to bind.
      port :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Bind where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "bind" $ \_o -> Bind
            <$> _o .: "port"
        ago = A.withArray "bind" $ \_a -> Bind
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON Bind where
    toEncoding (Bind _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "port" .= _0
        ]
    toJSON (Bind _0) = A.object $ P.catMaybes
        [ P.pure $ "port" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup Bind where
    Bind _0 <> Bind _ = Bind _0


------------------------------------------------------------------------------
instance M.Method Bind where
    type Result Bind = ()
    name _ = "Tethering.bind"


------------------------------------------------------------------------------
-- | Request browser port binding.
bind
    :: P.Int
    -- ^ Port number to bind.

    -> Bind
bind _0 = Bind _0


------------------------------------------------------------------------------
-- | Request browser port unbinding.
data Unbind = Unbind
    { -- | Port number to unbind.
      port :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Unbind where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "unbind" $ \_o -> Unbind
            <$> _o .: "port"
        ago = A.withArray "unbind" $ \_a -> Unbind
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON Unbind where
    toEncoding (Unbind _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "port" .= _0
        ]
    toJSON (Unbind _0) = A.object $ P.catMaybes
        [ P.pure $ "port" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup Unbind where
    Unbind _0 <> Unbind _ = Unbind _0


------------------------------------------------------------------------------
instance M.Method Unbind where
    type Result Unbind = ()
    name _ = "Tethering.unbind"


------------------------------------------------------------------------------
-- | Request browser port unbinding.
unbind
    :: P.Int
    -- ^ Port number to unbind.

    -> Unbind
unbind _0 = Unbind _0


------------------------------------------------------------------------------
-- | Informs that port was successfully bound and got a specified connection id.
data Accepted = Accepted
    { -- | Port number that was successfully bound.
      port :: !P.Int
      -- | Connection id to be used.
    , connectionId :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Accepted where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "accepted" $ \_o -> Accepted
            <$> _o .: "port"
            <*> _o .: "connectionId"
        ago = A.withArray "accepted" $ \_a -> Accepted
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON Accepted where
    toEncoding (Accepted _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "port" .= _0
        , P.pure $ "connectionId" .= _1
        ]
    toJSON (Accepted _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "port" .= _0
        , P.pure $ "connectionId" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup Accepted where
    Accepted _0 _1 <> Accepted _ _ = Accepted _0 _1


------------------------------------------------------------------------------
instance E.Event Accepted where
    type Result Accepted = Accepted
    name _ = "Tethering.accepted"


------------------------------------------------------------------------------
-- | Informs that port was successfully bound and got a specified connection id.
accepted :: P.Proxy Accepted
accepted = P.Proxy

