{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.Inspector{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

    ( module DevTools.API.Inspector.Types
    , module DevTools.API.Inspector
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
import           DevTools.API.Inspector.Types


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Disables inspector domain notifications.
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
    name _ = "Inspector.disable"


------------------------------------------------------------------------------
-- | Disables inspector domain notifications.
disable
    :: Disable
disable = Disable


------------------------------------------------------------------------------
-- | Enables inspector domain notifications.
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
    name _ = "Inspector.enable"


------------------------------------------------------------------------------
-- | Enables inspector domain notifications.
enable
    :: Enable
enable = Enable


------------------------------------------------------------------------------
-- | Fired when remote debugging connection is about to be terminated. Contains detach reason.
data Detached = Detached
    { -- | The reason why connection has been terminated.
      reason :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Detached where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "detached" $ \_o -> Detached
            <$> _o .: "reason"
        ago = A.withArray "detached" $ \_a -> Detached
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON Detached where
    toEncoding (Detached _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "reason" .= _0
        ]
    toJSON (Detached _0) = A.object $ P.catMaybes
        [ P.pure $ "reason" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup Detached where
    Detached _0 <> Detached _ = Detached _0


------------------------------------------------------------------------------
instance E.Event Detached where
    type Result Detached = Detached
    name _ = "Inspector.detached"


------------------------------------------------------------------------------
-- | Fired when remote debugging connection is about to be terminated. Contains detach reason.
detached :: P.Proxy Detached
detached = P.Proxy


------------------------------------------------------------------------------
-- | Fired when debugging target has crashed
data TargetCrashed = TargetCrashed
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TargetCrashed where
    parseJSON A.Null = P.pure TargetCrashed
    parseJSON v = A.withArray "targetCrashed" go v
        <|> A.withObject "targetCrashed" go v
      where
        go _ = P.pure TargetCrashed


------------------------------------------------------------------------------
instance A.ToJSON TargetCrashed where
    toEncoding TargetCrashed = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON TargetCrashed = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup TargetCrashed where
    TargetCrashed <> TargetCrashed = TargetCrashed


------------------------------------------------------------------------------
instance P.Monoid TargetCrashed where
    mempty = TargetCrashed


------------------------------------------------------------------------------
instance E.Event TargetCrashed where
    type Result TargetCrashed = ()
    name _ = "Inspector.targetCrashed"


------------------------------------------------------------------------------
-- | Fired when debugging target has crashed
targetCrashed :: P.Proxy TargetCrashed
targetCrashed = P.Proxy


------------------------------------------------------------------------------
-- | Fired when debugging target has reloaded after crash
data TargetReloadedAfterCrash = TargetReloadedAfterCrash
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TargetReloadedAfterCrash where
    parseJSON A.Null = P.pure TargetReloadedAfterCrash
    parseJSON v = A.withArray "targetReloadedAfterCrash" go v
        <|> A.withObject "targetReloadedAfterCrash" go v
      where
        go _ = P.pure TargetReloadedAfterCrash


------------------------------------------------------------------------------
instance A.ToJSON TargetReloadedAfterCrash where
    toEncoding TargetReloadedAfterCrash = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON TargetReloadedAfterCrash = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup TargetReloadedAfterCrash where
    TargetReloadedAfterCrash <> TargetReloadedAfterCrash = TargetReloadedAfterCrash


------------------------------------------------------------------------------
instance P.Monoid TargetReloadedAfterCrash where
    mempty = TargetReloadedAfterCrash


------------------------------------------------------------------------------
instance E.Event TargetReloadedAfterCrash where
    type Result TargetReloadedAfterCrash = ()
    name _ = "Inspector.targetReloadedAfterCrash"


------------------------------------------------------------------------------
-- | Fired when debugging target has reloaded after crash
targetReloadedAfterCrash :: P.Proxy TargetReloadedAfterCrash
targetReloadedAfterCrash = P.Proxy

