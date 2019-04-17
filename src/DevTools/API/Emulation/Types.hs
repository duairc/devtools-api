{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | This domain emulates different environments for the page.
module DevTools.API.Emulation.Types
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
-- | Screen orientation.
data ScreenOrientation = ScreenOrientation
    { -- | Orientation type.
      type_ :: !Type
      -- | Orientation angle.
    , angle :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ScreenOrientation where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "ScreenOrientation" $ \_o -> ScreenOrientation
            <$> _o .: "type"
            <*> _o .: "angle"
        ago = A.withArray "ScreenOrientation" $ \_a -> ScreenOrientation
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON ScreenOrientation where
    toEncoding (ScreenOrientation _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "type" .= _0
        , P.pure $ "angle" .= _1
        ]
    toJSON (ScreenOrientation _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "type" .= _0
        , P.pure $ "angle" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup ScreenOrientation where
    ScreenOrientation _0 _1 <> ScreenOrientation _ _ = ScreenOrientation _0 _1


------------------------------------------------------------------------------
data Type
    = PortraitPrimary
    | PortraitSecondary
    | LandscapePrimary
    | LandscapeSecondary
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Type where
    parseJSON = A.withText "Type" $ \t -> case t of
        "portraitPrimary" -> P.pure PortraitPrimary
        "portraitSecondary" -> P.pure PortraitSecondary
        "landscapePrimary" -> P.pure LandscapePrimary
        "landscapeSecondary" -> P.pure LandscapeSecondary
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Type where
    toJSON PortraitPrimary = "portraitPrimary"
    toJSON PortraitSecondary = "portraitSecondary"
    toJSON LandscapePrimary = "landscapePrimary"
    toJSON LandscapeSecondary = "landscapeSecondary"


------------------------------------------------------------------------------
-- | advance: If the scheduler runs out of immediate work, the virtual time base may fast forward to
-- allow the next delayed task (if any) to run; pause: The virtual time base may not advance;
-- pauseIfNetworkFetchesPending: The virtual time base may not advance if there are any pending
-- resource fetches.
{-# WARNING VirtualTimePolicy "This feature is marked as EXPERIMENTAL." #-}
data VirtualTimePolicy
    = Advance
    | Pause
    | PauseIfNetworkFetchesPending
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON VirtualTimePolicy where
    parseJSON = A.withText "VirtualTimePolicy" $ \t -> case t of
        "advance" -> P.pure Advance
        "pause" -> P.pure Pause
        "pauseIfNetworkFetchesPending" -> P.pure PauseIfNetworkFetchesPending
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON VirtualTimePolicy where
    toJSON Advance = "advance"
    toJSON Pause = "pause"
    toJSON PauseIfNetworkFetchesPending = "pauseIfNetworkFetchesPending"

