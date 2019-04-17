{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.DeviceOrientation{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

    ( module DevTools.API.DeviceOrientation.Types
    , module DevTools.API.DeviceOrientation
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
import           DevTools.API.DeviceOrientation.Types


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Clears the overridden Device Orientation.
data ClearDeviceOrientationOverride = ClearDeviceOrientationOverride
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ClearDeviceOrientationOverride where
    parseJSON A.Null = P.pure ClearDeviceOrientationOverride
    parseJSON v = A.withArray "clearDeviceOrientationOverride" go v
        <|> A.withObject "clearDeviceOrientationOverride" go v
      where
        go _ = P.pure ClearDeviceOrientationOverride


------------------------------------------------------------------------------
instance A.ToJSON ClearDeviceOrientationOverride where
    toEncoding ClearDeviceOrientationOverride = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON ClearDeviceOrientationOverride = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup ClearDeviceOrientationOverride where
    ClearDeviceOrientationOverride <> ClearDeviceOrientationOverride = ClearDeviceOrientationOverride


------------------------------------------------------------------------------
instance P.Monoid ClearDeviceOrientationOverride where
    mempty = ClearDeviceOrientationOverride


------------------------------------------------------------------------------
instance M.Method ClearDeviceOrientationOverride where
    type Result ClearDeviceOrientationOverride = ()
    name _ = "DeviceOrientation.clearDeviceOrientationOverride"


------------------------------------------------------------------------------
-- | Clears the overridden Device Orientation.
clearDeviceOrientationOverride
    :: ClearDeviceOrientationOverride
clearDeviceOrientationOverride = ClearDeviceOrientationOverride


------------------------------------------------------------------------------
-- | Overrides the Device Orientation.
data SetDeviceOrientationOverride = SetDeviceOrientationOverride
    { -- | Mock alpha
      alpha :: !P.Double
      -- | Mock beta
    , beta :: !P.Double
      -- | Mock gamma
    , gamma :: !P.Double
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetDeviceOrientationOverride where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setDeviceOrientationOverride" $ \_o -> SetDeviceOrientationOverride
            <$> _o .: "alpha"
            <*> _o .: "beta"
            <*> _o .: "gamma"
        ago = A.withArray "setDeviceOrientationOverride" $ \_a -> SetDeviceOrientationOverride
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON SetDeviceOrientationOverride where
    toEncoding (SetDeviceOrientationOverride _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "alpha" .= _0
        , P.pure $ "beta" .= _1
        , P.pure $ "gamma" .= _2
        ]
    toJSON (SetDeviceOrientationOverride _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "alpha" .= _0
        , P.pure $ "beta" .= _1
        , P.pure $ "gamma" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetDeviceOrientationOverride where
    SetDeviceOrientationOverride _0 _1 _2 <> SetDeviceOrientationOverride _ _ _ = SetDeviceOrientationOverride _0 _1 _2


------------------------------------------------------------------------------
instance M.Method SetDeviceOrientationOverride where
    type Result SetDeviceOrientationOverride = ()
    name _ = "DeviceOrientation.setDeviceOrientationOverride"


------------------------------------------------------------------------------
-- | Overrides the Device Orientation.
setDeviceOrientationOverride
    :: P.Double
    -- ^ Mock alpha

    -> P.Double
    -- ^ Mock beta

    -> P.Double
    -- ^ Mock gamma

    -> SetDeviceOrientationOverride
setDeviceOrientationOverride _0 _1 _2 = SetDeviceOrientationOverride _0 _1 _2

