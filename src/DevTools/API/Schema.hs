{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | This domain is deprecated.
module DevTools.API.Schema{-{-# DEPRECATED "This may be removed in a future release." #-}-}

    ( module DevTools.API.Schema.Types
    , module DevTools.API.Schema
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
import           DevTools.API.Schema.Types


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Returns supported domains.
data GetDomains = GetDomains
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetDomains where
    parseJSON A.Null = P.pure GetDomains
    parseJSON v = A.withArray "getDomains" go v
        <|> A.withObject "getDomains" go v
      where
        go _ = P.pure GetDomains


------------------------------------------------------------------------------
instance A.ToJSON GetDomains where
    toEncoding GetDomains = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetDomains = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetDomains where
    GetDomains <> GetDomains = GetDomains


------------------------------------------------------------------------------
instance P.Monoid GetDomains where
    mempty = GetDomains


------------------------------------------------------------------------------
-- | Returns supported domains.
data GetDomainsResult = GetDomainsResult
    { -- | List of supported domains.
      domains :: ![Domain]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetDomainsResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getDomainsResult" $ \_o -> GetDomainsResult
            <$> _o .: "domains"
        ago = A.withArray "getDomainsResult" $ \_a -> GetDomainsResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetDomainsResult where
    toEncoding (GetDomainsResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "domains" .= _0
        ]
    toJSON (GetDomainsResult _0) = A.object $ P.catMaybes
        [ P.pure $ "domains" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetDomainsResult where
    GetDomainsResult _0 <> GetDomainsResult _ = GetDomainsResult _0


------------------------------------------------------------------------------
instance M.Method GetDomains where
    type Result GetDomains = GetDomainsResult
    name _ = "Schema.getDomains"


------------------------------------------------------------------------------
-- | Returns supported domains.
getDomains
    :: GetDomains
getDomains = GetDomains

