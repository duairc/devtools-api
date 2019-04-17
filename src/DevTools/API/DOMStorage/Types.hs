{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Query and modify DOM storage.
module DevTools.API.DOMStorage.Types{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

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
-- | DOM Storage identifier.
data StorageId = StorageId
    { -- | Security origin for the storage.
      securityOrigin :: !T.Text
      -- | Whether the storage is local storage (not session storage).
    , isLocalStorage :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StorageId where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "StorageId" $ \_o -> StorageId
            <$> _o .: "securityOrigin"
            <*> _o .: "isLocalStorage"
        ago = A.withArray "StorageId" $ \_a -> StorageId
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON StorageId where
    toEncoding (StorageId _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "securityOrigin" .= _0
        , P.pure $ "isLocalStorage" .= _1
        ]
    toJSON (StorageId _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "securityOrigin" .= _0
        , P.pure $ "isLocalStorage" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup StorageId where
    StorageId _0 _1 <> StorageId _ _ = StorageId _0 _1


------------------------------------------------------------------------------
-- | DOM Storage item.
type Item = [T.Text]

