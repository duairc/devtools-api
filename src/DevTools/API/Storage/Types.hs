{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.Storage.Types{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

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
-- | Enum of possible storage types.
data StorageType
    = Appcache
    | Cookies
    | FileSystems
    | Indexeddb
    | LocalStorage
    | ShaderCache
    | Websql
    | ServiceWorkers
    | CacheStorage
    | All
    | Other
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StorageType where
    parseJSON = A.withText "StorageType" $ \t -> case t of
        "appcache" -> P.pure Appcache
        "cookies" -> P.pure Cookies
        "file_systems" -> P.pure FileSystems
        "indexeddb" -> P.pure Indexeddb
        "local_storage" -> P.pure LocalStorage
        "shader_cache" -> P.pure ShaderCache
        "websql" -> P.pure Websql
        "service_workers" -> P.pure ServiceWorkers
        "cache_storage" -> P.pure CacheStorage
        "all" -> P.pure All
        "other" -> P.pure Other
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON StorageType where
    toJSON Appcache = "appcache"
    toJSON Cookies = "cookies"
    toJSON FileSystems = "file_systems"
    toJSON Indexeddb = "indexeddb"
    toJSON LocalStorage = "local_storage"
    toJSON ShaderCache = "shader_cache"
    toJSON Websql = "websql"
    toJSON ServiceWorkers = "service_workers"
    toJSON CacheStorage = "cache_storage"
    toJSON All = "all"
    toJSON Other = "other"


------------------------------------------------------------------------------
-- | Usage for a storage type.
data UsageForType = UsageForType
    { -- | Name of storage type.
      storageType :: !StorageType
      -- | Storage usage (bytes).
    , usage :: !P.Double
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON UsageForType where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "UsageForType" $ \_o -> UsageForType
            <$> _o .: "storageType"
            <*> _o .: "usage"
        ago = A.withArray "UsageForType" $ \_a -> UsageForType
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON UsageForType where
    toEncoding (UsageForType _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "storageType" .= _0
        , P.pure $ "usage" .= _1
        ]
    toJSON (UsageForType _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "storageType" .= _0
        , P.pure $ "usage" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup UsageForType where
    UsageForType _0 _1 <> UsageForType _ _ = UsageForType _0 _1

