{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.Storage{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

    ( module DevTools.API.Storage.Types
    , module DevTools.API.Storage
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
import           DevTools.API.Storage.Types


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Clears storage for origin.
data ClearDataForOrigin = ClearDataForOrigin
    { -- | Security origin.
      origin :: !T.Text
      -- | Comma separated list of StorageType to clear.
    , storageTypes :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ClearDataForOrigin where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "clearDataForOrigin" $ \_o -> ClearDataForOrigin
            <$> _o .: "origin"
            <*> _o .: "storageTypes"
        ago = A.withArray "clearDataForOrigin" $ \_a -> ClearDataForOrigin
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON ClearDataForOrigin where
    toEncoding (ClearDataForOrigin _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "origin" .= _0
        , P.pure $ "storageTypes" .= _1
        ]
    toJSON (ClearDataForOrigin _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "origin" .= _0
        , P.pure $ "storageTypes" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup ClearDataForOrigin where
    ClearDataForOrigin _0 _1 <> ClearDataForOrigin _ _ = ClearDataForOrigin _0 _1


------------------------------------------------------------------------------
instance M.Method ClearDataForOrigin where
    type Result ClearDataForOrigin = ()
    name _ = "Storage.clearDataForOrigin"


------------------------------------------------------------------------------
-- | Clears storage for origin.
clearDataForOrigin
    :: T.Text
    -- ^ Security origin.

    -> T.Text
    -- ^ Comma separated list of StorageType to clear.

    -> ClearDataForOrigin
clearDataForOrigin _0 _1 = ClearDataForOrigin _0 _1


------------------------------------------------------------------------------
-- | Returns usage and quota in bytes.
data GetUsageAndQuota = GetUsageAndQuota
    { -- | Security origin.
      origin :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetUsageAndQuota where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getUsageAndQuota" $ \_o -> GetUsageAndQuota
            <$> _o .: "origin"
        ago = A.withArray "getUsageAndQuota" $ \_a -> GetUsageAndQuota
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetUsageAndQuota where
    toEncoding (GetUsageAndQuota _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "origin" .= _0
        ]
    toJSON (GetUsageAndQuota _0) = A.object $ P.catMaybes
        [ P.pure $ "origin" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetUsageAndQuota where
    GetUsageAndQuota _0 <> GetUsageAndQuota _ = GetUsageAndQuota _0


------------------------------------------------------------------------------
-- | Returns usage and quota in bytes.
data GetUsageAndQuotaResult = GetUsageAndQuotaResult
    { -- | Storage usage (bytes).
      usage :: !P.Double
      -- | Storage quota (bytes).
    , quota :: !P.Double
      -- | Storage usage per type (bytes).
    , usageBreakdown :: ![UsageForType]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetUsageAndQuotaResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getUsageAndQuotaResult" $ \_o -> GetUsageAndQuotaResult
            <$> _o .: "usage"
            <*> _o .: "quota"
            <*> _o .: "usageBreakdown"
        ago = A.withArray "getUsageAndQuotaResult" $ \_a -> GetUsageAndQuotaResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON GetUsageAndQuotaResult where
    toEncoding (GetUsageAndQuotaResult _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "usage" .= _0
        , P.pure $ "quota" .= _1
        , P.pure $ "usageBreakdown" .= _2
        ]
    toJSON (GetUsageAndQuotaResult _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "usage" .= _0
        , P.pure $ "quota" .= _1
        , P.pure $ "usageBreakdown" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetUsageAndQuotaResult where
    GetUsageAndQuotaResult _0 _1 _2 <> GetUsageAndQuotaResult _ _ _ = GetUsageAndQuotaResult _0 _1 _2


------------------------------------------------------------------------------
instance M.Method GetUsageAndQuota where
    type Result GetUsageAndQuota = GetUsageAndQuotaResult
    name _ = "Storage.getUsageAndQuota"


------------------------------------------------------------------------------
-- | Returns usage and quota in bytes.
getUsageAndQuota
    :: T.Text
    -- ^ Security origin.

    -> GetUsageAndQuota
getUsageAndQuota _0 = GetUsageAndQuota _0


------------------------------------------------------------------------------
-- | Registers origin to be notified when an update occurs to its cache storage list.
data TrackCacheStorageForOrigin = TrackCacheStorageForOrigin
    { -- | Security origin.
      origin :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TrackCacheStorageForOrigin where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "trackCacheStorageForOrigin" $ \_o -> TrackCacheStorageForOrigin
            <$> _o .: "origin"
        ago = A.withArray "trackCacheStorageForOrigin" $ \_a -> TrackCacheStorageForOrigin
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON TrackCacheStorageForOrigin where
    toEncoding (TrackCacheStorageForOrigin _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "origin" .= _0
        ]
    toJSON (TrackCacheStorageForOrigin _0) = A.object $ P.catMaybes
        [ P.pure $ "origin" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup TrackCacheStorageForOrigin where
    TrackCacheStorageForOrigin _0 <> TrackCacheStorageForOrigin _ = TrackCacheStorageForOrigin _0


------------------------------------------------------------------------------
instance M.Method TrackCacheStorageForOrigin where
    type Result TrackCacheStorageForOrigin = ()
    name _ = "Storage.trackCacheStorageForOrigin"


------------------------------------------------------------------------------
-- | Registers origin to be notified when an update occurs to its cache storage list.
trackCacheStorageForOrigin
    :: T.Text
    -- ^ Security origin.

    -> TrackCacheStorageForOrigin
trackCacheStorageForOrigin _0 = TrackCacheStorageForOrigin _0


------------------------------------------------------------------------------
-- | Registers origin to be notified when an update occurs to its IndexedDB.
data TrackIndexedDBForOrigin = TrackIndexedDBForOrigin
    { -- | Security origin.
      origin :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TrackIndexedDBForOrigin where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "trackIndexedDBForOrigin" $ \_o -> TrackIndexedDBForOrigin
            <$> _o .: "origin"
        ago = A.withArray "trackIndexedDBForOrigin" $ \_a -> TrackIndexedDBForOrigin
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON TrackIndexedDBForOrigin where
    toEncoding (TrackIndexedDBForOrigin _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "origin" .= _0
        ]
    toJSON (TrackIndexedDBForOrigin _0) = A.object $ P.catMaybes
        [ P.pure $ "origin" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup TrackIndexedDBForOrigin where
    TrackIndexedDBForOrigin _0 <> TrackIndexedDBForOrigin _ = TrackIndexedDBForOrigin _0


------------------------------------------------------------------------------
instance M.Method TrackIndexedDBForOrigin where
    type Result TrackIndexedDBForOrigin = ()
    name _ = "Storage.trackIndexedDBForOrigin"


------------------------------------------------------------------------------
-- | Registers origin to be notified when an update occurs to its IndexedDB.
trackIndexedDBForOrigin
    :: T.Text
    -- ^ Security origin.

    -> TrackIndexedDBForOrigin
trackIndexedDBForOrigin _0 = TrackIndexedDBForOrigin _0


------------------------------------------------------------------------------
-- | Unregisters origin from receiving notifications for cache storage.
data UntrackCacheStorageForOrigin = UntrackCacheStorageForOrigin
    { -- | Security origin.
      origin :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON UntrackCacheStorageForOrigin where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "untrackCacheStorageForOrigin" $ \_o -> UntrackCacheStorageForOrigin
            <$> _o .: "origin"
        ago = A.withArray "untrackCacheStorageForOrigin" $ \_a -> UntrackCacheStorageForOrigin
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON UntrackCacheStorageForOrigin where
    toEncoding (UntrackCacheStorageForOrigin _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "origin" .= _0
        ]
    toJSON (UntrackCacheStorageForOrigin _0) = A.object $ P.catMaybes
        [ P.pure $ "origin" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup UntrackCacheStorageForOrigin where
    UntrackCacheStorageForOrigin _0 <> UntrackCacheStorageForOrigin _ = UntrackCacheStorageForOrigin _0


------------------------------------------------------------------------------
instance M.Method UntrackCacheStorageForOrigin where
    type Result UntrackCacheStorageForOrigin = ()
    name _ = "Storage.untrackCacheStorageForOrigin"


------------------------------------------------------------------------------
-- | Unregisters origin from receiving notifications for cache storage.
untrackCacheStorageForOrigin
    :: T.Text
    -- ^ Security origin.

    -> UntrackCacheStorageForOrigin
untrackCacheStorageForOrigin _0 = UntrackCacheStorageForOrigin _0


------------------------------------------------------------------------------
-- | Unregisters origin from receiving notifications for IndexedDB.
data UntrackIndexedDBForOrigin = UntrackIndexedDBForOrigin
    { -- | Security origin.
      origin :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON UntrackIndexedDBForOrigin where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "untrackIndexedDBForOrigin" $ \_o -> UntrackIndexedDBForOrigin
            <$> _o .: "origin"
        ago = A.withArray "untrackIndexedDBForOrigin" $ \_a -> UntrackIndexedDBForOrigin
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON UntrackIndexedDBForOrigin where
    toEncoding (UntrackIndexedDBForOrigin _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "origin" .= _0
        ]
    toJSON (UntrackIndexedDBForOrigin _0) = A.object $ P.catMaybes
        [ P.pure $ "origin" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup UntrackIndexedDBForOrigin where
    UntrackIndexedDBForOrigin _0 <> UntrackIndexedDBForOrigin _ = UntrackIndexedDBForOrigin _0


------------------------------------------------------------------------------
instance M.Method UntrackIndexedDBForOrigin where
    type Result UntrackIndexedDBForOrigin = ()
    name _ = "Storage.untrackIndexedDBForOrigin"


------------------------------------------------------------------------------
-- | Unregisters origin from receiving notifications for IndexedDB.
untrackIndexedDBForOrigin
    :: T.Text
    -- ^ Security origin.

    -> UntrackIndexedDBForOrigin
untrackIndexedDBForOrigin _0 = UntrackIndexedDBForOrigin _0


------------------------------------------------------------------------------
-- | A cache's contents have been modified.
data CacheStorageContentUpdated = CacheStorageContentUpdated
    { -- | Origin to update.
      origin :: !T.Text
      -- | Name of cache in origin.
    , cacheName :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CacheStorageContentUpdated where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "cacheStorageContentUpdated" $ \_o -> CacheStorageContentUpdated
            <$> _o .: "origin"
            <*> _o .: "cacheName"
        ago = A.withArray "cacheStorageContentUpdated" $ \_a -> CacheStorageContentUpdated
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON CacheStorageContentUpdated where
    toEncoding (CacheStorageContentUpdated _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "origin" .= _0
        , P.pure $ "cacheName" .= _1
        ]
    toJSON (CacheStorageContentUpdated _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "origin" .= _0
        , P.pure $ "cacheName" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup CacheStorageContentUpdated where
    CacheStorageContentUpdated _0 _1 <> CacheStorageContentUpdated _ _ = CacheStorageContentUpdated _0 _1


------------------------------------------------------------------------------
instance E.Event CacheStorageContentUpdated where
    type Result CacheStorageContentUpdated = CacheStorageContentUpdated
    name _ = "Storage.cacheStorageContentUpdated"


------------------------------------------------------------------------------
-- | A cache's contents have been modified.
cacheStorageContentUpdated :: P.Proxy CacheStorageContentUpdated
cacheStorageContentUpdated = P.Proxy


------------------------------------------------------------------------------
-- | A cache has been added\/deleted.
data CacheStorageListUpdated = CacheStorageListUpdated
    { -- | Origin to update.
      origin :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CacheStorageListUpdated where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "cacheStorageListUpdated" $ \_o -> CacheStorageListUpdated
            <$> _o .: "origin"
        ago = A.withArray "cacheStorageListUpdated" $ \_a -> CacheStorageListUpdated
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON CacheStorageListUpdated where
    toEncoding (CacheStorageListUpdated _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "origin" .= _0
        ]
    toJSON (CacheStorageListUpdated _0) = A.object $ P.catMaybes
        [ P.pure $ "origin" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup CacheStorageListUpdated where
    CacheStorageListUpdated _0 <> CacheStorageListUpdated _ = CacheStorageListUpdated _0


------------------------------------------------------------------------------
instance E.Event CacheStorageListUpdated where
    type Result CacheStorageListUpdated = CacheStorageListUpdated
    name _ = "Storage.cacheStorageListUpdated"


------------------------------------------------------------------------------
-- | A cache has been added\/deleted.
cacheStorageListUpdated :: P.Proxy CacheStorageListUpdated
cacheStorageListUpdated = P.Proxy


------------------------------------------------------------------------------
-- | The origin's IndexedDB object store has been modified.
data IndexedDBContentUpdated = IndexedDBContentUpdated
    { -- | Origin to update.
      origin :: !T.Text
      -- | Database to update.
    , databaseName :: !T.Text
      -- | ObjectStore to update.
    , objectStoreName :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON IndexedDBContentUpdated where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "indexedDBContentUpdated" $ \_o -> IndexedDBContentUpdated
            <$> _o .: "origin"
            <*> _o .: "databaseName"
            <*> _o .: "objectStoreName"
        ago = A.withArray "indexedDBContentUpdated" $ \_a -> IndexedDBContentUpdated
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON IndexedDBContentUpdated where
    toEncoding (IndexedDBContentUpdated _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "origin" .= _0
        , P.pure $ "databaseName" .= _1
        , P.pure $ "objectStoreName" .= _2
        ]
    toJSON (IndexedDBContentUpdated _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "origin" .= _0
        , P.pure $ "databaseName" .= _1
        , P.pure $ "objectStoreName" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup IndexedDBContentUpdated where
    IndexedDBContentUpdated _0 _1 _2 <> IndexedDBContentUpdated _ _ _ = IndexedDBContentUpdated _0 _1 _2


------------------------------------------------------------------------------
instance E.Event IndexedDBContentUpdated where
    type Result IndexedDBContentUpdated = IndexedDBContentUpdated
    name _ = "Storage.indexedDBContentUpdated"


------------------------------------------------------------------------------
-- | The origin's IndexedDB object store has been modified.
indexedDBContentUpdated :: P.Proxy IndexedDBContentUpdated
indexedDBContentUpdated = P.Proxy


------------------------------------------------------------------------------
-- | The origin's IndexedDB database list has been modified.
data IndexedDBListUpdated = IndexedDBListUpdated
    { -- | Origin to update.
      origin :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON IndexedDBListUpdated where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "indexedDBListUpdated" $ \_o -> IndexedDBListUpdated
            <$> _o .: "origin"
        ago = A.withArray "indexedDBListUpdated" $ \_a -> IndexedDBListUpdated
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON IndexedDBListUpdated where
    toEncoding (IndexedDBListUpdated _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "origin" .= _0
        ]
    toJSON (IndexedDBListUpdated _0) = A.object $ P.catMaybes
        [ P.pure $ "origin" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup IndexedDBListUpdated where
    IndexedDBListUpdated _0 <> IndexedDBListUpdated _ = IndexedDBListUpdated _0


------------------------------------------------------------------------------
instance E.Event IndexedDBListUpdated where
    type Result IndexedDBListUpdated = IndexedDBListUpdated
    name _ = "Storage.indexedDBListUpdated"


------------------------------------------------------------------------------
-- | The origin's IndexedDB database list has been modified.
indexedDBListUpdated :: P.Proxy IndexedDBListUpdated
indexedDBListUpdated = P.Proxy

