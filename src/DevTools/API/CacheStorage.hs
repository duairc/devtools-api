{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.CacheStorage{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

    ( module DevTools.API.CacheStorage.Types
    , module DevTools.API.CacheStorage
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
import           DevTools.API.CacheStorage.Types


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Deletes a cache.
data DeleteCache = DeleteCache
    { -- | Id of cache for deletion.
      cacheId :: !CacheId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DeleteCache where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "deleteCache" $ \_o -> DeleteCache
            <$> _o .: "cacheId"
        ago = A.withArray "deleteCache" $ \_a -> DeleteCache
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON DeleteCache where
    toEncoding (DeleteCache _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "cacheId" .= _0
        ]
    toJSON (DeleteCache _0) = A.object $ P.catMaybes
        [ P.pure $ "cacheId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup DeleteCache where
    DeleteCache _0 <> DeleteCache _ = DeleteCache _0


------------------------------------------------------------------------------
instance M.Method DeleteCache where
    type Result DeleteCache = ()
    name _ = "CacheStorage.deleteCache"


------------------------------------------------------------------------------
-- | Deletes a cache.
deleteCache
    :: CacheId
    -- ^ Id of cache for deletion.

    -> DeleteCache
deleteCache _0 = DeleteCache _0


------------------------------------------------------------------------------
-- | Deletes a cache entry.
data DeleteEntry = DeleteEntry
    { -- | Id of cache where the entry will be deleted.
      cacheId :: !CacheId
      -- | URL spec of the request.
    , request :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DeleteEntry where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "deleteEntry" $ \_o -> DeleteEntry
            <$> _o .: "cacheId"
            <*> _o .: "request"
        ago = A.withArray "deleteEntry" $ \_a -> DeleteEntry
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON DeleteEntry where
    toEncoding (DeleteEntry _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "cacheId" .= _0
        , P.pure $ "request" .= _1
        ]
    toJSON (DeleteEntry _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "cacheId" .= _0
        , P.pure $ "request" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup DeleteEntry where
    DeleteEntry _0 _1 <> DeleteEntry _ _ = DeleteEntry _0 _1


------------------------------------------------------------------------------
instance M.Method DeleteEntry where
    type Result DeleteEntry = ()
    name _ = "CacheStorage.deleteEntry"


------------------------------------------------------------------------------
-- | Deletes a cache entry.
deleteEntry
    :: CacheId
    -- ^ Id of cache where the entry will be deleted.

    -> T.Text
    -- ^ URL spec of the request.

    -> DeleteEntry
deleteEntry _0 _1 = DeleteEntry _0 _1


------------------------------------------------------------------------------
-- | Requests cache names.
data RequestCacheNames = RequestCacheNames
    { -- | Security origin.
      securityOrigin :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RequestCacheNames where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "requestCacheNames" $ \_o -> RequestCacheNames
            <$> _o .: "securityOrigin"
        ago = A.withArray "requestCacheNames" $ \_a -> RequestCacheNames
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON RequestCacheNames where
    toEncoding (RequestCacheNames _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "securityOrigin" .= _0
        ]
    toJSON (RequestCacheNames _0) = A.object $ P.catMaybes
        [ P.pure $ "securityOrigin" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup RequestCacheNames where
    RequestCacheNames _0 <> RequestCacheNames _ = RequestCacheNames _0


------------------------------------------------------------------------------
-- | Requests cache names.
data RequestCacheNamesResult = RequestCacheNamesResult
    { -- | Caches for the security origin.
      caches :: ![Cache]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RequestCacheNamesResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "requestCacheNamesResult" $ \_o -> RequestCacheNamesResult
            <$> _o .: "caches"
        ago = A.withArray "requestCacheNamesResult" $ \_a -> RequestCacheNamesResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON RequestCacheNamesResult where
    toEncoding (RequestCacheNamesResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "caches" .= _0
        ]
    toJSON (RequestCacheNamesResult _0) = A.object $ P.catMaybes
        [ P.pure $ "caches" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup RequestCacheNamesResult where
    RequestCacheNamesResult _0 <> RequestCacheNamesResult _ = RequestCacheNamesResult _0


------------------------------------------------------------------------------
instance M.Method RequestCacheNames where
    type Result RequestCacheNames = RequestCacheNamesResult
    name _ = "CacheStorage.requestCacheNames"


------------------------------------------------------------------------------
-- | Requests cache names.
requestCacheNames
    :: T.Text
    -- ^ Security origin.

    -> RequestCacheNames
requestCacheNames _0 = RequestCacheNames _0


------------------------------------------------------------------------------
-- | Fetches cache entry.
data RequestCachedResponse = RequestCachedResponse
    { -- | Id of cache that contains the entry.
      cacheId :: !CacheId
      -- | URL spec of the request.
    , requestURL :: !T.Text
      -- | headers of the request.
    , requestHeaders :: ![Header]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RequestCachedResponse where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "requestCachedResponse" $ \_o -> RequestCachedResponse
            <$> _o .: "cacheId"
            <*> _o .: "requestURL"
            <*> _o .: "requestHeaders"
        ago = A.withArray "requestCachedResponse" $ \_a -> RequestCachedResponse
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON RequestCachedResponse where
    toEncoding (RequestCachedResponse _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "cacheId" .= _0
        , P.pure $ "requestURL" .= _1
        , P.pure $ "requestHeaders" .= _2
        ]
    toJSON (RequestCachedResponse _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "cacheId" .= _0
        , P.pure $ "requestURL" .= _1
        , P.pure $ "requestHeaders" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup RequestCachedResponse where
    RequestCachedResponse _0 _1 _2 <> RequestCachedResponse _ _ _ = RequestCachedResponse _0 _1 _2


------------------------------------------------------------------------------
-- | Fetches cache entry.
data RequestCachedResponseResult = RequestCachedResponseResult
    { -- | Response read from the cache.
      response :: !CachedResponse
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RequestCachedResponseResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "requestCachedResponseResult" $ \_o -> RequestCachedResponseResult
            <$> _o .: "response"
        ago = A.withArray "requestCachedResponseResult" $ \_a -> RequestCachedResponseResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON RequestCachedResponseResult where
    toEncoding (RequestCachedResponseResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "response" .= _0
        ]
    toJSON (RequestCachedResponseResult _0) = A.object $ P.catMaybes
        [ P.pure $ "response" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup RequestCachedResponseResult where
    RequestCachedResponseResult _0 <> RequestCachedResponseResult _ = RequestCachedResponseResult _0


------------------------------------------------------------------------------
instance M.Method RequestCachedResponse where
    type Result RequestCachedResponse = RequestCachedResponseResult
    name _ = "CacheStorage.requestCachedResponse"


------------------------------------------------------------------------------
-- | Fetches cache entry.
requestCachedResponse
    :: CacheId
    -- ^ Id of cache that contains the entry.

    -> T.Text
    -- ^ URL spec of the request.

    -> [Header]
    -- ^ headers of the request.

    -> RequestCachedResponse
requestCachedResponse _0 _1 _2 = RequestCachedResponse _0 _1 _2


------------------------------------------------------------------------------
-- | Requests data from cache.
data RequestEntries = RequestEntries
    { -- | ID of cache to get entries from.
      cacheId :: !CacheId
      -- | Number of records to skip.
    , skipCount :: !P.Int
      -- | Number of records to fetch.
    , pageSize :: !P.Int
      -- | If present, only return the entries containing this substring in the path
    , pathFilter :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RequestEntries where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "requestEntries" $ \_o -> RequestEntries
            <$> _o .: "cacheId"
            <*> _o .: "skipCount"
            <*> _o .: "pageSize"
            <*> _o .:? "pathFilter"
        ago = A.withArray "requestEntries" $ \_a -> RequestEntries
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON RequestEntries where
    toEncoding (RequestEntries _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "cacheId" .= _0
        , P.pure $ "skipCount" .= _1
        , P.pure $ "pageSize" .= _2
        , ("pathFilter" .=) <$> _3
        ]
    toJSON (RequestEntries _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "cacheId" .= _0
        , P.pure $ "skipCount" .= _1
        , P.pure $ "pageSize" .= _2
        , ("pathFilter" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup RequestEntries where
    RequestEntries _0 _1 _2 _3 <> RequestEntries _ _ _ __3 = RequestEntries _0 _1 _2 (_3 <|> __3)


------------------------------------------------------------------------------
-- | Requests data from cache.
data RequestEntriesResult = RequestEntriesResult
    { -- | Array of object store data entries.
      cacheDataEntries :: ![DataEntry]
      -- | Count of returned entries from this storage. If pathFilter is empty, it
      -- is the count of all entries from this storage.
    , returnCount :: !P.Double
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RequestEntriesResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "requestEntriesResult" $ \_o -> RequestEntriesResult
            <$> _o .: "cacheDataEntries"
            <*> _o .: "returnCount"
        ago = A.withArray "requestEntriesResult" $ \_a -> RequestEntriesResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON RequestEntriesResult where
    toEncoding (RequestEntriesResult _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "cacheDataEntries" .= _0
        , P.pure $ "returnCount" .= _1
        ]
    toJSON (RequestEntriesResult _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "cacheDataEntries" .= _0
        , P.pure $ "returnCount" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup RequestEntriesResult where
    RequestEntriesResult _0 _1 <> RequestEntriesResult _ _ = RequestEntriesResult _0 _1


------------------------------------------------------------------------------
instance M.Method RequestEntries where
    type Result RequestEntries = RequestEntriesResult
    name _ = "CacheStorage.requestEntries"


------------------------------------------------------------------------------
-- | Requests data from cache.
requestEntries
    :: CacheId
    -- ^ ID of cache to get entries from.

    -> P.Int
    -- ^ Number of records to skip.

    -> P.Int
    -- ^ Number of records to fetch.

    -> RequestEntries
requestEntries _0 _1 _2 = RequestEntries _0 _1 _2 P.empty

