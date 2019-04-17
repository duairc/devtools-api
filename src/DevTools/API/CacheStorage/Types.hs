{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.CacheStorage.Types{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

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
-- | Unique identifier of the Cache object.
type CacheId = T.Text


------------------------------------------------------------------------------
-- | type of HTTP response cached
data CachedResponseType
    = Basic
    | Cors
    | Default
    | Error
    | OpaqueResponse
    | OpaqueRedirect
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CachedResponseType where
    parseJSON = A.withText "CachedResponseType" $ \t -> case t of
        "basic" -> P.pure Basic
        "cors" -> P.pure Cors
        "default" -> P.pure Default
        "error" -> P.pure Error
        "opaqueResponse" -> P.pure OpaqueResponse
        "opaqueRedirect" -> P.pure OpaqueRedirect
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON CachedResponseType where
    toJSON Basic = "basic"
    toJSON Cors = "cors"
    toJSON Default = "default"
    toJSON Error = "error"
    toJSON OpaqueResponse = "opaqueResponse"
    toJSON OpaqueRedirect = "opaqueRedirect"


------------------------------------------------------------------------------
-- | Data entry.
data DataEntry = DataEntry
    { -- | Request URL.
      requestURL :: !T.Text
      -- | Request method.
    , requestMethod :: !T.Text
      -- | Request headers
    , requestHeaders :: ![Header]
      -- | Number of seconds since epoch.
    , responseTime :: !P.Double
      -- | HTTP response status code.
    , responseStatus :: !P.Int
      -- | HTTP response status text.
    , responseStatusText :: !T.Text
      -- | HTTP response type
    , responseType :: !CachedResponseType
      -- | Response headers
    , responseHeaders :: ![Header]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DataEntry where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "DataEntry" $ \_o -> DataEntry
            <$> _o .: "requestURL"
            <*> _o .: "requestMethod"
            <*> _o .: "requestHeaders"
            <*> _o .: "responseTime"
            <*> _o .: "responseStatus"
            <*> _o .: "responseStatusText"
            <*> _o .: "responseType"
            <*> _o .: "responseHeaders"
        ago = A.withArray "DataEntry" $ \_a -> DataEntry
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.maybe P.empty A.parseJSON (_a !? 6)
            <*> P.maybe P.empty A.parseJSON (_a !? 7)


------------------------------------------------------------------------------
instance A.ToJSON DataEntry where
    toEncoding (DataEntry _0 _1 _2 _3 _4 _5 _6 _7) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestURL" .= _0
        , P.pure $ "requestMethod" .= _1
        , P.pure $ "requestHeaders" .= _2
        , P.pure $ "responseTime" .= _3
        , P.pure $ "responseStatus" .= _4
        , P.pure $ "responseStatusText" .= _5
        , P.pure $ "responseType" .= _6
        , P.pure $ "responseHeaders" .= _7
        ]
    toJSON (DataEntry _0 _1 _2 _3 _4 _5 _6 _7) = A.object $ P.catMaybes
        [ P.pure $ "requestURL" .= _0
        , P.pure $ "requestMethod" .= _1
        , P.pure $ "requestHeaders" .= _2
        , P.pure $ "responseTime" .= _3
        , P.pure $ "responseStatus" .= _4
        , P.pure $ "responseStatusText" .= _5
        , P.pure $ "responseType" .= _6
        , P.pure $ "responseHeaders" .= _7
        ]


------------------------------------------------------------------------------
instance P.Semigroup DataEntry where
    DataEntry _0 _1 _2 _3 _4 _5 _6 _7 <> DataEntry _ _ _ _ _ _ _ _ = DataEntry _0 _1 _2 _3 _4 _5 _6 _7


------------------------------------------------------------------------------
-- | Cache identifier.
data Cache = Cache
    { -- | An opaque unique id of the cache.
      cacheId :: !CacheId
      -- | Security origin of the cache.
    , securityOrigin :: !T.Text
      -- | The name of the cache.
    , cacheName :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Cache where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "Cache" $ \_o -> Cache
            <$> _o .: "cacheId"
            <*> _o .: "securityOrigin"
            <*> _o .: "cacheName"
        ago = A.withArray "Cache" $ \_a -> Cache
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON Cache where
    toEncoding (Cache _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "cacheId" .= _0
        , P.pure $ "securityOrigin" .= _1
        , P.pure $ "cacheName" .= _2
        ]
    toJSON (Cache _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "cacheId" .= _0
        , P.pure $ "securityOrigin" .= _1
        , P.pure $ "cacheName" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup Cache where
    Cache _0 _1 _2 <> Cache _ _ _ = Cache _0 _1 _2


------------------------------------------------------------------------------
data Header = Header
    { name :: !T.Text
    , value :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Header where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "Header" $ \_o -> Header
            <$> _o .: "name"
            <*> _o .: "value"
        ago = A.withArray "Header" $ \_a -> Header
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON Header where
    toEncoding (Header _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "value" .= _1
        ]
    toJSON (Header _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "value" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup Header where
    Header _0 _1 <> Header _ _ = Header _0 _1


------------------------------------------------------------------------------
-- | Cached response
data CachedResponse = CachedResponse
    { -- | Entry content, base64-encoded.
      body :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CachedResponse where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "CachedResponse" $ \_o -> CachedResponse
            <$> _o .: "body"
        ago = A.withArray "CachedResponse" $ \_a -> CachedResponse
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON CachedResponse where
    toEncoding (CachedResponse _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "body" .= _0
        ]
    toJSON (CachedResponse _0) = A.object $ P.catMaybes
        [ P.pure $ "body" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup CachedResponse where
    CachedResponse _0 <> CachedResponse _ = CachedResponse _0

