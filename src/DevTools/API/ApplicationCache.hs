{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.ApplicationCache{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

    ( module DevTools.API.ApplicationCache.Types
    , module DevTools.API.ApplicationCache
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
import           DevTools.API.ApplicationCache.Types
import qualified DevTools.API.Page.Types as Page


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Enables application cache domain notifications.
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
    name _ = "ApplicationCache.enable"


------------------------------------------------------------------------------
-- | Enables application cache domain notifications.
enable
    :: Enable
enable = Enable


------------------------------------------------------------------------------
-- | Returns relevant application cache data for the document in given frame.
data GetApplicationCacheForFrame = GetApplicationCacheForFrame
    { -- | Identifier of the frame containing document whose application cache is retrieved.
      frameId :: !Page.FrameId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetApplicationCacheForFrame where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getApplicationCacheForFrame" $ \_o -> GetApplicationCacheForFrame
            <$> _o .: "frameId"
        ago = A.withArray "getApplicationCacheForFrame" $ \_a -> GetApplicationCacheForFrame
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetApplicationCacheForFrame where
    toEncoding (GetApplicationCacheForFrame _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        ]
    toJSON (GetApplicationCacheForFrame _0) = A.object $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetApplicationCacheForFrame where
    GetApplicationCacheForFrame _0 <> GetApplicationCacheForFrame _ = GetApplicationCacheForFrame _0


------------------------------------------------------------------------------
-- | Returns relevant application cache data for the document in given frame.
data GetApplicationCacheForFrameResult = GetApplicationCacheForFrameResult
    { -- | Relevant application cache data for the document in given frame.
      applicationCache :: !ApplicationCache
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetApplicationCacheForFrameResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getApplicationCacheForFrameResult" $ \_o -> GetApplicationCacheForFrameResult
            <$> _o .: "applicationCache"
        ago = A.withArray "getApplicationCacheForFrameResult" $ \_a -> GetApplicationCacheForFrameResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetApplicationCacheForFrameResult where
    toEncoding (GetApplicationCacheForFrameResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "applicationCache" .= _0
        ]
    toJSON (GetApplicationCacheForFrameResult _0) = A.object $ P.catMaybes
        [ P.pure $ "applicationCache" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetApplicationCacheForFrameResult where
    GetApplicationCacheForFrameResult _0 <> GetApplicationCacheForFrameResult _ = GetApplicationCacheForFrameResult _0


------------------------------------------------------------------------------
instance M.Method GetApplicationCacheForFrame where
    type Result GetApplicationCacheForFrame = GetApplicationCacheForFrameResult
    name _ = "ApplicationCache.getApplicationCacheForFrame"


------------------------------------------------------------------------------
-- | Returns relevant application cache data for the document in given frame.
getApplicationCacheForFrame
    :: Page.FrameId
    -- ^ Identifier of the frame containing document whose application cache is retrieved.

    -> GetApplicationCacheForFrame
getApplicationCacheForFrame _0 = GetApplicationCacheForFrame _0


------------------------------------------------------------------------------
-- | Returns array of frame identifiers with manifest urls for each frame containing a document
-- associated with some application cache.
data GetFramesWithManifests = GetFramesWithManifests
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetFramesWithManifests where
    parseJSON A.Null = P.pure GetFramesWithManifests
    parseJSON v = A.withArray "getFramesWithManifests" go v
        <|> A.withObject "getFramesWithManifests" go v
      where
        go _ = P.pure GetFramesWithManifests


------------------------------------------------------------------------------
instance A.ToJSON GetFramesWithManifests where
    toEncoding GetFramesWithManifests = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetFramesWithManifests = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetFramesWithManifests where
    GetFramesWithManifests <> GetFramesWithManifests = GetFramesWithManifests


------------------------------------------------------------------------------
instance P.Monoid GetFramesWithManifests where
    mempty = GetFramesWithManifests


------------------------------------------------------------------------------
-- | Returns array of frame identifiers with manifest urls for each frame containing a document
-- associated with some application cache.
data GetFramesWithManifestsResult = GetFramesWithManifestsResult
    { -- | Array of frame identifiers with manifest urls for each frame containing a document
      -- associated with some application cache.
      frameIds :: ![FrameWithManifest]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetFramesWithManifestsResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getFramesWithManifestsResult" $ \_o -> GetFramesWithManifestsResult
            <$> _o .: "frameIds"
        ago = A.withArray "getFramesWithManifestsResult" $ \_a -> GetFramesWithManifestsResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetFramesWithManifestsResult where
    toEncoding (GetFramesWithManifestsResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "frameIds" .= _0
        ]
    toJSON (GetFramesWithManifestsResult _0) = A.object $ P.catMaybes
        [ P.pure $ "frameIds" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetFramesWithManifestsResult where
    GetFramesWithManifestsResult _0 <> GetFramesWithManifestsResult _ = GetFramesWithManifestsResult _0


------------------------------------------------------------------------------
instance M.Method GetFramesWithManifests where
    type Result GetFramesWithManifests = GetFramesWithManifestsResult
    name _ = "ApplicationCache.getFramesWithManifests"


------------------------------------------------------------------------------
-- | Returns array of frame identifiers with manifest urls for each frame containing a document
-- associated with some application cache.
getFramesWithManifests
    :: GetFramesWithManifests
getFramesWithManifests = GetFramesWithManifests


------------------------------------------------------------------------------
-- | Returns manifest URL for document in the given frame.
data GetManifestForFrame = GetManifestForFrame
    { -- | Identifier of the frame containing document whose manifest is retrieved.
      frameId :: !Page.FrameId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetManifestForFrame where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getManifestForFrame" $ \_o -> GetManifestForFrame
            <$> _o .: "frameId"
        ago = A.withArray "getManifestForFrame" $ \_a -> GetManifestForFrame
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetManifestForFrame where
    toEncoding (GetManifestForFrame _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        ]
    toJSON (GetManifestForFrame _0) = A.object $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetManifestForFrame where
    GetManifestForFrame _0 <> GetManifestForFrame _ = GetManifestForFrame _0


------------------------------------------------------------------------------
-- | Returns manifest URL for document in the given frame.
data GetManifestForFrameResult = GetManifestForFrameResult
    { -- | Manifest URL for document in the given frame.
      manifestURL :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetManifestForFrameResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getManifestForFrameResult" $ \_o -> GetManifestForFrameResult
            <$> _o .: "manifestURL"
        ago = A.withArray "getManifestForFrameResult" $ \_a -> GetManifestForFrameResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetManifestForFrameResult where
    toEncoding (GetManifestForFrameResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "manifestURL" .= _0
        ]
    toJSON (GetManifestForFrameResult _0) = A.object $ P.catMaybes
        [ P.pure $ "manifestURL" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetManifestForFrameResult where
    GetManifestForFrameResult _0 <> GetManifestForFrameResult _ = GetManifestForFrameResult _0


------------------------------------------------------------------------------
instance M.Method GetManifestForFrame where
    type Result GetManifestForFrame = GetManifestForFrameResult
    name _ = "ApplicationCache.getManifestForFrame"


------------------------------------------------------------------------------
-- | Returns manifest URL for document in the given frame.
getManifestForFrame
    :: Page.FrameId
    -- ^ Identifier of the frame containing document whose manifest is retrieved.

    -> GetManifestForFrame
getManifestForFrame _0 = GetManifestForFrame _0


------------------------------------------------------------------------------
data ApplicationCacheStatusUpdated = ApplicationCacheStatusUpdated
    { -- | Identifier of the frame containing document whose application cache updated status.
      frameId :: !Page.FrameId
      -- | Manifest URL.
    , manifestURL :: !T.Text
      -- | Updated application cache status.
    , status :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ApplicationCacheStatusUpdated where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "applicationCacheStatusUpdated" $ \_o -> ApplicationCacheStatusUpdated
            <$> _o .: "frameId"
            <*> _o .: "manifestURL"
            <*> _o .: "status"
        ago = A.withArray "applicationCacheStatusUpdated" $ \_a -> ApplicationCacheStatusUpdated
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON ApplicationCacheStatusUpdated where
    toEncoding (ApplicationCacheStatusUpdated _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        , P.pure $ "manifestURL" .= _1
        , P.pure $ "status" .= _2
        ]
    toJSON (ApplicationCacheStatusUpdated _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        , P.pure $ "manifestURL" .= _1
        , P.pure $ "status" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup ApplicationCacheStatusUpdated where
    ApplicationCacheStatusUpdated _0 _1 _2 <> ApplicationCacheStatusUpdated _ _ _ = ApplicationCacheStatusUpdated _0 _1 _2


------------------------------------------------------------------------------
instance E.Event ApplicationCacheStatusUpdated where
    type Result ApplicationCacheStatusUpdated = ApplicationCacheStatusUpdated
    name _ = "ApplicationCache.applicationCacheStatusUpdated"


------------------------------------------------------------------------------
applicationCacheStatusUpdated :: P.Proxy ApplicationCacheStatusUpdated
applicationCacheStatusUpdated = P.Proxy


------------------------------------------------------------------------------
data NetworkStateUpdated = NetworkStateUpdated
    { isNowOnline :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON NetworkStateUpdated where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "networkStateUpdated" $ \_o -> NetworkStateUpdated
            <$> _o .: "isNowOnline"
        ago = A.withArray "networkStateUpdated" $ \_a -> NetworkStateUpdated
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON NetworkStateUpdated where
    toEncoding (NetworkStateUpdated _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "isNowOnline" .= _0
        ]
    toJSON (NetworkStateUpdated _0) = A.object $ P.catMaybes
        [ P.pure $ "isNowOnline" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup NetworkStateUpdated where
    NetworkStateUpdated _0 <> NetworkStateUpdated _ = NetworkStateUpdated _0


------------------------------------------------------------------------------
instance E.Event NetworkStateUpdated where
    type Result NetworkStateUpdated = NetworkStateUpdated
    name _ = "ApplicationCache.networkStateUpdated"


------------------------------------------------------------------------------
networkStateUpdated :: P.Proxy NetworkStateUpdated
networkStateUpdated = P.Proxy

