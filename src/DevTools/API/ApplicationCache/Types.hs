{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.ApplicationCache.Types{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

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


-- devtools-api---------------------------------------------------------------
import qualified DevTools.API.Page.Types as Page


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Detailed application cache resource information.
data ApplicationCacheResource = ApplicationCacheResource
    { -- | Resource url.
      url :: !T.Text
      -- | Resource size.
    , size :: !P.Int
      -- | Resource type.
    , type_ :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ApplicationCacheResource where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "ApplicationCacheResource" $ \_o -> ApplicationCacheResource
            <$> _o .: "url"
            <*> _o .: "size"
            <*> _o .: "type"
        ago = A.withArray "ApplicationCacheResource" $ \_a -> ApplicationCacheResource
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON ApplicationCacheResource where
    toEncoding (ApplicationCacheResource _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "url" .= _0
        , P.pure $ "size" .= _1
        , P.pure $ "type" .= _2
        ]
    toJSON (ApplicationCacheResource _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "url" .= _0
        , P.pure $ "size" .= _1
        , P.pure $ "type" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup ApplicationCacheResource where
    ApplicationCacheResource _0 _1 _2 <> ApplicationCacheResource _ _ _ = ApplicationCacheResource _0 _1 _2


------------------------------------------------------------------------------
-- | Detailed application cache information.
data ApplicationCache = ApplicationCache
    { -- | Manifest URL.
      manifestURL :: !T.Text
      -- | Application cache size.
    , size :: !P.Double
      -- | Application cache creation time.
    , creationTime :: !P.Double
      -- | Application cache update time.
    , updateTime :: !P.Double
      -- | Application cache resources.
    , resources :: ![ApplicationCacheResource]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ApplicationCache where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "ApplicationCache" $ \_o -> ApplicationCache
            <$> _o .: "manifestURL"
            <*> _o .: "size"
            <*> _o .: "creationTime"
            <*> _o .: "updateTime"
            <*> _o .: "resources"
        ago = A.withArray "ApplicationCache" $ \_a -> ApplicationCache
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON ApplicationCache where
    toEncoding (ApplicationCache _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "manifestURL" .= _0
        , P.pure $ "size" .= _1
        , P.pure $ "creationTime" .= _2
        , P.pure $ "updateTime" .= _3
        , P.pure $ "resources" .= _4
        ]
    toJSON (ApplicationCache _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ P.pure $ "manifestURL" .= _0
        , P.pure $ "size" .= _1
        , P.pure $ "creationTime" .= _2
        , P.pure $ "updateTime" .= _3
        , P.pure $ "resources" .= _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup ApplicationCache where
    ApplicationCache _0 _1 _2 _3 _4 <> ApplicationCache _ _ _ _ _ = ApplicationCache _0 _1 _2 _3 _4


------------------------------------------------------------------------------
-- | Frame identifier - manifest URL pair.
data FrameWithManifest = FrameWithManifest
    { -- | Frame identifier.
      frameId :: !Page.FrameId
      -- | Manifest URL.
    , manifestURL :: !T.Text
      -- | Application cache status.
    , status :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON FrameWithManifest where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "FrameWithManifest" $ \_o -> FrameWithManifest
            <$> _o .: "frameId"
            <*> _o .: "manifestURL"
            <*> _o .: "status"
        ago = A.withArray "FrameWithManifest" $ \_a -> FrameWithManifest
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON FrameWithManifest where
    toEncoding (FrameWithManifest _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        , P.pure $ "manifestURL" .= _1
        , P.pure $ "status" .= _2
        ]
    toJSON (FrameWithManifest _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        , P.pure $ "manifestURL" .= _1
        , P.pure $ "status" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup FrameWithManifest where
    FrameWithManifest _0 _1 _2 <> FrameWithManifest _ _ _ = FrameWithManifest _0 _1 _2

