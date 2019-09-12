{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Network domain allows tracking network activities of the page. It exposes information about http,
-- file, data and other requests and responses, their headers, bodies, timing, etc.
module DevTools.API.Network
    ( module DevTools.API.Network.Types
    , module DevTools.API.Network
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
import qualified DevTools.API.Debugger.Types as Debugger
import qualified DevTools.API.IO.Types as IO
import           DevTools.API.Network.Types
import qualified DevTools.API.Page.Types as Page


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Tells whether clearing browser cache is supported.
{-# DEPRECATED CanClearBrowserCache "This may be removed in a future release." #-}
data CanClearBrowserCache = CanClearBrowserCache
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CanClearBrowserCache where
    parseJSON A.Null = P.pure CanClearBrowserCache
    parseJSON v = A.withArray "canClearBrowserCache" go v
        <|> A.withObject "canClearBrowserCache" go v
      where
        go _ = P.pure CanClearBrowserCache


------------------------------------------------------------------------------
instance A.ToJSON CanClearBrowserCache where
    toEncoding CanClearBrowserCache = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON CanClearBrowserCache = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup CanClearBrowserCache where
    CanClearBrowserCache <> CanClearBrowserCache = CanClearBrowserCache


------------------------------------------------------------------------------
instance P.Monoid CanClearBrowserCache where
    mempty = CanClearBrowserCache


------------------------------------------------------------------------------
-- | Tells whether clearing browser cache is supported.
{-# DEPRECATED CanClearBrowserCacheResult "This may be removed in a future release." #-}
data CanClearBrowserCacheResult = CanClearBrowserCacheResult
    { -- | True if browser cache can be cleared.
      result :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CanClearBrowserCacheResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "canClearBrowserCacheResult" $ \_o -> CanClearBrowserCacheResult
            <$> _o .: "result"
        ago = A.withArray "canClearBrowserCacheResult" $ \_a -> CanClearBrowserCacheResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON CanClearBrowserCacheResult where
    toEncoding (CanClearBrowserCacheResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "result" .= _0
        ]
    toJSON (CanClearBrowserCacheResult _0) = A.object $ P.catMaybes
        [ P.pure $ "result" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup CanClearBrowserCacheResult where
    CanClearBrowserCacheResult _0 <> CanClearBrowserCacheResult _ = CanClearBrowserCacheResult _0


------------------------------------------------------------------------------
instance M.Method CanClearBrowserCache where
    type Result CanClearBrowserCache = CanClearBrowserCacheResult
    name _ = "Network.canClearBrowserCache"


------------------------------------------------------------------------------
-- | Tells whether clearing browser cache is supported.
{-# DEPRECATED canClearBrowserCache "This may be removed in a future release." #-}
canClearBrowserCache
    :: CanClearBrowserCache
canClearBrowserCache = CanClearBrowserCache


------------------------------------------------------------------------------
-- | Tells whether clearing browser cookies is supported.
{-# DEPRECATED CanClearBrowserCookies "This may be removed in a future release." #-}
data CanClearBrowserCookies = CanClearBrowserCookies
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CanClearBrowserCookies where
    parseJSON A.Null = P.pure CanClearBrowserCookies
    parseJSON v = A.withArray "canClearBrowserCookies" go v
        <|> A.withObject "canClearBrowserCookies" go v
      where
        go _ = P.pure CanClearBrowserCookies


------------------------------------------------------------------------------
instance A.ToJSON CanClearBrowserCookies where
    toEncoding CanClearBrowserCookies = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON CanClearBrowserCookies = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup CanClearBrowserCookies where
    CanClearBrowserCookies <> CanClearBrowserCookies = CanClearBrowserCookies


------------------------------------------------------------------------------
instance P.Monoid CanClearBrowserCookies where
    mempty = CanClearBrowserCookies


------------------------------------------------------------------------------
-- | Tells whether clearing browser cookies is supported.
{-# DEPRECATED CanClearBrowserCookiesResult "This may be removed in a future release." #-}
data CanClearBrowserCookiesResult = CanClearBrowserCookiesResult
    { -- | True if browser cookies can be cleared.
      result :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CanClearBrowserCookiesResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "canClearBrowserCookiesResult" $ \_o -> CanClearBrowserCookiesResult
            <$> _o .: "result"
        ago = A.withArray "canClearBrowserCookiesResult" $ \_a -> CanClearBrowserCookiesResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON CanClearBrowserCookiesResult where
    toEncoding (CanClearBrowserCookiesResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "result" .= _0
        ]
    toJSON (CanClearBrowserCookiesResult _0) = A.object $ P.catMaybes
        [ P.pure $ "result" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup CanClearBrowserCookiesResult where
    CanClearBrowserCookiesResult _0 <> CanClearBrowserCookiesResult _ = CanClearBrowserCookiesResult _0


------------------------------------------------------------------------------
instance M.Method CanClearBrowserCookies where
    type Result CanClearBrowserCookies = CanClearBrowserCookiesResult
    name _ = "Network.canClearBrowserCookies"


------------------------------------------------------------------------------
-- | Tells whether clearing browser cookies is supported.
{-# DEPRECATED canClearBrowserCookies "This may be removed in a future release." #-}
canClearBrowserCookies
    :: CanClearBrowserCookies
canClearBrowserCookies = CanClearBrowserCookies


------------------------------------------------------------------------------
-- | Tells whether emulation of network conditions is supported.
{-# DEPRECATED CanEmulateNetworkConditions "This may be removed in a future release." #-}
data CanEmulateNetworkConditions = CanEmulateNetworkConditions
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CanEmulateNetworkConditions where
    parseJSON A.Null = P.pure CanEmulateNetworkConditions
    parseJSON v = A.withArray "canEmulateNetworkConditions" go v
        <|> A.withObject "canEmulateNetworkConditions" go v
      where
        go _ = P.pure CanEmulateNetworkConditions


------------------------------------------------------------------------------
instance A.ToJSON CanEmulateNetworkConditions where
    toEncoding CanEmulateNetworkConditions = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON CanEmulateNetworkConditions = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup CanEmulateNetworkConditions where
    CanEmulateNetworkConditions <> CanEmulateNetworkConditions = CanEmulateNetworkConditions


------------------------------------------------------------------------------
instance P.Monoid CanEmulateNetworkConditions where
    mempty = CanEmulateNetworkConditions


------------------------------------------------------------------------------
-- | Tells whether emulation of network conditions is supported.
{-# DEPRECATED CanEmulateNetworkConditionsResult "This may be removed in a future release." #-}
data CanEmulateNetworkConditionsResult = CanEmulateNetworkConditionsResult
    { -- | True if emulation of network conditions is supported.
      result :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CanEmulateNetworkConditionsResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "canEmulateNetworkConditionsResult" $ \_o -> CanEmulateNetworkConditionsResult
            <$> _o .: "result"
        ago = A.withArray "canEmulateNetworkConditionsResult" $ \_a -> CanEmulateNetworkConditionsResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON CanEmulateNetworkConditionsResult where
    toEncoding (CanEmulateNetworkConditionsResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "result" .= _0
        ]
    toJSON (CanEmulateNetworkConditionsResult _0) = A.object $ P.catMaybes
        [ P.pure $ "result" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup CanEmulateNetworkConditionsResult where
    CanEmulateNetworkConditionsResult _0 <> CanEmulateNetworkConditionsResult _ = CanEmulateNetworkConditionsResult _0


------------------------------------------------------------------------------
instance M.Method CanEmulateNetworkConditions where
    type Result CanEmulateNetworkConditions = CanEmulateNetworkConditionsResult
    name _ = "Network.canEmulateNetworkConditions"


------------------------------------------------------------------------------
-- | Tells whether emulation of network conditions is supported.
{-# DEPRECATED canEmulateNetworkConditions "This may be removed in a future release." #-}
canEmulateNetworkConditions
    :: CanEmulateNetworkConditions
canEmulateNetworkConditions = CanEmulateNetworkConditions


------------------------------------------------------------------------------
-- | Clears browser cache.
data ClearBrowserCache = ClearBrowserCache
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ClearBrowserCache where
    parseJSON A.Null = P.pure ClearBrowserCache
    parseJSON v = A.withArray "clearBrowserCache" go v
        <|> A.withObject "clearBrowserCache" go v
      where
        go _ = P.pure ClearBrowserCache


------------------------------------------------------------------------------
instance A.ToJSON ClearBrowserCache where
    toEncoding ClearBrowserCache = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON ClearBrowserCache = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup ClearBrowserCache where
    ClearBrowserCache <> ClearBrowserCache = ClearBrowserCache


------------------------------------------------------------------------------
instance P.Monoid ClearBrowserCache where
    mempty = ClearBrowserCache


------------------------------------------------------------------------------
instance M.Method ClearBrowserCache where
    type Result ClearBrowserCache = ()
    name _ = "Network.clearBrowserCache"


------------------------------------------------------------------------------
-- | Clears browser cache.
clearBrowserCache
    :: ClearBrowserCache
clearBrowserCache = ClearBrowserCache


------------------------------------------------------------------------------
-- | Clears browser cookies.
data ClearBrowserCookies = ClearBrowserCookies
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ClearBrowserCookies where
    parseJSON A.Null = P.pure ClearBrowserCookies
    parseJSON v = A.withArray "clearBrowserCookies" go v
        <|> A.withObject "clearBrowserCookies" go v
      where
        go _ = P.pure ClearBrowserCookies


------------------------------------------------------------------------------
instance A.ToJSON ClearBrowserCookies where
    toEncoding ClearBrowserCookies = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON ClearBrowserCookies = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup ClearBrowserCookies where
    ClearBrowserCookies <> ClearBrowserCookies = ClearBrowserCookies


------------------------------------------------------------------------------
instance P.Monoid ClearBrowserCookies where
    mempty = ClearBrowserCookies


------------------------------------------------------------------------------
instance M.Method ClearBrowserCookies where
    type Result ClearBrowserCookies = ()
    name _ = "Network.clearBrowserCookies"


------------------------------------------------------------------------------
-- | Clears browser cookies.
clearBrowserCookies
    :: ClearBrowserCookies
clearBrowserCookies = ClearBrowserCookies


------------------------------------------------------------------------------
-- | Response to Network.requestIntercepted which either modifies the request to continue with any
-- modifications, or blocks it, or completes it with the provided response bytes. If a network
-- fetch occurs as a result which encounters a redirect an additional Network.requestIntercepted
-- event will be sent with the same InterceptionId.
-- Deprecated, use Fetch.continueRequest, Fetch.fulfillRequest and Fetch.failRequest instead.
{-# DEPRECATED ContinueInterceptedRequest "This may be removed in a future release." #-}
{-{-# WARNING ContinueInterceptedRequest "This feature is marked as EXPERIMENTAL." #-}-}
data ContinueInterceptedRequest = ContinueInterceptedRequest
    { interceptionId :: !InterceptionId
      -- | If set this causes the request to fail with the given reason. Passing @Aborted@ for requests
      -- marked with @isNavigationRequest@ also cancels the navigation. Must not be set in response
      -- to an authChallenge.
    , errorReason :: !(P.Maybe ErrorReason)
      -- | If set the requests completes using with the provided base64 encoded raw response, including
      -- HTTP status line and headers etc... Must not be set in response to an authChallenge.
    , rawResponse :: !(P.Maybe T.Text)
      -- | If set the request url will be modified in a way that's not observable by page. Must not be
      -- set in response to an authChallenge.
    , url :: !(P.Maybe T.Text)
      -- | If set this allows the request method to be overridden. Must not be set in response to an
      -- authChallenge.
    , method :: !(P.Maybe T.Text)
      -- | If set this allows postData to be set. Must not be set in response to an authChallenge.
    , postData :: !(P.Maybe T.Text)
      -- | If set this allows the request headers to be changed. Must not be set in response to an
      -- authChallenge.
    , headers :: !(P.Maybe Headers)
      -- | Response to a requestIntercepted with an authChallenge. Must not be set otherwise.
    , authChallengeResponse :: !(P.Maybe AuthChallengeResponse)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ContinueInterceptedRequest where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "continueInterceptedRequest" $ \_o -> ContinueInterceptedRequest
            <$> _o .: "interceptionId"
            <*> _o .:? "errorReason"
            <*> _o .:? "rawResponse"
            <*> _o .:? "url"
            <*> _o .:? "method"
            <*> _o .:? "postData"
            <*> _o .:? "headers"
            <*> _o .:? "authChallengeResponse"
        ago = A.withArray "continueInterceptedRequest" $ \_a -> ContinueInterceptedRequest
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)
            <*> P.traverse A.parseJSON (_a !? 7)


------------------------------------------------------------------------------
instance A.ToJSON ContinueInterceptedRequest where
    toEncoding (ContinueInterceptedRequest _0 _1 _2 _3 _4 _5 _6 _7) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "interceptionId" .= _0
        , ("errorReason" .=) <$> _1
        , ("rawResponse" .=) <$> _2
        , ("url" .=) <$> _3
        , ("method" .=) <$> _4
        , ("postData" .=) <$> _5
        , ("headers" .=) <$> _6
        , ("authChallengeResponse" .=) <$> _7
        ]
    toJSON (ContinueInterceptedRequest _0 _1 _2 _3 _4 _5 _6 _7) = A.object $ P.catMaybes
        [ P.pure $ "interceptionId" .= _0
        , ("errorReason" .=) <$> _1
        , ("rawResponse" .=) <$> _2
        , ("url" .=) <$> _3
        , ("method" .=) <$> _4
        , ("postData" .=) <$> _5
        , ("headers" .=) <$> _6
        , ("authChallengeResponse" .=) <$> _7
        ]


------------------------------------------------------------------------------
instance P.Semigroup ContinueInterceptedRequest where
    ContinueInterceptedRequest _0 _1 _2 _3 _4 _5 _6 _7 <> ContinueInterceptedRequest _ __1 __2 __3 __4 __5 __6 __7 = ContinueInterceptedRequest _0 (_1 <|> __1) (_2 <|> __2) (_3 <|> __3) (_4 <|> __4) (_5 <|> __5) (_6 <|> __6) (_7 <|> __7)


------------------------------------------------------------------------------
instance M.Method ContinueInterceptedRequest where
    type Result ContinueInterceptedRequest = ()
    name _ = "Network.continueInterceptedRequest"


------------------------------------------------------------------------------
-- | Response to Network.requestIntercepted which either modifies the request to continue with any
-- modifications, or blocks it, or completes it with the provided response bytes. If a network
-- fetch occurs as a result which encounters a redirect an additional Network.requestIntercepted
-- event will be sent with the same InterceptionId.
-- Deprecated, use Fetch.continueRequest, Fetch.fulfillRequest and Fetch.failRequest instead.
{-# DEPRECATED continueInterceptedRequest "This may be removed in a future release." #-}
{-{-# WARNING continueInterceptedRequest "This feature is marked as EXPERIMENTAL." #-}-}
continueInterceptedRequest
    :: InterceptionId
    -> ContinueInterceptedRequest
continueInterceptedRequest _0 = ContinueInterceptedRequest _0 P.empty P.empty P.empty P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Deletes browser cookies with matching name and url or domain\/path pair.
data DeleteCookies = DeleteCookies
    { -- | Name of the cookies to remove.
      name :: !T.Text
      -- | If specified, deletes all the cookies with the given name where domain and path match
      -- provided URL.
    , url :: !(P.Maybe T.Text)
      -- | If specified, deletes only cookies with the exact domain.
    , domain :: !(P.Maybe T.Text)
      -- | If specified, deletes only cookies with the exact path.
    , path :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DeleteCookies where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "deleteCookies" $ \_o -> DeleteCookies
            <$> _o .: "name"
            <*> _o .:? "url"
            <*> _o .:? "domain"
            <*> _o .:? "path"
        ago = A.withArray "deleteCookies" $ \_a -> DeleteCookies
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON DeleteCookies where
    toEncoding (DeleteCookies _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "name" .= _0
        , ("url" .=) <$> _1
        , ("domain" .=) <$> _2
        , ("path" .=) <$> _3
        ]
    toJSON (DeleteCookies _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "name" .= _0
        , ("url" .=) <$> _1
        , ("domain" .=) <$> _2
        , ("path" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup DeleteCookies where
    DeleteCookies _0 _1 _2 _3 <> DeleteCookies _ __1 __2 __3 = DeleteCookies _0 (_1 <|> __1) (_2 <|> __2) (_3 <|> __3)


------------------------------------------------------------------------------
instance M.Method DeleteCookies where
    type Result DeleteCookies = ()
    name _ = "Network.deleteCookies"


------------------------------------------------------------------------------
-- | Deletes browser cookies with matching name and url or domain\/path pair.
deleteCookies
    :: T.Text
    -- ^ Name of the cookies to remove.

    -> DeleteCookies
deleteCookies _0 = DeleteCookies _0 P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Disables network tracking, prevents network events from being sent to the client.
data Disable = Disable
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Disable where
    parseJSON A.Null = P.pure Disable
    parseJSON v = A.withArray "disable" go v
        <|> A.withObject "disable" go v
      where
        go _ = P.pure Disable


------------------------------------------------------------------------------
instance A.ToJSON Disable where
    toEncoding Disable = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON Disable = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup Disable where
    Disable <> Disable = Disable


------------------------------------------------------------------------------
instance P.Monoid Disable where
    mempty = Disable


------------------------------------------------------------------------------
instance M.Method Disable where
    type Result Disable = ()
    name _ = "Network.disable"


------------------------------------------------------------------------------
-- | Disables network tracking, prevents network events from being sent to the client.
disable
    :: Disable
disable = Disable


------------------------------------------------------------------------------
-- | Activates emulation of network conditions.
data EmulateNetworkConditions = EmulateNetworkConditions
    { -- | True to emulate internet disconnection.
      offline :: !P.Bool
      -- | Minimum latency from request sent to response headers received (ms).
    , latency :: !P.Double
      -- | Maximal aggregated download throughput (bytes\/sec). -1 disables download throttling.
    , downloadThroughput :: !P.Double
      -- | Maximal aggregated upload throughput (bytes\/sec).  -1 disables upload throttling.
    , uploadThroughput :: !P.Double
      -- | Connection type if known.
    , connectionType :: !(P.Maybe ConnectionType)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON EmulateNetworkConditions where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "emulateNetworkConditions" $ \_o -> EmulateNetworkConditions
            <$> _o .: "offline"
            <*> _o .: "latency"
            <*> _o .: "downloadThroughput"
            <*> _o .: "uploadThroughput"
            <*> _o .:? "connectionType"
        ago = A.withArray "emulateNetworkConditions" $ \_a -> EmulateNetworkConditions
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON EmulateNetworkConditions where
    toEncoding (EmulateNetworkConditions _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "offline" .= _0
        , P.pure $ "latency" .= _1
        , P.pure $ "downloadThroughput" .= _2
        , P.pure $ "uploadThroughput" .= _3
        , ("connectionType" .=) <$> _4
        ]
    toJSON (EmulateNetworkConditions _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ P.pure $ "offline" .= _0
        , P.pure $ "latency" .= _1
        , P.pure $ "downloadThroughput" .= _2
        , P.pure $ "uploadThroughput" .= _3
        , ("connectionType" .=) <$> _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup EmulateNetworkConditions where
    EmulateNetworkConditions _0 _1 _2 _3 _4 <> EmulateNetworkConditions _ _ _ _ __4 = EmulateNetworkConditions _0 _1 _2 _3 (_4 <|> __4)


------------------------------------------------------------------------------
instance M.Method EmulateNetworkConditions where
    type Result EmulateNetworkConditions = ()
    name _ = "Network.emulateNetworkConditions"


------------------------------------------------------------------------------
-- | Activates emulation of network conditions.
emulateNetworkConditions
    :: P.Bool
    -- ^ True to emulate internet disconnection.

    -> P.Double
    -- ^ Minimum latency from request sent to response headers received (ms).

    -> P.Double
    -- ^ Maximal aggregated download throughput (bytes\/sec). -1 disables download throttling.

    -> P.Double
    -- ^ Maximal aggregated upload throughput (bytes\/sec).  -1 disables upload throttling.

    -> EmulateNetworkConditions
emulateNetworkConditions _0 _1 _2 _3 = EmulateNetworkConditions _0 _1 _2 _3 P.empty


------------------------------------------------------------------------------
-- | Enables network tracking, network events will now be delivered to the client.
{-# WARNING maxTotalBufferSize, maxResourceBufferSize "This feature is marked as EXPERIMENTAL." #-}
data Enable = Enable
    { -- | Buffer size in bytes to use when preserving network payloads (XHRs, etc).
      maxTotalBufferSize :: !(P.Maybe P.Int)
      -- | Per-resource buffer size in bytes to use when preserving network payloads (XHRs, etc).
    , maxResourceBufferSize :: !(P.Maybe P.Int)
      -- | Longest post body size (in bytes) that would be included in requestWillBeSent notification
    , maxPostDataSize :: !(P.Maybe P.Int)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Enable where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "enable" $ \_o -> Enable
            <$> _o .:? "maxTotalBufferSize"
            <*> _o .:? "maxResourceBufferSize"
            <*> _o .:? "maxPostDataSize"
        ago = A.withArray "enable" $ \_a -> Enable
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON Enable where
    toEncoding (Enable _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ ("maxTotalBufferSize" .=) <$> _0
        , ("maxResourceBufferSize" .=) <$> _1
        , ("maxPostDataSize" .=) <$> _2
        ]
    toJSON (Enable _0 _1 _2) = A.object $ P.catMaybes
        [ ("maxTotalBufferSize" .=) <$> _0
        , ("maxResourceBufferSize" .=) <$> _1
        , ("maxPostDataSize" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup Enable where
    Enable _0 _1 _2 <> Enable __0 __1 __2 = Enable (_0 <|> __0) (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
instance P.Monoid Enable where
    mempty = Enable P.empty P.empty P.empty


------------------------------------------------------------------------------
instance M.Method Enable where
    type Result Enable = ()
    name _ = "Network.enable"


------------------------------------------------------------------------------
-- | Enables network tracking, network events will now be delivered to the client.
enable
    :: Enable
enable = Enable P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Returns all browser cookies. Depending on the backend support, will return detailed cookie
-- information in the @cookies@ field.
data GetAllCookies = GetAllCookies
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetAllCookies where
    parseJSON A.Null = P.pure GetAllCookies
    parseJSON v = A.withArray "getAllCookies" go v
        <|> A.withObject "getAllCookies" go v
      where
        go _ = P.pure GetAllCookies


------------------------------------------------------------------------------
instance A.ToJSON GetAllCookies where
    toEncoding GetAllCookies = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetAllCookies = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetAllCookies where
    GetAllCookies <> GetAllCookies = GetAllCookies


------------------------------------------------------------------------------
instance P.Monoid GetAllCookies where
    mempty = GetAllCookies


------------------------------------------------------------------------------
-- | Returns all browser cookies. Depending on the backend support, will return detailed cookie
-- information in the @cookies@ field.
data GetAllCookiesResult = GetAllCookiesResult
    { -- | Array of cookie objects.
      cookies :: ![Cookie]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetAllCookiesResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getAllCookiesResult" $ \_o -> GetAllCookiesResult
            <$> _o .: "cookies"
        ago = A.withArray "getAllCookiesResult" $ \_a -> GetAllCookiesResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetAllCookiesResult where
    toEncoding (GetAllCookiesResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "cookies" .= _0
        ]
    toJSON (GetAllCookiesResult _0) = A.object $ P.catMaybes
        [ P.pure $ "cookies" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetAllCookiesResult where
    GetAllCookiesResult _0 <> GetAllCookiesResult _ = GetAllCookiesResult _0


------------------------------------------------------------------------------
instance M.Method GetAllCookies where
    type Result GetAllCookies = GetAllCookiesResult
    name _ = "Network.getAllCookies"


------------------------------------------------------------------------------
-- | Returns all browser cookies. Depending on the backend support, will return detailed cookie
-- information in the @cookies@ field.
getAllCookies
    :: GetAllCookies
getAllCookies = GetAllCookies


------------------------------------------------------------------------------
-- | Returns the DER-encoded certificate.
{-# WARNING GetCertificate "This feature is marked as EXPERIMENTAL." #-}
data GetCertificate = GetCertificate
    { -- | Origin to get certificate for.
      origin :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetCertificate where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getCertificate" $ \_o -> GetCertificate
            <$> _o .: "origin"
        ago = A.withArray "getCertificate" $ \_a -> GetCertificate
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetCertificate where
    toEncoding (GetCertificate _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "origin" .= _0
        ]
    toJSON (GetCertificate _0) = A.object $ P.catMaybes
        [ P.pure $ "origin" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetCertificate where
    GetCertificate _0 <> GetCertificate _ = GetCertificate _0


------------------------------------------------------------------------------
-- | Returns the DER-encoded certificate.
{-# WARNING GetCertificateResult "This feature is marked as EXPERIMENTAL." #-}
data GetCertificateResult = GetCertificateResult
    { tableNames :: ![T.Text]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetCertificateResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getCertificateResult" $ \_o -> GetCertificateResult
            <$> _o .: "tableNames"
        ago = A.withArray "getCertificateResult" $ \_a -> GetCertificateResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetCertificateResult where
    toEncoding (GetCertificateResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "tableNames" .= _0
        ]
    toJSON (GetCertificateResult _0) = A.object $ P.catMaybes
        [ P.pure $ "tableNames" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetCertificateResult where
    GetCertificateResult _0 <> GetCertificateResult _ = GetCertificateResult _0


------------------------------------------------------------------------------
instance M.Method GetCertificate where
    type Result GetCertificate = GetCertificateResult
    name _ = "Network.getCertificate"


------------------------------------------------------------------------------
-- | Returns the DER-encoded certificate.
{-# WARNING getCertificate "This feature is marked as EXPERIMENTAL." #-}
getCertificate
    :: T.Text
    -- ^ Origin to get certificate for.

    -> GetCertificate
getCertificate _0 = GetCertificate _0


------------------------------------------------------------------------------
-- | Returns all browser cookies for the current URL. Depending on the backend support, will return
-- detailed cookie information in the @cookies@ field.
data GetCookies = GetCookies
    { -- | The list of URLs for which applicable cookies will be fetched
      urls :: !(P.Maybe [T.Text])
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetCookies where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getCookies" $ \_o -> GetCookies
            <$> _o .:? "urls"
        ago = A.withArray "getCookies" $ \_a -> GetCookies
            <$> P.traverse A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetCookies where
    toEncoding (GetCookies _0) = A.pairs $ P.fold $ P.catMaybes
        [ ("urls" .=) <$> _0
        ]
    toJSON (GetCookies _0) = A.object $ P.catMaybes
        [ ("urls" .=) <$> _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetCookies where
    GetCookies _0 <> GetCookies __0 = GetCookies (_0 <|> __0)


------------------------------------------------------------------------------
instance P.Monoid GetCookies where
    mempty = GetCookies P.empty


------------------------------------------------------------------------------
-- | Returns all browser cookies for the current URL. Depending on the backend support, will return
-- detailed cookie information in the @cookies@ field.
data GetCookiesResult = GetCookiesResult
    { -- | Array of cookie objects.
      cookies :: ![Cookie]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetCookiesResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getCookiesResult" $ \_o -> GetCookiesResult
            <$> _o .: "cookies"
        ago = A.withArray "getCookiesResult" $ \_a -> GetCookiesResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetCookiesResult where
    toEncoding (GetCookiesResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "cookies" .= _0
        ]
    toJSON (GetCookiesResult _0) = A.object $ P.catMaybes
        [ P.pure $ "cookies" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetCookiesResult where
    GetCookiesResult _0 <> GetCookiesResult _ = GetCookiesResult _0


------------------------------------------------------------------------------
instance M.Method GetCookies where
    type Result GetCookies = GetCookiesResult
    name _ = "Network.getCookies"


------------------------------------------------------------------------------
-- | Returns all browser cookies for the current URL. Depending on the backend support, will return
-- detailed cookie information in the @cookies@ field.
getCookies
    :: GetCookies
getCookies = GetCookies P.empty


------------------------------------------------------------------------------
-- | Returns content served for the given request.
data GetResponseBody = GetResponseBody
    { -- | Identifier of the network request to get content for.
      requestId :: !RequestId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetResponseBody where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getResponseBody" $ \_o -> GetResponseBody
            <$> _o .: "requestId"
        ago = A.withArray "getResponseBody" $ \_a -> GetResponseBody
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetResponseBody where
    toEncoding (GetResponseBody _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        ]
    toJSON (GetResponseBody _0) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetResponseBody where
    GetResponseBody _0 <> GetResponseBody _ = GetResponseBody _0


------------------------------------------------------------------------------
-- | Returns content served for the given request.
data GetResponseBodyResult = GetResponseBodyResult
    { -- | Response body.
      body :: !T.Text
      -- | True, if content was sent as base64.
    , base64Encoded :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetResponseBodyResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getResponseBodyResult" $ \_o -> GetResponseBodyResult
            <$> _o .: "body"
            <*> _o .: "base64Encoded"
        ago = A.withArray "getResponseBodyResult" $ \_a -> GetResponseBodyResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON GetResponseBodyResult where
    toEncoding (GetResponseBodyResult _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "body" .= _0
        , P.pure $ "base64Encoded" .= _1
        ]
    toJSON (GetResponseBodyResult _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "body" .= _0
        , P.pure $ "base64Encoded" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetResponseBodyResult where
    GetResponseBodyResult _0 _1 <> GetResponseBodyResult _ _ = GetResponseBodyResult _0 _1


------------------------------------------------------------------------------
instance M.Method GetResponseBody where
    type Result GetResponseBody = GetResponseBodyResult
    name _ = "Network.getResponseBody"


------------------------------------------------------------------------------
-- | Returns content served for the given request.
getResponseBody
    :: RequestId
    -- ^ Identifier of the network request to get content for.

    -> GetResponseBody
getResponseBody _0 = GetResponseBody _0


------------------------------------------------------------------------------
-- | Returns post data sent with the request. Returns an error when no data was sent with the request.
data GetRequestPostData = GetRequestPostData
    { -- | Identifier of the network request to get content for.
      requestId :: !RequestId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetRequestPostData where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getRequestPostData" $ \_o -> GetRequestPostData
            <$> _o .: "requestId"
        ago = A.withArray "getRequestPostData" $ \_a -> GetRequestPostData
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetRequestPostData where
    toEncoding (GetRequestPostData _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        ]
    toJSON (GetRequestPostData _0) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetRequestPostData where
    GetRequestPostData _0 <> GetRequestPostData _ = GetRequestPostData _0


------------------------------------------------------------------------------
-- | Returns post data sent with the request. Returns an error when no data was sent with the request.
data GetRequestPostDataResult = GetRequestPostDataResult
    { -- | Request body string, omitting files from multipart requests
      postData :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetRequestPostDataResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getRequestPostDataResult" $ \_o -> GetRequestPostDataResult
            <$> _o .: "postData"
        ago = A.withArray "getRequestPostDataResult" $ \_a -> GetRequestPostDataResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetRequestPostDataResult where
    toEncoding (GetRequestPostDataResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "postData" .= _0
        ]
    toJSON (GetRequestPostDataResult _0) = A.object $ P.catMaybes
        [ P.pure $ "postData" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetRequestPostDataResult where
    GetRequestPostDataResult _0 <> GetRequestPostDataResult _ = GetRequestPostDataResult _0


------------------------------------------------------------------------------
instance M.Method GetRequestPostData where
    type Result GetRequestPostData = GetRequestPostDataResult
    name _ = "Network.getRequestPostData"


------------------------------------------------------------------------------
-- | Returns post data sent with the request. Returns an error when no data was sent with the request.
getRequestPostData
    :: RequestId
    -- ^ Identifier of the network request to get content for.

    -> GetRequestPostData
getRequestPostData _0 = GetRequestPostData _0


------------------------------------------------------------------------------
-- | Returns content served for the given currently intercepted request.
{-# WARNING GetResponseBodyForInterception "This feature is marked as EXPERIMENTAL." #-}
data GetResponseBodyForInterception = GetResponseBodyForInterception
    { -- | Identifier for the intercepted request to get body for.
      interceptionId :: !InterceptionId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetResponseBodyForInterception where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getResponseBodyForInterception" $ \_o -> GetResponseBodyForInterception
            <$> _o .: "interceptionId"
        ago = A.withArray "getResponseBodyForInterception" $ \_a -> GetResponseBodyForInterception
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetResponseBodyForInterception where
    toEncoding (GetResponseBodyForInterception _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "interceptionId" .= _0
        ]
    toJSON (GetResponseBodyForInterception _0) = A.object $ P.catMaybes
        [ P.pure $ "interceptionId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetResponseBodyForInterception where
    GetResponseBodyForInterception _0 <> GetResponseBodyForInterception _ = GetResponseBodyForInterception _0


------------------------------------------------------------------------------
-- | Returns content served for the given currently intercepted request.
{-# WARNING GetResponseBodyForInterceptionResult "This feature is marked as EXPERIMENTAL." #-}
data GetResponseBodyForInterceptionResult = GetResponseBodyForInterceptionResult
    { -- | Response body.
      body :: !T.Text
      -- | True, if content was sent as base64.
    , base64Encoded :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetResponseBodyForInterceptionResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getResponseBodyForInterceptionResult" $ \_o -> GetResponseBodyForInterceptionResult
            <$> _o .: "body"
            <*> _o .: "base64Encoded"
        ago = A.withArray "getResponseBodyForInterceptionResult" $ \_a -> GetResponseBodyForInterceptionResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON GetResponseBodyForInterceptionResult where
    toEncoding (GetResponseBodyForInterceptionResult _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "body" .= _0
        , P.pure $ "base64Encoded" .= _1
        ]
    toJSON (GetResponseBodyForInterceptionResult _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "body" .= _0
        , P.pure $ "base64Encoded" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetResponseBodyForInterceptionResult where
    GetResponseBodyForInterceptionResult _0 _1 <> GetResponseBodyForInterceptionResult _ _ = GetResponseBodyForInterceptionResult _0 _1


------------------------------------------------------------------------------
instance M.Method GetResponseBodyForInterception where
    type Result GetResponseBodyForInterception = GetResponseBodyForInterceptionResult
    name _ = "Network.getResponseBodyForInterception"


------------------------------------------------------------------------------
-- | Returns content served for the given currently intercepted request.
{-# WARNING getResponseBodyForInterception "This feature is marked as EXPERIMENTAL." #-}
getResponseBodyForInterception
    :: InterceptionId
    -- ^ Identifier for the intercepted request to get body for.

    -> GetResponseBodyForInterception
getResponseBodyForInterception _0 = GetResponseBodyForInterception _0


------------------------------------------------------------------------------
-- | Returns a handle to the stream representing the response body. Note that after this command,
-- the intercepted request can't be continued as is -- you either need to cancel it or to provide
-- the response body. The stream only supports sequential read, IO.read will fail if the position
-- is specified.
{-# WARNING TakeResponseBodyForInterceptionAsStream "This feature is marked as EXPERIMENTAL." #-}
data TakeResponseBodyForInterceptionAsStream = TakeResponseBodyForInterceptionAsStream
    { interceptionId :: !InterceptionId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TakeResponseBodyForInterceptionAsStream where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "takeResponseBodyForInterceptionAsStream" $ \_o -> TakeResponseBodyForInterceptionAsStream
            <$> _o .: "interceptionId"
        ago = A.withArray "takeResponseBodyForInterceptionAsStream" $ \_a -> TakeResponseBodyForInterceptionAsStream
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON TakeResponseBodyForInterceptionAsStream where
    toEncoding (TakeResponseBodyForInterceptionAsStream _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "interceptionId" .= _0
        ]
    toJSON (TakeResponseBodyForInterceptionAsStream _0) = A.object $ P.catMaybes
        [ P.pure $ "interceptionId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup TakeResponseBodyForInterceptionAsStream where
    TakeResponseBodyForInterceptionAsStream _0 <> TakeResponseBodyForInterceptionAsStream _ = TakeResponseBodyForInterceptionAsStream _0


------------------------------------------------------------------------------
-- | Returns a handle to the stream representing the response body. Note that after this command,
-- the intercepted request can't be continued as is -- you either need to cancel it or to provide
-- the response body. The stream only supports sequential read, IO.read will fail if the position
-- is specified.
{-# WARNING TakeResponseBodyForInterceptionAsStreamResult "This feature is marked as EXPERIMENTAL." #-}
data TakeResponseBodyForInterceptionAsStreamResult = TakeResponseBodyForInterceptionAsStreamResult
    { stream :: !IO.StreamHandle
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TakeResponseBodyForInterceptionAsStreamResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "takeResponseBodyForInterceptionAsStreamResult" $ \_o -> TakeResponseBodyForInterceptionAsStreamResult
            <$> _o .: "stream"
        ago = A.withArray "takeResponseBodyForInterceptionAsStreamResult" $ \_a -> TakeResponseBodyForInterceptionAsStreamResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON TakeResponseBodyForInterceptionAsStreamResult where
    toEncoding (TakeResponseBodyForInterceptionAsStreamResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "stream" .= _0
        ]
    toJSON (TakeResponseBodyForInterceptionAsStreamResult _0) = A.object $ P.catMaybes
        [ P.pure $ "stream" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup TakeResponseBodyForInterceptionAsStreamResult where
    TakeResponseBodyForInterceptionAsStreamResult _0 <> TakeResponseBodyForInterceptionAsStreamResult _ = TakeResponseBodyForInterceptionAsStreamResult _0


------------------------------------------------------------------------------
instance M.Method TakeResponseBodyForInterceptionAsStream where
    type Result TakeResponseBodyForInterceptionAsStream = TakeResponseBodyForInterceptionAsStreamResult
    name _ = "Network.takeResponseBodyForInterceptionAsStream"


------------------------------------------------------------------------------
-- | Returns a handle to the stream representing the response body. Note that after this command,
-- the intercepted request can't be continued as is -- you either need to cancel it or to provide
-- the response body. The stream only supports sequential read, IO.read will fail if the position
-- is specified.
{-# WARNING takeResponseBodyForInterceptionAsStream "This feature is marked as EXPERIMENTAL." #-}
takeResponseBodyForInterceptionAsStream
    :: InterceptionId
    -> TakeResponseBodyForInterceptionAsStream
takeResponseBodyForInterceptionAsStream _0 = TakeResponseBodyForInterceptionAsStream _0


------------------------------------------------------------------------------
-- | This method sends a new XMLHttpRequest which is identical to the original one. The following
-- parameters should be identical: method, url, async, request body, extra headers, withCredentials
-- attribute, user, password.
{-# WARNING ReplayXHR "This feature is marked as EXPERIMENTAL." #-}
data ReplayXHR = ReplayXHR
    { -- | Identifier of XHR to replay.
      requestId :: !RequestId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ReplayXHR where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "replayXHR" $ \_o -> ReplayXHR
            <$> _o .: "requestId"
        ago = A.withArray "replayXHR" $ \_a -> ReplayXHR
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON ReplayXHR where
    toEncoding (ReplayXHR _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        ]
    toJSON (ReplayXHR _0) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup ReplayXHR where
    ReplayXHR _0 <> ReplayXHR _ = ReplayXHR _0


------------------------------------------------------------------------------
instance M.Method ReplayXHR where
    type Result ReplayXHR = ()
    name _ = "Network.replayXHR"


------------------------------------------------------------------------------
-- | This method sends a new XMLHttpRequest which is identical to the original one. The following
-- parameters should be identical: method, url, async, request body, extra headers, withCredentials
-- attribute, user, password.
{-# WARNING replayXHR "This feature is marked as EXPERIMENTAL." #-}
replayXHR
    :: RequestId
    -- ^ Identifier of XHR to replay.

    -> ReplayXHR
replayXHR _0 = ReplayXHR _0


------------------------------------------------------------------------------
-- | Searches for given string in response content.
{-# WARNING SearchInResponseBody "This feature is marked as EXPERIMENTAL." #-}
data SearchInResponseBody = SearchInResponseBody
    { -- | Identifier of the network response to search.
      requestId :: !RequestId
      -- | String to search for.
    , query :: !T.Text
      -- | If true, search is case sensitive.
    , caseSensitive :: !(P.Maybe P.Bool)
      -- | If true, treats string parameter as regex.
    , isRegex :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SearchInResponseBody where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "searchInResponseBody" $ \_o -> SearchInResponseBody
            <$> _o .: "requestId"
            <*> _o .: "query"
            <*> _o .:? "caseSensitive"
            <*> _o .:? "isRegex"
        ago = A.withArray "searchInResponseBody" $ \_a -> SearchInResponseBody
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON SearchInResponseBody where
    toEncoding (SearchInResponseBody _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "query" .= _1
        , ("caseSensitive" .=) <$> _2
        , ("isRegex" .=) <$> _3
        ]
    toJSON (SearchInResponseBody _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "query" .= _1
        , ("caseSensitive" .=) <$> _2
        , ("isRegex" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup SearchInResponseBody where
    SearchInResponseBody _0 _1 _2 _3 <> SearchInResponseBody _ _ __2 __3 = SearchInResponseBody _0 _1 (_2 <|> __2) (_3 <|> __3)


------------------------------------------------------------------------------
-- | Searches for given string in response content.
{-# WARNING SearchInResponseBodyResult "This feature is marked as EXPERIMENTAL." #-}
data SearchInResponseBodyResult = SearchInResponseBodyResult
    { -- | List of search matches.
      result :: ![Debugger.SearchMatch]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SearchInResponseBodyResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "searchInResponseBodyResult" $ \_o -> SearchInResponseBodyResult
            <$> _o .: "result"
        ago = A.withArray "searchInResponseBodyResult" $ \_a -> SearchInResponseBodyResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SearchInResponseBodyResult where
    toEncoding (SearchInResponseBodyResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "result" .= _0
        ]
    toJSON (SearchInResponseBodyResult _0) = A.object $ P.catMaybes
        [ P.pure $ "result" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SearchInResponseBodyResult where
    SearchInResponseBodyResult _0 <> SearchInResponseBodyResult _ = SearchInResponseBodyResult _0


------------------------------------------------------------------------------
instance M.Method SearchInResponseBody where
    type Result SearchInResponseBody = SearchInResponseBodyResult
    name _ = "Network.searchInResponseBody"


------------------------------------------------------------------------------
-- | Searches for given string in response content.
{-# WARNING searchInResponseBody "This feature is marked as EXPERIMENTAL." #-}
searchInResponseBody
    :: RequestId
    -- ^ Identifier of the network response to search.

    -> T.Text
    -- ^ String to search for.

    -> SearchInResponseBody
searchInResponseBody _0 _1 = SearchInResponseBody _0 _1 P.empty P.empty


------------------------------------------------------------------------------
-- | Blocks URLs from loading.
{-# WARNING SetBlockedURLs "This feature is marked as EXPERIMENTAL." #-}
data SetBlockedURLs = SetBlockedURLs
    { -- | URL patterns to block. Wildcards ('*') are allowed.
      urls :: ![T.Text]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetBlockedURLs where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setBlockedURLs" $ \_o -> SetBlockedURLs
            <$> _o .: "urls"
        ago = A.withArray "setBlockedURLs" $ \_a -> SetBlockedURLs
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetBlockedURLs where
    toEncoding (SetBlockedURLs _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "urls" .= _0
        ]
    toJSON (SetBlockedURLs _0) = A.object $ P.catMaybes
        [ P.pure $ "urls" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetBlockedURLs where
    SetBlockedURLs _0 <> SetBlockedURLs _ = SetBlockedURLs _0


------------------------------------------------------------------------------
instance M.Method SetBlockedURLs where
    type Result SetBlockedURLs = ()
    name _ = "Network.setBlockedURLs"


------------------------------------------------------------------------------
-- | Blocks URLs from loading.
{-# WARNING setBlockedURLs "This feature is marked as EXPERIMENTAL." #-}
setBlockedURLs
    :: [T.Text]
    -- ^ URL patterns to block. Wildcards ('*') are allowed.

    -> SetBlockedURLs
setBlockedURLs _0 = SetBlockedURLs _0


------------------------------------------------------------------------------
-- | Toggles ignoring of service worker for each request.
{-# WARNING SetBypassServiceWorker "This feature is marked as EXPERIMENTAL." #-}
data SetBypassServiceWorker = SetBypassServiceWorker
    { -- | Bypass service worker and load from network.
      bypass :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetBypassServiceWorker where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setBypassServiceWorker" $ \_o -> SetBypassServiceWorker
            <$> _o .: "bypass"
        ago = A.withArray "setBypassServiceWorker" $ \_a -> SetBypassServiceWorker
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetBypassServiceWorker where
    toEncoding (SetBypassServiceWorker _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "bypass" .= _0
        ]
    toJSON (SetBypassServiceWorker _0) = A.object $ P.catMaybes
        [ P.pure $ "bypass" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetBypassServiceWorker where
    SetBypassServiceWorker _0 <> SetBypassServiceWorker _ = SetBypassServiceWorker _0


------------------------------------------------------------------------------
instance M.Method SetBypassServiceWorker where
    type Result SetBypassServiceWorker = ()
    name _ = "Network.setBypassServiceWorker"


------------------------------------------------------------------------------
-- | Toggles ignoring of service worker for each request.
{-# WARNING setBypassServiceWorker "This feature is marked as EXPERIMENTAL." #-}
setBypassServiceWorker
    :: P.Bool
    -- ^ Bypass service worker and load from network.

    -> SetBypassServiceWorker
setBypassServiceWorker _0 = SetBypassServiceWorker _0


------------------------------------------------------------------------------
-- | Toggles ignoring cache for each request. If @true@, cache will not be used.
data SetCacheDisabled = SetCacheDisabled
    { -- | Cache disabled state.
      cacheDisabled :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetCacheDisabled where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setCacheDisabled" $ \_o -> SetCacheDisabled
            <$> _o .: "cacheDisabled"
        ago = A.withArray "setCacheDisabled" $ \_a -> SetCacheDisabled
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetCacheDisabled where
    toEncoding (SetCacheDisabled _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "cacheDisabled" .= _0
        ]
    toJSON (SetCacheDisabled _0) = A.object $ P.catMaybes
        [ P.pure $ "cacheDisabled" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetCacheDisabled where
    SetCacheDisabled _0 <> SetCacheDisabled _ = SetCacheDisabled _0


------------------------------------------------------------------------------
instance M.Method SetCacheDisabled where
    type Result SetCacheDisabled = ()
    name _ = "Network.setCacheDisabled"


------------------------------------------------------------------------------
-- | Toggles ignoring cache for each request. If @true@, cache will not be used.
setCacheDisabled
    :: P.Bool
    -- ^ Cache disabled state.

    -> SetCacheDisabled
setCacheDisabled _0 = SetCacheDisabled _0


------------------------------------------------------------------------------
-- | Sets a cookie with the given cookie data; may overwrite equivalent cookies if they exist.
data SetCookie = SetCookie
    { -- | Cookie name.
      name :: !T.Text
      -- | Cookie value.
    , value :: !T.Text
      -- | The request-URI to associate with the setting of the cookie. This value can affect the
      -- default domain and path values of the created cookie.
    , url :: !(P.Maybe T.Text)
      -- | Cookie domain.
    , domain :: !(P.Maybe T.Text)
      -- | Cookie path.
    , path :: !(P.Maybe T.Text)
      -- | True if cookie is secure.
    , secure :: !(P.Maybe P.Bool)
      -- | True if cookie is http-only.
    , httpOnly :: !(P.Maybe P.Bool)
      -- | Cookie SameSite type.
    , sameSite :: !(P.Maybe CookieSameSite)
      -- | Cookie expiration date, session cookie if not set
    , expires :: !(P.Maybe TimeSinceEpoch)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetCookie where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setCookie" $ \_o -> SetCookie
            <$> _o .: "name"
            <*> _o .: "value"
            <*> _o .:? "url"
            <*> _o .:? "domain"
            <*> _o .:? "path"
            <*> _o .:? "secure"
            <*> _o .:? "httpOnly"
            <*> _o .:? "sameSite"
            <*> _o .:? "expires"
        ago = A.withArray "setCookie" $ \_a -> SetCookie
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)
            <*> P.traverse A.parseJSON (_a !? 7)
            <*> P.traverse A.parseJSON (_a !? 8)


------------------------------------------------------------------------------
instance A.ToJSON SetCookie where
    toEncoding (SetCookie _0 _1 _2 _3 _4 _5 _6 _7 _8) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "value" .= _1
        , ("url" .=) <$> _2
        , ("domain" .=) <$> _3
        , ("path" .=) <$> _4
        , ("secure" .=) <$> _5
        , ("httpOnly" .=) <$> _6
        , ("sameSite" .=) <$> _7
        , ("expires" .=) <$> _8
        ]
    toJSON (SetCookie _0 _1 _2 _3 _4 _5 _6 _7 _8) = A.object $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "value" .= _1
        , ("url" .=) <$> _2
        , ("domain" .=) <$> _3
        , ("path" .=) <$> _4
        , ("secure" .=) <$> _5
        , ("httpOnly" .=) <$> _6
        , ("sameSite" .=) <$> _7
        , ("expires" .=) <$> _8
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetCookie where
    SetCookie _0 _1 _2 _3 _4 _5 _6 _7 _8 <> SetCookie _ _ __2 __3 __4 __5 __6 __7 __8 = SetCookie _0 _1 (_2 <|> __2) (_3 <|> __3) (_4 <|> __4) (_5 <|> __5) (_6 <|> __6) (_7 <|> __7) (_8 <|> __8)


------------------------------------------------------------------------------
-- | Sets a cookie with the given cookie data; may overwrite equivalent cookies if they exist.
data SetCookieResult = SetCookieResult
    { -- | True if successfully set cookie.
      success :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetCookieResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setCookieResult" $ \_o -> SetCookieResult
            <$> _o .: "success"
        ago = A.withArray "setCookieResult" $ \_a -> SetCookieResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetCookieResult where
    toEncoding (SetCookieResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "success" .= _0
        ]
    toJSON (SetCookieResult _0) = A.object $ P.catMaybes
        [ P.pure $ "success" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetCookieResult where
    SetCookieResult _0 <> SetCookieResult _ = SetCookieResult _0


------------------------------------------------------------------------------
instance M.Method SetCookie where
    type Result SetCookie = SetCookieResult
    name _ = "Network.setCookie"


------------------------------------------------------------------------------
-- | Sets a cookie with the given cookie data; may overwrite equivalent cookies if they exist.
setCookie
    :: T.Text
    -- ^ Cookie name.

    -> T.Text
    -- ^ Cookie value.

    -> SetCookie
setCookie _0 _1 = SetCookie _0 _1 P.empty P.empty P.empty P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Sets given cookies.
data SetCookies = SetCookies
    { -- | Cookies to be set.
      cookies :: ![CookieParam]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetCookies where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setCookies" $ \_o -> SetCookies
            <$> _o .: "cookies"
        ago = A.withArray "setCookies" $ \_a -> SetCookies
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetCookies where
    toEncoding (SetCookies _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "cookies" .= _0
        ]
    toJSON (SetCookies _0) = A.object $ P.catMaybes
        [ P.pure $ "cookies" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetCookies where
    SetCookies _0 <> SetCookies _ = SetCookies _0


------------------------------------------------------------------------------
instance M.Method SetCookies where
    type Result SetCookies = ()
    name _ = "Network.setCookies"


------------------------------------------------------------------------------
-- | Sets given cookies.
setCookies
    :: [CookieParam]
    -- ^ Cookies to be set.

    -> SetCookies
setCookies _0 = SetCookies _0


------------------------------------------------------------------------------
-- | For testing.
{-# WARNING SetDataSizeLimitsForTest "This feature is marked as EXPERIMENTAL." #-}
data SetDataSizeLimitsForTest = SetDataSizeLimitsForTest
    { -- | Maximum total buffer size.
      maxTotalSize :: !P.Int
      -- | Maximum per-resource size.
    , maxResourceSize :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetDataSizeLimitsForTest where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setDataSizeLimitsForTest" $ \_o -> SetDataSizeLimitsForTest
            <$> _o .: "maxTotalSize"
            <*> _o .: "maxResourceSize"
        ago = A.withArray "setDataSizeLimitsForTest" $ \_a -> SetDataSizeLimitsForTest
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SetDataSizeLimitsForTest where
    toEncoding (SetDataSizeLimitsForTest _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "maxTotalSize" .= _0
        , P.pure $ "maxResourceSize" .= _1
        ]
    toJSON (SetDataSizeLimitsForTest _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "maxTotalSize" .= _0
        , P.pure $ "maxResourceSize" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetDataSizeLimitsForTest where
    SetDataSizeLimitsForTest _0 _1 <> SetDataSizeLimitsForTest _ _ = SetDataSizeLimitsForTest _0 _1


------------------------------------------------------------------------------
instance M.Method SetDataSizeLimitsForTest where
    type Result SetDataSizeLimitsForTest = ()
    name _ = "Network.setDataSizeLimitsForTest"


------------------------------------------------------------------------------
-- | For testing.
{-# WARNING setDataSizeLimitsForTest "This feature is marked as EXPERIMENTAL." #-}
setDataSizeLimitsForTest
    :: P.Int
    -- ^ Maximum total buffer size.

    -> P.Int
    -- ^ Maximum per-resource size.

    -> SetDataSizeLimitsForTest
setDataSizeLimitsForTest _0 _1 = SetDataSizeLimitsForTest _0 _1


------------------------------------------------------------------------------
-- | Specifies whether to always send extra HTTP headers with the requests from this page.
data SetExtraHTTPHeaders = SetExtraHTTPHeaders
    { -- | Map with extra HTTP headers.
      headers :: !Headers
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetExtraHTTPHeaders where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setExtraHTTPHeaders" $ \_o -> SetExtraHTTPHeaders
            <$> _o .: "headers"
        ago = A.withArray "setExtraHTTPHeaders" $ \_a -> SetExtraHTTPHeaders
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetExtraHTTPHeaders where
    toEncoding (SetExtraHTTPHeaders _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "headers" .= _0
        ]
    toJSON (SetExtraHTTPHeaders _0) = A.object $ P.catMaybes
        [ P.pure $ "headers" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetExtraHTTPHeaders where
    SetExtraHTTPHeaders _0 <> SetExtraHTTPHeaders _ = SetExtraHTTPHeaders _0


------------------------------------------------------------------------------
instance M.Method SetExtraHTTPHeaders where
    type Result SetExtraHTTPHeaders = ()
    name _ = "Network.setExtraHTTPHeaders"


------------------------------------------------------------------------------
-- | Specifies whether to always send extra HTTP headers with the requests from this page.
setExtraHTTPHeaders
    :: Headers
    -- ^ Map with extra HTTP headers.

    -> SetExtraHTTPHeaders
setExtraHTTPHeaders _0 = SetExtraHTTPHeaders _0


------------------------------------------------------------------------------
-- | Sets the requests to intercept that match the provided patterns and optionally resource types.
-- Deprecated, please use Fetch.enable instead.
{-# DEPRECATED SetRequestInterception "This may be removed in a future release." #-}
{-{-# WARNING SetRequestInterception "This feature is marked as EXPERIMENTAL." #-}-}
data SetRequestInterception = SetRequestInterception
    { -- | Requests matching any of these patterns will be forwarded and wait for the corresponding
      -- continueInterceptedRequest call.
      patterns :: ![RequestPattern]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetRequestInterception where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setRequestInterception" $ \_o -> SetRequestInterception
            <$> _o .: "patterns"
        ago = A.withArray "setRequestInterception" $ \_a -> SetRequestInterception
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetRequestInterception where
    toEncoding (SetRequestInterception _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "patterns" .= _0
        ]
    toJSON (SetRequestInterception _0) = A.object $ P.catMaybes
        [ P.pure $ "patterns" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetRequestInterception where
    SetRequestInterception _0 <> SetRequestInterception _ = SetRequestInterception _0


------------------------------------------------------------------------------
instance M.Method SetRequestInterception where
    type Result SetRequestInterception = ()
    name _ = "Network.setRequestInterception"


------------------------------------------------------------------------------
-- | Sets the requests to intercept that match the provided patterns and optionally resource types.
-- Deprecated, please use Fetch.enable instead.
{-# DEPRECATED setRequestInterception "This may be removed in a future release." #-}
{-{-# WARNING setRequestInterception "This feature is marked as EXPERIMENTAL." #-}-}
setRequestInterception
    :: [RequestPattern]
    -- ^ Requests matching any of these patterns will be forwarded and wait for the corresponding

    -- continueInterceptedRequest call.

    -> SetRequestInterception
setRequestInterception _0 = SetRequestInterception _0


------------------------------------------------------------------------------
-- | Allows overriding user agent with the given string.
data SetUserAgentOverride = SetUserAgentOverride
    { -- | User agent to use.
      userAgent :: !T.Text
      -- | Browser langugage to emulate.
    , acceptLanguage :: !(P.Maybe T.Text)
      -- | The platform navigator.platform should return.
    , platform :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetUserAgentOverride where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setUserAgentOverride" $ \_o -> SetUserAgentOverride
            <$> _o .: "userAgent"
            <*> _o .:? "acceptLanguage"
            <*> _o .:? "platform"
        ago = A.withArray "setUserAgentOverride" $ \_a -> SetUserAgentOverride
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON SetUserAgentOverride where
    toEncoding (SetUserAgentOverride _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "userAgent" .= _0
        , ("acceptLanguage" .=) <$> _1
        , ("platform" .=) <$> _2
        ]
    toJSON (SetUserAgentOverride _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "userAgent" .= _0
        , ("acceptLanguage" .=) <$> _1
        , ("platform" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetUserAgentOverride where
    SetUserAgentOverride _0 _1 _2 <> SetUserAgentOverride _ __1 __2 = SetUserAgentOverride _0 (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
instance M.Method SetUserAgentOverride where
    type Result SetUserAgentOverride = ()
    name _ = "Network.setUserAgentOverride"


------------------------------------------------------------------------------
-- | Allows overriding user agent with the given string.
setUserAgentOverride
    :: T.Text
    -- ^ User agent to use.

    -> SetUserAgentOverride
setUserAgentOverride _0 = SetUserAgentOverride _0 P.empty P.empty


------------------------------------------------------------------------------
-- | Fired when data chunk was received over the network.
data DataReceived = DataReceived
    { -- | Request identifier.
      requestId :: !RequestId
      -- | Timestamp.
    , timestamp :: !MonotonicTime
      -- | Data chunk length.
    , dataLength :: !P.Int
      -- | Actual bytes received (might be less than dataLength for compressed encodings).
    , encodedDataLength :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DataReceived where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "dataReceived" $ \_o -> DataReceived
            <$> _o .: "requestId"
            <*> _o .: "timestamp"
            <*> _o .: "dataLength"
            <*> _o .: "encodedDataLength"
        ago = A.withArray "dataReceived" $ \_a -> DataReceived
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON DataReceived where
    toEncoding (DataReceived _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "timestamp" .= _1
        , P.pure $ "dataLength" .= _2
        , P.pure $ "encodedDataLength" .= _3
        ]
    toJSON (DataReceived _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "timestamp" .= _1
        , P.pure $ "dataLength" .= _2
        , P.pure $ "encodedDataLength" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup DataReceived where
    DataReceived _0 _1 _2 _3 <> DataReceived _ _ _ _ = DataReceived _0 _1 _2 _3


------------------------------------------------------------------------------
instance E.Event DataReceived where
    type Result DataReceived = DataReceived
    name _ = "Network.dataReceived"


------------------------------------------------------------------------------
-- | Fired when data chunk was received over the network.
dataReceived :: P.Proxy DataReceived
dataReceived = P.Proxy


------------------------------------------------------------------------------
-- | Fired when EventSource message is received.
data EventSourceMessageReceived = EventSourceMessageReceived
    { -- | Request identifier.
      requestId :: !RequestId
      -- | Timestamp.
    , timestamp :: !MonotonicTime
      -- | Message type.
    , eventName :: !T.Text
      -- | Message identifier.
    , eventId :: !T.Text
      -- | Message content.
    , data_ :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON EventSourceMessageReceived where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "eventSourceMessageReceived" $ \_o -> EventSourceMessageReceived
            <$> _o .: "requestId"
            <*> _o .: "timestamp"
            <*> _o .: "eventName"
            <*> _o .: "eventId"
            <*> _o .: "data"
        ago = A.withArray "eventSourceMessageReceived" $ \_a -> EventSourceMessageReceived
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON EventSourceMessageReceived where
    toEncoding (EventSourceMessageReceived _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "timestamp" .= _1
        , P.pure $ "eventName" .= _2
        , P.pure $ "eventId" .= _3
        , P.pure $ "data" .= _4
        ]
    toJSON (EventSourceMessageReceived _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "timestamp" .= _1
        , P.pure $ "eventName" .= _2
        , P.pure $ "eventId" .= _3
        , P.pure $ "data" .= _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup EventSourceMessageReceived where
    EventSourceMessageReceived _0 _1 _2 _3 _4 <> EventSourceMessageReceived _ _ _ _ _ = EventSourceMessageReceived _0 _1 _2 _3 _4


------------------------------------------------------------------------------
instance E.Event EventSourceMessageReceived where
    type Result EventSourceMessageReceived = EventSourceMessageReceived
    name _ = "Network.eventSourceMessageReceived"


------------------------------------------------------------------------------
-- | Fired when EventSource message is received.
eventSourceMessageReceived :: P.Proxy EventSourceMessageReceived
eventSourceMessageReceived = P.Proxy


------------------------------------------------------------------------------
-- | Fired when HTTP request has failed to load.
data LoadingFailed = LoadingFailed
    { -- | Request identifier.
      requestId :: !RequestId
      -- | Timestamp.
    , timestamp :: !MonotonicTime
      -- | Resource type.
    , type_ :: !ResourceType
      -- | User friendly error message.
    , errorText :: !T.Text
      -- | True if loading was canceled.
    , canceled :: !(P.Maybe P.Bool)
      -- | The reason why loading was blocked, if any.
    , blockedReason :: !(P.Maybe BlockedReason)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON LoadingFailed where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "loadingFailed" $ \_o -> LoadingFailed
            <$> _o .: "requestId"
            <*> _o .: "timestamp"
            <*> _o .: "type"
            <*> _o .: "errorText"
            <*> _o .:? "canceled"
            <*> _o .:? "blockedReason"
        ago = A.withArray "loadingFailed" $ \_a -> LoadingFailed
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)


------------------------------------------------------------------------------
instance A.ToJSON LoadingFailed where
    toEncoding (LoadingFailed _0 _1 _2 _3 _4 _5) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "timestamp" .= _1
        , P.pure $ "type" .= _2
        , P.pure $ "errorText" .= _3
        , ("canceled" .=) <$> _4
        , ("blockedReason" .=) <$> _5
        ]
    toJSON (LoadingFailed _0 _1 _2 _3 _4 _5) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "timestamp" .= _1
        , P.pure $ "type" .= _2
        , P.pure $ "errorText" .= _3
        , ("canceled" .=) <$> _4
        , ("blockedReason" .=) <$> _5
        ]


------------------------------------------------------------------------------
instance P.Semigroup LoadingFailed where
    LoadingFailed _0 _1 _2 _3 _4 _5 <> LoadingFailed _ _ _ _ __4 __5 = LoadingFailed _0 _1 _2 _3 (_4 <|> __4) (_5 <|> __5)


------------------------------------------------------------------------------
instance E.Event LoadingFailed where
    type Result LoadingFailed = LoadingFailed
    name _ = "Network.loadingFailed"


------------------------------------------------------------------------------
-- | Fired when HTTP request has failed to load.
loadingFailed :: P.Proxy LoadingFailed
loadingFailed = P.Proxy


------------------------------------------------------------------------------
-- | Fired when HTTP request has finished loading.
data LoadingFinished = LoadingFinished
    { -- | Request identifier.
      requestId :: !RequestId
      -- | Timestamp.
    , timestamp :: !MonotonicTime
      -- | Total number of bytes received for this request.
    , encodedDataLength :: !P.Double
      -- | Set when 1) response was blocked by Cross-Origin Read Blocking and also
      -- 2) this needs to be reported to the DevTools console.
    , shouldReportCorbBlocking :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON LoadingFinished where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "loadingFinished" $ \_o -> LoadingFinished
            <$> _o .: "requestId"
            <*> _o .: "timestamp"
            <*> _o .: "encodedDataLength"
            <*> _o .:? "shouldReportCorbBlocking"
        ago = A.withArray "loadingFinished" $ \_a -> LoadingFinished
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON LoadingFinished where
    toEncoding (LoadingFinished _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "timestamp" .= _1
        , P.pure $ "encodedDataLength" .= _2
        , ("shouldReportCorbBlocking" .=) <$> _3
        ]
    toJSON (LoadingFinished _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "timestamp" .= _1
        , P.pure $ "encodedDataLength" .= _2
        , ("shouldReportCorbBlocking" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup LoadingFinished where
    LoadingFinished _0 _1 _2 _3 <> LoadingFinished _ _ _ __3 = LoadingFinished _0 _1 _2 (_3 <|> __3)


------------------------------------------------------------------------------
instance E.Event LoadingFinished where
    type Result LoadingFinished = LoadingFinished
    name _ = "Network.loadingFinished"


------------------------------------------------------------------------------
-- | Fired when HTTP request has finished loading.
loadingFinished :: P.Proxy LoadingFinished
loadingFinished = P.Proxy


------------------------------------------------------------------------------
-- | Details of an intercepted HTTP request, which must be either allowed, blocked, modified or
-- mocked.
-- Deprecated, use Fetch.requestPaused instead.
{-# DEPRECATED RequestIntercepted "This may be removed in a future release." #-}
{-{-# WARNING RequestIntercepted "This feature is marked as EXPERIMENTAL." #-}-}
data RequestIntercepted = RequestIntercepted
    { -- | Each request the page makes will have a unique id, however if any redirects are encountered
      -- while processing that fetch, they will be reported with the same id as the original fetch.
      -- Likewise if HTTP authentication is needed then the same fetch id will be used.
      interceptionId :: !InterceptionId
    , request :: !Request
      -- | The id of the frame that initiated the request.
    , frameId :: !Page.FrameId
      -- | How the requested resource will be used.
    , resourceType :: !ResourceType
      -- | Whether this is a navigation request, which can abort the navigation completely.
    , isNavigationRequest :: !P.Bool
      -- | Set if the request is a navigation that will result in a download.
      -- Only present after response is received from the server (i.e. HeadersReceived stage).
    , isDownload :: !(P.Maybe P.Bool)
      -- | Redirect location, only sent if a redirect was intercepted.
    , redirectUrl :: !(P.Maybe T.Text)
      -- | Details of the Authorization Challenge encountered. If this is set then
      -- continueInterceptedRequest must contain an authChallengeResponse.
    , authChallenge :: !(P.Maybe AuthChallenge)
      -- | Response error if intercepted at response stage or if redirect occurred while intercepting
      -- request.
    , responseErrorReason :: !(P.Maybe ErrorReason)
      -- | Response code if intercepted at response stage or if redirect occurred while intercepting
      -- request or auth retry occurred.
    , responseStatusCode :: !(P.Maybe P.Int)
      -- | Response headers if intercepted at the response stage or if redirect occurred while
      -- intercepting request or auth retry occurred.
    , responseHeaders :: !(P.Maybe Headers)
      -- | If the intercepted request had a corresponding requestWillBeSent event fired for it, then
      -- this requestId will be the same as the requestId present in the requestWillBeSent event.
    , requestId :: !(P.Maybe RequestId)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RequestIntercepted where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "requestIntercepted" $ \_o -> RequestIntercepted
            <$> _o .: "interceptionId"
            <*> _o .: "request"
            <*> _o .: "frameId"
            <*> _o .: "resourceType"
            <*> _o .: "isNavigationRequest"
            <*> _o .:? "isDownload"
            <*> _o .:? "redirectUrl"
            <*> _o .:? "authChallenge"
            <*> _o .:? "responseErrorReason"
            <*> _o .:? "responseStatusCode"
            <*> _o .:? "responseHeaders"
            <*> _o .:? "requestId"
        ago = A.withArray "requestIntercepted" $ \_a -> RequestIntercepted
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)
            <*> P.traverse A.parseJSON (_a !? 7)
            <*> P.traverse A.parseJSON (_a !? 8)
            <*> P.traverse A.parseJSON (_a !? 9)
            <*> P.traverse A.parseJSON (_a !? 10)
            <*> P.traverse A.parseJSON (_a !? 11)


------------------------------------------------------------------------------
instance A.ToJSON RequestIntercepted where
    toEncoding (RequestIntercepted _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "interceptionId" .= _0
        , P.pure $ "request" .= _1
        , P.pure $ "frameId" .= _2
        , P.pure $ "resourceType" .= _3
        , P.pure $ "isNavigationRequest" .= _4
        , ("isDownload" .=) <$> _5
        , ("redirectUrl" .=) <$> _6
        , ("authChallenge" .=) <$> _7
        , ("responseErrorReason" .=) <$> _8
        , ("responseStatusCode" .=) <$> _9
        , ("responseHeaders" .=) <$> _10
        , ("requestId" .=) <$> _11
        ]
    toJSON (RequestIntercepted _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11) = A.object $ P.catMaybes
        [ P.pure $ "interceptionId" .= _0
        , P.pure $ "request" .= _1
        , P.pure $ "frameId" .= _2
        , P.pure $ "resourceType" .= _3
        , P.pure $ "isNavigationRequest" .= _4
        , ("isDownload" .=) <$> _5
        , ("redirectUrl" .=) <$> _6
        , ("authChallenge" .=) <$> _7
        , ("responseErrorReason" .=) <$> _8
        , ("responseStatusCode" .=) <$> _9
        , ("responseHeaders" .=) <$> _10
        , ("requestId" .=) <$> _11
        ]


------------------------------------------------------------------------------
instance P.Semigroup RequestIntercepted where
    RequestIntercepted _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 <> RequestIntercepted _ _ _ _ _ __5 __6 __7 __8 __9 __10 __11 = RequestIntercepted _0 _1 _2 _3 _4 (_5 <|> __5) (_6 <|> __6) (_7 <|> __7) (_8 <|> __8) (_9 <|> __9) (_10 <|> __10) (_11 <|> __11)


------------------------------------------------------------------------------
instance E.Event RequestIntercepted where
    type Result RequestIntercepted = RequestIntercepted
    name _ = "Network.requestIntercepted"


------------------------------------------------------------------------------
-- | Details of an intercepted HTTP request, which must be either allowed, blocked, modified or
-- mocked.
-- Deprecated, use Fetch.requestPaused instead.
{-# DEPRECATED requestIntercepted "This may be removed in a future release." #-}
{-{-# WARNING requestIntercepted "This feature is marked as EXPERIMENTAL." #-}-}
requestIntercepted :: P.Proxy RequestIntercepted
requestIntercepted = P.Proxy


------------------------------------------------------------------------------
-- | Fired if request ended up loading from cache.
data RequestServedFromCache = RequestServedFromCache
    { -- | Request identifier.
      requestId :: !RequestId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RequestServedFromCache where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "requestServedFromCache" $ \_o -> RequestServedFromCache
            <$> _o .: "requestId"
        ago = A.withArray "requestServedFromCache" $ \_a -> RequestServedFromCache
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON RequestServedFromCache where
    toEncoding (RequestServedFromCache _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        ]
    toJSON (RequestServedFromCache _0) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup RequestServedFromCache where
    RequestServedFromCache _0 <> RequestServedFromCache _ = RequestServedFromCache _0


------------------------------------------------------------------------------
instance E.Event RequestServedFromCache where
    type Result RequestServedFromCache = RequestServedFromCache
    name _ = "Network.requestServedFromCache"


------------------------------------------------------------------------------
-- | Fired if request ended up loading from cache.
requestServedFromCache :: P.Proxy RequestServedFromCache
requestServedFromCache = P.Proxy


------------------------------------------------------------------------------
-- | Fired when page is about to send HTTP request.
data RequestWillBeSent = RequestWillBeSent
    { -- | Request identifier.
      requestId :: !RequestId
      -- | Loader identifier. Empty string if the request is fetched from worker.
    , loaderId :: !LoaderId
      -- | URL of the document this request is loaded for.
    , documentURL :: !T.Text
      -- | Request data.
    , request :: !Request
      -- | Timestamp.
    , timestamp :: !MonotonicTime
      -- | Timestamp.
    , wallTime :: !TimeSinceEpoch
      -- | Request initiator.
    , initiator :: !Initiator
      -- | Redirect response data.
    , redirectResponse :: !(P.Maybe Response)
      -- | Type of this resource.
    , type_ :: !(P.Maybe ResourceType)
      -- | Frame identifier.
    , frameId :: !(P.Maybe Page.FrameId)
      -- | Whether the request is initiated by a user gesture. Defaults to false.
    , hasUserGesture :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RequestWillBeSent where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "requestWillBeSent" $ \_o -> RequestWillBeSent
            <$> _o .: "requestId"
            <*> _o .: "loaderId"
            <*> _o .: "documentURL"
            <*> _o .: "request"
            <*> _o .: "timestamp"
            <*> _o .: "wallTime"
            <*> _o .: "initiator"
            <*> _o .:? "redirectResponse"
            <*> _o .:? "type"
            <*> _o .:? "frameId"
            <*> _o .:? "hasUserGesture"
        ago = A.withArray "requestWillBeSent" $ \_a -> RequestWillBeSent
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.maybe P.empty A.parseJSON (_a !? 6)
            <*> P.traverse A.parseJSON (_a !? 7)
            <*> P.traverse A.parseJSON (_a !? 8)
            <*> P.traverse A.parseJSON (_a !? 9)
            <*> P.traverse A.parseJSON (_a !? 10)


------------------------------------------------------------------------------
instance A.ToJSON RequestWillBeSent where
    toEncoding (RequestWillBeSent _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "loaderId" .= _1
        , P.pure $ "documentURL" .= _2
        , P.pure $ "request" .= _3
        , P.pure $ "timestamp" .= _4
        , P.pure $ "wallTime" .= _5
        , P.pure $ "initiator" .= _6
        , ("redirectResponse" .=) <$> _7
        , ("type" .=) <$> _8
        , ("frameId" .=) <$> _9
        , ("hasUserGesture" .=) <$> _10
        ]
    toJSON (RequestWillBeSent _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "loaderId" .= _1
        , P.pure $ "documentURL" .= _2
        , P.pure $ "request" .= _3
        , P.pure $ "timestamp" .= _4
        , P.pure $ "wallTime" .= _5
        , P.pure $ "initiator" .= _6
        , ("redirectResponse" .=) <$> _7
        , ("type" .=) <$> _8
        , ("frameId" .=) <$> _9
        , ("hasUserGesture" .=) <$> _10
        ]


------------------------------------------------------------------------------
instance P.Semigroup RequestWillBeSent where
    RequestWillBeSent _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 <> RequestWillBeSent _ _ _ _ _ _ _ __7 __8 __9 __10 = RequestWillBeSent _0 _1 _2 _3 _4 _5 _6 (_7 <|> __7) (_8 <|> __8) (_9 <|> __9) (_10 <|> __10)


------------------------------------------------------------------------------
instance E.Event RequestWillBeSent where
    type Result RequestWillBeSent = RequestWillBeSent
    name _ = "Network.requestWillBeSent"


------------------------------------------------------------------------------
-- | Fired when page is about to send HTTP request.
requestWillBeSent :: P.Proxy RequestWillBeSent
requestWillBeSent = P.Proxy


------------------------------------------------------------------------------
-- | Fired when resource loading priority is changed
{-# WARNING ResourceChangedPriority "This feature is marked as EXPERIMENTAL." #-}
data ResourceChangedPriority = ResourceChangedPriority
    { -- | Request identifier.
      requestId :: !RequestId
      -- | New priority
    , newPriority :: !ResourcePriority
      -- | Timestamp.
    , timestamp :: !MonotonicTime
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ResourceChangedPriority where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "resourceChangedPriority" $ \_o -> ResourceChangedPriority
            <$> _o .: "requestId"
            <*> _o .: "newPriority"
            <*> _o .: "timestamp"
        ago = A.withArray "resourceChangedPriority" $ \_a -> ResourceChangedPriority
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON ResourceChangedPriority where
    toEncoding (ResourceChangedPriority _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "newPriority" .= _1
        , P.pure $ "timestamp" .= _2
        ]
    toJSON (ResourceChangedPriority _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "newPriority" .= _1
        , P.pure $ "timestamp" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup ResourceChangedPriority where
    ResourceChangedPriority _0 _1 _2 <> ResourceChangedPriority _ _ _ = ResourceChangedPriority _0 _1 _2


------------------------------------------------------------------------------
instance E.Event ResourceChangedPriority where
    type Result ResourceChangedPriority = ResourceChangedPriority
    name _ = "Network.resourceChangedPriority"


------------------------------------------------------------------------------
-- | Fired when resource loading priority is changed
{-# WARNING resourceChangedPriority "This feature is marked as EXPERIMENTAL." #-}
resourceChangedPriority :: P.Proxy ResourceChangedPriority
resourceChangedPriority = P.Proxy


------------------------------------------------------------------------------
-- | Fired when a signed exchange was received over the network
{-# WARNING SignedExchangeReceived "This feature is marked as EXPERIMENTAL." #-}
data SignedExchangeReceived = SignedExchangeReceived
    { -- | Request identifier.
      requestId :: !RequestId
      -- | Information about the signed exchange response.
    , info :: !SignedExchangeInfo
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SignedExchangeReceived where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "signedExchangeReceived" $ \_o -> SignedExchangeReceived
            <$> _o .: "requestId"
            <*> _o .: "info"
        ago = A.withArray "signedExchangeReceived" $ \_a -> SignedExchangeReceived
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SignedExchangeReceived where
    toEncoding (SignedExchangeReceived _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "info" .= _1
        ]
    toJSON (SignedExchangeReceived _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "info" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SignedExchangeReceived where
    SignedExchangeReceived _0 _1 <> SignedExchangeReceived _ _ = SignedExchangeReceived _0 _1


------------------------------------------------------------------------------
instance E.Event SignedExchangeReceived where
    type Result SignedExchangeReceived = SignedExchangeReceived
    name _ = "Network.signedExchangeReceived"


------------------------------------------------------------------------------
-- | Fired when a signed exchange was received over the network
{-# WARNING signedExchangeReceived "This feature is marked as EXPERIMENTAL." #-}
signedExchangeReceived :: P.Proxy SignedExchangeReceived
signedExchangeReceived = P.Proxy


------------------------------------------------------------------------------
-- | Fired when HTTP response is available.
data ResponseReceived = ResponseReceived
    { -- | Request identifier.
      requestId :: !RequestId
      -- | Loader identifier. Empty string if the request is fetched from worker.
    , loaderId :: !LoaderId
      -- | Timestamp.
    , timestamp :: !MonotonicTime
      -- | Resource type.
    , type_ :: !ResourceType
      -- | Response data.
    , response :: !Response
      -- | Frame identifier.
    , frameId :: !(P.Maybe Page.FrameId)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ResponseReceived where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "responseReceived" $ \_o -> ResponseReceived
            <$> _o .: "requestId"
            <*> _o .: "loaderId"
            <*> _o .: "timestamp"
            <*> _o .: "type"
            <*> _o .: "response"
            <*> _o .:? "frameId"
        ago = A.withArray "responseReceived" $ \_a -> ResponseReceived
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)


------------------------------------------------------------------------------
instance A.ToJSON ResponseReceived where
    toEncoding (ResponseReceived _0 _1 _2 _3 _4 _5) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "loaderId" .= _1
        , P.pure $ "timestamp" .= _2
        , P.pure $ "type" .= _3
        , P.pure $ "response" .= _4
        , ("frameId" .=) <$> _5
        ]
    toJSON (ResponseReceived _0 _1 _2 _3 _4 _5) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "loaderId" .= _1
        , P.pure $ "timestamp" .= _2
        , P.pure $ "type" .= _3
        , P.pure $ "response" .= _4
        , ("frameId" .=) <$> _5
        ]


------------------------------------------------------------------------------
instance P.Semigroup ResponseReceived where
    ResponseReceived _0 _1 _2 _3 _4 _5 <> ResponseReceived _ _ _ _ _ __5 = ResponseReceived _0 _1 _2 _3 _4 (_5 <|> __5)


------------------------------------------------------------------------------
instance E.Event ResponseReceived where
    type Result ResponseReceived = ResponseReceived
    name _ = "Network.responseReceived"


------------------------------------------------------------------------------
-- | Fired when HTTP response is available.
responseReceived :: P.Proxy ResponseReceived
responseReceived = P.Proxy


------------------------------------------------------------------------------
-- | Fired when WebSocket is closed.
data WebSocketClosed = WebSocketClosed
    { -- | Request identifier.
      requestId :: !RequestId
      -- | Timestamp.
    , timestamp :: !MonotonicTime
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON WebSocketClosed where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "webSocketClosed" $ \_o -> WebSocketClosed
            <$> _o .: "requestId"
            <*> _o .: "timestamp"
        ago = A.withArray "webSocketClosed" $ \_a -> WebSocketClosed
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON WebSocketClosed where
    toEncoding (WebSocketClosed _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "timestamp" .= _1
        ]
    toJSON (WebSocketClosed _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "timestamp" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup WebSocketClosed where
    WebSocketClosed _0 _1 <> WebSocketClosed _ _ = WebSocketClosed _0 _1


------------------------------------------------------------------------------
instance E.Event WebSocketClosed where
    type Result WebSocketClosed = WebSocketClosed
    name _ = "Network.webSocketClosed"


------------------------------------------------------------------------------
-- | Fired when WebSocket is closed.
webSocketClosed :: P.Proxy WebSocketClosed
webSocketClosed = P.Proxy


------------------------------------------------------------------------------
-- | Fired upon WebSocket creation.
data WebSocketCreated = WebSocketCreated
    { -- | Request identifier.
      requestId :: !RequestId
      -- | WebSocket request URL.
    , url :: !T.Text
      -- | Request initiator.
    , initiator :: !(P.Maybe Initiator)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON WebSocketCreated where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "webSocketCreated" $ \_o -> WebSocketCreated
            <$> _o .: "requestId"
            <*> _o .: "url"
            <*> _o .:? "initiator"
        ago = A.withArray "webSocketCreated" $ \_a -> WebSocketCreated
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON WebSocketCreated where
    toEncoding (WebSocketCreated _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "url" .= _1
        , ("initiator" .=) <$> _2
        ]
    toJSON (WebSocketCreated _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "url" .= _1
        , ("initiator" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup WebSocketCreated where
    WebSocketCreated _0 _1 _2 <> WebSocketCreated _ _ __2 = WebSocketCreated _0 _1 (_2 <|> __2)


------------------------------------------------------------------------------
instance E.Event WebSocketCreated where
    type Result WebSocketCreated = WebSocketCreated
    name _ = "Network.webSocketCreated"


------------------------------------------------------------------------------
-- | Fired upon WebSocket creation.
webSocketCreated :: P.Proxy WebSocketCreated
webSocketCreated = P.Proxy


------------------------------------------------------------------------------
-- | Fired when WebSocket message error occurs.
data WebSocketFrameError = WebSocketFrameError
    { -- | Request identifier.
      requestId :: !RequestId
      -- | Timestamp.
    , timestamp :: !MonotonicTime
      -- | WebSocket error message.
    , errorMessage :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON WebSocketFrameError where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "webSocketFrameError" $ \_o -> WebSocketFrameError
            <$> _o .: "requestId"
            <*> _o .: "timestamp"
            <*> _o .: "errorMessage"
        ago = A.withArray "webSocketFrameError" $ \_a -> WebSocketFrameError
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON WebSocketFrameError where
    toEncoding (WebSocketFrameError _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "timestamp" .= _1
        , P.pure $ "errorMessage" .= _2
        ]
    toJSON (WebSocketFrameError _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "timestamp" .= _1
        , P.pure $ "errorMessage" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup WebSocketFrameError where
    WebSocketFrameError _0 _1 _2 <> WebSocketFrameError _ _ _ = WebSocketFrameError _0 _1 _2


------------------------------------------------------------------------------
instance E.Event WebSocketFrameError where
    type Result WebSocketFrameError = WebSocketFrameError
    name _ = "Network.webSocketFrameError"


------------------------------------------------------------------------------
-- | Fired when WebSocket message error occurs.
webSocketFrameError :: P.Proxy WebSocketFrameError
webSocketFrameError = P.Proxy


------------------------------------------------------------------------------
-- | Fired when WebSocket message is received.
data WebSocketFrameReceived = WebSocketFrameReceived
    { -- | Request identifier.
      requestId :: !RequestId
      -- | Timestamp.
    , timestamp :: !MonotonicTime
      -- | WebSocket response data.
    , response :: !WebSocketFrame
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON WebSocketFrameReceived where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "webSocketFrameReceived" $ \_o -> WebSocketFrameReceived
            <$> _o .: "requestId"
            <*> _o .: "timestamp"
            <*> _o .: "response"
        ago = A.withArray "webSocketFrameReceived" $ \_a -> WebSocketFrameReceived
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON WebSocketFrameReceived where
    toEncoding (WebSocketFrameReceived _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "timestamp" .= _1
        , P.pure $ "response" .= _2
        ]
    toJSON (WebSocketFrameReceived _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "timestamp" .= _1
        , P.pure $ "response" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup WebSocketFrameReceived where
    WebSocketFrameReceived _0 _1 _2 <> WebSocketFrameReceived _ _ _ = WebSocketFrameReceived _0 _1 _2


------------------------------------------------------------------------------
instance E.Event WebSocketFrameReceived where
    type Result WebSocketFrameReceived = WebSocketFrameReceived
    name _ = "Network.webSocketFrameReceived"


------------------------------------------------------------------------------
-- | Fired when WebSocket message is received.
webSocketFrameReceived :: P.Proxy WebSocketFrameReceived
webSocketFrameReceived = P.Proxy


------------------------------------------------------------------------------
-- | Fired when WebSocket message is sent.
data WebSocketFrameSent = WebSocketFrameSent
    { -- | Request identifier.
      requestId :: !RequestId
      -- | Timestamp.
    , timestamp :: !MonotonicTime
      -- | WebSocket response data.
    , response :: !WebSocketFrame
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON WebSocketFrameSent where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "webSocketFrameSent" $ \_o -> WebSocketFrameSent
            <$> _o .: "requestId"
            <*> _o .: "timestamp"
            <*> _o .: "response"
        ago = A.withArray "webSocketFrameSent" $ \_a -> WebSocketFrameSent
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON WebSocketFrameSent where
    toEncoding (WebSocketFrameSent _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "timestamp" .= _1
        , P.pure $ "response" .= _2
        ]
    toJSON (WebSocketFrameSent _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "timestamp" .= _1
        , P.pure $ "response" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup WebSocketFrameSent where
    WebSocketFrameSent _0 _1 _2 <> WebSocketFrameSent _ _ _ = WebSocketFrameSent _0 _1 _2


------------------------------------------------------------------------------
instance E.Event WebSocketFrameSent where
    type Result WebSocketFrameSent = WebSocketFrameSent
    name _ = "Network.webSocketFrameSent"


------------------------------------------------------------------------------
-- | Fired when WebSocket message is sent.
webSocketFrameSent :: P.Proxy WebSocketFrameSent
webSocketFrameSent = P.Proxy


------------------------------------------------------------------------------
-- | Fired when WebSocket handshake response becomes available.
data WebSocketHandshakeResponseReceived = WebSocketHandshakeResponseReceived
    { -- | Request identifier.
      requestId :: !RequestId
      -- | Timestamp.
    , timestamp :: !MonotonicTime
      -- | WebSocket response data.
    , response :: !WebSocketResponse
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON WebSocketHandshakeResponseReceived where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "webSocketHandshakeResponseReceived" $ \_o -> WebSocketHandshakeResponseReceived
            <$> _o .: "requestId"
            <*> _o .: "timestamp"
            <*> _o .: "response"
        ago = A.withArray "webSocketHandshakeResponseReceived" $ \_a -> WebSocketHandshakeResponseReceived
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON WebSocketHandshakeResponseReceived where
    toEncoding (WebSocketHandshakeResponseReceived _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "timestamp" .= _1
        , P.pure $ "response" .= _2
        ]
    toJSON (WebSocketHandshakeResponseReceived _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "timestamp" .= _1
        , P.pure $ "response" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup WebSocketHandshakeResponseReceived where
    WebSocketHandshakeResponseReceived _0 _1 _2 <> WebSocketHandshakeResponseReceived _ _ _ = WebSocketHandshakeResponseReceived _0 _1 _2


------------------------------------------------------------------------------
instance E.Event WebSocketHandshakeResponseReceived where
    type Result WebSocketHandshakeResponseReceived = WebSocketHandshakeResponseReceived
    name _ = "Network.webSocketHandshakeResponseReceived"


------------------------------------------------------------------------------
-- | Fired when WebSocket handshake response becomes available.
webSocketHandshakeResponseReceived :: P.Proxy WebSocketHandshakeResponseReceived
webSocketHandshakeResponseReceived = P.Proxy


------------------------------------------------------------------------------
-- | Fired when WebSocket is about to initiate handshake.
data WebSocketWillSendHandshakeRequest = WebSocketWillSendHandshakeRequest
    { -- | Request identifier.
      requestId :: !RequestId
      -- | Timestamp.
    , timestamp :: !MonotonicTime
      -- | UTC Timestamp.
    , wallTime :: !TimeSinceEpoch
      -- | WebSocket request data.
    , request :: !WebSocketRequest
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON WebSocketWillSendHandshakeRequest where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "webSocketWillSendHandshakeRequest" $ \_o -> WebSocketWillSendHandshakeRequest
            <$> _o .: "requestId"
            <*> _o .: "timestamp"
            <*> _o .: "wallTime"
            <*> _o .: "request"
        ago = A.withArray "webSocketWillSendHandshakeRequest" $ \_a -> WebSocketWillSendHandshakeRequest
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON WebSocketWillSendHandshakeRequest where
    toEncoding (WebSocketWillSendHandshakeRequest _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "timestamp" .= _1
        , P.pure $ "wallTime" .= _2
        , P.pure $ "request" .= _3
        ]
    toJSON (WebSocketWillSendHandshakeRequest _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "timestamp" .= _1
        , P.pure $ "wallTime" .= _2
        , P.pure $ "request" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup WebSocketWillSendHandshakeRequest where
    WebSocketWillSendHandshakeRequest _0 _1 _2 _3 <> WebSocketWillSendHandshakeRequest _ _ _ _ = WebSocketWillSendHandshakeRequest _0 _1 _2 _3


------------------------------------------------------------------------------
instance E.Event WebSocketWillSendHandshakeRequest where
    type Result WebSocketWillSendHandshakeRequest = WebSocketWillSendHandshakeRequest
    name _ = "Network.webSocketWillSendHandshakeRequest"


------------------------------------------------------------------------------
-- | Fired when WebSocket is about to initiate handshake.
webSocketWillSendHandshakeRequest :: P.Proxy WebSocketWillSendHandshakeRequest
webSocketWillSendHandshakeRequest = P.Proxy


------------------------------------------------------------------------------
-- | Fired when additional information about a requestWillBeSent event is available from the
-- network stack. Not every requestWillBeSent event will have an additional
-- requestWillBeSentExtraInfo fired for it, and there is no guarantee whether requestWillBeSent
-- or requestWillBeSentExtraInfo will be fired first for the same request.
{-# WARNING RequestWillBeSentExtraInfo "This feature is marked as EXPERIMENTAL." #-}
data RequestWillBeSentExtraInfo = RequestWillBeSentExtraInfo
    { -- | Request identifier. Used to match this information to an existing requestWillBeSent event.
      requestId :: !RequestId
      -- | A list of cookies which will not be sent with this request along with corresponding reasons
      -- for blocking.
    , blockedCookies :: ![BlockedCookieWithReason]
      -- | Raw request headers as they will be sent over the wire.
    , headers :: !Headers
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RequestWillBeSentExtraInfo where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "requestWillBeSentExtraInfo" $ \_o -> RequestWillBeSentExtraInfo
            <$> _o .: "requestId"
            <*> _o .: "blockedCookies"
            <*> _o .: "headers"
        ago = A.withArray "requestWillBeSentExtraInfo" $ \_a -> RequestWillBeSentExtraInfo
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON RequestWillBeSentExtraInfo where
    toEncoding (RequestWillBeSentExtraInfo _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "blockedCookies" .= _1
        , P.pure $ "headers" .= _2
        ]
    toJSON (RequestWillBeSentExtraInfo _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "blockedCookies" .= _1
        , P.pure $ "headers" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup RequestWillBeSentExtraInfo where
    RequestWillBeSentExtraInfo _0 _1 _2 <> RequestWillBeSentExtraInfo _ _ _ = RequestWillBeSentExtraInfo _0 _1 _2


------------------------------------------------------------------------------
instance E.Event RequestWillBeSentExtraInfo where
    type Result RequestWillBeSentExtraInfo = RequestWillBeSentExtraInfo
    name _ = "Network.requestWillBeSentExtraInfo"


------------------------------------------------------------------------------
-- | Fired when additional information about a requestWillBeSent event is available from the
-- network stack. Not every requestWillBeSent event will have an additional
-- requestWillBeSentExtraInfo fired for it, and there is no guarantee whether requestWillBeSent
-- or requestWillBeSentExtraInfo will be fired first for the same request.
{-# WARNING requestWillBeSentExtraInfo "This feature is marked as EXPERIMENTAL." #-}
requestWillBeSentExtraInfo :: P.Proxy RequestWillBeSentExtraInfo
requestWillBeSentExtraInfo = P.Proxy


------------------------------------------------------------------------------
-- | Fired when additional information about a responseReceived event is available from the network
-- stack. Not every responseReceived event will have an additional responseReceivedExtraInfo for
-- it, and responseReceivedExtraInfo may be fired before or after responseReceived.
{-# WARNING ResponseReceivedExtraInfo "This feature is marked as EXPERIMENTAL." #-}
data ResponseReceivedExtraInfo = ResponseReceivedExtraInfo
    { -- | Request identifier. Used to match this information to another responseReceived event.
      requestId :: !RequestId
      -- | A list of cookies which were not stored from the response along with the corresponding
      -- reasons for blocking. The cookies here may not be valid due to syntax errors, which
      -- are represented by the invalid cookie line string instead of a proper cookie.
    , blockedCookies :: ![BlockedSetCookieWithReason]
      -- | Raw response headers as they were received over the wire.
    , headers :: !Headers
      -- | Raw response header text as it was received over the wire. The raw text may not always be
      -- available, such as in the case of HTTP\/2 or QUIC.
    , headersText :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ResponseReceivedExtraInfo where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "responseReceivedExtraInfo" $ \_o -> ResponseReceivedExtraInfo
            <$> _o .: "requestId"
            <*> _o .: "blockedCookies"
            <*> _o .: "headers"
            <*> _o .:? "headersText"
        ago = A.withArray "responseReceivedExtraInfo" $ \_a -> ResponseReceivedExtraInfo
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON ResponseReceivedExtraInfo where
    toEncoding (ResponseReceivedExtraInfo _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "blockedCookies" .= _1
        , P.pure $ "headers" .= _2
        , ("headersText" .=) <$> _3
        ]
    toJSON (ResponseReceivedExtraInfo _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "blockedCookies" .= _1
        , P.pure $ "headers" .= _2
        , ("headersText" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup ResponseReceivedExtraInfo where
    ResponseReceivedExtraInfo _0 _1 _2 _3 <> ResponseReceivedExtraInfo _ _ _ __3 = ResponseReceivedExtraInfo _0 _1 _2 (_3 <|> __3)


------------------------------------------------------------------------------
instance E.Event ResponseReceivedExtraInfo where
    type Result ResponseReceivedExtraInfo = ResponseReceivedExtraInfo
    name _ = "Network.responseReceivedExtraInfo"


------------------------------------------------------------------------------
-- | Fired when additional information about a responseReceived event is available from the network
-- stack. Not every responseReceived event will have an additional responseReceivedExtraInfo for
-- it, and responseReceivedExtraInfo may be fired before or after responseReceived.
{-# WARNING responseReceivedExtraInfo "This feature is marked as EXPERIMENTAL." #-}
responseReceivedExtraInfo :: P.Proxy ResponseReceivedExtraInfo
responseReceivedExtraInfo = P.Proxy

