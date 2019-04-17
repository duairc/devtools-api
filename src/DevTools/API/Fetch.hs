{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | A domain for letting clients substitute browser's network layer with client code.
module DevTools.API.Fetch{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

    ( module DevTools.API.Fetch.Types
    , module DevTools.API.Fetch
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
import           DevTools.API.Fetch.Types
import qualified DevTools.API.IO.Types as IO
import qualified DevTools.API.Network.Types as Network
import qualified DevTools.API.Page.Types as Page


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Disables the fetch domain.
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
    name _ = "Fetch.disable"


------------------------------------------------------------------------------
-- | Disables the fetch domain.
disable
    :: Disable
disable = Disable


------------------------------------------------------------------------------
-- | Enables issuing of requestPaused events. A request will be paused until client
-- calls one of failRequest, fulfillRequest or continueRequest\/continueWithAuth.
data Enable = Enable
    { -- | If specified, only requests matching any of these patterns will produce
      -- fetchRequested event and will be paused until clients response. If not set,
      -- all requests will be affected.
      patterns :: !(P.Maybe [RequestPattern])
      -- | If true, authRequired events will be issued and requests will be paused
      -- expecting a call to continueWithAuth.
    , handleAuthRequests :: !(P.Maybe P.Bool)
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
            <$> _o .:? "patterns"
            <*> _o .:? "handleAuthRequests"
        ago = A.withArray "enable" $ \_a -> Enable
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON Enable where
    toEncoding (Enable _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ ("patterns" .=) <$> _0
        , ("handleAuthRequests" .=) <$> _1
        ]
    toJSON (Enable _0 _1) = A.object $ P.catMaybes
        [ ("patterns" .=) <$> _0
        , ("handleAuthRequests" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup Enable where
    Enable _0 _1 <> Enable __0 __1 = Enable (_0 <|> __0) (_1 <|> __1)


------------------------------------------------------------------------------
instance P.Monoid Enable where
    mempty = Enable P.empty P.empty


------------------------------------------------------------------------------
instance M.Method Enable where
    type Result Enable = ()
    name _ = "Fetch.enable"


------------------------------------------------------------------------------
-- | Enables issuing of requestPaused events. A request will be paused until client
-- calls one of failRequest, fulfillRequest or continueRequest\/continueWithAuth.
enable
    :: Enable
enable = Enable P.empty P.empty


------------------------------------------------------------------------------
-- | Causes the request to fail with specified reason.
data FailRequest = FailRequest
    { -- | An id the client received in requestPaused event.
      requestId :: !RequestId
      -- | Causes the request to fail with the given reason.
    , errorReason :: !Network.ErrorReason
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON FailRequest where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "failRequest" $ \_o -> FailRequest
            <$> _o .: "requestId"
            <*> _o .: "errorReason"
        ago = A.withArray "failRequest" $ \_a -> FailRequest
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON FailRequest where
    toEncoding (FailRequest _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "errorReason" .= _1
        ]
    toJSON (FailRequest _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "errorReason" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup FailRequest where
    FailRequest _0 _1 <> FailRequest _ _ = FailRequest _0 _1


------------------------------------------------------------------------------
instance M.Method FailRequest where
    type Result FailRequest = ()
    name _ = "Fetch.failRequest"


------------------------------------------------------------------------------
-- | Causes the request to fail with specified reason.
failRequest
    :: RequestId
    -- ^ An id the client received in requestPaused event.

    -> Network.ErrorReason
    -- ^ Causes the request to fail with the given reason.

    -> FailRequest
failRequest _0 _1 = FailRequest _0 _1


------------------------------------------------------------------------------
-- | Provides response to the request.
data FulfillRequest = FulfillRequest
    { -- | An id the client received in requestPaused event.
      requestId :: !RequestId
      -- | An HTTP response code.
    , responseCode :: !P.Int
      -- | Response headers.
    , responseHeaders :: ![HeaderEntry]
      -- | A response body.
    , body :: !(P.Maybe T.Text)
      -- | A textual representation of responseCode.
      -- If absent, a standard phrase mathcing responseCode is used.
    , responsePhrase :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON FulfillRequest where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "fulfillRequest" $ \_o -> FulfillRequest
            <$> _o .: "requestId"
            <*> _o .: "responseCode"
            <*> _o .: "responseHeaders"
            <*> _o .:? "body"
            <*> _o .:? "responsePhrase"
        ago = A.withArray "fulfillRequest" $ \_a -> FulfillRequest
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON FulfillRequest where
    toEncoding (FulfillRequest _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "responseCode" .= _1
        , P.pure $ "responseHeaders" .= _2
        , ("body" .=) <$> _3
        , ("responsePhrase" .=) <$> _4
        ]
    toJSON (FulfillRequest _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "responseCode" .= _1
        , P.pure $ "responseHeaders" .= _2
        , ("body" .=) <$> _3
        , ("responsePhrase" .=) <$> _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup FulfillRequest where
    FulfillRequest _0 _1 _2 _3 _4 <> FulfillRequest _ _ _ __3 __4 = FulfillRequest _0 _1 _2 (_3 <|> __3) (_4 <|> __4)


------------------------------------------------------------------------------
instance M.Method FulfillRequest where
    type Result FulfillRequest = ()
    name _ = "Fetch.fulfillRequest"


------------------------------------------------------------------------------
-- | Provides response to the request.
fulfillRequest
    :: RequestId
    -- ^ An id the client received in requestPaused event.

    -> P.Int
    -- ^ An HTTP response code.

    -> [HeaderEntry]
    -- ^ Response headers.

    -> FulfillRequest
fulfillRequest _0 _1 _2 = FulfillRequest _0 _1 _2 P.empty P.empty


------------------------------------------------------------------------------
-- | Continues the request, optionally modifying some of its parameters.
data ContinueRequest = ContinueRequest
    { -- | An id the client received in requestPaused event.
      requestId :: !RequestId
      -- | If set, the request url will be modified in a way that's not observable by page.
    , url :: !(P.Maybe T.Text)
      -- | If set, the request method is overridden.
    , method :: !(P.Maybe T.Text)
      -- | If set, overrides the post data in the request.
    , postData :: !(P.Maybe T.Text)
      -- | If set, overrides the request headrts.
    , headers :: !(P.Maybe [HeaderEntry])
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ContinueRequest where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "continueRequest" $ \_o -> ContinueRequest
            <$> _o .: "requestId"
            <*> _o .:? "url"
            <*> _o .:? "method"
            <*> _o .:? "postData"
            <*> _o .:? "headers"
        ago = A.withArray "continueRequest" $ \_a -> ContinueRequest
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON ContinueRequest where
    toEncoding (ContinueRequest _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , ("url" .=) <$> _1
        , ("method" .=) <$> _2
        , ("postData" .=) <$> _3
        , ("headers" .=) <$> _4
        ]
    toJSON (ContinueRequest _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , ("url" .=) <$> _1
        , ("method" .=) <$> _2
        , ("postData" .=) <$> _3
        , ("headers" .=) <$> _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup ContinueRequest where
    ContinueRequest _0 _1 _2 _3 _4 <> ContinueRequest _ __1 __2 __3 __4 = ContinueRequest _0 (_1 <|> __1) (_2 <|> __2) (_3 <|> __3) (_4 <|> __4)


------------------------------------------------------------------------------
instance M.Method ContinueRequest where
    type Result ContinueRequest = ()
    name _ = "Fetch.continueRequest"


------------------------------------------------------------------------------
-- | Continues the request, optionally modifying some of its parameters.
continueRequest
    :: RequestId
    -- ^ An id the client received in requestPaused event.

    -> ContinueRequest
continueRequest _0 = ContinueRequest _0 P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Continues a request supplying authChallengeResponse following authRequired event.
data ContinueWithAuth = ContinueWithAuth
    { -- | An id the client received in authRequired event.
      requestId :: !RequestId
      -- | Response to  with an authChallenge.
    , authChallengeResponse :: !AuthChallengeResponse
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ContinueWithAuth where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "continueWithAuth" $ \_o -> ContinueWithAuth
            <$> _o .: "requestId"
            <*> _o .: "authChallengeResponse"
        ago = A.withArray "continueWithAuth" $ \_a -> ContinueWithAuth
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON ContinueWithAuth where
    toEncoding (ContinueWithAuth _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "authChallengeResponse" .= _1
        ]
    toJSON (ContinueWithAuth _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "authChallengeResponse" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup ContinueWithAuth where
    ContinueWithAuth _0 _1 <> ContinueWithAuth _ _ = ContinueWithAuth _0 _1


------------------------------------------------------------------------------
instance M.Method ContinueWithAuth where
    type Result ContinueWithAuth = ()
    name _ = "Fetch.continueWithAuth"


------------------------------------------------------------------------------
-- | Continues a request supplying authChallengeResponse following authRequired event.
continueWithAuth
    :: RequestId
    -- ^ An id the client received in authRequired event.

    -> AuthChallengeResponse
    -- ^ Response to  with an authChallenge.

    -> ContinueWithAuth
continueWithAuth _0 _1 = ContinueWithAuth _0 _1


------------------------------------------------------------------------------
-- | Causes the body of the response to be received from the server and
-- returned as a single string. May only be issued for a request that
-- is paused in the Response stage and is mutually exclusive with
-- takeResponseBodyForInterceptionAsStream. Calling other methods that
-- affect the request or disabling fetch domain before body is received
-- results in an undefined behavior.
data GetResponseBody = GetResponseBody
    { -- | Identifier for the intercepted request to get body for.
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
-- | Causes the body of the response to be received from the server and
-- returned as a single string. May only be issued for a request that
-- is paused in the Response stage and is mutually exclusive with
-- takeResponseBodyForInterceptionAsStream. Calling other methods that
-- affect the request or disabling fetch domain before body is received
-- results in an undefined behavior.
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
    name _ = "Fetch.getResponseBody"


------------------------------------------------------------------------------
-- | Causes the body of the response to be received from the server and
-- returned as a single string. May only be issued for a request that
-- is paused in the Response stage and is mutually exclusive with
-- takeResponseBodyForInterceptionAsStream. Calling other methods that
-- affect the request or disabling fetch domain before body is received
-- results in an undefined behavior.
getResponseBody
    :: RequestId
    -- ^ Identifier for the intercepted request to get body for.

    -> GetResponseBody
getResponseBody _0 = GetResponseBody _0


------------------------------------------------------------------------------
-- | Returns a handle to the stream representing the response body.
-- The request must be paused in the HeadersReceived stage.
-- Note that after this command the request can't be continued
-- as is -- client either needs to cancel it or to provide the
-- response body.
-- The stream only supports sequential read, IO.read will fail if the position
-- is specified.
-- This method is mutually exclusive with getResponseBody.
-- Calling other methods that affect the request or disabling fetch
-- domain before body is received results in an undefined behavior.
data TakeResponseBodyAsStream = TakeResponseBodyAsStream
    { requestId :: !RequestId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TakeResponseBodyAsStream where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "takeResponseBodyAsStream" $ \_o -> TakeResponseBodyAsStream
            <$> _o .: "requestId"
        ago = A.withArray "takeResponseBodyAsStream" $ \_a -> TakeResponseBodyAsStream
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON TakeResponseBodyAsStream where
    toEncoding (TakeResponseBodyAsStream _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        ]
    toJSON (TakeResponseBodyAsStream _0) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup TakeResponseBodyAsStream where
    TakeResponseBodyAsStream _0 <> TakeResponseBodyAsStream _ = TakeResponseBodyAsStream _0


------------------------------------------------------------------------------
-- | Returns a handle to the stream representing the response body.
-- The request must be paused in the HeadersReceived stage.
-- Note that after this command the request can't be continued
-- as is -- client either needs to cancel it or to provide the
-- response body.
-- The stream only supports sequential read, IO.read will fail if the position
-- is specified.
-- This method is mutually exclusive with getResponseBody.
-- Calling other methods that affect the request or disabling fetch
-- domain before body is received results in an undefined behavior.
data TakeResponseBodyAsStreamResult = TakeResponseBodyAsStreamResult
    { stream :: !IO.StreamHandle
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TakeResponseBodyAsStreamResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "takeResponseBodyAsStreamResult" $ \_o -> TakeResponseBodyAsStreamResult
            <$> _o .: "stream"
        ago = A.withArray "takeResponseBodyAsStreamResult" $ \_a -> TakeResponseBodyAsStreamResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON TakeResponseBodyAsStreamResult where
    toEncoding (TakeResponseBodyAsStreamResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "stream" .= _0
        ]
    toJSON (TakeResponseBodyAsStreamResult _0) = A.object $ P.catMaybes
        [ P.pure $ "stream" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup TakeResponseBodyAsStreamResult where
    TakeResponseBodyAsStreamResult _0 <> TakeResponseBodyAsStreamResult _ = TakeResponseBodyAsStreamResult _0


------------------------------------------------------------------------------
instance M.Method TakeResponseBodyAsStream where
    type Result TakeResponseBodyAsStream = TakeResponseBodyAsStreamResult
    name _ = "Fetch.takeResponseBodyAsStream"


------------------------------------------------------------------------------
-- | Returns a handle to the stream representing the response body.
-- The request must be paused in the HeadersReceived stage.
-- Note that after this command the request can't be continued
-- as is -- client either needs to cancel it or to provide the
-- response body.
-- The stream only supports sequential read, IO.read will fail if the position
-- is specified.
-- This method is mutually exclusive with getResponseBody.
-- Calling other methods that affect the request or disabling fetch
-- domain before body is received results in an undefined behavior.
takeResponseBodyAsStream
    :: RequestId
    -> TakeResponseBodyAsStream
takeResponseBodyAsStream _0 = TakeResponseBodyAsStream _0


------------------------------------------------------------------------------
-- | Issued when the domain is enabled and the request URL matches the
-- specified filter. The request is paused until the client responds
-- with one of continueRequest, failRequest or fulfillRequest.
-- The stage of the request can be determined by presence of responseErrorReason
-- and responseStatusCode -- the request is at the response stage if either
-- of these fields is present and in the request stage otherwise.
data RequestPaused = RequestPaused
    { -- | Each request the page makes will have a unique id.
      requestId :: !RequestId
      -- | The details of the request.
    , request :: !Network.Request
      -- | The id of the frame that initiated the request.
    , frameId :: !Page.FrameId
      -- | How the requested resource will be used.
    , resourceType :: !Network.ResourceType
      -- | Response error if intercepted at response stage.
    , responseErrorReason :: !(P.Maybe Network.ErrorReason)
      -- | Response code if intercepted at response stage.
    , responseStatusCode :: !(P.Maybe P.Int)
      -- | Response headers if intercepted at the response stage.
    , responseHeaders :: !(P.Maybe [HeaderEntry])
      -- | If the intercepted request had a corresponding Network.requestWillBeSent event fired for it,
      -- then this networkId will be the same as the requestId present in the requestWillBeSent event.
    , networkId :: !(P.Maybe RequestId)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RequestPaused where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "requestPaused" $ \_o -> RequestPaused
            <$> _o .: "requestId"
            <*> _o .: "request"
            <*> _o .: "frameId"
            <*> _o .: "resourceType"
            <*> _o .:? "responseErrorReason"
            <*> _o .:? "responseStatusCode"
            <*> _o .:? "responseHeaders"
            <*> _o .:? "networkId"
        ago = A.withArray "requestPaused" $ \_a -> RequestPaused
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)
            <*> P.traverse A.parseJSON (_a !? 7)


------------------------------------------------------------------------------
instance A.ToJSON RequestPaused where
    toEncoding (RequestPaused _0 _1 _2 _3 _4 _5 _6 _7) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "request" .= _1
        , P.pure $ "frameId" .= _2
        , P.pure $ "resourceType" .= _3
        , ("responseErrorReason" .=) <$> _4
        , ("responseStatusCode" .=) <$> _5
        , ("responseHeaders" .=) <$> _6
        , ("networkId" .=) <$> _7
        ]
    toJSON (RequestPaused _0 _1 _2 _3 _4 _5 _6 _7) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "request" .= _1
        , P.pure $ "frameId" .= _2
        , P.pure $ "resourceType" .= _3
        , ("responseErrorReason" .=) <$> _4
        , ("responseStatusCode" .=) <$> _5
        , ("responseHeaders" .=) <$> _6
        , ("networkId" .=) <$> _7
        ]


------------------------------------------------------------------------------
instance P.Semigroup RequestPaused where
    RequestPaused _0 _1 _2 _3 _4 _5 _6 _7 <> RequestPaused _ _ _ _ __4 __5 __6 __7 = RequestPaused _0 _1 _2 _3 (_4 <|> __4) (_5 <|> __5) (_6 <|> __6) (_7 <|> __7)


------------------------------------------------------------------------------
instance E.Event RequestPaused where
    type Result RequestPaused = RequestPaused
    name _ = "Fetch.requestPaused"


------------------------------------------------------------------------------
-- | Issued when the domain is enabled and the request URL matches the
-- specified filter. The request is paused until the client responds
-- with one of continueRequest, failRequest or fulfillRequest.
-- The stage of the request can be determined by presence of responseErrorReason
-- and responseStatusCode -- the request is at the response stage if either
-- of these fields is present and in the request stage otherwise.
requestPaused :: P.Proxy RequestPaused
requestPaused = P.Proxy


------------------------------------------------------------------------------
-- | Issued when the domain is enabled with handleAuthRequests set to true.
-- The request is paused until client responds with continueWithAuth.
data AuthRequired = AuthRequired
    { -- | Each request the page makes will have a unique id.
      requestId :: !RequestId
      -- | The details of the request.
    , request :: !Network.Request
      -- | The id of the frame that initiated the request.
    , frameId :: !Page.FrameId
      -- | How the requested resource will be used.
    , resourceType :: !Network.ResourceType
      -- | Details of the Authorization Challenge encountered.
      -- If this is set, client should respond with continueRequest that
      -- contains AuthChallengeResponse.
    , authChallenge :: !AuthChallenge
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AuthRequired where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "authRequired" $ \_o -> AuthRequired
            <$> _o .: "requestId"
            <*> _o .: "request"
            <*> _o .: "frameId"
            <*> _o .: "resourceType"
            <*> _o .: "authChallenge"
        ago = A.withArray "authRequired" $ \_a -> AuthRequired
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON AuthRequired where
    toEncoding (AuthRequired _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "request" .= _1
        , P.pure $ "frameId" .= _2
        , P.pure $ "resourceType" .= _3
        , P.pure $ "authChallenge" .= _4
        ]
    toJSON (AuthRequired _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "request" .= _1
        , P.pure $ "frameId" .= _2
        , P.pure $ "resourceType" .= _3
        , P.pure $ "authChallenge" .= _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup AuthRequired where
    AuthRequired _0 _1 _2 _3 _4 <> AuthRequired _ _ _ _ _ = AuthRequired _0 _1 _2 _3 _4


------------------------------------------------------------------------------
instance E.Event AuthRequired where
    type Result AuthRequired = AuthRequired
    name _ = "Fetch.authRequired"


------------------------------------------------------------------------------
-- | Issued when the domain is enabled with handleAuthRequests set to true.
-- The request is paused until client responds with continueWithAuth.
authRequired :: P.Proxy AuthRequired
authRequired = P.Proxy

