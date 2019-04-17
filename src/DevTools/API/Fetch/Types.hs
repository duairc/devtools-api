{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | A domain for letting clients substitute browser's network layer with client code.
module DevTools.API.Fetch.Types{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

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
import qualified DevTools.API.Network.Types as Network


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Unique request identifier.
type RequestId = T.Text


------------------------------------------------------------------------------
-- | Stages of the request to handle. Request will intercept before the request is
-- sent. Response will intercept after the response is received (but before response
-- body is received.
{-# WARNING RequestStage "This feature is marked as EXPERIMENTAL." #-}
data RequestStage
    = Request
    | Response
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RequestStage where
    parseJSON = A.withText "RequestStage" $ \t -> case t of
        "Request" -> P.pure Request
        "Response" -> P.pure Response
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON RequestStage where
    toJSON Request = "Request"
    toJSON Response = "Response"


------------------------------------------------------------------------------
{-# WARNING RequestPattern "This feature is marked as EXPERIMENTAL." #-}
data RequestPattern = RequestPattern
    { -- | Wildcards ('*' -> zero or more, '?' -> exactly one) are allowed. Escape character is
      -- backslash. Omitting is equivalent to "*".
      urlPattern :: !(P.Maybe T.Text)
      -- | If set, only requests for matching resource types will be intercepted.
    , resourceType :: !(P.Maybe Network.ResourceType)
      -- | Stage at wich to begin intercepting requests. Default is Request.
    , requestStage :: !(P.Maybe RequestStage)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RequestPattern where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "RequestPattern" $ \_o -> RequestPattern
            <$> _o .:? "urlPattern"
            <*> _o .:? "resourceType"
            <*> _o .:? "requestStage"
        ago = A.withArray "RequestPattern" $ \_a -> RequestPattern
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON RequestPattern where
    toEncoding (RequestPattern _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ ("urlPattern" .=) <$> _0
        , ("resourceType" .=) <$> _1
        , ("requestStage" .=) <$> _2
        ]
    toJSON (RequestPattern _0 _1 _2) = A.object $ P.catMaybes
        [ ("urlPattern" .=) <$> _0
        , ("resourceType" .=) <$> _1
        , ("requestStage" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup RequestPattern where
    RequestPattern _0 _1 _2 <> RequestPattern __0 __1 __2 = RequestPattern (_0 <|> __0) (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
instance P.Monoid RequestPattern where
    mempty = RequestPattern P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Response HTTP header entry
data HeaderEntry = HeaderEntry
    { name :: !T.Text
    , value :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON HeaderEntry where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "HeaderEntry" $ \_o -> HeaderEntry
            <$> _o .: "name"
            <*> _o .: "value"
        ago = A.withArray "HeaderEntry" $ \_a -> HeaderEntry
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON HeaderEntry where
    toEncoding (HeaderEntry _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "value" .= _1
        ]
    toJSON (HeaderEntry _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "value" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup HeaderEntry where
    HeaderEntry _0 _1 <> HeaderEntry _ _ = HeaderEntry _0 _1


------------------------------------------------------------------------------
-- | Authorization challenge for HTTP status code 401 or 407.
{-# WARNING AuthChallenge "This feature is marked as EXPERIMENTAL." #-}
data AuthChallenge = AuthChallenge
    { -- | Source of the authentication challenge.
      source :: !(P.Maybe Source)
      -- | Origin of the challenger.
    , origin :: !T.Text
      -- | The authentication scheme used, such as basic or digest
    , scheme :: !T.Text
      -- | The realm of the challenge. May be empty.
    , realm :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AuthChallenge where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "AuthChallenge" $ \_o -> AuthChallenge
            <$> _o .:? "source"
            <*> _o .: "origin"
            <*> _o .: "scheme"
            <*> _o .: "realm"
        ago = A.withArray "AuthChallenge" $ \_a -> AuthChallenge
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON AuthChallenge where
    toEncoding (AuthChallenge _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ ("source" .=) <$> _0
        , P.pure $ "origin" .= _1
        , P.pure $ "scheme" .= _2
        , P.pure $ "realm" .= _3
        ]
    toJSON (AuthChallenge _0 _1 _2 _3) = A.object $ P.catMaybes
        [ ("source" .=) <$> _0
        , P.pure $ "origin" .= _1
        , P.pure $ "scheme" .= _2
        , P.pure $ "realm" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup AuthChallenge where
    AuthChallenge _0 _1 _2 _3 <> AuthChallenge __0 _ _ _ = AuthChallenge (_0 <|> __0) _1 _2 _3


------------------------------------------------------------------------------
data Source
    = Server
    | Proxy
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Source where
    parseJSON = A.withText "Source" $ \t -> case t of
        "Server" -> P.pure Server
        "Proxy" -> P.pure Proxy
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Source where
    toJSON Server = "Server"
    toJSON Proxy = "Proxy"


------------------------------------------------------------------------------
-- | Response to an AuthChallenge.
{-# WARNING AuthChallengeResponse "This feature is marked as EXPERIMENTAL." #-}
data AuthChallengeResponse = AuthChallengeResponse
    { -- | The decision on what to do in response to the authorization challenge.  Default means
      -- deferring to the default behavior of the net stack, which will likely either the Cancel
      -- authentication or display a popup dialog box.
      response :: !Response
      -- | The username to provide, possibly empty. Should only be set if response is
      -- ProvideCredentials.
    , username :: !(P.Maybe T.Text)
      -- | The password to provide, possibly empty. Should only be set if response is
      -- ProvideCredentials.
    , password :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AuthChallengeResponse where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "AuthChallengeResponse" $ \_o -> AuthChallengeResponse
            <$> _o .: "response"
            <*> _o .:? "username"
            <*> _o .:? "password"
        ago = A.withArray "AuthChallengeResponse" $ \_a -> AuthChallengeResponse
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON AuthChallengeResponse where
    toEncoding (AuthChallengeResponse _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "response" .= _0
        , ("username" .=) <$> _1
        , ("password" .=) <$> _2
        ]
    toJSON (AuthChallengeResponse _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "response" .= _0
        , ("username" .=) <$> _1
        , ("password" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup AuthChallengeResponse where
    AuthChallengeResponse _0 _1 _2 <> AuthChallengeResponse _ __1 __2 = AuthChallengeResponse _0 (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
data Response
    = Default
    | CancelAuth
    | ProvideCredentials
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Response where
    parseJSON = A.withText "Response" $ \t -> case t of
        "Default" -> P.pure Default
        "CancelAuth" -> P.pure CancelAuth
        "ProvideCredentials" -> P.pure ProvideCredentials
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Response where
    toJSON Default = "Default"
    toJSON CancelAuth = "CancelAuth"
    toJSON ProvideCredentials = "ProvideCredentials"

