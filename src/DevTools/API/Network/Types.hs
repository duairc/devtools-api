{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Network domain allows tracking network activities of the page. It exposes information about http,
-- file, data and other requests and responses, their headers, bodies, timing, etc.
module DevTools.API.Network.Types
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
import qualified DevTools.API.Runtime.Types as Runtime
import qualified DevTools.API.Security.Types as Security


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Resource type as it was perceived by the rendering engine.
data ResourceType
    = Document
    | Stylesheet
    | Image
    | Media
    | Font
    | Script
    | TextTrack
    | XHR
    | Fetch
    | EventSource
    | WebSocket
    | Manifest
    | SignedExchange
    | Ping
    | CSPViolationReport
    | Other
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ResourceType where
    parseJSON = A.withText "ResourceType" $ \t -> case t of
        "Document" -> P.pure Document
        "Stylesheet" -> P.pure Stylesheet
        "Image" -> P.pure Image
        "Media" -> P.pure Media
        "Font" -> P.pure Font
        "Script" -> P.pure Script
        "TextTrack" -> P.pure TextTrack
        "XHR" -> P.pure XHR
        "Fetch" -> P.pure Fetch
        "EventSource" -> P.pure EventSource
        "WebSocket" -> P.pure WebSocket
        "Manifest" -> P.pure Manifest
        "SignedExchange" -> P.pure SignedExchange
        "Ping" -> P.pure Ping
        "CSPViolationReport" -> P.pure CSPViolationReport
        "Other" -> P.pure Other
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON ResourceType where
    toJSON Document = "Document"
    toJSON Stylesheet = "Stylesheet"
    toJSON Image = "Image"
    toJSON Media = "Media"
    toJSON Font = "Font"
    toJSON Script = "Script"
    toJSON TextTrack = "TextTrack"
    toJSON XHR = "XHR"
    toJSON Fetch = "Fetch"
    toJSON EventSource = "EventSource"
    toJSON WebSocket = "WebSocket"
    toJSON Manifest = "Manifest"
    toJSON SignedExchange = "SignedExchange"
    toJSON Ping = "Ping"
    toJSON CSPViolationReport = "CSPViolationReport"
    toJSON Other = "Other"


------------------------------------------------------------------------------
-- | Unique loader identifier.
type LoaderId = T.Text


------------------------------------------------------------------------------
-- | Unique request identifier.
type RequestId = T.Text


------------------------------------------------------------------------------
-- | Unique intercepted request identifier.
type InterceptionId = T.Text


------------------------------------------------------------------------------
-- | Network level fetch failure reason.
data ErrorReason
    = Failed
    | Aborted
    | TimedOut
    | AccessDenied
    | ConnectionClosed
    | ConnectionReset
    | ConnectionRefused
    | ConnectionAborted
    | ConnectionFailed
    | NameNotResolved
    | InternetDisconnected
    | AddressUnreachable
    | BlockedByClient
    | BlockedByResponse
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ErrorReason where
    parseJSON = A.withText "ErrorReason" $ \t -> case t of
        "Failed" -> P.pure Failed
        "Aborted" -> P.pure Aborted
        "TimedOut" -> P.pure TimedOut
        "AccessDenied" -> P.pure AccessDenied
        "ConnectionClosed" -> P.pure ConnectionClosed
        "ConnectionReset" -> P.pure ConnectionReset
        "ConnectionRefused" -> P.pure ConnectionRefused
        "ConnectionAborted" -> P.pure ConnectionAborted
        "ConnectionFailed" -> P.pure ConnectionFailed
        "NameNotResolved" -> P.pure NameNotResolved
        "InternetDisconnected" -> P.pure InternetDisconnected
        "AddressUnreachable" -> P.pure AddressUnreachable
        "BlockedByClient" -> P.pure BlockedByClient
        "BlockedByResponse" -> P.pure BlockedByResponse
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON ErrorReason where
    toJSON Failed = "Failed"
    toJSON Aborted = "Aborted"
    toJSON TimedOut = "TimedOut"
    toJSON AccessDenied = "AccessDenied"
    toJSON ConnectionClosed = "ConnectionClosed"
    toJSON ConnectionReset = "ConnectionReset"
    toJSON ConnectionRefused = "ConnectionRefused"
    toJSON ConnectionAborted = "ConnectionAborted"
    toJSON ConnectionFailed = "ConnectionFailed"
    toJSON NameNotResolved = "NameNotResolved"
    toJSON InternetDisconnected = "InternetDisconnected"
    toJSON AddressUnreachable = "AddressUnreachable"
    toJSON BlockedByClient = "BlockedByClient"
    toJSON BlockedByResponse = "BlockedByResponse"


------------------------------------------------------------------------------
-- | UTC time in seconds, counted from January 1, 1970.
type TimeSinceEpoch = P.Double


------------------------------------------------------------------------------
-- | Monotonically increasing time in seconds since an arbitrary point in the past.
type MonotonicTime = P.Double


------------------------------------------------------------------------------
-- | Request \/ response headers as keys \/ values of JSON object.
type Headers = A.Object


------------------------------------------------------------------------------
-- | The underlying connection technology that the browser is supposedly using.
data ConnectionType
    = None
    | Cellular2g
    | Cellular3g
    | Cellular4g
    | Bluetooth
    | Ethernet
    | Wifi
    | Wimax
    | Other_
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ConnectionType where
    parseJSON = A.withText "ConnectionType" $ \t -> case t of
        "none" -> P.pure None
        "cellular2g" -> P.pure Cellular2g
        "cellular3g" -> P.pure Cellular3g
        "cellular4g" -> P.pure Cellular4g
        "bluetooth" -> P.pure Bluetooth
        "ethernet" -> P.pure Ethernet
        "wifi" -> P.pure Wifi
        "wimax" -> P.pure Wimax
        "other" -> P.pure Other_
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON ConnectionType where
    toJSON None = "none"
    toJSON Cellular2g = "cellular2g"
    toJSON Cellular3g = "cellular3g"
    toJSON Cellular4g = "cellular4g"
    toJSON Bluetooth = "bluetooth"
    toJSON Ethernet = "ethernet"
    toJSON Wifi = "wifi"
    toJSON Wimax = "wimax"
    toJSON Other_ = "other"


------------------------------------------------------------------------------
-- | Represents the cookie's 'SameSite' status:
-- https:\/\/tools.ietf.org\/html\/draft-west-first-party-cookies
data CookieSameSite
    = Strict
    | Lax
    | Extended
    | None_
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CookieSameSite where
    parseJSON = A.withText "CookieSameSite" $ \t -> case t of
        "Strict" -> P.pure Strict
        "Lax" -> P.pure Lax
        "Extended" -> P.pure Extended
        "None" -> P.pure None_
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON CookieSameSite where
    toJSON Strict = "Strict"
    toJSON Lax = "Lax"
    toJSON Extended = "Extended"
    toJSON None_ = "None"


------------------------------------------------------------------------------
-- | Timing information for the request.
{-# WARNING workerStart, workerReady, pushStart, pushEnd "This feature is marked as EXPERIMENTAL." #-}
data ResourceTiming = ResourceTiming
    { -- | Timing's requestTime is a baseline in seconds, while the other numbers are ticks in
      -- milliseconds relatively to this requestTime.
      requestTime :: !P.Double
      -- | Started resolving proxy.
    , proxyStart :: !P.Double
      -- | Finished resolving proxy.
    , proxyEnd :: !P.Double
      -- | Started DNS address resolve.
    , dnsStart :: !P.Double
      -- | Finished DNS address resolve.
    , dnsEnd :: !P.Double
      -- | Started connecting to the remote host.
    , connectStart :: !P.Double
      -- | Connected to the remote host.
    , connectEnd :: !P.Double
      -- | Started SSL handshake.
    , sslStart :: !P.Double
      -- | Finished SSL handshake.
    , sslEnd :: !P.Double
      -- | Started running ServiceWorker.
    , workerStart :: !P.Double
      -- | Finished Starting ServiceWorker.
    , workerReady :: !P.Double
      -- | Started sending request.
    , sendStart :: !P.Double
      -- | Finished sending request.
    , sendEnd :: !P.Double
      -- | Time the server started pushing request.
    , pushStart :: !P.Double
      -- | Time the server finished pushing request.
    , pushEnd :: !P.Double
      -- | Finished receiving response headers.
    , receiveHeadersEnd :: !P.Double
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ResourceTiming where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "ResourceTiming" $ \_o -> ResourceTiming
            <$> _o .: "requestTime"
            <*> _o .: "proxyStart"
            <*> _o .: "proxyEnd"
            <*> _o .: "dnsStart"
            <*> _o .: "dnsEnd"
            <*> _o .: "connectStart"
            <*> _o .: "connectEnd"
            <*> _o .: "sslStart"
            <*> _o .: "sslEnd"
            <*> _o .: "workerStart"
            <*> _o .: "workerReady"
            <*> _o .: "sendStart"
            <*> _o .: "sendEnd"
            <*> _o .: "pushStart"
            <*> _o .: "pushEnd"
            <*> _o .: "receiveHeadersEnd"
        ago = A.withArray "ResourceTiming" $ \_a -> ResourceTiming
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.maybe P.empty A.parseJSON (_a !? 6)
            <*> P.maybe P.empty A.parseJSON (_a !? 7)
            <*> P.maybe P.empty A.parseJSON (_a !? 8)
            <*> P.maybe P.empty A.parseJSON (_a !? 9)
            <*> P.maybe P.empty A.parseJSON (_a !? 10)
            <*> P.maybe P.empty A.parseJSON (_a !? 11)
            <*> P.maybe P.empty A.parseJSON (_a !? 12)
            <*> P.maybe P.empty A.parseJSON (_a !? 13)
            <*> P.maybe P.empty A.parseJSON (_a !? 14)
            <*> P.maybe P.empty A.parseJSON (_a !? 15)


------------------------------------------------------------------------------
instance A.ToJSON ResourceTiming where
    toEncoding (ResourceTiming _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 _15) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestTime" .= _0
        , P.pure $ "proxyStart" .= _1
        , P.pure $ "proxyEnd" .= _2
        , P.pure $ "dnsStart" .= _3
        , P.pure $ "dnsEnd" .= _4
        , P.pure $ "connectStart" .= _5
        , P.pure $ "connectEnd" .= _6
        , P.pure $ "sslStart" .= _7
        , P.pure $ "sslEnd" .= _8
        , P.pure $ "workerStart" .= _9
        , P.pure $ "workerReady" .= _10
        , P.pure $ "sendStart" .= _11
        , P.pure $ "sendEnd" .= _12
        , P.pure $ "pushStart" .= _13
        , P.pure $ "pushEnd" .= _14
        , P.pure $ "receiveHeadersEnd" .= _15
        ]
    toJSON (ResourceTiming _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 _15) = A.object $ P.catMaybes
        [ P.pure $ "requestTime" .= _0
        , P.pure $ "proxyStart" .= _1
        , P.pure $ "proxyEnd" .= _2
        , P.pure $ "dnsStart" .= _3
        , P.pure $ "dnsEnd" .= _4
        , P.pure $ "connectStart" .= _5
        , P.pure $ "connectEnd" .= _6
        , P.pure $ "sslStart" .= _7
        , P.pure $ "sslEnd" .= _8
        , P.pure $ "workerStart" .= _9
        , P.pure $ "workerReady" .= _10
        , P.pure $ "sendStart" .= _11
        , P.pure $ "sendEnd" .= _12
        , P.pure $ "pushStart" .= _13
        , P.pure $ "pushEnd" .= _14
        , P.pure $ "receiveHeadersEnd" .= _15
        ]


------------------------------------------------------------------------------
instance P.Semigroup ResourceTiming where
    ResourceTiming _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 _15 <> ResourceTiming _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = ResourceTiming _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 _15


------------------------------------------------------------------------------
-- | Loading priority of a resource request.
data ResourcePriority
    = VeryLow
    | Low
    | Medium
    | High
    | VeryHigh
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ResourcePriority where
    parseJSON = A.withText "ResourcePriority" $ \t -> case t of
        "VeryLow" -> P.pure VeryLow
        "Low" -> P.pure Low
        "Medium" -> P.pure Medium
        "High" -> P.pure High
        "VeryHigh" -> P.pure VeryHigh
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON ResourcePriority where
    toJSON VeryLow = "VeryLow"
    toJSON Low = "Low"
    toJSON Medium = "Medium"
    toJSON High = "High"
    toJSON VeryHigh = "VeryHigh"


------------------------------------------------------------------------------
-- | HTTP request data.
data Request = Request
    { -- | Request URL (without fragment).
      url :: !T.Text
      -- | Fragment of the requested URL starting with hash, if present.
    , urlFragment :: !(P.Maybe T.Text)
      -- | HTTP request method.
    , method :: !T.Text
      -- | HTTP request headers.
    , headers :: !Headers
      -- | HTTP POST request data.
    , postData :: !(P.Maybe T.Text)
      -- | True when the request has POST data. Note that postData might still be omitted when this flag is true when the data is too long.
    , hasPostData :: !(P.Maybe P.Bool)
      -- | The mixed content type of the request.
    , mixedContentType :: !(P.Maybe Security.MixedContentType)
      -- | Priority of the resource request at the time request is sent.
    , initialPriority :: !ResourcePriority
      -- | The referrer policy of the request, as defined in https:\/\/www.w3.org\/TR\/referrer-policy\/
    , referrerPolicy :: !ReferrerPolicy
      -- | Whether is loaded via link preload.
    , isLinkPreload :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Request where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "Request" $ \_o -> Request
            <$> _o .: "url"
            <*> _o .:? "urlFragment"
            <*> _o .: "method"
            <*> _o .: "headers"
            <*> _o .:? "postData"
            <*> _o .:? "hasPostData"
            <*> _o .:? "mixedContentType"
            <*> _o .: "initialPriority"
            <*> _o .: "referrerPolicy"
            <*> _o .:? "isLinkPreload"
        ago = A.withArray "Request" $ \_a -> Request
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)
            <*> P.maybe P.empty A.parseJSON (_a !? 7)
            <*> P.maybe P.empty A.parseJSON (_a !? 8)
            <*> P.traverse A.parseJSON (_a !? 9)


------------------------------------------------------------------------------
instance A.ToJSON Request where
    toEncoding (Request _0 _1 _2 _3 _4 _5 _6 _7 _8 _9) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "url" .= _0
        , ("urlFragment" .=) <$> _1
        , P.pure $ "method" .= _2
        , P.pure $ "headers" .= _3
        , ("postData" .=) <$> _4
        , ("hasPostData" .=) <$> _5
        , ("mixedContentType" .=) <$> _6
        , P.pure $ "initialPriority" .= _7
        , P.pure $ "referrerPolicy" .= _8
        , ("isLinkPreload" .=) <$> _9
        ]
    toJSON (Request _0 _1 _2 _3 _4 _5 _6 _7 _8 _9) = A.object $ P.catMaybes
        [ P.pure $ "url" .= _0
        , ("urlFragment" .=) <$> _1
        , P.pure $ "method" .= _2
        , P.pure $ "headers" .= _3
        , ("postData" .=) <$> _4
        , ("hasPostData" .=) <$> _5
        , ("mixedContentType" .=) <$> _6
        , P.pure $ "initialPriority" .= _7
        , P.pure $ "referrerPolicy" .= _8
        , ("isLinkPreload" .=) <$> _9
        ]


------------------------------------------------------------------------------
instance P.Semigroup Request where
    Request _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 <> Request _ __1 _ _ __4 __5 __6 _ _ __9 = Request _0 (_1 <|> __1) _2 _3 (_4 <|> __4) (_5 <|> __5) (_6 <|> __6) _7 _8 (_9 <|> __9)


------------------------------------------------------------------------------
data ReferrerPolicy
    = UnsafeUrl
    | NoReferrerWhenDowngrade
    | NoReferrer
    | Origin
    | OriginWhenCrossOrigin
    | SameOrigin
    | StrictOrigin
    | StrictOriginWhenCrossOrigin
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ReferrerPolicy where
    parseJSON = A.withText "ReferrerPolicy" $ \t -> case t of
        "unsafe-url" -> P.pure UnsafeUrl
        "no-referrer-when-downgrade" -> P.pure NoReferrerWhenDowngrade
        "no-referrer" -> P.pure NoReferrer
        "origin" -> P.pure Origin
        "origin-when-cross-origin" -> P.pure OriginWhenCrossOrigin
        "same-origin" -> P.pure SameOrigin
        "strict-origin" -> P.pure StrictOrigin
        "strict-origin-when-cross-origin" -> P.pure StrictOriginWhenCrossOrigin
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON ReferrerPolicy where
    toJSON UnsafeUrl = "unsafe-url"
    toJSON NoReferrerWhenDowngrade = "no-referrer-when-downgrade"
    toJSON NoReferrer = "no-referrer"
    toJSON Origin = "origin"
    toJSON OriginWhenCrossOrigin = "origin-when-cross-origin"
    toJSON SameOrigin = "same-origin"
    toJSON StrictOrigin = "strict-origin"
    toJSON StrictOriginWhenCrossOrigin = "strict-origin-when-cross-origin"


------------------------------------------------------------------------------
-- | Details of a signed certificate timestamp (SCT).
data SignedCertificateTimestamp = SignedCertificateTimestamp
    { -- | Validation status.
      status :: !T.Text
      -- | Origin.
    , origin :: !T.Text
      -- | Log name \/ description.
    , logDescription :: !T.Text
      -- | Log ID.
    , logId :: !T.Text
      -- | Issuance date.
    , timestamp :: !TimeSinceEpoch
      -- | Hash algorithm.
    , hashAlgorithm :: !T.Text
      -- | Signature algorithm.
    , signatureAlgorithm :: !T.Text
      -- | Signature data.
    , signatureData :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SignedCertificateTimestamp where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "SignedCertificateTimestamp" $ \_o -> SignedCertificateTimestamp
            <$> _o .: "status"
            <*> _o .: "origin"
            <*> _o .: "logDescription"
            <*> _o .: "logId"
            <*> _o .: "timestamp"
            <*> _o .: "hashAlgorithm"
            <*> _o .: "signatureAlgorithm"
            <*> _o .: "signatureData"
        ago = A.withArray "SignedCertificateTimestamp" $ \_a -> SignedCertificateTimestamp
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.maybe P.empty A.parseJSON (_a !? 6)
            <*> P.maybe P.empty A.parseJSON (_a !? 7)


------------------------------------------------------------------------------
instance A.ToJSON SignedCertificateTimestamp where
    toEncoding (SignedCertificateTimestamp _0 _1 _2 _3 _4 _5 _6 _7) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "status" .= _0
        , P.pure $ "origin" .= _1
        , P.pure $ "logDescription" .= _2
        , P.pure $ "logId" .= _3
        , P.pure $ "timestamp" .= _4
        , P.pure $ "hashAlgorithm" .= _5
        , P.pure $ "signatureAlgorithm" .= _6
        , P.pure $ "signatureData" .= _7
        ]
    toJSON (SignedCertificateTimestamp _0 _1 _2 _3 _4 _5 _6 _7) = A.object $ P.catMaybes
        [ P.pure $ "status" .= _0
        , P.pure $ "origin" .= _1
        , P.pure $ "logDescription" .= _2
        , P.pure $ "logId" .= _3
        , P.pure $ "timestamp" .= _4
        , P.pure $ "hashAlgorithm" .= _5
        , P.pure $ "signatureAlgorithm" .= _6
        , P.pure $ "signatureData" .= _7
        ]


------------------------------------------------------------------------------
instance P.Semigroup SignedCertificateTimestamp where
    SignedCertificateTimestamp _0 _1 _2 _3 _4 _5 _6 _7 <> SignedCertificateTimestamp _ _ _ _ _ _ _ _ = SignedCertificateTimestamp _0 _1 _2 _3 _4 _5 _6 _7


------------------------------------------------------------------------------
-- | Security details about a request.
data SecurityDetails = SecurityDetails
    { -- | Protocol name (e.g. "TLS 1.2" or "QUIC").
      protocol :: !T.Text
      -- | Key Exchange used by the connection, or the empty string if not applicable.
    , keyExchange :: !T.Text
      -- | (EC)DH group used by the connection, if applicable.
    , keyExchangeGroup :: !(P.Maybe T.Text)
      -- | Cipher name.
    , cipher :: !T.Text
      -- | TLS MAC. Note that AEAD ciphers do not have separate MACs.
    , mac :: !(P.Maybe T.Text)
      -- | Certificate ID value.
    , certificateId :: !Security.CertificateId
      -- | Certificate subject name.
    , subjectName :: !T.Text
      -- | Subject Alternative Name (SAN) DNS names and IP addresses.
    , sanList :: ![T.Text]
      -- | Name of the issuing CA.
    , issuer :: !T.Text
      -- | Certificate valid from date.
    , validFrom :: !TimeSinceEpoch
      -- | Certificate valid to (expiration) date
    , validTo :: !TimeSinceEpoch
      -- | List of signed certificate timestamps (SCTs).
    , signedCertificateTimestampList :: ![SignedCertificateTimestamp]
      -- | Whether the request complied with Certificate Transparency policy
    , certificateTransparencyCompliance :: !CertificateTransparencyCompliance
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SecurityDetails where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "SecurityDetails" $ \_o -> SecurityDetails
            <$> _o .: "protocol"
            <*> _o .: "keyExchange"
            <*> _o .:? "keyExchangeGroup"
            <*> _o .: "cipher"
            <*> _o .:? "mac"
            <*> _o .: "certificateId"
            <*> _o .: "subjectName"
            <*> _o .: "sanList"
            <*> _o .: "issuer"
            <*> _o .: "validFrom"
            <*> _o .: "validTo"
            <*> _o .: "signedCertificateTimestampList"
            <*> _o .: "certificateTransparencyCompliance"
        ago = A.withArray "SecurityDetails" $ \_a -> SecurityDetails
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.maybe P.empty A.parseJSON (_a !? 6)
            <*> P.maybe P.empty A.parseJSON (_a !? 7)
            <*> P.maybe P.empty A.parseJSON (_a !? 8)
            <*> P.maybe P.empty A.parseJSON (_a !? 9)
            <*> P.maybe P.empty A.parseJSON (_a !? 10)
            <*> P.maybe P.empty A.parseJSON (_a !? 11)
            <*> P.maybe P.empty A.parseJSON (_a !? 12)


------------------------------------------------------------------------------
instance A.ToJSON SecurityDetails where
    toEncoding (SecurityDetails _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "protocol" .= _0
        , P.pure $ "keyExchange" .= _1
        , ("keyExchangeGroup" .=) <$> _2
        , P.pure $ "cipher" .= _3
        , ("mac" .=) <$> _4
        , P.pure $ "certificateId" .= _5
        , P.pure $ "subjectName" .= _6
        , P.pure $ "sanList" .= _7
        , P.pure $ "issuer" .= _8
        , P.pure $ "validFrom" .= _9
        , P.pure $ "validTo" .= _10
        , P.pure $ "signedCertificateTimestampList" .= _11
        , P.pure $ "certificateTransparencyCompliance" .= _12
        ]
    toJSON (SecurityDetails _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12) = A.object $ P.catMaybes
        [ P.pure $ "protocol" .= _0
        , P.pure $ "keyExchange" .= _1
        , ("keyExchangeGroup" .=) <$> _2
        , P.pure $ "cipher" .= _3
        , ("mac" .=) <$> _4
        , P.pure $ "certificateId" .= _5
        , P.pure $ "subjectName" .= _6
        , P.pure $ "sanList" .= _7
        , P.pure $ "issuer" .= _8
        , P.pure $ "validFrom" .= _9
        , P.pure $ "validTo" .= _10
        , P.pure $ "signedCertificateTimestampList" .= _11
        , P.pure $ "certificateTransparencyCompliance" .= _12
        ]


------------------------------------------------------------------------------
instance P.Semigroup SecurityDetails where
    SecurityDetails _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 <> SecurityDetails _ _ __2 _ __4 _ _ _ _ _ _ _ _ = SecurityDetails _0 _1 (_2 <|> __2) _3 (_4 <|> __4) _5 _6 _7 _8 _9 _10 _11 _12


------------------------------------------------------------------------------
-- | Whether the request complied with Certificate Transparency policy.
data CertificateTransparencyCompliance
    = Unknown
    | NotCompliant
    | Compliant
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CertificateTransparencyCompliance where
    parseJSON = A.withText "CertificateTransparencyCompliance" $ \t -> case t of
        "unknown" -> P.pure Unknown
        "not-compliant" -> P.pure NotCompliant
        "compliant" -> P.pure Compliant
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON CertificateTransparencyCompliance where
    toJSON Unknown = "unknown"
    toJSON NotCompliant = "not-compliant"
    toJSON Compliant = "compliant"


------------------------------------------------------------------------------
-- | The reason why request was blocked.
data BlockedReason
    = Other__
    | Csp
    | MixedContent
    | Origin_
    | Inspector
    | SubresourceFilter
    | ContentType
    | CollapsedByClient
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON BlockedReason where
    parseJSON = A.withText "BlockedReason" $ \t -> case t of
        "other" -> P.pure Other__
        "csp" -> P.pure Csp
        "mixed-content" -> P.pure MixedContent
        "origin" -> P.pure Origin_
        "inspector" -> P.pure Inspector
        "subresource-filter" -> P.pure SubresourceFilter
        "content-type" -> P.pure ContentType
        "collapsed-by-client" -> P.pure CollapsedByClient
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON BlockedReason where
    toJSON Other__ = "other"
    toJSON Csp = "csp"
    toJSON MixedContent = "mixed-content"
    toJSON Origin_ = "origin"
    toJSON Inspector = "inspector"
    toJSON SubresourceFilter = "subresource-filter"
    toJSON ContentType = "content-type"
    toJSON CollapsedByClient = "collapsed-by-client"


------------------------------------------------------------------------------
-- | HTTP response data.
data Response = Response
    { -- | Response URL. This URL can be different from CachedResource.url in case of redirect.
      url :: !T.Text
      -- | HTTP response status code.
    , status :: !P.Int
      -- | HTTP response status text.
    , statusText :: !T.Text
      -- | HTTP response headers.
    , headers :: !Headers
      -- | HTTP response headers text.
    , headersText :: !(P.Maybe T.Text)
      -- | Resource mimeType as determined by the browser.
    , mimeType :: !T.Text
      -- | Refined HTTP request headers that were actually transmitted over the network.
    , requestHeaders :: !(P.Maybe Headers)
      -- | HTTP request headers text.
    , requestHeadersText :: !(P.Maybe T.Text)
      -- | Specifies whether physical connection was actually reused for this request.
    , connectionReused :: !P.Bool
      -- | Physical connection id that was actually used for this request.
    , connectionId :: !P.Double
      -- | Remote IP address.
    , remoteIPAddress :: !(P.Maybe T.Text)
      -- | Remote port.
    , remotePort :: !(P.Maybe P.Int)
      -- | Specifies that the request was served from the disk cache.
    , fromDiskCache :: !(P.Maybe P.Bool)
      -- | Specifies that the request was served from the ServiceWorker.
    , fromServiceWorker :: !(P.Maybe P.Bool)
      -- | Specifies that the request was served from the prefetch cache.
    , fromPrefetchCache :: !(P.Maybe P.Bool)
      -- | Total number of bytes received for this request so far.
    , encodedDataLength :: !P.Double
      -- | Timing information for the given request.
    , timing :: !(P.Maybe ResourceTiming)
      -- | Protocol used to fetch this request.
    , protocol :: !(P.Maybe T.Text)
      -- | Security state of the request resource.
    , securityState :: !Security.SecurityState
      -- | Security details for the request.
    , securityDetails :: !(P.Maybe SecurityDetails)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Response where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "Response" $ \_o -> Response
            <$> _o .: "url"
            <*> _o .: "status"
            <*> _o .: "statusText"
            <*> _o .: "headers"
            <*> _o .:? "headersText"
            <*> _o .: "mimeType"
            <*> _o .:? "requestHeaders"
            <*> _o .:? "requestHeadersText"
            <*> _o .: "connectionReused"
            <*> _o .: "connectionId"
            <*> _o .:? "remoteIPAddress"
            <*> _o .:? "remotePort"
            <*> _o .:? "fromDiskCache"
            <*> _o .:? "fromServiceWorker"
            <*> _o .:? "fromPrefetchCache"
            <*> _o .: "encodedDataLength"
            <*> _o .:? "timing"
            <*> _o .:? "protocol"
            <*> _o .: "securityState"
            <*> _o .:? "securityDetails"
        ago = A.withArray "Response" $ \_a -> Response
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)
            <*> P.traverse A.parseJSON (_a !? 7)
            <*> P.maybe P.empty A.parseJSON (_a !? 8)
            <*> P.maybe P.empty A.parseJSON (_a !? 9)
            <*> P.traverse A.parseJSON (_a !? 10)
            <*> P.traverse A.parseJSON (_a !? 11)
            <*> P.traverse A.parseJSON (_a !? 12)
            <*> P.traverse A.parseJSON (_a !? 13)
            <*> P.traverse A.parseJSON (_a !? 14)
            <*> P.maybe P.empty A.parseJSON (_a !? 15)
            <*> P.traverse A.parseJSON (_a !? 16)
            <*> P.traverse A.parseJSON (_a !? 17)
            <*> P.maybe P.empty A.parseJSON (_a !? 18)
            <*> P.traverse A.parseJSON (_a !? 19)


------------------------------------------------------------------------------
instance A.ToJSON Response where
    toEncoding (Response _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 _15 _16 _17 _18 _19) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "url" .= _0
        , P.pure $ "status" .= _1
        , P.pure $ "statusText" .= _2
        , P.pure $ "headers" .= _3
        , ("headersText" .=) <$> _4
        , P.pure $ "mimeType" .= _5
        , ("requestHeaders" .=) <$> _6
        , ("requestHeadersText" .=) <$> _7
        , P.pure $ "connectionReused" .= _8
        , P.pure $ "connectionId" .= _9
        , ("remoteIPAddress" .=) <$> _10
        , ("remotePort" .=) <$> _11
        , ("fromDiskCache" .=) <$> _12
        , ("fromServiceWorker" .=) <$> _13
        , ("fromPrefetchCache" .=) <$> _14
        , P.pure $ "encodedDataLength" .= _15
        , ("timing" .=) <$> _16
        , ("protocol" .=) <$> _17
        , P.pure $ "securityState" .= _18
        , ("securityDetails" .=) <$> _19
        ]
    toJSON (Response _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 _15 _16 _17 _18 _19) = A.object $ P.catMaybes
        [ P.pure $ "url" .= _0
        , P.pure $ "status" .= _1
        , P.pure $ "statusText" .= _2
        , P.pure $ "headers" .= _3
        , ("headersText" .=) <$> _4
        , P.pure $ "mimeType" .= _5
        , ("requestHeaders" .=) <$> _6
        , ("requestHeadersText" .=) <$> _7
        , P.pure $ "connectionReused" .= _8
        , P.pure $ "connectionId" .= _9
        , ("remoteIPAddress" .=) <$> _10
        , ("remotePort" .=) <$> _11
        , ("fromDiskCache" .=) <$> _12
        , ("fromServiceWorker" .=) <$> _13
        , ("fromPrefetchCache" .=) <$> _14
        , P.pure $ "encodedDataLength" .= _15
        , ("timing" .=) <$> _16
        , ("protocol" .=) <$> _17
        , P.pure $ "securityState" .= _18
        , ("securityDetails" .=) <$> _19
        ]


------------------------------------------------------------------------------
instance P.Semigroup Response where
    Response _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 _15 _16 _17 _18 _19 <> Response _ _ _ _ __4 _ __6 __7 _ _ __10 __11 __12 __13 __14 _ __16 __17 _ __19 = Response _0 _1 _2 _3 (_4 <|> __4) _5 (_6 <|> __6) (_7 <|> __7) _8 _9 (_10 <|> __10) (_11 <|> __11) (_12 <|> __12) (_13 <|> __13) (_14 <|> __14) _15 (_16 <|> __16) (_17 <|> __17) _18 (_19 <|> __19)


------------------------------------------------------------------------------
-- | WebSocket request data.
data WebSocketRequest = WebSocketRequest
    { -- | HTTP request headers.
      headers :: !Headers
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON WebSocketRequest where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "WebSocketRequest" $ \_o -> WebSocketRequest
            <$> _o .: "headers"
        ago = A.withArray "WebSocketRequest" $ \_a -> WebSocketRequest
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON WebSocketRequest where
    toEncoding (WebSocketRequest _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "headers" .= _0
        ]
    toJSON (WebSocketRequest _0) = A.object $ P.catMaybes
        [ P.pure $ "headers" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup WebSocketRequest where
    WebSocketRequest _0 <> WebSocketRequest _ = WebSocketRequest _0


------------------------------------------------------------------------------
-- | WebSocket response data.
data WebSocketResponse = WebSocketResponse
    { -- | HTTP response status code.
      status :: !P.Int
      -- | HTTP response status text.
    , statusText :: !T.Text
      -- | HTTP response headers.
    , headers :: !Headers
      -- | HTTP response headers text.
    , headersText :: !(P.Maybe T.Text)
      -- | HTTP request headers.
    , requestHeaders :: !(P.Maybe Headers)
      -- | HTTP request headers text.
    , requestHeadersText :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON WebSocketResponse where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "WebSocketResponse" $ \_o -> WebSocketResponse
            <$> _o .: "status"
            <*> _o .: "statusText"
            <*> _o .: "headers"
            <*> _o .:? "headersText"
            <*> _o .:? "requestHeaders"
            <*> _o .:? "requestHeadersText"
        ago = A.withArray "WebSocketResponse" $ \_a -> WebSocketResponse
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)


------------------------------------------------------------------------------
instance A.ToJSON WebSocketResponse where
    toEncoding (WebSocketResponse _0 _1 _2 _3 _4 _5) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "status" .= _0
        , P.pure $ "statusText" .= _1
        , P.pure $ "headers" .= _2
        , ("headersText" .=) <$> _3
        , ("requestHeaders" .=) <$> _4
        , ("requestHeadersText" .=) <$> _5
        ]
    toJSON (WebSocketResponse _0 _1 _2 _3 _4 _5) = A.object $ P.catMaybes
        [ P.pure $ "status" .= _0
        , P.pure $ "statusText" .= _1
        , P.pure $ "headers" .= _2
        , ("headersText" .=) <$> _3
        , ("requestHeaders" .=) <$> _4
        , ("requestHeadersText" .=) <$> _5
        ]


------------------------------------------------------------------------------
instance P.Semigroup WebSocketResponse where
    WebSocketResponse _0 _1 _2 _3 _4 _5 <> WebSocketResponse _ _ _ __3 __4 __5 = WebSocketResponse _0 _1 _2 (_3 <|> __3) (_4 <|> __4) (_5 <|> __5)


------------------------------------------------------------------------------
-- | WebSocket message data. This represents an entire WebSocket message, not just a fragmented frame as the name suggests.
data WebSocketFrame = WebSocketFrame
    { -- | WebSocket message opcode.
      opcode :: !P.Double
      -- | WebSocket message mask.
    , mask :: !P.Bool
      -- | WebSocket message payload data.
      -- If the opcode is 1, this is a text message and payloadData is a UTF-8 string.
      -- If the opcode isn't 1, then payloadData is a base64 encoded string representing binary data.
    , payloadData :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON WebSocketFrame where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "WebSocketFrame" $ \_o -> WebSocketFrame
            <$> _o .: "opcode"
            <*> _o .: "mask"
            <*> _o .: "payloadData"
        ago = A.withArray "WebSocketFrame" $ \_a -> WebSocketFrame
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON WebSocketFrame where
    toEncoding (WebSocketFrame _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "opcode" .= _0
        , P.pure $ "mask" .= _1
        , P.pure $ "payloadData" .= _2
        ]
    toJSON (WebSocketFrame _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "opcode" .= _0
        , P.pure $ "mask" .= _1
        , P.pure $ "payloadData" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup WebSocketFrame where
    WebSocketFrame _0 _1 _2 <> WebSocketFrame _ _ _ = WebSocketFrame _0 _1 _2


------------------------------------------------------------------------------
-- | Information about the cached resource.
data CachedResource = CachedResource
    { -- | Resource URL. This is the url of the original network request.
      url :: !T.Text
      -- | Type of this resource.
    , type_ :: !ResourceType
      -- | Cached response data.
    , response :: !(P.Maybe Response)
      -- | Cached response body size.
    , bodySize :: !P.Double
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CachedResource where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "CachedResource" $ \_o -> CachedResource
            <$> _o .: "url"
            <*> _o .: "type"
            <*> _o .:? "response"
            <*> _o .: "bodySize"
        ago = A.withArray "CachedResource" $ \_a -> CachedResource
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON CachedResource where
    toEncoding (CachedResource _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "url" .= _0
        , P.pure $ "type" .= _1
        , ("response" .=) <$> _2
        , P.pure $ "bodySize" .= _3
        ]
    toJSON (CachedResource _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "url" .= _0
        , P.pure $ "type" .= _1
        , ("response" .=) <$> _2
        , P.pure $ "bodySize" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup CachedResource where
    CachedResource _0 _1 _2 _3 <> CachedResource _ _ __2 _ = CachedResource _0 _1 (_2 <|> __2) _3


------------------------------------------------------------------------------
-- | Information about the request initiator.
data Initiator = Initiator
    { -- | Type of this initiator.
      type_ :: !Type
      -- | Initiator JavaScript stack trace, set for Script only.
    , stack :: !(P.Maybe Runtime.StackTrace)
      -- | Initiator URL, set for Parser type or for Script type (when script is importing module) or for SignedExchange type.
    , url :: !(P.Maybe T.Text)
      -- | Initiator line number, set for Parser type or for Script type (when script is importing
      -- module) (0-based).
    , lineNumber :: !(P.Maybe P.Double)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Initiator where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "Initiator" $ \_o -> Initiator
            <$> _o .: "type"
            <*> _o .:? "stack"
            <*> _o .:? "url"
            <*> _o .:? "lineNumber"
        ago = A.withArray "Initiator" $ \_a -> Initiator
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON Initiator where
    toEncoding (Initiator _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "type" .= _0
        , ("stack" .=) <$> _1
        , ("url" .=) <$> _2
        , ("lineNumber" .=) <$> _3
        ]
    toJSON (Initiator _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "type" .= _0
        , ("stack" .=) <$> _1
        , ("url" .=) <$> _2
        , ("lineNumber" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup Initiator where
    Initiator _0 _1 _2 _3 <> Initiator _ __1 __2 __3 = Initiator _0 (_1 <|> __1) (_2 <|> __2) (_3 <|> __3)


------------------------------------------------------------------------------
data Type
    = Parser
    | Script_
    | Preload
    | SignedExchange_
    | Other___
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Type where
    parseJSON = A.withText "Type" $ \t -> case t of
        "parser" -> P.pure Parser
        "script" -> P.pure Script_
        "preload" -> P.pure Preload
        "SignedExchange" -> P.pure SignedExchange_
        "other" -> P.pure Other___
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Type where
    toJSON Parser = "parser"
    toJSON Script_ = "script"
    toJSON Preload = "preload"
    toJSON SignedExchange_ = "SignedExchange"
    toJSON Other___ = "other"


------------------------------------------------------------------------------
-- | Cookie object
data Cookie = Cookie
    { -- | Cookie name.
      name :: !T.Text
      -- | Cookie value.
    , value :: !T.Text
      -- | Cookie domain.
    , domain :: !T.Text
      -- | Cookie path.
    , path :: !T.Text
      -- | Cookie expiration date as the number of seconds since the UNIX epoch.
    , expires :: !P.Double
      -- | Cookie size.
    , size :: !P.Int
      -- | True if cookie is http-only.
    , httpOnly :: !P.Bool
      -- | True if cookie is secure.
    , secure :: !P.Bool
      -- | True in case of session cookie.
    , session :: !P.Bool
      -- | Cookie SameSite type.
    , sameSite :: !(P.Maybe CookieSameSite)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Cookie where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "Cookie" $ \_o -> Cookie
            <$> _o .: "name"
            <*> _o .: "value"
            <*> _o .: "domain"
            <*> _o .: "path"
            <*> _o .: "expires"
            <*> _o .: "size"
            <*> _o .: "httpOnly"
            <*> _o .: "secure"
            <*> _o .: "session"
            <*> _o .:? "sameSite"
        ago = A.withArray "Cookie" $ \_a -> Cookie
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.maybe P.empty A.parseJSON (_a !? 6)
            <*> P.maybe P.empty A.parseJSON (_a !? 7)
            <*> P.maybe P.empty A.parseJSON (_a !? 8)
            <*> P.traverse A.parseJSON (_a !? 9)


------------------------------------------------------------------------------
instance A.ToJSON Cookie where
    toEncoding (Cookie _0 _1 _2 _3 _4 _5 _6 _7 _8 _9) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "value" .= _1
        , P.pure $ "domain" .= _2
        , P.pure $ "path" .= _3
        , P.pure $ "expires" .= _4
        , P.pure $ "size" .= _5
        , P.pure $ "httpOnly" .= _6
        , P.pure $ "secure" .= _7
        , P.pure $ "session" .= _8
        , ("sameSite" .=) <$> _9
        ]
    toJSON (Cookie _0 _1 _2 _3 _4 _5 _6 _7 _8 _9) = A.object $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "value" .= _1
        , P.pure $ "domain" .= _2
        , P.pure $ "path" .= _3
        , P.pure $ "expires" .= _4
        , P.pure $ "size" .= _5
        , P.pure $ "httpOnly" .= _6
        , P.pure $ "secure" .= _7
        , P.pure $ "session" .= _8
        , ("sameSite" .=) <$> _9
        ]


------------------------------------------------------------------------------
instance P.Semigroup Cookie where
    Cookie _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 <> Cookie _ _ _ _ _ _ _ _ _ __9 = Cookie _0 _1 _2 _3 _4 _5 _6 _7 _8 (_9 <|> __9)


------------------------------------------------------------------------------
-- | Types of reasons why a cookie may not be stored from a response.
{-# WARNING SetCookieBlockedReason "This feature is marked as EXPERIMENTAL." #-}
data SetCookieBlockedReason
    = SecureOnly
    | SameSiteStrict
    | SameSiteLax
    | SameSiteExtended
    | SameSiteUnspecifiedTreatedAsLax
    | SameSiteNoneInsecure
    | UserPreferences
    | SyntaxError
    | SchemeNotSupported
    | OverwriteSecure
    | InvalidDomain
    | InvalidPrefix
    | UnknownError
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetCookieBlockedReason where
    parseJSON = A.withText "SetCookieBlockedReason" $ \t -> case t of
        "SecureOnly" -> P.pure SecureOnly
        "SameSiteStrict" -> P.pure SameSiteStrict
        "SameSiteLax" -> P.pure SameSiteLax
        "SameSiteExtended" -> P.pure SameSiteExtended
        "SameSiteUnspecifiedTreatedAsLax" -> P.pure SameSiteUnspecifiedTreatedAsLax
        "SameSiteNoneInsecure" -> P.pure SameSiteNoneInsecure
        "UserPreferences" -> P.pure UserPreferences
        "SyntaxError" -> P.pure SyntaxError
        "SchemeNotSupported" -> P.pure SchemeNotSupported
        "OverwriteSecure" -> P.pure OverwriteSecure
        "InvalidDomain" -> P.pure InvalidDomain
        "InvalidPrefix" -> P.pure InvalidPrefix
        "UnknownError" -> P.pure UnknownError
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON SetCookieBlockedReason where
    toJSON SecureOnly = "SecureOnly"
    toJSON SameSiteStrict = "SameSiteStrict"
    toJSON SameSiteLax = "SameSiteLax"
    toJSON SameSiteExtended = "SameSiteExtended"
    toJSON SameSiteUnspecifiedTreatedAsLax = "SameSiteUnspecifiedTreatedAsLax"
    toJSON SameSiteNoneInsecure = "SameSiteNoneInsecure"
    toJSON UserPreferences = "UserPreferences"
    toJSON SyntaxError = "SyntaxError"
    toJSON SchemeNotSupported = "SchemeNotSupported"
    toJSON OverwriteSecure = "OverwriteSecure"
    toJSON InvalidDomain = "InvalidDomain"
    toJSON InvalidPrefix = "InvalidPrefix"
    toJSON UnknownError = "UnknownError"


------------------------------------------------------------------------------
-- | Types of reasons why a cookie may not be sent with a request.
{-# WARNING CookieBlockedReason "This feature is marked as EXPERIMENTAL." #-}
data CookieBlockedReason
    = SecureOnly_
    | NotOnPath
    | DomainMismatch
    | SameSiteStrict_
    | SameSiteLax_
    | SameSiteExtended_
    | SameSiteUnspecifiedTreatedAsLax_
    | SameSiteNoneInsecure_
    | UserPreferences_
    | UnknownError_
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CookieBlockedReason where
    parseJSON = A.withText "CookieBlockedReason" $ \t -> case t of
        "SecureOnly" -> P.pure SecureOnly_
        "NotOnPath" -> P.pure NotOnPath
        "DomainMismatch" -> P.pure DomainMismatch
        "SameSiteStrict" -> P.pure SameSiteStrict_
        "SameSiteLax" -> P.pure SameSiteLax_
        "SameSiteExtended" -> P.pure SameSiteExtended_
        "SameSiteUnspecifiedTreatedAsLax" -> P.pure SameSiteUnspecifiedTreatedAsLax_
        "SameSiteNoneInsecure" -> P.pure SameSiteNoneInsecure_
        "UserPreferences" -> P.pure UserPreferences_
        "UnknownError" -> P.pure UnknownError_
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON CookieBlockedReason where
    toJSON SecureOnly_ = "SecureOnly"
    toJSON NotOnPath = "NotOnPath"
    toJSON DomainMismatch = "DomainMismatch"
    toJSON SameSiteStrict_ = "SameSiteStrict"
    toJSON SameSiteLax_ = "SameSiteLax"
    toJSON SameSiteExtended_ = "SameSiteExtended"
    toJSON SameSiteUnspecifiedTreatedAsLax_ = "SameSiteUnspecifiedTreatedAsLax"
    toJSON SameSiteNoneInsecure_ = "SameSiteNoneInsecure"
    toJSON UserPreferences_ = "UserPreferences"
    toJSON UnknownError_ = "UnknownError"


------------------------------------------------------------------------------
-- | A cookie which was not stored from a response with the corresponding reason.
{-# WARNING BlockedSetCookieWithReason "This feature is marked as EXPERIMENTAL." #-}
data BlockedSetCookieWithReason = BlockedSetCookieWithReason
    { -- | The reason this cookie was blocked.
      blockedReason :: !SetCookieBlockedReason
      -- | The string representing this individual cookie as it would appear in the header.
      -- This is not the entire "cookie" or "set-cookie" header which could have multiple cookies.
    , cookieLine :: !T.Text
      -- | The cookie object which represents the cookie which was not stored. It is optional because
      -- sometimes complete cookie information is not available, such as in the case of parsing
      -- errors.
    , cookie :: !(P.Maybe Cookie)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON BlockedSetCookieWithReason where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "BlockedSetCookieWithReason" $ \_o -> BlockedSetCookieWithReason
            <$> _o .: "blockedReason"
            <*> _o .: "cookieLine"
            <*> _o .:? "cookie"
        ago = A.withArray "BlockedSetCookieWithReason" $ \_a -> BlockedSetCookieWithReason
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON BlockedSetCookieWithReason where
    toEncoding (BlockedSetCookieWithReason _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "blockedReason" .= _0
        , P.pure $ "cookieLine" .= _1
        , ("cookie" .=) <$> _2
        ]
    toJSON (BlockedSetCookieWithReason _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "blockedReason" .= _0
        , P.pure $ "cookieLine" .= _1
        , ("cookie" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup BlockedSetCookieWithReason where
    BlockedSetCookieWithReason _0 _1 _2 <> BlockedSetCookieWithReason _ _ __2 = BlockedSetCookieWithReason _0 _1 (_2 <|> __2)


------------------------------------------------------------------------------
-- | A cookie with was not sent with a request with the corresponding reason.
{-# WARNING BlockedCookieWithReason "This feature is marked as EXPERIMENTAL." #-}
data BlockedCookieWithReason = BlockedCookieWithReason
    { -- | The reason the cookie was blocked.
      blockedReason :: !CookieBlockedReason
      -- | The cookie object representing the cookie which was not sent.
    , cookie :: !Cookie
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON BlockedCookieWithReason where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "BlockedCookieWithReason" $ \_o -> BlockedCookieWithReason
            <$> _o .: "blockedReason"
            <*> _o .: "cookie"
        ago = A.withArray "BlockedCookieWithReason" $ \_a -> BlockedCookieWithReason
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON BlockedCookieWithReason where
    toEncoding (BlockedCookieWithReason _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "blockedReason" .= _0
        , P.pure $ "cookie" .= _1
        ]
    toJSON (BlockedCookieWithReason _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "blockedReason" .= _0
        , P.pure $ "cookie" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup BlockedCookieWithReason where
    BlockedCookieWithReason _0 _1 <> BlockedCookieWithReason _ _ = BlockedCookieWithReason _0 _1


------------------------------------------------------------------------------
-- | Cookie parameter object
data CookieParam = CookieParam
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
instance A.FromJSON CookieParam where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "CookieParam" $ \_o -> CookieParam
            <$> _o .: "name"
            <*> _o .: "value"
            <*> _o .:? "url"
            <*> _o .:? "domain"
            <*> _o .:? "path"
            <*> _o .:? "secure"
            <*> _o .:? "httpOnly"
            <*> _o .:? "sameSite"
            <*> _o .:? "expires"
        ago = A.withArray "CookieParam" $ \_a -> CookieParam
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
instance A.ToJSON CookieParam where
    toEncoding (CookieParam _0 _1 _2 _3 _4 _5 _6 _7 _8) = A.pairs $ P.fold $ P.catMaybes
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
    toJSON (CookieParam _0 _1 _2 _3 _4 _5 _6 _7 _8) = A.object $ P.catMaybes
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
instance P.Semigroup CookieParam where
    CookieParam _0 _1 _2 _3 _4 _5 _6 _7 _8 <> CookieParam _ _ __2 __3 __4 __5 __6 __7 __8 = CookieParam _0 _1 (_2 <|> __2) (_3 <|> __3) (_4 <|> __4) (_5 <|> __5) (_6 <|> __6) (_7 <|> __7) (_8 <|> __8)


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
      response :: !Response_
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
data Response_
    = Default
    | CancelAuth
    | ProvideCredentials
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Response_ where
    parseJSON = A.withText "Response" $ \t -> case t of
        "Default" -> P.pure Default
        "CancelAuth" -> P.pure CancelAuth
        "ProvideCredentials" -> P.pure ProvideCredentials
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Response_ where
    toJSON Default = "Default"
    toJSON CancelAuth = "CancelAuth"
    toJSON ProvideCredentials = "ProvideCredentials"


------------------------------------------------------------------------------
-- | Stages of the interception to begin intercepting. Request will intercept before the request is
-- sent. Response will intercept after the response is received.
{-# WARNING InterceptionStage "This feature is marked as EXPERIMENTAL." #-}
data InterceptionStage
    = Request_
    | HeadersReceived
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON InterceptionStage where
    parseJSON = A.withText "InterceptionStage" $ \t -> case t of
        "Request" -> P.pure Request_
        "HeadersReceived" -> P.pure HeadersReceived
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON InterceptionStage where
    toJSON Request_ = "Request"
    toJSON HeadersReceived = "HeadersReceived"


------------------------------------------------------------------------------
-- | Request pattern for interception.
{-# WARNING RequestPattern "This feature is marked as EXPERIMENTAL." #-}
data RequestPattern = RequestPattern
    { -- | Wildcards ('*' -> zero or more, '?' -> exactly one) are allowed. Escape character is
      -- backslash. Omitting is equivalent to "*".
      urlPattern :: !(P.Maybe T.Text)
      -- | If set, only requests for matching resource types will be intercepted.
    , resourceType :: !(P.Maybe ResourceType)
      -- | Stage at wich to begin intercepting requests. Default is Request.
    , interceptionStage :: !(P.Maybe InterceptionStage)
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
            <*> _o .:? "interceptionStage"
        ago = A.withArray "RequestPattern" $ \_a -> RequestPattern
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON RequestPattern where
    toEncoding (RequestPattern _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ ("urlPattern" .=) <$> _0
        , ("resourceType" .=) <$> _1
        , ("interceptionStage" .=) <$> _2
        ]
    toJSON (RequestPattern _0 _1 _2) = A.object $ P.catMaybes
        [ ("urlPattern" .=) <$> _0
        , ("resourceType" .=) <$> _1
        , ("interceptionStage" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup RequestPattern where
    RequestPattern _0 _1 _2 <> RequestPattern __0 __1 __2 = RequestPattern (_0 <|> __0) (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
instance P.Monoid RequestPattern where
    mempty = RequestPattern P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Information about a signed exchange signature.
-- https:\/\/wicg.github.io\/webpackage\/draft-yasskin-httpbis-origin-signed-exchanges-impl.html#rfc.section.3.1
{-# WARNING SignedExchangeSignature "This feature is marked as EXPERIMENTAL." #-}
data SignedExchangeSignature = SignedExchangeSignature
    { -- | Signed exchange signature label.
      label :: !T.Text
      -- | The hex string of signed exchange signature.
    , signature :: !T.Text
      -- | Signed exchange signature integrity.
    , integrity :: !T.Text
      -- | Signed exchange signature cert Url.
    , certUrl :: !(P.Maybe T.Text)
      -- | The hex string of signed exchange signature cert sha256.
    , certSha256 :: !(P.Maybe T.Text)
      -- | Signed exchange signature validity Url.
    , validityUrl :: !T.Text
      -- | Signed exchange signature date.
    , date :: !P.Int
      -- | Signed exchange signature expires.
    , expires :: !P.Int
      -- | The encoded certificates.
    , certificates :: !(P.Maybe [T.Text])
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SignedExchangeSignature where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "SignedExchangeSignature" $ \_o -> SignedExchangeSignature
            <$> _o .: "label"
            <*> _o .: "signature"
            <*> _o .: "integrity"
            <*> _o .:? "certUrl"
            <*> _o .:? "certSha256"
            <*> _o .: "validityUrl"
            <*> _o .: "date"
            <*> _o .: "expires"
            <*> _o .:? "certificates"
        ago = A.withArray "SignedExchangeSignature" $ \_a -> SignedExchangeSignature
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.maybe P.empty A.parseJSON (_a !? 6)
            <*> P.maybe P.empty A.parseJSON (_a !? 7)
            <*> P.traverse A.parseJSON (_a !? 8)


------------------------------------------------------------------------------
instance A.ToJSON SignedExchangeSignature where
    toEncoding (SignedExchangeSignature _0 _1 _2 _3 _4 _5 _6 _7 _8) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "label" .= _0
        , P.pure $ "signature" .= _1
        , P.pure $ "integrity" .= _2
        , ("certUrl" .=) <$> _3
        , ("certSha256" .=) <$> _4
        , P.pure $ "validityUrl" .= _5
        , P.pure $ "date" .= _6
        , P.pure $ "expires" .= _7
        , ("certificates" .=) <$> _8
        ]
    toJSON (SignedExchangeSignature _0 _1 _2 _3 _4 _5 _6 _7 _8) = A.object $ P.catMaybes
        [ P.pure $ "label" .= _0
        , P.pure $ "signature" .= _1
        , P.pure $ "integrity" .= _2
        , ("certUrl" .=) <$> _3
        , ("certSha256" .=) <$> _4
        , P.pure $ "validityUrl" .= _5
        , P.pure $ "date" .= _6
        , P.pure $ "expires" .= _7
        , ("certificates" .=) <$> _8
        ]


------------------------------------------------------------------------------
instance P.Semigroup SignedExchangeSignature where
    SignedExchangeSignature _0 _1 _2 _3 _4 _5 _6 _7 _8 <> SignedExchangeSignature _ _ _ __3 __4 _ _ _ __8 = SignedExchangeSignature _0 _1 _2 (_3 <|> __3) (_4 <|> __4) _5 _6 _7 (_8 <|> __8)


------------------------------------------------------------------------------
-- | Information about a signed exchange header.
-- https:\/\/wicg.github.io\/webpackage\/draft-yasskin-httpbis-origin-signed-exchanges-impl.html#cbor-representation
{-# WARNING SignedExchangeHeader "This feature is marked as EXPERIMENTAL." #-}
data SignedExchangeHeader = SignedExchangeHeader
    { -- | Signed exchange request URL.
      requestUrl :: !T.Text
      -- | Signed exchange response code.
    , responseCode :: !P.Int
      -- | Signed exchange response headers.
    , responseHeaders :: !Headers
      -- | Signed exchange response signature.
    , signatures :: ![SignedExchangeSignature]
      -- | Signed exchange header integrity hash in the form of "sha256-<base64-hash-value>".
    , headerIntegrity :: !T.Text
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SignedExchangeHeader where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "SignedExchangeHeader" $ \_o -> SignedExchangeHeader
            <$> _o .: "requestUrl"
            <*> _o .: "responseCode"
            <*> _o .: "responseHeaders"
            <*> _o .: "signatures"
            <*> _o .: "headerIntegrity"
        ago = A.withArray "SignedExchangeHeader" $ \_a -> SignedExchangeHeader
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON SignedExchangeHeader where
    toEncoding (SignedExchangeHeader _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestUrl" .= _0
        , P.pure $ "responseCode" .= _1
        , P.pure $ "responseHeaders" .= _2
        , P.pure $ "signatures" .= _3
        , P.pure $ "headerIntegrity" .= _4
        ]
    toJSON (SignedExchangeHeader _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ P.pure $ "requestUrl" .= _0
        , P.pure $ "responseCode" .= _1
        , P.pure $ "responseHeaders" .= _2
        , P.pure $ "signatures" .= _3
        , P.pure $ "headerIntegrity" .= _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup SignedExchangeHeader where
    SignedExchangeHeader _0 _1 _2 _3 _4 <> SignedExchangeHeader _ _ _ _ _ = SignedExchangeHeader _0 _1 _2 _3 _4


------------------------------------------------------------------------------
-- | Field type for a signed exchange related error.
{-# WARNING SignedExchangeErrorField "This feature is marked as EXPERIMENTAL." #-}
data SignedExchangeErrorField
    = SignatureSig
    | SignatureIntegrity
    | SignatureCertUrl
    | SignatureCertSha256
    | SignatureValidityUrl
    | SignatureTimestamps
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SignedExchangeErrorField where
    parseJSON = A.withText "SignedExchangeErrorField" $ \t -> case t of
        "signatureSig" -> P.pure SignatureSig
        "signatureIntegrity" -> P.pure SignatureIntegrity
        "signatureCertUrl" -> P.pure SignatureCertUrl
        "signatureCertSha256" -> P.pure SignatureCertSha256
        "signatureValidityUrl" -> P.pure SignatureValidityUrl
        "signatureTimestamps" -> P.pure SignatureTimestamps
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON SignedExchangeErrorField where
    toJSON SignatureSig = "signatureSig"
    toJSON SignatureIntegrity = "signatureIntegrity"
    toJSON SignatureCertUrl = "signatureCertUrl"
    toJSON SignatureCertSha256 = "signatureCertSha256"
    toJSON SignatureValidityUrl = "signatureValidityUrl"
    toJSON SignatureTimestamps = "signatureTimestamps"


------------------------------------------------------------------------------
-- | Information about a signed exchange response.
{-# WARNING SignedExchangeError "This feature is marked as EXPERIMENTAL." #-}
data SignedExchangeError = SignedExchangeError
    { -- | Error message.
      message :: !T.Text
      -- | The index of the signature which caused the error.
    , signatureIndex :: !(P.Maybe P.Int)
      -- | The field which caused the error.
    , errorField :: !(P.Maybe SignedExchangeErrorField)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SignedExchangeError where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "SignedExchangeError" $ \_o -> SignedExchangeError
            <$> _o .: "message"
            <*> _o .:? "signatureIndex"
            <*> _o .:? "errorField"
        ago = A.withArray "SignedExchangeError" $ \_a -> SignedExchangeError
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON SignedExchangeError where
    toEncoding (SignedExchangeError _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "message" .= _0
        , ("signatureIndex" .=) <$> _1
        , ("errorField" .=) <$> _2
        ]
    toJSON (SignedExchangeError _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "message" .= _0
        , ("signatureIndex" .=) <$> _1
        , ("errorField" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup SignedExchangeError where
    SignedExchangeError _0 _1 _2 <> SignedExchangeError _ __1 __2 = SignedExchangeError _0 (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
-- | Information about a signed exchange response.
{-# WARNING SignedExchangeInfo "This feature is marked as EXPERIMENTAL." #-}
data SignedExchangeInfo = SignedExchangeInfo
    { -- | The outer response of signed HTTP exchange which was received from network.
      outerResponse :: !Response
      -- | Information about the signed exchange header.
    , header :: !(P.Maybe SignedExchangeHeader)
      -- | Security details for the signed exchange header.
    , securityDetails :: !(P.Maybe SecurityDetails)
      -- | Errors occurred while handling the signed exchagne.
    , errors :: !(P.Maybe [SignedExchangeError])
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SignedExchangeInfo where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "SignedExchangeInfo" $ \_o -> SignedExchangeInfo
            <$> _o .: "outerResponse"
            <*> _o .:? "header"
            <*> _o .:? "securityDetails"
            <*> _o .:? "errors"
        ago = A.withArray "SignedExchangeInfo" $ \_a -> SignedExchangeInfo
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON SignedExchangeInfo where
    toEncoding (SignedExchangeInfo _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "outerResponse" .= _0
        , ("header" .=) <$> _1
        , ("securityDetails" .=) <$> _2
        , ("errors" .=) <$> _3
        ]
    toJSON (SignedExchangeInfo _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "outerResponse" .= _0
        , ("header" .=) <$> _1
        , ("securityDetails" .=) <$> _2
        , ("errors" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup SignedExchangeInfo where
    SignedExchangeInfo _0 _1 _2 _3 <> SignedExchangeInfo _ __1 __2 __3 = SignedExchangeInfo _0 (_1 <|> __1) (_2 <|> __2) (_3 <|> __3)

