{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | This domain allows configuring virtual authenticators to test the WebAuthn
-- API.
module DevTools.API.WebAuthn.Types{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

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
type AuthenticatorId = T.Text


------------------------------------------------------------------------------
data AuthenticatorProtocol
    = U2f
    | Ctap2
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AuthenticatorProtocol where
    parseJSON = A.withText "AuthenticatorProtocol" $ \t -> case t of
        "u2f" -> P.pure U2f
        "ctap2" -> P.pure Ctap2
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON AuthenticatorProtocol where
    toJSON U2f = "u2f"
    toJSON Ctap2 = "ctap2"


------------------------------------------------------------------------------
data AuthenticatorTransport
    = Usb
    | Nfc
    | Ble
    | Cable
    | Internal
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AuthenticatorTransport where
    parseJSON = A.withText "AuthenticatorTransport" $ \t -> case t of
        "usb" -> P.pure Usb
        "nfc" -> P.pure Nfc
        "ble" -> P.pure Ble
        "cable" -> P.pure Cable
        "internal" -> P.pure Internal
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON AuthenticatorTransport where
    toJSON Usb = "usb"
    toJSON Nfc = "nfc"
    toJSON Ble = "ble"
    toJSON Cable = "cable"
    toJSON Internal = "internal"


------------------------------------------------------------------------------
data VirtualAuthenticatorOptions = VirtualAuthenticatorOptions
    { protocol :: !AuthenticatorProtocol
    , transport :: !AuthenticatorTransport
    , hasResidentKey :: !P.Bool
    , hasUserVerification :: !P.Bool
      -- | If set to true, tests of user presence will succeed immediately.
      -- Otherwise, they will not be resolved. Defaults to true.
    , automaticPresenceSimulation :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON VirtualAuthenticatorOptions where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "VirtualAuthenticatorOptions" $ \_o -> VirtualAuthenticatorOptions
            <$> _o .: "protocol"
            <*> _o .: "transport"
            <*> _o .: "hasResidentKey"
            <*> _o .: "hasUserVerification"
            <*> _o .:? "automaticPresenceSimulation"
        ago = A.withArray "VirtualAuthenticatorOptions" $ \_a -> VirtualAuthenticatorOptions
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON VirtualAuthenticatorOptions where
    toEncoding (VirtualAuthenticatorOptions _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "protocol" .= _0
        , P.pure $ "transport" .= _1
        , P.pure $ "hasResidentKey" .= _2
        , P.pure $ "hasUserVerification" .= _3
        , ("automaticPresenceSimulation" .=) <$> _4
        ]
    toJSON (VirtualAuthenticatorOptions _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ P.pure $ "protocol" .= _0
        , P.pure $ "transport" .= _1
        , P.pure $ "hasResidentKey" .= _2
        , P.pure $ "hasUserVerification" .= _3
        , ("automaticPresenceSimulation" .=) <$> _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup VirtualAuthenticatorOptions where
    VirtualAuthenticatorOptions _0 _1 _2 _3 _4 <> VirtualAuthenticatorOptions _ _ _ _ __4 = VirtualAuthenticatorOptions _0 _1 _2 _3 (_4 <|> __4)


------------------------------------------------------------------------------
data Credential = Credential
    { credentialId :: !T.Text
    , isResidentCredential :: !P.Bool
      -- | Relying Party ID the credential is scoped to. Must be set when adding a
      -- credential.
    , rpId :: !(P.Maybe T.Text)
      -- | The ECDSA P-256 private key in PKCS#8 format.
    , privateKey :: !T.Text
      -- | An opaque byte sequence with a maximum size of 64 bytes mapping the
      -- credential to a specific user.
    , userHandle :: !(P.Maybe T.Text)
      -- | Signature counter. This is incremented by one for each successful
      -- assertion.
      -- See https:\/\/w3c.github.io\/webauthn\/#signature-counter
    , signCount :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Credential where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "Credential" $ \_o -> Credential
            <$> _o .: "credentialId"
            <*> _o .: "isResidentCredential"
            <*> _o .:? "rpId"
            <*> _o .: "privateKey"
            <*> _o .:? "userHandle"
            <*> _o .: "signCount"
        ago = A.withArray "Credential" $ \_a -> Credential
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)


------------------------------------------------------------------------------
instance A.ToJSON Credential where
    toEncoding (Credential _0 _1 _2 _3 _4 _5) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "credentialId" .= _0
        , P.pure $ "isResidentCredential" .= _1
        , ("rpId" .=) <$> _2
        , P.pure $ "privateKey" .= _3
        , ("userHandle" .=) <$> _4
        , P.pure $ "signCount" .= _5
        ]
    toJSON (Credential _0 _1 _2 _3 _4 _5) = A.object $ P.catMaybes
        [ P.pure $ "credentialId" .= _0
        , P.pure $ "isResidentCredential" .= _1
        , ("rpId" .=) <$> _2
        , P.pure $ "privateKey" .= _3
        , ("userHandle" .=) <$> _4
        , P.pure $ "signCount" .= _5
        ]


------------------------------------------------------------------------------
instance P.Semigroup Credential where
    Credential _0 _1 _2 _3 _4 _5 <> Credential _ _ __2 _ __4 _ = Credential _0 _1 (_2 <|> __2) _3 (_4 <|> __4) _5

