{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | This domain allows configuring virtual authenticators to test the WebAuthn
-- API.
module DevTools.API.WebAuthn{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

    ( module DevTools.API.WebAuthn.Types
    , module DevTools.API.WebAuthn
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
import           DevTools.API.WebAuthn.Types


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Enable the WebAuthn domain and start intercepting credential storage and
-- retrieval with a virtual authenticator.
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
    name _ = "WebAuthn.enable"


------------------------------------------------------------------------------
-- | Enable the WebAuthn domain and start intercepting credential storage and
-- retrieval with a virtual authenticator.
enable
    :: Enable
enable = Enable


------------------------------------------------------------------------------
-- | Disable the WebAuthn domain.
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
    name _ = "WebAuthn.disable"


------------------------------------------------------------------------------
-- | Disable the WebAuthn domain.
disable
    :: Disable
disable = Disable


------------------------------------------------------------------------------
-- | Creates and adds a virtual authenticator.
data AddVirtualAuthenticator = AddVirtualAuthenticator
    { options :: !VirtualAuthenticatorOptions
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AddVirtualAuthenticator where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "addVirtualAuthenticator" $ \_o -> AddVirtualAuthenticator
            <$> _o .: "options"
        ago = A.withArray "addVirtualAuthenticator" $ \_a -> AddVirtualAuthenticator
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON AddVirtualAuthenticator where
    toEncoding (AddVirtualAuthenticator _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "options" .= _0
        ]
    toJSON (AddVirtualAuthenticator _0) = A.object $ P.catMaybes
        [ P.pure $ "options" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup AddVirtualAuthenticator where
    AddVirtualAuthenticator _0 <> AddVirtualAuthenticator _ = AddVirtualAuthenticator _0


------------------------------------------------------------------------------
-- | Creates and adds a virtual authenticator.
data AddVirtualAuthenticatorResult = AddVirtualAuthenticatorResult
    { authenticatorId :: !AuthenticatorId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AddVirtualAuthenticatorResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "addVirtualAuthenticatorResult" $ \_o -> AddVirtualAuthenticatorResult
            <$> _o .: "authenticatorId"
        ago = A.withArray "addVirtualAuthenticatorResult" $ \_a -> AddVirtualAuthenticatorResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON AddVirtualAuthenticatorResult where
    toEncoding (AddVirtualAuthenticatorResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "authenticatorId" .= _0
        ]
    toJSON (AddVirtualAuthenticatorResult _0) = A.object $ P.catMaybes
        [ P.pure $ "authenticatorId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup AddVirtualAuthenticatorResult where
    AddVirtualAuthenticatorResult _0 <> AddVirtualAuthenticatorResult _ = AddVirtualAuthenticatorResult _0


------------------------------------------------------------------------------
instance M.Method AddVirtualAuthenticator where
    type Result AddVirtualAuthenticator = AddVirtualAuthenticatorResult
    name _ = "WebAuthn.addVirtualAuthenticator"


------------------------------------------------------------------------------
-- | Creates and adds a virtual authenticator.
addVirtualAuthenticator
    :: VirtualAuthenticatorOptions
    -> AddVirtualAuthenticator
addVirtualAuthenticator _0 = AddVirtualAuthenticator _0


------------------------------------------------------------------------------
-- | Removes the given authenticator.
data RemoveVirtualAuthenticator = RemoveVirtualAuthenticator
    { authenticatorId :: !AuthenticatorId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RemoveVirtualAuthenticator where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "removeVirtualAuthenticator" $ \_o -> RemoveVirtualAuthenticator
            <$> _o .: "authenticatorId"
        ago = A.withArray "removeVirtualAuthenticator" $ \_a -> RemoveVirtualAuthenticator
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON RemoveVirtualAuthenticator where
    toEncoding (RemoveVirtualAuthenticator _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "authenticatorId" .= _0
        ]
    toJSON (RemoveVirtualAuthenticator _0) = A.object $ P.catMaybes
        [ P.pure $ "authenticatorId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup RemoveVirtualAuthenticator where
    RemoveVirtualAuthenticator _0 <> RemoveVirtualAuthenticator _ = RemoveVirtualAuthenticator _0


------------------------------------------------------------------------------
instance M.Method RemoveVirtualAuthenticator where
    type Result RemoveVirtualAuthenticator = ()
    name _ = "WebAuthn.removeVirtualAuthenticator"


------------------------------------------------------------------------------
-- | Removes the given authenticator.
removeVirtualAuthenticator
    :: AuthenticatorId
    -> RemoveVirtualAuthenticator
removeVirtualAuthenticator _0 = RemoveVirtualAuthenticator _0


------------------------------------------------------------------------------
-- | Adds the credential to the specified authenticator.
data AddCredential = AddCredential
    { authenticatorId :: !AuthenticatorId
    , credential :: !Credential
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AddCredential where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "addCredential" $ \_o -> AddCredential
            <$> _o .: "authenticatorId"
            <*> _o .: "credential"
        ago = A.withArray "addCredential" $ \_a -> AddCredential
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON AddCredential where
    toEncoding (AddCredential _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "authenticatorId" .= _0
        , P.pure $ "credential" .= _1
        ]
    toJSON (AddCredential _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "authenticatorId" .= _0
        , P.pure $ "credential" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup AddCredential where
    AddCredential _0 _1 <> AddCredential _ _ = AddCredential _0 _1


------------------------------------------------------------------------------
instance M.Method AddCredential where
    type Result AddCredential = ()
    name _ = "WebAuthn.addCredential"


------------------------------------------------------------------------------
-- | Adds the credential to the specified authenticator.
addCredential
    :: AuthenticatorId
    -> Credential
    -> AddCredential
addCredential _0 _1 = AddCredential _0 _1


------------------------------------------------------------------------------
-- | Returns a single credential stored in the given virtual authenticator that
-- matches the credential ID.
data GetCredential = GetCredential
    { authenticatorId :: !AuthenticatorId
    , credentialId :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetCredential where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getCredential" $ \_o -> GetCredential
            <$> _o .: "authenticatorId"
            <*> _o .: "credentialId"
        ago = A.withArray "getCredential" $ \_a -> GetCredential
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON GetCredential where
    toEncoding (GetCredential _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "authenticatorId" .= _0
        , P.pure $ "credentialId" .= _1
        ]
    toJSON (GetCredential _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "authenticatorId" .= _0
        , P.pure $ "credentialId" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetCredential where
    GetCredential _0 _1 <> GetCredential _ _ = GetCredential _0 _1


------------------------------------------------------------------------------
-- | Returns a single credential stored in the given virtual authenticator that
-- matches the credential ID.
data GetCredentialResult = GetCredentialResult
    { credential :: !Credential
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetCredentialResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getCredentialResult" $ \_o -> GetCredentialResult
            <$> _o .: "credential"
        ago = A.withArray "getCredentialResult" $ \_a -> GetCredentialResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetCredentialResult where
    toEncoding (GetCredentialResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "credential" .= _0
        ]
    toJSON (GetCredentialResult _0) = A.object $ P.catMaybes
        [ P.pure $ "credential" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetCredentialResult where
    GetCredentialResult _0 <> GetCredentialResult _ = GetCredentialResult _0


------------------------------------------------------------------------------
instance M.Method GetCredential where
    type Result GetCredential = GetCredentialResult
    name _ = "WebAuthn.getCredential"


------------------------------------------------------------------------------
-- | Returns a single credential stored in the given virtual authenticator that
-- matches the credential ID.
getCredential
    :: AuthenticatorId
    -> T.Text
    -> GetCredential
getCredential _0 _1 = GetCredential _0 _1


------------------------------------------------------------------------------
-- | Returns all the credentials stored in the given virtual authenticator.
data GetCredentials = GetCredentials
    { authenticatorId :: !AuthenticatorId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetCredentials where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getCredentials" $ \_o -> GetCredentials
            <$> _o .: "authenticatorId"
        ago = A.withArray "getCredentials" $ \_a -> GetCredentials
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetCredentials where
    toEncoding (GetCredentials _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "authenticatorId" .= _0
        ]
    toJSON (GetCredentials _0) = A.object $ P.catMaybes
        [ P.pure $ "authenticatorId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetCredentials where
    GetCredentials _0 <> GetCredentials _ = GetCredentials _0


------------------------------------------------------------------------------
-- | Returns all the credentials stored in the given virtual authenticator.
data GetCredentialsResult = GetCredentialsResult
    { credentials :: ![Credential]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetCredentialsResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getCredentialsResult" $ \_o -> GetCredentialsResult
            <$> _o .: "credentials"
        ago = A.withArray "getCredentialsResult" $ \_a -> GetCredentialsResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetCredentialsResult where
    toEncoding (GetCredentialsResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "credentials" .= _0
        ]
    toJSON (GetCredentialsResult _0) = A.object $ P.catMaybes
        [ P.pure $ "credentials" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetCredentialsResult where
    GetCredentialsResult _0 <> GetCredentialsResult _ = GetCredentialsResult _0


------------------------------------------------------------------------------
instance M.Method GetCredentials where
    type Result GetCredentials = GetCredentialsResult
    name _ = "WebAuthn.getCredentials"


------------------------------------------------------------------------------
-- | Returns all the credentials stored in the given virtual authenticator.
getCredentials
    :: AuthenticatorId
    -> GetCredentials
getCredentials _0 = GetCredentials _0


------------------------------------------------------------------------------
-- | Removes a credential from the authenticator.
data RemoveCredential = RemoveCredential
    { authenticatorId :: !AuthenticatorId
    , credentialId :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RemoveCredential where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "removeCredential" $ \_o -> RemoveCredential
            <$> _o .: "authenticatorId"
            <*> _o .: "credentialId"
        ago = A.withArray "removeCredential" $ \_a -> RemoveCredential
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON RemoveCredential where
    toEncoding (RemoveCredential _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "authenticatorId" .= _0
        , P.pure $ "credentialId" .= _1
        ]
    toJSON (RemoveCredential _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "authenticatorId" .= _0
        , P.pure $ "credentialId" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup RemoveCredential where
    RemoveCredential _0 _1 <> RemoveCredential _ _ = RemoveCredential _0 _1


------------------------------------------------------------------------------
instance M.Method RemoveCredential where
    type Result RemoveCredential = ()
    name _ = "WebAuthn.removeCredential"


------------------------------------------------------------------------------
-- | Removes a credential from the authenticator.
removeCredential
    :: AuthenticatorId
    -> T.Text
    -> RemoveCredential
removeCredential _0 _1 = RemoveCredential _0 _1


------------------------------------------------------------------------------
-- | Clears all the credentials from the specified device.
data ClearCredentials = ClearCredentials
    { authenticatorId :: !AuthenticatorId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ClearCredentials where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "clearCredentials" $ \_o -> ClearCredentials
            <$> _o .: "authenticatorId"
        ago = A.withArray "clearCredentials" $ \_a -> ClearCredentials
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON ClearCredentials where
    toEncoding (ClearCredentials _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "authenticatorId" .= _0
        ]
    toJSON (ClearCredentials _0) = A.object $ P.catMaybes
        [ P.pure $ "authenticatorId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup ClearCredentials where
    ClearCredentials _0 <> ClearCredentials _ = ClearCredentials _0


------------------------------------------------------------------------------
instance M.Method ClearCredentials where
    type Result ClearCredentials = ()
    name _ = "WebAuthn.clearCredentials"


------------------------------------------------------------------------------
-- | Clears all the credentials from the specified device.
clearCredentials
    :: AuthenticatorId
    -> ClearCredentials
clearCredentials _0 = ClearCredentials _0


------------------------------------------------------------------------------
-- | Sets whether User Verification succeeds or fails for an authenticator.
-- The default is true.
data SetUserVerified = SetUserVerified
    { authenticatorId :: !AuthenticatorId
    , isUserVerified :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetUserVerified where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setUserVerified" $ \_o -> SetUserVerified
            <$> _o .: "authenticatorId"
            <*> _o .: "isUserVerified"
        ago = A.withArray "setUserVerified" $ \_a -> SetUserVerified
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SetUserVerified where
    toEncoding (SetUserVerified _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "authenticatorId" .= _0
        , P.pure $ "isUserVerified" .= _1
        ]
    toJSON (SetUserVerified _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "authenticatorId" .= _0
        , P.pure $ "isUserVerified" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetUserVerified where
    SetUserVerified _0 _1 <> SetUserVerified _ _ = SetUserVerified _0 _1


------------------------------------------------------------------------------
instance M.Method SetUserVerified where
    type Result SetUserVerified = ()
    name _ = "WebAuthn.setUserVerified"


------------------------------------------------------------------------------
-- | Sets whether User Verification succeeds or fails for an authenticator.
-- The default is true.
setUserVerified
    :: AuthenticatorId
    -> P.Bool
    -> SetUserVerified
setUserVerified _0 _1 = SetUserVerified _0 _1

