{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Security
module DevTools.API.Security
    ( module DevTools.API.Security.Types
    , module DevTools.API.Security
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
import           DevTools.API.Security.Types


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Disables tracking security state changes.
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
    name _ = "Security.disable"


------------------------------------------------------------------------------
-- | Disables tracking security state changes.
disable
    :: Disable
disable = Disable


------------------------------------------------------------------------------
-- | Enables tracking security state changes.
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
    name _ = "Security.enable"


------------------------------------------------------------------------------
-- | Enables tracking security state changes.
enable
    :: Enable
enable = Enable


------------------------------------------------------------------------------
-- | Enable\/disable whether all certificate errors should be ignored.
{-# WARNING SetIgnoreCertificateErrors "This feature is marked as EXPERIMENTAL." #-}
data SetIgnoreCertificateErrors = SetIgnoreCertificateErrors
    { -- | If true, all certificate errors will be ignored.
      ignore :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetIgnoreCertificateErrors where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setIgnoreCertificateErrors" $ \_o -> SetIgnoreCertificateErrors
            <$> _o .: "ignore"
        ago = A.withArray "setIgnoreCertificateErrors" $ \_a -> SetIgnoreCertificateErrors
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetIgnoreCertificateErrors where
    toEncoding (SetIgnoreCertificateErrors _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "ignore" .= _0
        ]
    toJSON (SetIgnoreCertificateErrors _0) = A.object $ P.catMaybes
        [ P.pure $ "ignore" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetIgnoreCertificateErrors where
    SetIgnoreCertificateErrors _0 <> SetIgnoreCertificateErrors _ = SetIgnoreCertificateErrors _0


------------------------------------------------------------------------------
instance M.Method SetIgnoreCertificateErrors where
    type Result SetIgnoreCertificateErrors = ()
    name _ = "Security.setIgnoreCertificateErrors"


------------------------------------------------------------------------------
-- | Enable\/disable whether all certificate errors should be ignored.
{-# WARNING setIgnoreCertificateErrors "This feature is marked as EXPERIMENTAL." #-}
setIgnoreCertificateErrors
    :: P.Bool
    -- ^ If true, all certificate errors will be ignored.

    -> SetIgnoreCertificateErrors
setIgnoreCertificateErrors _0 = SetIgnoreCertificateErrors _0


------------------------------------------------------------------------------
-- | Handles a certificate error that fired a certificateError event.
{-# DEPRECATED HandleCertificateError "This may be removed in a future release." #-}
data HandleCertificateError = HandleCertificateError
    { -- | The ID of the event.
      eventId :: !P.Int
      -- | The action to take on the certificate error.
    , action :: !CertificateErrorAction
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON HandleCertificateError where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "handleCertificateError" $ \_o -> HandleCertificateError
            <$> _o .: "eventId"
            <*> _o .: "action"
        ago = A.withArray "handleCertificateError" $ \_a -> HandleCertificateError
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON HandleCertificateError where
    toEncoding (HandleCertificateError _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "eventId" .= _0
        , P.pure $ "action" .= _1
        ]
    toJSON (HandleCertificateError _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "eventId" .= _0
        , P.pure $ "action" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup HandleCertificateError where
    HandleCertificateError _0 _1 <> HandleCertificateError _ _ = HandleCertificateError _0 _1


------------------------------------------------------------------------------
instance M.Method HandleCertificateError where
    type Result HandleCertificateError = ()
    name _ = "Security.handleCertificateError"


------------------------------------------------------------------------------
-- | Handles a certificate error that fired a certificateError event.
{-# DEPRECATED handleCertificateError "This may be removed in a future release." #-}
handleCertificateError
    :: P.Int
    -- ^ The ID of the event.

    -> CertificateErrorAction
    -- ^ The action to take on the certificate error.

    -> HandleCertificateError
handleCertificateError _0 _1 = HandleCertificateError _0 _1


------------------------------------------------------------------------------
-- | Enable\/disable overriding certificate errors. If enabled, all certificate error events need to
-- be handled by the DevTools client and should be answered with @handleCertificateError@ commands.
{-# DEPRECATED SetOverrideCertificateErrors "This may be removed in a future release." #-}
data SetOverrideCertificateErrors = SetOverrideCertificateErrors
    { -- | If true, certificate errors will be overridden.
      override :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetOverrideCertificateErrors where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setOverrideCertificateErrors" $ \_o -> SetOverrideCertificateErrors
            <$> _o .: "override"
        ago = A.withArray "setOverrideCertificateErrors" $ \_a -> SetOverrideCertificateErrors
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetOverrideCertificateErrors where
    toEncoding (SetOverrideCertificateErrors _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "override" .= _0
        ]
    toJSON (SetOverrideCertificateErrors _0) = A.object $ P.catMaybes
        [ P.pure $ "override" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetOverrideCertificateErrors where
    SetOverrideCertificateErrors _0 <> SetOverrideCertificateErrors _ = SetOverrideCertificateErrors _0


------------------------------------------------------------------------------
instance M.Method SetOverrideCertificateErrors where
    type Result SetOverrideCertificateErrors = ()
    name _ = "Security.setOverrideCertificateErrors"


------------------------------------------------------------------------------
-- | Enable\/disable overriding certificate errors. If enabled, all certificate error events need to
-- be handled by the DevTools client and should be answered with @handleCertificateError@ commands.
{-# DEPRECATED setOverrideCertificateErrors "This may be removed in a future release." #-}
setOverrideCertificateErrors
    :: P.Bool
    -- ^ If true, certificate errors will be overridden.

    -> SetOverrideCertificateErrors
setOverrideCertificateErrors _0 = SetOverrideCertificateErrors _0


------------------------------------------------------------------------------
-- | There is a certificate error. If overriding certificate errors is enabled, then it should be
-- handled with the @handleCertificateError@ command. Note: this event does not fire if the
-- certificate error has been allowed internally. Only one client per target should override
-- certificate errors at the same time.
{-# DEPRECATED CertificateError "This may be removed in a future release." #-}
data CertificateError = CertificateError
    { -- | The ID of the event.
      eventId :: !P.Int
      -- | The type of the error.
    , errorType :: !T.Text
      -- | The url that was requested.
    , requestURL :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CertificateError where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "certificateError" $ \_o -> CertificateError
            <$> _o .: "eventId"
            <*> _o .: "errorType"
            <*> _o .: "requestURL"
        ago = A.withArray "certificateError" $ \_a -> CertificateError
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON CertificateError where
    toEncoding (CertificateError _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "eventId" .= _0
        , P.pure $ "errorType" .= _1
        , P.pure $ "requestURL" .= _2
        ]
    toJSON (CertificateError _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "eventId" .= _0
        , P.pure $ "errorType" .= _1
        , P.pure $ "requestURL" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup CertificateError where
    CertificateError _0 _1 _2 <> CertificateError _ _ _ = CertificateError _0 _1 _2


------------------------------------------------------------------------------
instance E.Event CertificateError where
    type Result CertificateError = CertificateError
    name _ = "Security.certificateError"


------------------------------------------------------------------------------
-- | There is a certificate error. If overriding certificate errors is enabled, then it should be
-- handled with the @handleCertificateError@ command. Note: this event does not fire if the
-- certificate error has been allowed internally. Only one client per target should override
-- certificate errors at the same time.
{-# DEPRECATED certificateError "This may be removed in a future release." #-}
certificateError :: P.Proxy CertificateError
certificateError = P.Proxy


------------------------------------------------------------------------------
-- | The security state of the page changed.
{-# DEPRECATED insecureContentStatus "This may be removed in a future release." #-}
data SecurityStateChanged = SecurityStateChanged
    { -- | Security state.
      securityState :: !SecurityState
      -- | True if the page was loaded over cryptographic transport such as HTTPS.
    , schemeIsCryptographic :: !P.Bool
      -- | List of explanations for the security state. If the overall security state is @insecure@ or
      -- @warning@, at least one corresponding explanation should be included.
    , explanations :: ![SecurityStateExplanation]
      -- | Information about insecure content on the page.
    , insecureContentStatus :: !InsecureContentStatus
      -- | Overrides user-visible description of the state.
    , summary :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SecurityStateChanged where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "securityStateChanged" $ \_o -> SecurityStateChanged
            <$> _o .: "securityState"
            <*> _o .: "schemeIsCryptographic"
            <*> _o .: "explanations"
            <*> _o .: "insecureContentStatus"
            <*> _o .:? "summary"
        ago = A.withArray "securityStateChanged" $ \_a -> SecurityStateChanged
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON SecurityStateChanged where
    toEncoding (SecurityStateChanged _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "securityState" .= _0
        , P.pure $ "schemeIsCryptographic" .= _1
        , P.pure $ "explanations" .= _2
        , P.pure $ "insecureContentStatus" .= _3
        , ("summary" .=) <$> _4
        ]
    toJSON (SecurityStateChanged _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ P.pure $ "securityState" .= _0
        , P.pure $ "schemeIsCryptographic" .= _1
        , P.pure $ "explanations" .= _2
        , P.pure $ "insecureContentStatus" .= _3
        , ("summary" .=) <$> _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup SecurityStateChanged where
    SecurityStateChanged _0 _1 _2 _3 _4 <> SecurityStateChanged _ _ _ _ __4 = SecurityStateChanged _0 _1 _2 _3 (_4 <|> __4)


------------------------------------------------------------------------------
instance E.Event SecurityStateChanged where
    type Result SecurityStateChanged = SecurityStateChanged
    name _ = "Security.securityStateChanged"


------------------------------------------------------------------------------
-- | The security state of the page changed.
securityStateChanged :: P.Proxy SecurityStateChanged
securityStateChanged = P.Proxy

