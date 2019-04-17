{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Security
module DevTools.API.Security.Types
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
-- | An internal certificate ID value.
type CertificateId = P.Int


------------------------------------------------------------------------------
-- | A description of mixed content (HTTP resources on HTTPS pages), as defined by
-- https:\/\/www.w3.org\/TR\/mixed-content\/#categories
data MixedContentType
    = Blockable
    | OptionallyBlockable
    | None
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON MixedContentType where
    parseJSON = A.withText "MixedContentType" $ \t -> case t of
        "blockable" -> P.pure Blockable
        "optionally-blockable" -> P.pure OptionallyBlockable
        "none" -> P.pure None
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON MixedContentType where
    toJSON Blockable = "blockable"
    toJSON OptionallyBlockable = "optionally-blockable"
    toJSON None = "none"


------------------------------------------------------------------------------
-- | The security level of a page or resource.
data SecurityState
    = Unknown
    | Neutral
    | Insecure
    | Secure
    | Info
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SecurityState where
    parseJSON = A.withText "SecurityState" $ \t -> case t of
        "unknown" -> P.pure Unknown
        "neutral" -> P.pure Neutral
        "insecure" -> P.pure Insecure
        "secure" -> P.pure Secure
        "info" -> P.pure Info
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON SecurityState where
    toJSON Unknown = "unknown"
    toJSON Neutral = "neutral"
    toJSON Insecure = "insecure"
    toJSON Secure = "secure"
    toJSON Info = "info"


------------------------------------------------------------------------------
-- | An explanation of an factor contributing to the security state.
data SecurityStateExplanation = SecurityStateExplanation
    { -- | Security state representing the severity of the factor being explained.
      securityState :: !SecurityState
      -- | Title describing the type of factor.
    , title :: !T.Text
      -- | Short phrase describing the type of factor.
    , summary :: !T.Text
      -- | Full text explanation of the factor.
    , description :: !T.Text
      -- | The type of mixed content described by the explanation.
    , mixedContentType :: !MixedContentType
      -- | Page certificate.
    , certificate :: ![T.Text]
      -- | Recommendations to fix any issues.
    , recommendations :: !(P.Maybe [T.Text])
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SecurityStateExplanation where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "SecurityStateExplanation" $ \_o -> SecurityStateExplanation
            <$> _o .: "securityState"
            <*> _o .: "title"
            <*> _o .: "summary"
            <*> _o .: "description"
            <*> _o .: "mixedContentType"
            <*> _o .: "certificate"
            <*> _o .:? "recommendations"
        ago = A.withArray "SecurityStateExplanation" $ \_a -> SecurityStateExplanation
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)


------------------------------------------------------------------------------
instance A.ToJSON SecurityStateExplanation where
    toEncoding (SecurityStateExplanation _0 _1 _2 _3 _4 _5 _6) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "securityState" .= _0
        , P.pure $ "title" .= _1
        , P.pure $ "summary" .= _2
        , P.pure $ "description" .= _3
        , P.pure $ "mixedContentType" .= _4
        , P.pure $ "certificate" .= _5
        , ("recommendations" .=) <$> _6
        ]
    toJSON (SecurityStateExplanation _0 _1 _2 _3 _4 _5 _6) = A.object $ P.catMaybes
        [ P.pure $ "securityState" .= _0
        , P.pure $ "title" .= _1
        , P.pure $ "summary" .= _2
        , P.pure $ "description" .= _3
        , P.pure $ "mixedContentType" .= _4
        , P.pure $ "certificate" .= _5
        , ("recommendations" .=) <$> _6
        ]


------------------------------------------------------------------------------
instance P.Semigroup SecurityStateExplanation where
    SecurityStateExplanation _0 _1 _2 _3 _4 _5 _6 <> SecurityStateExplanation _ _ _ _ _ _ __6 = SecurityStateExplanation _0 _1 _2 _3 _4 _5 (_6 <|> __6)


------------------------------------------------------------------------------
-- | Information about insecure content on the page.
{-# DEPRECATED InsecureContentStatus "This may be removed in a future release." #-}
data InsecureContentStatus = InsecureContentStatus
    { -- | Always false.
      ranMixedContent :: !P.Bool
      -- | Always false.
    , displayedMixedContent :: !P.Bool
      -- | Always false.
    , containedMixedForm :: !P.Bool
      -- | Always false.
    , ranContentWithCertErrors :: !P.Bool
      -- | Always false.
    , displayedContentWithCertErrors :: !P.Bool
      -- | Always set to unknown.
    , ranInsecureContentStyle :: !SecurityState
      -- | Always set to unknown.
    , displayedInsecureContentStyle :: !SecurityState
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON InsecureContentStatus where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "InsecureContentStatus" $ \_o -> InsecureContentStatus
            <$> _o .: "ranMixedContent"
            <*> _o .: "displayedMixedContent"
            <*> _o .: "containedMixedForm"
            <*> _o .: "ranContentWithCertErrors"
            <*> _o .: "displayedContentWithCertErrors"
            <*> _o .: "ranInsecureContentStyle"
            <*> _o .: "displayedInsecureContentStyle"
        ago = A.withArray "InsecureContentStatus" $ \_a -> InsecureContentStatus
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.maybe P.empty A.parseJSON (_a !? 6)


------------------------------------------------------------------------------
instance A.ToJSON InsecureContentStatus where
    toEncoding (InsecureContentStatus _0 _1 _2 _3 _4 _5 _6) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "ranMixedContent" .= _0
        , P.pure $ "displayedMixedContent" .= _1
        , P.pure $ "containedMixedForm" .= _2
        , P.pure $ "ranContentWithCertErrors" .= _3
        , P.pure $ "displayedContentWithCertErrors" .= _4
        , P.pure $ "ranInsecureContentStyle" .= _5
        , P.pure $ "displayedInsecureContentStyle" .= _6
        ]
    toJSON (InsecureContentStatus _0 _1 _2 _3 _4 _5 _6) = A.object $ P.catMaybes
        [ P.pure $ "ranMixedContent" .= _0
        , P.pure $ "displayedMixedContent" .= _1
        , P.pure $ "containedMixedForm" .= _2
        , P.pure $ "ranContentWithCertErrors" .= _3
        , P.pure $ "displayedContentWithCertErrors" .= _4
        , P.pure $ "ranInsecureContentStyle" .= _5
        , P.pure $ "displayedInsecureContentStyle" .= _6
        ]


------------------------------------------------------------------------------
instance P.Semigroup InsecureContentStatus where
    InsecureContentStatus _0 _1 _2 _3 _4 _5 _6 <> InsecureContentStatus _ _ _ _ _ _ _ = InsecureContentStatus _0 _1 _2 _3 _4 _5 _6


------------------------------------------------------------------------------
-- | The action to take when a certificate error occurs. continue will continue processing the
-- request and cancel will cancel the request.
data CertificateErrorAction
    = Continue
    | Cancel
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CertificateErrorAction where
    parseJSON = A.withText "CertificateErrorAction" $ \t -> case t of
        "continue" -> P.pure Continue
        "cancel" -> P.pure Cancel
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON CertificateErrorAction where
    toJSON Continue = "continue"
    toJSON Cancel = "cancel"

