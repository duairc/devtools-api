{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Audits domain allows investigation of page violations and possible improvements.
module DevTools.API.Audits{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

    ( module DevTools.API.Audits.Types
    , module DevTools.API.Audits
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
import           DevTools.API.Audits.Types
import qualified DevTools.API.Network.Types as Network


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Returns the response body and size if it were re-encoded with the specified settings. Only
-- applies to images.
data GetEncodedResponse = GetEncodedResponse
    { -- | Identifier of the network request to get content for.
      requestId :: !Network.RequestId
      -- | The encoding to use.
    , encoding :: !Encoding
      -- | The quality of the encoding (0-1). (defaults to 1)
    , quality :: !(P.Maybe P.Double)
      -- | Whether to only return the size information (defaults to false).
    , sizeOnly :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetEncodedResponse where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getEncodedResponse" $ \_o -> GetEncodedResponse
            <$> _o .: "requestId"
            <*> _o .: "encoding"
            <*> _o .:? "quality"
            <*> _o .:? "sizeOnly"
        ago = A.withArray "getEncodedResponse" $ \_a -> GetEncodedResponse
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON GetEncodedResponse where
    toEncoding (GetEncodedResponse _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "encoding" .= _1
        , ("quality" .=) <$> _2
        , ("sizeOnly" .=) <$> _3
        ]
    toJSON (GetEncodedResponse _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "requestId" .= _0
        , P.pure $ "encoding" .= _1
        , ("quality" .=) <$> _2
        , ("sizeOnly" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetEncodedResponse where
    GetEncodedResponse _0 _1 _2 _3 <> GetEncodedResponse _ _ __2 __3 = GetEncodedResponse _0 _1 (_2 <|> __2) (_3 <|> __3)


------------------------------------------------------------------------------
data Encoding
    = Webp
    | Jpeg
    | Png
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Encoding where
    parseJSON = A.withText "Encoding" $ \t -> case t of
        "webp" -> P.pure Webp
        "jpeg" -> P.pure Jpeg
        "png" -> P.pure Png
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Encoding where
    toJSON Webp = "webp"
    toJSON Jpeg = "jpeg"
    toJSON Png = "png"


------------------------------------------------------------------------------
-- | Returns the response body and size if it were re-encoded with the specified settings. Only
-- applies to images.
data GetEncodedResponseResult = GetEncodedResponseResult
    { -- | The encoded body as a base64 string. Omitted if sizeOnly is true.
      body :: !(P.Maybe T.Text)
      -- | Size before re-encoding.
    , originalSize :: !P.Int
      -- | Size after re-encoding.
    , encodedSize :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetEncodedResponseResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getEncodedResponseResult" $ \_o -> GetEncodedResponseResult
            <$> _o .:? "body"
            <*> _o .: "originalSize"
            <*> _o .: "encodedSize"
        ago = A.withArray "getEncodedResponseResult" $ \_a -> GetEncodedResponseResult
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON GetEncodedResponseResult where
    toEncoding (GetEncodedResponseResult _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ ("body" .=) <$> _0
        , P.pure $ "originalSize" .= _1
        , P.pure $ "encodedSize" .= _2
        ]
    toJSON (GetEncodedResponseResult _0 _1 _2) = A.object $ P.catMaybes
        [ ("body" .=) <$> _0
        , P.pure $ "originalSize" .= _1
        , P.pure $ "encodedSize" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetEncodedResponseResult where
    GetEncodedResponseResult _0 _1 _2 <> GetEncodedResponseResult __0 _ _ = GetEncodedResponseResult (_0 <|> __0) _1 _2


------------------------------------------------------------------------------
instance M.Method GetEncodedResponse where
    type Result GetEncodedResponse = GetEncodedResponseResult
    name _ = "Audits.getEncodedResponse"


------------------------------------------------------------------------------
-- | Returns the response body and size if it were re-encoded with the specified settings. Only
-- applies to images.
getEncodedResponse
    :: Network.RequestId
    -- ^ Identifier of the network request to get content for.

    -> Encoding
    -- ^ The encoding to use.

    -> GetEncodedResponse
getEncodedResponse _0 _1 = GetEncodedResponse _0 _1 P.empty P.empty

