{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | This domain provides experimental commands only supported in headless mode.
module DevTools.API.HeadlessExperimental.Types{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

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
-- | Encoding options for a screenshot.
data ScreenshotParams = ScreenshotParams
    { -- | Image compression format (defaults to png).
      format :: !(P.Maybe Format)
      -- | Compression quality from range [0..100] (jpeg only).
    , quality :: !(P.Maybe P.Int)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ScreenshotParams where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "ScreenshotParams" $ \_o -> ScreenshotParams
            <$> _o .:? "format"
            <*> _o .:? "quality"
        ago = A.withArray "ScreenshotParams" $ \_a -> ScreenshotParams
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON ScreenshotParams where
    toEncoding (ScreenshotParams _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ ("format" .=) <$> _0
        , ("quality" .=) <$> _1
        ]
    toJSON (ScreenshotParams _0 _1) = A.object $ P.catMaybes
        [ ("format" .=) <$> _0
        , ("quality" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup ScreenshotParams where
    ScreenshotParams _0 _1 <> ScreenshotParams __0 __1 = ScreenshotParams (_0 <|> __0) (_1 <|> __1)


------------------------------------------------------------------------------
instance P.Monoid ScreenshotParams where
    mempty = ScreenshotParams P.empty P.empty


------------------------------------------------------------------------------
data Format
    = Jpeg
    | Png
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Format where
    parseJSON = A.withText "Format" $ \t -> case t of
        "jpeg" -> P.pure Jpeg
        "png" -> P.pure Png
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Format where
    toJSON Jpeg = "jpeg"
    toJSON Png = "png"

