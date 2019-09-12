{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | A domain for interacting with Cast, Presentation API, and Remote Playback API
-- functionalities.
module DevTools.API.Cast.Types{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

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
data Sink = Sink
    { name :: !T.Text
    , id :: !T.Text
      -- | Text describing the current session. Present only if there is an active
      -- session on the sink.
    , session :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Sink where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "Sink" $ \_o -> Sink
            <$> _o .: "name"
            <*> _o .: "id"
            <*> _o .:? "session"
        ago = A.withArray "Sink" $ \_a -> Sink
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON Sink where
    toEncoding (Sink _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "id" .= _1
        , ("session" .=) <$> _2
        ]
    toJSON (Sink _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "id" .= _1
        , ("session" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup Sink where
    Sink _0 _1 _2 <> Sink _ _ __2 = Sink _0 _1 (_2 <|> __2)

