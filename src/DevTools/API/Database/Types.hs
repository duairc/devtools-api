{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.Database.Types{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

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
-- | Unique identifier of Database object.
type DatabaseId = T.Text


------------------------------------------------------------------------------
-- | Database object.
data Database = Database
    { -- | Database ID.
      id :: !DatabaseId
      -- | Database domain.
    , domain :: !T.Text
      -- | Database name.
    , name :: !T.Text
      -- | Database version.
    , version :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Database where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "Database" $ \_o -> Database
            <$> _o .: "id"
            <*> _o .: "domain"
            <*> _o .: "name"
            <*> _o .: "version"
        ago = A.withArray "Database" $ \_a -> Database
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON Database where
    toEncoding (Database _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "id" .= _0
        , P.pure $ "domain" .= _1
        , P.pure $ "name" .= _2
        , P.pure $ "version" .= _3
        ]
    toJSON (Database _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "id" .= _0
        , P.pure $ "domain" .= _1
        , P.pure $ "name" .= _2
        , P.pure $ "version" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup Database where
    Database _0 _1 _2 _3 <> Database _ _ _ _ = Database _0 _1 _2 _3


------------------------------------------------------------------------------
-- | Database error.
data Error = Error
    { -- | Error message.
      message :: !T.Text
      -- | Error code.
    , code :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Error where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "Error" $ \_o -> Error
            <$> _o .: "message"
            <*> _o .: "code"
        ago = A.withArray "Error" $ \_a -> Error
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON Error where
    toEncoding (Error _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "message" .= _0
        , P.pure $ "code" .= _1
        ]
    toJSON (Error _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "message" .= _0
        , P.pure $ "code" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup Error where
    Error _0 _1 <> Error _ _ = Error _0 _1

