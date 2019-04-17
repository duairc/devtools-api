{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.IndexedDB.Types{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

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


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Database with an array of object stores.
data DatabaseWithObjectStores = DatabaseWithObjectStores
    { -- | Database name.
      name :: !T.Text
      -- | Database version (type is not 'integer', as the standard
      -- requires the version number to be 'unsigned long long')
    , version :: !P.Double
      -- | Object stores in this database.
    , objectStores :: ![ObjectStore]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DatabaseWithObjectStores where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "DatabaseWithObjectStores" $ \_o -> DatabaseWithObjectStores
            <$> _o .: "name"
            <*> _o .: "version"
            <*> _o .: "objectStores"
        ago = A.withArray "DatabaseWithObjectStores" $ \_a -> DatabaseWithObjectStores
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON DatabaseWithObjectStores where
    toEncoding (DatabaseWithObjectStores _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "version" .= _1
        , P.pure $ "objectStores" .= _2
        ]
    toJSON (DatabaseWithObjectStores _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "version" .= _1
        , P.pure $ "objectStores" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup DatabaseWithObjectStores where
    DatabaseWithObjectStores _0 _1 _2 <> DatabaseWithObjectStores _ _ _ = DatabaseWithObjectStores _0 _1 _2


------------------------------------------------------------------------------
-- | Object store.
data ObjectStore = ObjectStore
    { -- | Object store name.
      name :: !T.Text
      -- | Object store key path.
    , keyPath :: !KeyPath
      -- | If true, object store has auto increment flag set.
    , autoIncrement :: !P.Bool
      -- | Indexes in this object store.
    , indexes :: ![ObjectStoreIndex]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ObjectStore where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "ObjectStore" $ \_o -> ObjectStore
            <$> _o .: "name"
            <*> _o .: "keyPath"
            <*> _o .: "autoIncrement"
            <*> _o .: "indexes"
        ago = A.withArray "ObjectStore" $ \_a -> ObjectStore
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON ObjectStore where
    toEncoding (ObjectStore _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "keyPath" .= _1
        , P.pure $ "autoIncrement" .= _2
        , P.pure $ "indexes" .= _3
        ]
    toJSON (ObjectStore _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "keyPath" .= _1
        , P.pure $ "autoIncrement" .= _2
        , P.pure $ "indexes" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup ObjectStore where
    ObjectStore _0 _1 _2 _3 <> ObjectStore _ _ _ _ = ObjectStore _0 _1 _2 _3


------------------------------------------------------------------------------
-- | Object store index.
data ObjectStoreIndex = ObjectStoreIndex
    { -- | Index name.
      name :: !T.Text
      -- | Index key path.
    , keyPath :: !KeyPath
      -- | If true, index is unique.
    , unique :: !P.Bool
      -- | If true, index allows multiple entries for a key.
    , multiEntry :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ObjectStoreIndex where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "ObjectStoreIndex" $ \_o -> ObjectStoreIndex
            <$> _o .: "name"
            <*> _o .: "keyPath"
            <*> _o .: "unique"
            <*> _o .: "multiEntry"
        ago = A.withArray "ObjectStoreIndex" $ \_a -> ObjectStoreIndex
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON ObjectStoreIndex where
    toEncoding (ObjectStoreIndex _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "keyPath" .= _1
        , P.pure $ "unique" .= _2
        , P.pure $ "multiEntry" .= _3
        ]
    toJSON (ObjectStoreIndex _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "keyPath" .= _1
        , P.pure $ "unique" .= _2
        , P.pure $ "multiEntry" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup ObjectStoreIndex where
    ObjectStoreIndex _0 _1 _2 _3 <> ObjectStoreIndex _ _ _ _ = ObjectStoreIndex _0 _1 _2 _3


------------------------------------------------------------------------------
-- | Key.
data Key = Key
    { -- | Key type.
      type_ :: !Type
      -- | Number value.
    , number :: !(P.Maybe P.Double)
      -- | String value.
    , string :: !(P.Maybe T.Text)
      -- | Date value.
    , date :: !(P.Maybe P.Double)
      -- | Array value.
    , array :: !(P.Maybe [Key])
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Key where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "Key" $ \_o -> Key
            <$> _o .: "type"
            <*> _o .:? "number"
            <*> _o .:? "string"
            <*> _o .:? "date"
            <*> _o .:? "array"
        ago = A.withArray "Key" $ \_a -> Key
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON Key where
    toEncoding (Key _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "type" .= _0
        , ("number" .=) <$> _1
        , ("string" .=) <$> _2
        , ("date" .=) <$> _3
        , ("array" .=) <$> _4
        ]
    toJSON (Key _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ P.pure $ "type" .= _0
        , ("number" .=) <$> _1
        , ("string" .=) <$> _2
        , ("date" .=) <$> _3
        , ("array" .=) <$> _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup Key where
    Key _0 _1 _2 _3 _4 <> Key _ __1 __2 __3 __4 = Key _0 (_1 <|> __1) (_2 <|> __2) (_3 <|> __3) (_4 <|> __4)


------------------------------------------------------------------------------
data Type
    = Number
    | String
    | Date
    | Array
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Type where
    parseJSON = A.withText "Type" $ \t -> case t of
        "number" -> P.pure Number
        "string" -> P.pure String
        "date" -> P.pure Date
        "array" -> P.pure Array
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Type where
    toJSON Number = "number"
    toJSON String = "string"
    toJSON Date = "date"
    toJSON Array = "array"


------------------------------------------------------------------------------
-- | Key range.
data KeyRange = KeyRange
    { -- | Lower bound.
      lower :: !(P.Maybe Key)
      -- | Upper bound.
    , upper :: !(P.Maybe Key)
      -- | If true lower bound is open.
    , lowerOpen :: !P.Bool
      -- | If true upper bound is open.
    , upperOpen :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON KeyRange where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "KeyRange" $ \_o -> KeyRange
            <$> _o .:? "lower"
            <*> _o .:? "upper"
            <*> _o .: "lowerOpen"
            <*> _o .: "upperOpen"
        ago = A.withArray "KeyRange" $ \_a -> KeyRange
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON KeyRange where
    toEncoding (KeyRange _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ ("lower" .=) <$> _0
        , ("upper" .=) <$> _1
        , P.pure $ "lowerOpen" .= _2
        , P.pure $ "upperOpen" .= _3
        ]
    toJSON (KeyRange _0 _1 _2 _3) = A.object $ P.catMaybes
        [ ("lower" .=) <$> _0
        , ("upper" .=) <$> _1
        , P.pure $ "lowerOpen" .= _2
        , P.pure $ "upperOpen" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup KeyRange where
    KeyRange _0 _1 _2 _3 <> KeyRange __0 __1 _ _ = KeyRange (_0 <|> __0) (_1 <|> __1) _2 _3


------------------------------------------------------------------------------
-- | Data entry.
data DataEntry = DataEntry
    { -- | Key object.
      key :: !Runtime.RemoteObject
      -- | Primary key object.
    , primaryKey :: !Runtime.RemoteObject
      -- | Value object.
    , value :: !Runtime.RemoteObject
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DataEntry where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "DataEntry" $ \_o -> DataEntry
            <$> _o .: "key"
            <*> _o .: "primaryKey"
            <*> _o .: "value"
        ago = A.withArray "DataEntry" $ \_a -> DataEntry
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON DataEntry where
    toEncoding (DataEntry _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "key" .= _0
        , P.pure $ "primaryKey" .= _1
        , P.pure $ "value" .= _2
        ]
    toJSON (DataEntry _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "key" .= _0
        , P.pure $ "primaryKey" .= _1
        , P.pure $ "value" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup DataEntry where
    DataEntry _0 _1 _2 <> DataEntry _ _ _ = DataEntry _0 _1 _2


------------------------------------------------------------------------------
-- | Key path.
data KeyPath = KeyPath
    { -- | Key path type.
      type_ :: !Type_
      -- | String value.
    , string :: !(P.Maybe T.Text)
      -- | Array value.
    , array :: !(P.Maybe [T.Text])
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON KeyPath where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "KeyPath" $ \_o -> KeyPath
            <$> _o .: "type"
            <*> _o .:? "string"
            <*> _o .:? "array"
        ago = A.withArray "KeyPath" $ \_a -> KeyPath
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON KeyPath where
    toEncoding (KeyPath _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "type" .= _0
        , ("string" .=) <$> _1
        , ("array" .=) <$> _2
        ]
    toJSON (KeyPath _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "type" .= _0
        , ("string" .=) <$> _1
        , ("array" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup KeyPath where
    KeyPath _0 _1 _2 <> KeyPath _ __1 __2 = KeyPath _0 (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
data Type_
    = Null
    | String_
    | Array_
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Type_ where
    parseJSON = A.withText "Type" $ \t -> case t of
        "null" -> P.pure Null
        "string" -> P.pure String_
        "array" -> P.pure Array_
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Type_ where
    toJSON Null = "null"
    toJSON String_ = "string"
    toJSON Array_ = "array"

