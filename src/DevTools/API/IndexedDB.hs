{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.IndexedDB{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

    ( module DevTools.API.IndexedDB.Types
    , module DevTools.API.IndexedDB
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
import           DevTools.API.IndexedDB.Types


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Clears all entries from an object store.
data ClearObjectStore = ClearObjectStore
    { -- | Security origin.
      securityOrigin :: !T.Text
      -- | Database name.
    , databaseName :: !T.Text
      -- | Object store name.
    , objectStoreName :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ClearObjectStore where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "clearObjectStore" $ \_o -> ClearObjectStore
            <$> _o .: "securityOrigin"
            <*> _o .: "databaseName"
            <*> _o .: "objectStoreName"
        ago = A.withArray "clearObjectStore" $ \_a -> ClearObjectStore
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON ClearObjectStore where
    toEncoding (ClearObjectStore _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "securityOrigin" .= _0
        , P.pure $ "databaseName" .= _1
        , P.pure $ "objectStoreName" .= _2
        ]
    toJSON (ClearObjectStore _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "securityOrigin" .= _0
        , P.pure $ "databaseName" .= _1
        , P.pure $ "objectStoreName" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup ClearObjectStore where
    ClearObjectStore _0 _1 _2 <> ClearObjectStore _ _ _ = ClearObjectStore _0 _1 _2


------------------------------------------------------------------------------
instance M.Method ClearObjectStore where
    type Result ClearObjectStore = ()
    name _ = "IndexedDB.clearObjectStore"


------------------------------------------------------------------------------
-- | Clears all entries from an object store.
clearObjectStore
    :: T.Text
    -- ^ Security origin.

    -> T.Text
    -- ^ Database name.

    -> T.Text
    -- ^ Object store name.

    -> ClearObjectStore
clearObjectStore _0 _1 _2 = ClearObjectStore _0 _1 _2


------------------------------------------------------------------------------
-- | Deletes a database.
data DeleteDatabase = DeleteDatabase
    { -- | Security origin.
      securityOrigin :: !T.Text
      -- | Database name.
    , databaseName :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DeleteDatabase where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "deleteDatabase" $ \_o -> DeleteDatabase
            <$> _o .: "securityOrigin"
            <*> _o .: "databaseName"
        ago = A.withArray "deleteDatabase" $ \_a -> DeleteDatabase
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON DeleteDatabase where
    toEncoding (DeleteDatabase _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "securityOrigin" .= _0
        , P.pure $ "databaseName" .= _1
        ]
    toJSON (DeleteDatabase _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "securityOrigin" .= _0
        , P.pure $ "databaseName" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup DeleteDatabase where
    DeleteDatabase _0 _1 <> DeleteDatabase _ _ = DeleteDatabase _0 _1


------------------------------------------------------------------------------
instance M.Method DeleteDatabase where
    type Result DeleteDatabase = ()
    name _ = "IndexedDB.deleteDatabase"


------------------------------------------------------------------------------
-- | Deletes a database.
deleteDatabase
    :: T.Text
    -- ^ Security origin.

    -> T.Text
    -- ^ Database name.

    -> DeleteDatabase
deleteDatabase _0 _1 = DeleteDatabase _0 _1


------------------------------------------------------------------------------
-- | Delete a range of entries from an object store
data DeleteObjectStoreEntries = DeleteObjectStoreEntries
    { securityOrigin :: !T.Text
    , databaseName :: !T.Text
    , objectStoreName :: !T.Text
      -- | Range of entry keys to delete
    , keyRange :: !KeyRange
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DeleteObjectStoreEntries where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "deleteObjectStoreEntries" $ \_o -> DeleteObjectStoreEntries
            <$> _o .: "securityOrigin"
            <*> _o .: "databaseName"
            <*> _o .: "objectStoreName"
            <*> _o .: "keyRange"
        ago = A.withArray "deleteObjectStoreEntries" $ \_a -> DeleteObjectStoreEntries
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON DeleteObjectStoreEntries where
    toEncoding (DeleteObjectStoreEntries _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "securityOrigin" .= _0
        , P.pure $ "databaseName" .= _1
        , P.pure $ "objectStoreName" .= _2
        , P.pure $ "keyRange" .= _3
        ]
    toJSON (DeleteObjectStoreEntries _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "securityOrigin" .= _0
        , P.pure $ "databaseName" .= _1
        , P.pure $ "objectStoreName" .= _2
        , P.pure $ "keyRange" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup DeleteObjectStoreEntries where
    DeleteObjectStoreEntries _0 _1 _2 _3 <> DeleteObjectStoreEntries _ _ _ _ = DeleteObjectStoreEntries _0 _1 _2 _3


------------------------------------------------------------------------------
instance M.Method DeleteObjectStoreEntries where
    type Result DeleteObjectStoreEntries = ()
    name _ = "IndexedDB.deleteObjectStoreEntries"


------------------------------------------------------------------------------
-- | Delete a range of entries from an object store
deleteObjectStoreEntries
    :: T.Text
    -> T.Text
    -> T.Text
    -> KeyRange
    -- ^ Range of entry keys to delete

    -> DeleteObjectStoreEntries
deleteObjectStoreEntries _0 _1 _2 _3 = DeleteObjectStoreEntries _0 _1 _2 _3


------------------------------------------------------------------------------
-- | Disables events from backend.
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
    name _ = "IndexedDB.disable"


------------------------------------------------------------------------------
-- | Disables events from backend.
disable
    :: Disable
disable = Disable


------------------------------------------------------------------------------
-- | Enables events from backend.
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
    name _ = "IndexedDB.enable"


------------------------------------------------------------------------------
-- | Enables events from backend.
enable
    :: Enable
enable = Enable


------------------------------------------------------------------------------
-- | Requests data from object store or index.
data RequestData = RequestData
    { -- | Security origin.
      securityOrigin :: !T.Text
      -- | Database name.
    , databaseName :: !T.Text
      -- | Object store name.
    , objectStoreName :: !T.Text
      -- | Index name, empty string for object store data requests.
    , indexName :: !T.Text
      -- | Number of records to skip.
    , skipCount :: !P.Int
      -- | Number of records to fetch.
    , pageSize :: !P.Int
      -- | Key range.
    , keyRange :: !(P.Maybe KeyRange)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RequestData where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "requestData" $ \_o -> RequestData
            <$> _o .: "securityOrigin"
            <*> _o .: "databaseName"
            <*> _o .: "objectStoreName"
            <*> _o .: "indexName"
            <*> _o .: "skipCount"
            <*> _o .: "pageSize"
            <*> _o .:? "keyRange"
        ago = A.withArray "requestData" $ \_a -> RequestData
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)


------------------------------------------------------------------------------
instance A.ToJSON RequestData where
    toEncoding (RequestData _0 _1 _2 _3 _4 _5 _6) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "securityOrigin" .= _0
        , P.pure $ "databaseName" .= _1
        , P.pure $ "objectStoreName" .= _2
        , P.pure $ "indexName" .= _3
        , P.pure $ "skipCount" .= _4
        , P.pure $ "pageSize" .= _5
        , ("keyRange" .=) <$> _6
        ]
    toJSON (RequestData _0 _1 _2 _3 _4 _5 _6) = A.object $ P.catMaybes
        [ P.pure $ "securityOrigin" .= _0
        , P.pure $ "databaseName" .= _1
        , P.pure $ "objectStoreName" .= _2
        , P.pure $ "indexName" .= _3
        , P.pure $ "skipCount" .= _4
        , P.pure $ "pageSize" .= _5
        , ("keyRange" .=) <$> _6
        ]


------------------------------------------------------------------------------
instance P.Semigroup RequestData where
    RequestData _0 _1 _2 _3 _4 _5 _6 <> RequestData _ _ _ _ _ _ __6 = RequestData _0 _1 _2 _3 _4 _5 (_6 <|> __6)


------------------------------------------------------------------------------
-- | Requests data from object store or index.
data RequestDataResult = RequestDataResult
    { -- | Array of object store data entries.
      objectStoreDataEntries :: ![DataEntry]
      -- | If true, there are more entries to fetch in the given range.
    , hasMore :: !P.Bool
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RequestDataResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "requestDataResult" $ \_o -> RequestDataResult
            <$> _o .: "objectStoreDataEntries"
            <*> _o .: "hasMore"
        ago = A.withArray "requestDataResult" $ \_a -> RequestDataResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON RequestDataResult where
    toEncoding (RequestDataResult _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "objectStoreDataEntries" .= _0
        , P.pure $ "hasMore" .= _1
        ]
    toJSON (RequestDataResult _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "objectStoreDataEntries" .= _0
        , P.pure $ "hasMore" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup RequestDataResult where
    RequestDataResult _0 _1 <> RequestDataResult _ _ = RequestDataResult _0 _1


------------------------------------------------------------------------------
instance M.Method RequestData where
    type Result RequestData = RequestDataResult
    name _ = "IndexedDB.requestData"


------------------------------------------------------------------------------
-- | Requests data from object store or index.
requestData
    :: T.Text
    -- ^ Security origin.

    -> T.Text
    -- ^ Database name.

    -> T.Text
    -- ^ Object store name.

    -> T.Text
    -- ^ Index name, empty string for object store data requests.

    -> P.Int
    -- ^ Number of records to skip.

    -> P.Int
    -- ^ Number of records to fetch.

    -> RequestData
requestData _0 _1 _2 _3 _4 _5 = RequestData _0 _1 _2 _3 _4 _5 P.empty


------------------------------------------------------------------------------
-- | Gets metadata of an object store
data GetMetadata = GetMetadata
    { -- | Security origin.
      securityOrigin :: !T.Text
      -- | Database name.
    , databaseName :: !T.Text
      -- | Object store name.
    , objectStoreName :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetMetadata where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getMetadata" $ \_o -> GetMetadata
            <$> _o .: "securityOrigin"
            <*> _o .: "databaseName"
            <*> _o .: "objectStoreName"
        ago = A.withArray "getMetadata" $ \_a -> GetMetadata
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON GetMetadata where
    toEncoding (GetMetadata _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "securityOrigin" .= _0
        , P.pure $ "databaseName" .= _1
        , P.pure $ "objectStoreName" .= _2
        ]
    toJSON (GetMetadata _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "securityOrigin" .= _0
        , P.pure $ "databaseName" .= _1
        , P.pure $ "objectStoreName" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetMetadata where
    GetMetadata _0 _1 _2 <> GetMetadata _ _ _ = GetMetadata _0 _1 _2


------------------------------------------------------------------------------
-- | Gets metadata of an object store
data GetMetadataResult = GetMetadataResult
    { -- | the entries count
      entriesCount :: !P.Double
      -- | the current value of key generator, to become the next inserted
      -- key into the object store. Valid if objectStore.autoIncrement
      -- is true.
    , keyGeneratorValue :: !P.Double
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetMetadataResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getMetadataResult" $ \_o -> GetMetadataResult
            <$> _o .: "entriesCount"
            <*> _o .: "keyGeneratorValue"
        ago = A.withArray "getMetadataResult" $ \_a -> GetMetadataResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON GetMetadataResult where
    toEncoding (GetMetadataResult _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "entriesCount" .= _0
        , P.pure $ "keyGeneratorValue" .= _1
        ]
    toJSON (GetMetadataResult _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "entriesCount" .= _0
        , P.pure $ "keyGeneratorValue" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetMetadataResult where
    GetMetadataResult _0 _1 <> GetMetadataResult _ _ = GetMetadataResult _0 _1


------------------------------------------------------------------------------
instance M.Method GetMetadata where
    type Result GetMetadata = GetMetadataResult
    name _ = "IndexedDB.getMetadata"


------------------------------------------------------------------------------
-- | Gets metadata of an object store
getMetadata
    :: T.Text
    -- ^ Security origin.

    -> T.Text
    -- ^ Database name.

    -> T.Text
    -- ^ Object store name.

    -> GetMetadata
getMetadata _0 _1 _2 = GetMetadata _0 _1 _2


------------------------------------------------------------------------------
-- | Requests database with given name in given frame.
data RequestDatabase = RequestDatabase
    { -- | Security origin.
      securityOrigin :: !T.Text
      -- | Database name.
    , databaseName :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RequestDatabase where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "requestDatabase" $ \_o -> RequestDatabase
            <$> _o .: "securityOrigin"
            <*> _o .: "databaseName"
        ago = A.withArray "requestDatabase" $ \_a -> RequestDatabase
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON RequestDatabase where
    toEncoding (RequestDatabase _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "securityOrigin" .= _0
        , P.pure $ "databaseName" .= _1
        ]
    toJSON (RequestDatabase _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "securityOrigin" .= _0
        , P.pure $ "databaseName" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup RequestDatabase where
    RequestDatabase _0 _1 <> RequestDatabase _ _ = RequestDatabase _0 _1


------------------------------------------------------------------------------
-- | Requests database with given name in given frame.
data RequestDatabaseResult = RequestDatabaseResult
    { -- | Database with an array of object stores.
      databaseWithObjectStores :: !DatabaseWithObjectStores
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RequestDatabaseResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "requestDatabaseResult" $ \_o -> RequestDatabaseResult
            <$> _o .: "databaseWithObjectStores"
        ago = A.withArray "requestDatabaseResult" $ \_a -> RequestDatabaseResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON RequestDatabaseResult where
    toEncoding (RequestDatabaseResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "databaseWithObjectStores" .= _0
        ]
    toJSON (RequestDatabaseResult _0) = A.object $ P.catMaybes
        [ P.pure $ "databaseWithObjectStores" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup RequestDatabaseResult where
    RequestDatabaseResult _0 <> RequestDatabaseResult _ = RequestDatabaseResult _0


------------------------------------------------------------------------------
instance M.Method RequestDatabase where
    type Result RequestDatabase = RequestDatabaseResult
    name _ = "IndexedDB.requestDatabase"


------------------------------------------------------------------------------
-- | Requests database with given name in given frame.
requestDatabase
    :: T.Text
    -- ^ Security origin.

    -> T.Text
    -- ^ Database name.

    -> RequestDatabase
requestDatabase _0 _1 = RequestDatabase _0 _1


------------------------------------------------------------------------------
-- | Requests database names for given security origin.
data RequestDatabaseNames = RequestDatabaseNames
    { -- | Security origin.
      securityOrigin :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RequestDatabaseNames where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "requestDatabaseNames" $ \_o -> RequestDatabaseNames
            <$> _o .: "securityOrigin"
        ago = A.withArray "requestDatabaseNames" $ \_a -> RequestDatabaseNames
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON RequestDatabaseNames where
    toEncoding (RequestDatabaseNames _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "securityOrigin" .= _0
        ]
    toJSON (RequestDatabaseNames _0) = A.object $ P.catMaybes
        [ P.pure $ "securityOrigin" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup RequestDatabaseNames where
    RequestDatabaseNames _0 <> RequestDatabaseNames _ = RequestDatabaseNames _0


------------------------------------------------------------------------------
-- | Requests database names for given security origin.
data RequestDatabaseNamesResult = RequestDatabaseNamesResult
    { -- | Database names for origin.
      databaseNames :: ![T.Text]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RequestDatabaseNamesResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "requestDatabaseNamesResult" $ \_o -> RequestDatabaseNamesResult
            <$> _o .: "databaseNames"
        ago = A.withArray "requestDatabaseNamesResult" $ \_a -> RequestDatabaseNamesResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON RequestDatabaseNamesResult where
    toEncoding (RequestDatabaseNamesResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "databaseNames" .= _0
        ]
    toJSON (RequestDatabaseNamesResult _0) = A.object $ P.catMaybes
        [ P.pure $ "databaseNames" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup RequestDatabaseNamesResult where
    RequestDatabaseNamesResult _0 <> RequestDatabaseNamesResult _ = RequestDatabaseNamesResult _0


------------------------------------------------------------------------------
instance M.Method RequestDatabaseNames where
    type Result RequestDatabaseNames = RequestDatabaseNamesResult
    name _ = "IndexedDB.requestDatabaseNames"


------------------------------------------------------------------------------
-- | Requests database names for given security origin.
requestDatabaseNames
    :: T.Text
    -- ^ Security origin.

    -> RequestDatabaseNames
requestDatabaseNames _0 = RequestDatabaseNames _0

