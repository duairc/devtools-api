{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.Database{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

    ( module DevTools.API.Database.Types
    , module DevTools.API.Database
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
import           DevTools.API.Database.Types


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Disables database tracking, prevents database events from being sent to the client.
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
    name _ = "Database.disable"


------------------------------------------------------------------------------
-- | Disables database tracking, prevents database events from being sent to the client.
disable
    :: Disable
disable = Disable


------------------------------------------------------------------------------
-- | Enables database tracking, database events will now be delivered to the client.
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
    name _ = "Database.enable"


------------------------------------------------------------------------------
-- | Enables database tracking, database events will now be delivered to the client.
enable
    :: Enable
enable = Enable


------------------------------------------------------------------------------
data ExecuteSQL = ExecuteSQL
    { databaseId :: !DatabaseId
    , query :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ExecuteSQL where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "executeSQL" $ \_o -> ExecuteSQL
            <$> _o .: "databaseId"
            <*> _o .: "query"
        ago = A.withArray "executeSQL" $ \_a -> ExecuteSQL
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON ExecuteSQL where
    toEncoding (ExecuteSQL _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "databaseId" .= _0
        , P.pure $ "query" .= _1
        ]
    toJSON (ExecuteSQL _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "databaseId" .= _0
        , P.pure $ "query" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup ExecuteSQL where
    ExecuteSQL _0 _1 <> ExecuteSQL _ _ = ExecuteSQL _0 _1


------------------------------------------------------------------------------
data ExecuteSQLResult = ExecuteSQLResult
    { columnNames :: !(P.Maybe [T.Text])
    , values :: !(P.Maybe [A.Value])
    , sqlError :: !(P.Maybe Error)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ExecuteSQLResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "executeSQLResult" $ \_o -> ExecuteSQLResult
            <$> _o .:? "columnNames"
            <*> _o .:? "values"
            <*> _o .:? "sqlError"
        ago = A.withArray "executeSQLResult" $ \_a -> ExecuteSQLResult
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON ExecuteSQLResult where
    toEncoding (ExecuteSQLResult _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ ("columnNames" .=) <$> _0
        , ("values" .=) <$> _1
        , ("sqlError" .=) <$> _2
        ]
    toJSON (ExecuteSQLResult _0 _1 _2) = A.object $ P.catMaybes
        [ ("columnNames" .=) <$> _0
        , ("values" .=) <$> _1
        , ("sqlError" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup ExecuteSQLResult where
    ExecuteSQLResult _0 _1 _2 <> ExecuteSQLResult __0 __1 __2 = ExecuteSQLResult (_0 <|> __0) (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
instance P.Monoid ExecuteSQLResult where
    mempty = ExecuteSQLResult P.empty P.empty P.empty


------------------------------------------------------------------------------
instance M.Method ExecuteSQL where
    type Result ExecuteSQL = ExecuteSQLResult
    name _ = "Database.executeSQL"


------------------------------------------------------------------------------
executeSQL
    :: DatabaseId
    -> T.Text
    -> ExecuteSQL
executeSQL _0 _1 = ExecuteSQL _0 _1


------------------------------------------------------------------------------
data GetDatabaseTableNames = GetDatabaseTableNames
    { databaseId :: !DatabaseId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetDatabaseTableNames where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getDatabaseTableNames" $ \_o -> GetDatabaseTableNames
            <$> _o .: "databaseId"
        ago = A.withArray "getDatabaseTableNames" $ \_a -> GetDatabaseTableNames
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetDatabaseTableNames where
    toEncoding (GetDatabaseTableNames _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "databaseId" .= _0
        ]
    toJSON (GetDatabaseTableNames _0) = A.object $ P.catMaybes
        [ P.pure $ "databaseId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetDatabaseTableNames where
    GetDatabaseTableNames _0 <> GetDatabaseTableNames _ = GetDatabaseTableNames _0


------------------------------------------------------------------------------
data GetDatabaseTableNamesResult = GetDatabaseTableNamesResult
    { tableNames :: ![T.Text]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetDatabaseTableNamesResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getDatabaseTableNamesResult" $ \_o -> GetDatabaseTableNamesResult
            <$> _o .: "tableNames"
        ago = A.withArray "getDatabaseTableNamesResult" $ \_a -> GetDatabaseTableNamesResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetDatabaseTableNamesResult where
    toEncoding (GetDatabaseTableNamesResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "tableNames" .= _0
        ]
    toJSON (GetDatabaseTableNamesResult _0) = A.object $ P.catMaybes
        [ P.pure $ "tableNames" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetDatabaseTableNamesResult where
    GetDatabaseTableNamesResult _0 <> GetDatabaseTableNamesResult _ = GetDatabaseTableNamesResult _0


------------------------------------------------------------------------------
instance M.Method GetDatabaseTableNames where
    type Result GetDatabaseTableNames = GetDatabaseTableNamesResult
    name _ = "Database.getDatabaseTableNames"


------------------------------------------------------------------------------
getDatabaseTableNames
    :: DatabaseId
    -> GetDatabaseTableNames
getDatabaseTableNames _0 = GetDatabaseTableNames _0


------------------------------------------------------------------------------
data AddDatabase = AddDatabase
    { database :: !Database
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AddDatabase where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "addDatabase" $ \_o -> AddDatabase
            <$> _o .: "database"
        ago = A.withArray "addDatabase" $ \_a -> AddDatabase
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON AddDatabase where
    toEncoding (AddDatabase _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "database" .= _0
        ]
    toJSON (AddDatabase _0) = A.object $ P.catMaybes
        [ P.pure $ "database" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup AddDatabase where
    AddDatabase _0 <> AddDatabase _ = AddDatabase _0


------------------------------------------------------------------------------
instance E.Event AddDatabase where
    type Result AddDatabase = AddDatabase
    name _ = "Database.addDatabase"


------------------------------------------------------------------------------
addDatabase :: P.Proxy AddDatabase
addDatabase = P.Proxy

