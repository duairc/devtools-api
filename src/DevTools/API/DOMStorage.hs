{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Query and modify DOM storage.
module DevTools.API.DOMStorage{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

    ( module DevTools.API.DOMStorage.Types
    , module DevTools.API.DOMStorage
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
import           DevTools.API.DOMStorage.Types


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
data Clear = Clear
    { storageId :: !StorageId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Clear where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "clear" $ \_o -> Clear
            <$> _o .: "storageId"
        ago = A.withArray "clear" $ \_a -> Clear
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON Clear where
    toEncoding (Clear _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "storageId" .= _0
        ]
    toJSON (Clear _0) = A.object $ P.catMaybes
        [ P.pure $ "storageId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup Clear where
    Clear _0 <> Clear _ = Clear _0


------------------------------------------------------------------------------
instance M.Method Clear where
    type Result Clear = ()
    name _ = "DOMStorage.clear"


------------------------------------------------------------------------------
clear
    :: StorageId
    -> Clear
clear _0 = Clear _0


------------------------------------------------------------------------------
-- | Disables storage tracking, prevents storage events from being sent to the client.
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
    name _ = "DOMStorage.disable"


------------------------------------------------------------------------------
-- | Disables storage tracking, prevents storage events from being sent to the client.
disable
    :: Disable
disable = Disable


------------------------------------------------------------------------------
-- | Enables storage tracking, storage events will now be delivered to the client.
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
    name _ = "DOMStorage.enable"


------------------------------------------------------------------------------
-- | Enables storage tracking, storage events will now be delivered to the client.
enable
    :: Enable
enable = Enable


------------------------------------------------------------------------------
data GetDOMStorageItems = GetDOMStorageItems
    { storageId :: !StorageId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetDOMStorageItems where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getDOMStorageItems" $ \_o -> GetDOMStorageItems
            <$> _o .: "storageId"
        ago = A.withArray "getDOMStorageItems" $ \_a -> GetDOMStorageItems
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetDOMStorageItems where
    toEncoding (GetDOMStorageItems _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "storageId" .= _0
        ]
    toJSON (GetDOMStorageItems _0) = A.object $ P.catMaybes
        [ P.pure $ "storageId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetDOMStorageItems where
    GetDOMStorageItems _0 <> GetDOMStorageItems _ = GetDOMStorageItems _0


------------------------------------------------------------------------------
data GetDOMStorageItemsResult = GetDOMStorageItemsResult
    { entries :: ![Item]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetDOMStorageItemsResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getDOMStorageItemsResult" $ \_o -> GetDOMStorageItemsResult
            <$> _o .: "entries"
        ago = A.withArray "getDOMStorageItemsResult" $ \_a -> GetDOMStorageItemsResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetDOMStorageItemsResult where
    toEncoding (GetDOMStorageItemsResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "entries" .= _0
        ]
    toJSON (GetDOMStorageItemsResult _0) = A.object $ P.catMaybes
        [ P.pure $ "entries" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetDOMStorageItemsResult where
    GetDOMStorageItemsResult _0 <> GetDOMStorageItemsResult _ = GetDOMStorageItemsResult _0


------------------------------------------------------------------------------
instance M.Method GetDOMStorageItems where
    type Result GetDOMStorageItems = GetDOMStorageItemsResult
    name _ = "DOMStorage.getDOMStorageItems"


------------------------------------------------------------------------------
getDOMStorageItems
    :: StorageId
    -> GetDOMStorageItems
getDOMStorageItems _0 = GetDOMStorageItems _0


------------------------------------------------------------------------------
data RemoveDOMStorageItem = RemoveDOMStorageItem
    { storageId :: !StorageId
    , key :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RemoveDOMStorageItem where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "removeDOMStorageItem" $ \_o -> RemoveDOMStorageItem
            <$> _o .: "storageId"
            <*> _o .: "key"
        ago = A.withArray "removeDOMStorageItem" $ \_a -> RemoveDOMStorageItem
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON RemoveDOMStorageItem where
    toEncoding (RemoveDOMStorageItem _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "storageId" .= _0
        , P.pure $ "key" .= _1
        ]
    toJSON (RemoveDOMStorageItem _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "storageId" .= _0
        , P.pure $ "key" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup RemoveDOMStorageItem where
    RemoveDOMStorageItem _0 _1 <> RemoveDOMStorageItem _ _ = RemoveDOMStorageItem _0 _1


------------------------------------------------------------------------------
instance M.Method RemoveDOMStorageItem where
    type Result RemoveDOMStorageItem = ()
    name _ = "DOMStorage.removeDOMStorageItem"


------------------------------------------------------------------------------
removeDOMStorageItem
    :: StorageId
    -> T.Text
    -> RemoveDOMStorageItem
removeDOMStorageItem _0 _1 = RemoveDOMStorageItem _0 _1


------------------------------------------------------------------------------
data SetDOMStorageItem = SetDOMStorageItem
    { storageId :: !StorageId
    , key :: !T.Text
    , value :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetDOMStorageItem where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setDOMStorageItem" $ \_o -> SetDOMStorageItem
            <$> _o .: "storageId"
            <*> _o .: "key"
            <*> _o .: "value"
        ago = A.withArray "setDOMStorageItem" $ \_a -> SetDOMStorageItem
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON SetDOMStorageItem where
    toEncoding (SetDOMStorageItem _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "storageId" .= _0
        , P.pure $ "key" .= _1
        , P.pure $ "value" .= _2
        ]
    toJSON (SetDOMStorageItem _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "storageId" .= _0
        , P.pure $ "key" .= _1
        , P.pure $ "value" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetDOMStorageItem where
    SetDOMStorageItem _0 _1 _2 <> SetDOMStorageItem _ _ _ = SetDOMStorageItem _0 _1 _2


------------------------------------------------------------------------------
instance M.Method SetDOMStorageItem where
    type Result SetDOMStorageItem = ()
    name _ = "DOMStorage.setDOMStorageItem"


------------------------------------------------------------------------------
setDOMStorageItem
    :: StorageId
    -> T.Text
    -> T.Text
    -> SetDOMStorageItem
setDOMStorageItem _0 _1 _2 = SetDOMStorageItem _0 _1 _2


------------------------------------------------------------------------------
data DomStorageItemAdded = DomStorageItemAdded
    { storageId :: !StorageId
    , key :: !T.Text
    , newValue :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DomStorageItemAdded where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "domStorageItemAdded" $ \_o -> DomStorageItemAdded
            <$> _o .: "storageId"
            <*> _o .: "key"
            <*> _o .: "newValue"
        ago = A.withArray "domStorageItemAdded" $ \_a -> DomStorageItemAdded
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON DomStorageItemAdded where
    toEncoding (DomStorageItemAdded _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "storageId" .= _0
        , P.pure $ "key" .= _1
        , P.pure $ "newValue" .= _2
        ]
    toJSON (DomStorageItemAdded _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "storageId" .= _0
        , P.pure $ "key" .= _1
        , P.pure $ "newValue" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup DomStorageItemAdded where
    DomStorageItemAdded _0 _1 _2 <> DomStorageItemAdded _ _ _ = DomStorageItemAdded _0 _1 _2


------------------------------------------------------------------------------
instance E.Event DomStorageItemAdded where
    type Result DomStorageItemAdded = DomStorageItemAdded
    name _ = "DOMStorage.domStorageItemAdded"


------------------------------------------------------------------------------
domStorageItemAdded :: P.Proxy DomStorageItemAdded
domStorageItemAdded = P.Proxy


------------------------------------------------------------------------------
data DomStorageItemRemoved = DomStorageItemRemoved
    { storageId :: !StorageId
    , key :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DomStorageItemRemoved where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "domStorageItemRemoved" $ \_o -> DomStorageItemRemoved
            <$> _o .: "storageId"
            <*> _o .: "key"
        ago = A.withArray "domStorageItemRemoved" $ \_a -> DomStorageItemRemoved
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON DomStorageItemRemoved where
    toEncoding (DomStorageItemRemoved _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "storageId" .= _0
        , P.pure $ "key" .= _1
        ]
    toJSON (DomStorageItemRemoved _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "storageId" .= _0
        , P.pure $ "key" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup DomStorageItemRemoved where
    DomStorageItemRemoved _0 _1 <> DomStorageItemRemoved _ _ = DomStorageItemRemoved _0 _1


------------------------------------------------------------------------------
instance E.Event DomStorageItemRemoved where
    type Result DomStorageItemRemoved = DomStorageItemRemoved
    name _ = "DOMStorage.domStorageItemRemoved"


------------------------------------------------------------------------------
domStorageItemRemoved :: P.Proxy DomStorageItemRemoved
domStorageItemRemoved = P.Proxy


------------------------------------------------------------------------------
data DomStorageItemUpdated = DomStorageItemUpdated
    { storageId :: !StorageId
    , key :: !T.Text
    , oldValue :: !T.Text
    , newValue :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DomStorageItemUpdated where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "domStorageItemUpdated" $ \_o -> DomStorageItemUpdated
            <$> _o .: "storageId"
            <*> _o .: "key"
            <*> _o .: "oldValue"
            <*> _o .: "newValue"
        ago = A.withArray "domStorageItemUpdated" $ \_a -> DomStorageItemUpdated
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON DomStorageItemUpdated where
    toEncoding (DomStorageItemUpdated _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "storageId" .= _0
        , P.pure $ "key" .= _1
        , P.pure $ "oldValue" .= _2
        , P.pure $ "newValue" .= _3
        ]
    toJSON (DomStorageItemUpdated _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "storageId" .= _0
        , P.pure $ "key" .= _1
        , P.pure $ "oldValue" .= _2
        , P.pure $ "newValue" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup DomStorageItemUpdated where
    DomStorageItemUpdated _0 _1 _2 _3 <> DomStorageItemUpdated _ _ _ _ = DomStorageItemUpdated _0 _1 _2 _3


------------------------------------------------------------------------------
instance E.Event DomStorageItemUpdated where
    type Result DomStorageItemUpdated = DomStorageItemUpdated
    name _ = "DOMStorage.domStorageItemUpdated"


------------------------------------------------------------------------------
domStorageItemUpdated :: P.Proxy DomStorageItemUpdated
domStorageItemUpdated = P.Proxy


------------------------------------------------------------------------------
data DomStorageItemsCleared = DomStorageItemsCleared
    { storageId :: !StorageId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DomStorageItemsCleared where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "domStorageItemsCleared" $ \_o -> DomStorageItemsCleared
            <$> _o .: "storageId"
        ago = A.withArray "domStorageItemsCleared" $ \_a -> DomStorageItemsCleared
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON DomStorageItemsCleared where
    toEncoding (DomStorageItemsCleared _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "storageId" .= _0
        ]
    toJSON (DomStorageItemsCleared _0) = A.object $ P.catMaybes
        [ P.pure $ "storageId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup DomStorageItemsCleared where
    DomStorageItemsCleared _0 <> DomStorageItemsCleared _ = DomStorageItemsCleared _0


------------------------------------------------------------------------------
instance E.Event DomStorageItemsCleared where
    type Result DomStorageItemsCleared = DomStorageItemsCleared
    name _ = "DOMStorage.domStorageItemsCleared"


------------------------------------------------------------------------------
domStorageItemsCleared :: P.Proxy DomStorageItemsCleared
domStorageItemsCleared = P.Proxy

