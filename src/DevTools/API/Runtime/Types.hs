{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Runtime domain exposes JavaScript runtime by means of remote evaluation and mirror objects.
-- Evaluation results are returned as mirror object that expose object type, string representation
-- and unique identifier that can be used for further object reference. Original objects are
-- maintained in memory unless they are either explicitly released or are released along with the
-- other objects in their object group.
module DevTools.API.Runtime.Types
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
-- | Unique script identifier.
type ScriptId = T.Text


------------------------------------------------------------------------------
-- | Unique object identifier.
type RemoteObjectId = T.Text


------------------------------------------------------------------------------
-- | Primitive value which cannot be JSON-stringified. Includes values @-0@, @NaN@, @Infinity@,
-- @-Infinity@, and bigint literals.
type UnserializableValue = T.Text


------------------------------------------------------------------------------
-- | Mirror object referencing original JavaScript object.
{-# WARNING preview, customPreview "This feature is marked as EXPERIMENTAL." #-}
data RemoteObject = RemoteObject
    { -- | Object type.
      type_ :: !Type
      -- | Object subtype hint. Specified for @object@ type values only.
    , subtype :: !(P.Maybe Subtype)
      -- | Object class (constructor) name. Specified for @object@ type values only.
    , className :: !(P.Maybe T.Text)
      -- | Remote object value in case of primitive values or JSON values (if it was requested).
    , value :: !(P.Maybe A.Value)
      -- | Primitive value which can not be JSON-stringified does not have @value@, but gets this
      -- property.
    , unserializableValue :: !(P.Maybe UnserializableValue)
      -- | String representation of the object.
    , description :: !(P.Maybe T.Text)
      -- | Unique object identifier (for non-primitive values).
    , objectId :: !(P.Maybe RemoteObjectId)
      -- | Preview containing abbreviated property values. Specified for @object@ type values only.
    , preview :: !(P.Maybe ObjectPreview)
    , customPreview :: !(P.Maybe CustomPreview)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RemoteObject where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "RemoteObject" $ \_o -> RemoteObject
            <$> _o .: "type"
            <*> _o .:? "subtype"
            <*> _o .:? "className"
            <*> _o .:? "value"
            <*> _o .:? "unserializableValue"
            <*> _o .:? "description"
            <*> _o .:? "objectId"
            <*> _o .:? "preview"
            <*> _o .:? "customPreview"
        ago = A.withArray "RemoteObject" $ \_a -> RemoteObject
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)
            <*> P.traverse A.parseJSON (_a !? 7)
            <*> P.traverse A.parseJSON (_a !? 8)


------------------------------------------------------------------------------
instance A.ToJSON RemoteObject where
    toEncoding (RemoteObject _0 _1 _2 _3 _4 _5 _6 _7 _8) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "type" .= _0
        , ("subtype" .=) <$> _1
        , ("className" .=) <$> _2
        , ("value" .=) <$> _3
        , ("unserializableValue" .=) <$> _4
        , ("description" .=) <$> _5
        , ("objectId" .=) <$> _6
        , ("preview" .=) <$> _7
        , ("customPreview" .=) <$> _8
        ]
    toJSON (RemoteObject _0 _1 _2 _3 _4 _5 _6 _7 _8) = A.object $ P.catMaybes
        [ P.pure $ "type" .= _0
        , ("subtype" .=) <$> _1
        , ("className" .=) <$> _2
        , ("value" .=) <$> _3
        , ("unserializableValue" .=) <$> _4
        , ("description" .=) <$> _5
        , ("objectId" .=) <$> _6
        , ("preview" .=) <$> _7
        , ("customPreview" .=) <$> _8
        ]


------------------------------------------------------------------------------
instance P.Semigroup RemoteObject where
    RemoteObject _0 _1 _2 _3 _4 _5 _6 _7 _8 <> RemoteObject _ __1 __2 __3 __4 __5 __6 __7 __8 = RemoteObject _0 (_1 <|> __1) (_2 <|> __2) (_3 <|> __3) (_4 <|> __4) (_5 <|> __5) (_6 <|> __6) (_7 <|> __7) (_8 <|> __8)


------------------------------------------------------------------------------
data Type
    = Object
    | Function
    | Undefined
    | String
    | Number
    | Boolean
    | Symbol
    | Bigint
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Type where
    parseJSON = A.withText "Type" $ \t -> case t of
        "object" -> P.pure Object
        "function" -> P.pure Function
        "undefined" -> P.pure Undefined
        "string" -> P.pure String
        "number" -> P.pure Number
        "boolean" -> P.pure Boolean
        "symbol" -> P.pure Symbol
        "bigint" -> P.pure Bigint
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Type where
    toJSON Object = "object"
    toJSON Function = "function"
    toJSON Undefined = "undefined"
    toJSON String = "string"
    toJSON Number = "number"
    toJSON Boolean = "boolean"
    toJSON Symbol = "symbol"
    toJSON Bigint = "bigint"


------------------------------------------------------------------------------
data Subtype
    = Array
    | Null
    | Node
    | Regexp
    | Date
    | Map
    | Set
    | Weakmap
    | Weakset
    | Iterator
    | Generator
    | Error
    | Proxy
    | Promise
    | Typedarray
    | Arraybuffer
    | Dataview
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Subtype where
    parseJSON = A.withText "Subtype" $ \t -> case t of
        "array" -> P.pure Array
        "null" -> P.pure Null
        "node" -> P.pure Node
        "regexp" -> P.pure Regexp
        "date" -> P.pure Date
        "map" -> P.pure Map
        "set" -> P.pure Set
        "weakmap" -> P.pure Weakmap
        "weakset" -> P.pure Weakset
        "iterator" -> P.pure Iterator
        "generator" -> P.pure Generator
        "error" -> P.pure Error
        "proxy" -> P.pure Proxy
        "promise" -> P.pure Promise
        "typedarray" -> P.pure Typedarray
        "arraybuffer" -> P.pure Arraybuffer
        "dataview" -> P.pure Dataview
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Subtype where
    toJSON Array = "array"
    toJSON Null = "null"
    toJSON Node = "node"
    toJSON Regexp = "regexp"
    toJSON Date = "date"
    toJSON Map = "map"
    toJSON Set = "set"
    toJSON Weakmap = "weakmap"
    toJSON Weakset = "weakset"
    toJSON Iterator = "iterator"
    toJSON Generator = "generator"
    toJSON Error = "error"
    toJSON Proxy = "proxy"
    toJSON Promise = "promise"
    toJSON Typedarray = "typedarray"
    toJSON Arraybuffer = "arraybuffer"
    toJSON Dataview = "dataview"


------------------------------------------------------------------------------
{-# WARNING CustomPreview "This feature is marked as EXPERIMENTAL." #-}
data CustomPreview = CustomPreview
    { -- | The JSON-stringified result of formatter.header(object, config) call.
      -- It contains json ML array that represents RemoteObject.
      header :: !T.Text
      -- | If formatter returns true as a result of formatter.hasBody call then bodyGetterId will
      -- contain RemoteObjectId for the function that returns result of formatter.body(object, config) call.
      -- The result value is json ML array.
    , bodyGetterId :: !(P.Maybe RemoteObjectId)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CustomPreview where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "CustomPreview" $ \_o -> CustomPreview
            <$> _o .: "header"
            <*> _o .:? "bodyGetterId"
        ago = A.withArray "CustomPreview" $ \_a -> CustomPreview
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON CustomPreview where
    toEncoding (CustomPreview _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "header" .= _0
        , ("bodyGetterId" .=) <$> _1
        ]
    toJSON (CustomPreview _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "header" .= _0
        , ("bodyGetterId" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup CustomPreview where
    CustomPreview _0 _1 <> CustomPreview _ __1 = CustomPreview _0 (_1 <|> __1)


------------------------------------------------------------------------------
-- | Object containing abbreviated remote object value.
{-# WARNING ObjectPreview "This feature is marked as EXPERIMENTAL." #-}
data ObjectPreview = ObjectPreview
    { -- | Object type.
      type_ :: !Type
      -- | Object subtype hint. Specified for @object@ type values only.
    , subtype :: !(P.Maybe Subtype_)
      -- | String representation of the object.
    , description :: !(P.Maybe T.Text)
      -- | True iff some of the properties or entries of the original object did not fit.
    , overflow :: !P.Bool
      -- | List of the properties.
    , properties :: ![PropertyPreview]
      -- | List of the entries. Specified for @map@ and @set@ subtype values only.
    , entries :: !(P.Maybe [EntryPreview])
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ObjectPreview where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "ObjectPreview" $ \_o -> ObjectPreview
            <$> _o .: "type"
            <*> _o .:? "subtype"
            <*> _o .:? "description"
            <*> _o .: "overflow"
            <*> _o .: "properties"
            <*> _o .:? "entries"
        ago = A.withArray "ObjectPreview" $ \_a -> ObjectPreview
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)


------------------------------------------------------------------------------
instance A.ToJSON ObjectPreview where
    toEncoding (ObjectPreview _0 _1 _2 _3 _4 _5) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "type" .= _0
        , ("subtype" .=) <$> _1
        , ("description" .=) <$> _2
        , P.pure $ "overflow" .= _3
        , P.pure $ "properties" .= _4
        , ("entries" .=) <$> _5
        ]
    toJSON (ObjectPreview _0 _1 _2 _3 _4 _5) = A.object $ P.catMaybes
        [ P.pure $ "type" .= _0
        , ("subtype" .=) <$> _1
        , ("description" .=) <$> _2
        , P.pure $ "overflow" .= _3
        , P.pure $ "properties" .= _4
        , ("entries" .=) <$> _5
        ]


------------------------------------------------------------------------------
instance P.Semigroup ObjectPreview where
    ObjectPreview _0 _1 _2 _3 _4 _5 <> ObjectPreview _ __1 __2 _ _ __5 = ObjectPreview _0 (_1 <|> __1) (_2 <|> __2) _3 _4 (_5 <|> __5)


------------------------------------------------------------------------------
data Subtype_
    = Array_
    | Null_
    | Node_
    | Regexp_
    | Date_
    | Map_
    | Set_
    | Weakmap_
    | Weakset_
    | Iterator_
    | Generator_
    | Error_
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Subtype_ where
    parseJSON = A.withText "Subtype" $ \t -> case t of
        "array" -> P.pure Array_
        "null" -> P.pure Null_
        "node" -> P.pure Node_
        "regexp" -> P.pure Regexp_
        "date" -> P.pure Date_
        "map" -> P.pure Map_
        "set" -> P.pure Set_
        "weakmap" -> P.pure Weakmap_
        "weakset" -> P.pure Weakset_
        "iterator" -> P.pure Iterator_
        "generator" -> P.pure Generator_
        "error" -> P.pure Error_
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Subtype_ where
    toJSON Array_ = "array"
    toJSON Null_ = "null"
    toJSON Node_ = "node"
    toJSON Regexp_ = "regexp"
    toJSON Date_ = "date"
    toJSON Map_ = "map"
    toJSON Set_ = "set"
    toJSON Weakmap_ = "weakmap"
    toJSON Weakset_ = "weakset"
    toJSON Iterator_ = "iterator"
    toJSON Generator_ = "generator"
    toJSON Error_ = "error"


------------------------------------------------------------------------------
{-# WARNING PropertyPreview "This feature is marked as EXPERIMENTAL." #-}
data PropertyPreview = PropertyPreview
    { -- | Property name.
      name :: !T.Text
      -- | Object type. Accessor means that the property itself is an accessor property.
    , type_ :: !Type_
      -- | User-friendly property value string.
    , value :: !(P.Maybe T.Text)
      -- | Nested value preview.
    , valuePreview :: !(P.Maybe ObjectPreview)
      -- | Object subtype hint. Specified for @object@ type values only.
    , subtype :: !(P.Maybe Subtype_)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PropertyPreview where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "PropertyPreview" $ \_o -> PropertyPreview
            <$> _o .: "name"
            <*> _o .: "type"
            <*> _o .:? "value"
            <*> _o .:? "valuePreview"
            <*> _o .:? "subtype"
        ago = A.withArray "PropertyPreview" $ \_a -> PropertyPreview
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON PropertyPreview where
    toEncoding (PropertyPreview _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "type" .= _1
        , ("value" .=) <$> _2
        , ("valuePreview" .=) <$> _3
        , ("subtype" .=) <$> _4
        ]
    toJSON (PropertyPreview _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "type" .= _1
        , ("value" .=) <$> _2
        , ("valuePreview" .=) <$> _3
        , ("subtype" .=) <$> _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup PropertyPreview where
    PropertyPreview _0 _1 _2 _3 _4 <> PropertyPreview _ _ __2 __3 __4 = PropertyPreview _0 _1 (_2 <|> __2) (_3 <|> __3) (_4 <|> __4)


------------------------------------------------------------------------------
data Type_
    = Object_
    | Function_
    | Undefined_
    | String_
    | Number_
    | Boolean_
    | Symbol_
    | Accessor
    | Bigint_
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Type_ where
    parseJSON = A.withText "Type" $ \t -> case t of
        "object" -> P.pure Object_
        "function" -> P.pure Function_
        "undefined" -> P.pure Undefined_
        "string" -> P.pure String_
        "number" -> P.pure Number_
        "boolean" -> P.pure Boolean_
        "symbol" -> P.pure Symbol_
        "accessor" -> P.pure Accessor
        "bigint" -> P.pure Bigint_
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Type_ where
    toJSON Object_ = "object"
    toJSON Function_ = "function"
    toJSON Undefined_ = "undefined"
    toJSON String_ = "string"
    toJSON Number_ = "number"
    toJSON Boolean_ = "boolean"
    toJSON Symbol_ = "symbol"
    toJSON Accessor = "accessor"
    toJSON Bigint_ = "bigint"


------------------------------------------------------------------------------
{-# WARNING EntryPreview "This feature is marked as EXPERIMENTAL." #-}
data EntryPreview = EntryPreview
    { -- | Preview of the key. Specified for map-like collection entries.
      key :: !(P.Maybe ObjectPreview)
      -- | Preview of the value.
    , value :: !ObjectPreview
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON EntryPreview where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "EntryPreview" $ \_o -> EntryPreview
            <$> _o .:? "key"
            <*> _o .: "value"
        ago = A.withArray "EntryPreview" $ \_a -> EntryPreview
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON EntryPreview where
    toEncoding (EntryPreview _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ ("key" .=) <$> _0
        , P.pure $ "value" .= _1
        ]
    toJSON (EntryPreview _0 _1) = A.object $ P.catMaybes
        [ ("key" .=) <$> _0
        , P.pure $ "value" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup EntryPreview where
    EntryPreview _0 _1 <> EntryPreview __0 _ = EntryPreview (_0 <|> __0) _1


------------------------------------------------------------------------------
-- | Object property descriptor.
data PropertyDescriptor = PropertyDescriptor
    { -- | Property name or symbol description.
      name :: !T.Text
      -- | The value associated with the property.
    , value :: !(P.Maybe RemoteObject)
      -- | True if the value associated with the property may be changed (data descriptors only).
    , writable :: !(P.Maybe P.Bool)
      -- | A function which serves as a getter for the property, or @undefined@ if there is no getter
      -- (accessor descriptors only).
    , get :: !(P.Maybe RemoteObject)
      -- | A function which serves as a setter for the property, or @undefined@ if there is no setter
      -- (accessor descriptors only).
    , set :: !(P.Maybe RemoteObject)
      -- | True if the type of this property descriptor may be changed and if the property may be
      -- deleted from the corresponding object.
    , configurable :: !P.Bool
      -- | True if this property shows up during enumeration of the properties on the corresponding
      -- object.
    , enumerable :: !P.Bool
      -- | True if the result was thrown during the evaluation.
    , wasThrown :: !(P.Maybe P.Bool)
      -- | True if the property is owned for the object.
    , isOwn :: !(P.Maybe P.Bool)
      -- | Property symbol object, if the property is of the @symbol@ type.
    , symbol :: !(P.Maybe RemoteObject)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PropertyDescriptor where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "PropertyDescriptor" $ \_o -> PropertyDescriptor
            <$> _o .: "name"
            <*> _o .:? "value"
            <*> _o .:? "writable"
            <*> _o .:? "get"
            <*> _o .:? "set"
            <*> _o .: "configurable"
            <*> _o .: "enumerable"
            <*> _o .:? "wasThrown"
            <*> _o .:? "isOwn"
            <*> _o .:? "symbol"
        ago = A.withArray "PropertyDescriptor" $ \_a -> PropertyDescriptor
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.maybe P.empty A.parseJSON (_a !? 6)
            <*> P.traverse A.parseJSON (_a !? 7)
            <*> P.traverse A.parseJSON (_a !? 8)
            <*> P.traverse A.parseJSON (_a !? 9)


------------------------------------------------------------------------------
instance A.ToJSON PropertyDescriptor where
    toEncoding (PropertyDescriptor _0 _1 _2 _3 _4 _5 _6 _7 _8 _9) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "name" .= _0
        , ("value" .=) <$> _1
        , ("writable" .=) <$> _2
        , ("get" .=) <$> _3
        , ("set" .=) <$> _4
        , P.pure $ "configurable" .= _5
        , P.pure $ "enumerable" .= _6
        , ("wasThrown" .=) <$> _7
        , ("isOwn" .=) <$> _8
        , ("symbol" .=) <$> _9
        ]
    toJSON (PropertyDescriptor _0 _1 _2 _3 _4 _5 _6 _7 _8 _9) = A.object $ P.catMaybes
        [ P.pure $ "name" .= _0
        , ("value" .=) <$> _1
        , ("writable" .=) <$> _2
        , ("get" .=) <$> _3
        , ("set" .=) <$> _4
        , P.pure $ "configurable" .= _5
        , P.pure $ "enumerable" .= _6
        , ("wasThrown" .=) <$> _7
        , ("isOwn" .=) <$> _8
        , ("symbol" .=) <$> _9
        ]


------------------------------------------------------------------------------
instance P.Semigroup PropertyDescriptor where
    PropertyDescriptor _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 <> PropertyDescriptor _ __1 __2 __3 __4 _ _ __7 __8 __9 = PropertyDescriptor _0 (_1 <|> __1) (_2 <|> __2) (_3 <|> __3) (_4 <|> __4) _5 _6 (_7 <|> __7) (_8 <|> __8) (_9 <|> __9)


------------------------------------------------------------------------------
-- | Object internal property descriptor. This property isn't normally visible in JavaScript code.
data InternalPropertyDescriptor = InternalPropertyDescriptor
    { -- | Conventional property name.
      name :: !T.Text
      -- | The value associated with the property.
    , value :: !(P.Maybe RemoteObject)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON InternalPropertyDescriptor where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "InternalPropertyDescriptor" $ \_o -> InternalPropertyDescriptor
            <$> _o .: "name"
            <*> _o .:? "value"
        ago = A.withArray "InternalPropertyDescriptor" $ \_a -> InternalPropertyDescriptor
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON InternalPropertyDescriptor where
    toEncoding (InternalPropertyDescriptor _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "name" .= _0
        , ("value" .=) <$> _1
        ]
    toJSON (InternalPropertyDescriptor _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "name" .= _0
        , ("value" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup InternalPropertyDescriptor where
    InternalPropertyDescriptor _0 _1 <> InternalPropertyDescriptor _ __1 = InternalPropertyDescriptor _0 (_1 <|> __1)


------------------------------------------------------------------------------
-- | Object private field descriptor.
{-# WARNING PrivatePropertyDescriptor "This feature is marked as EXPERIMENTAL." #-}
data PrivatePropertyDescriptor = PrivatePropertyDescriptor
    { -- | Private property name.
      name :: !T.Text
      -- | The value associated with the private property.
    , value :: !RemoteObject
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PrivatePropertyDescriptor where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "PrivatePropertyDescriptor" $ \_o -> PrivatePropertyDescriptor
            <$> _o .: "name"
            <*> _o .: "value"
        ago = A.withArray "PrivatePropertyDescriptor" $ \_a -> PrivatePropertyDescriptor
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON PrivatePropertyDescriptor where
    toEncoding (PrivatePropertyDescriptor _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "value" .= _1
        ]
    toJSON (PrivatePropertyDescriptor _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "value" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup PrivatePropertyDescriptor where
    PrivatePropertyDescriptor _0 _1 <> PrivatePropertyDescriptor _ _ = PrivatePropertyDescriptor _0 _1


------------------------------------------------------------------------------
-- | Represents function call argument. Either remote object id @objectId@, primitive @value@,
-- unserializable primitive value or neither of (for undefined) them should be specified.
data CallArgument = CallArgument
    { -- | Primitive value or serializable javascript object.
      value :: !(P.Maybe A.Value)
      -- | Primitive value which can not be JSON-stringified.
    , unserializableValue :: !(P.Maybe UnserializableValue)
      -- | Remote object handle.
    , objectId :: !(P.Maybe RemoteObjectId)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CallArgument where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "CallArgument" $ \_o -> CallArgument
            <$> _o .:? "value"
            <*> _o .:? "unserializableValue"
            <*> _o .:? "objectId"
        ago = A.withArray "CallArgument" $ \_a -> CallArgument
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON CallArgument where
    toEncoding (CallArgument _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ ("value" .=) <$> _0
        , ("unserializableValue" .=) <$> _1
        , ("objectId" .=) <$> _2
        ]
    toJSON (CallArgument _0 _1 _2) = A.object $ P.catMaybes
        [ ("value" .=) <$> _0
        , ("unserializableValue" .=) <$> _1
        , ("objectId" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup CallArgument where
    CallArgument _0 _1 _2 <> CallArgument __0 __1 __2 = CallArgument (_0 <|> __0) (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
instance P.Monoid CallArgument where
    mempty = CallArgument P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Id of an execution context.
type ExecutionContextId = P.Int


------------------------------------------------------------------------------
-- | Description of an isolated world.
data ExecutionContextDescription = ExecutionContextDescription
    { -- | Unique id of the execution context. It can be used to specify in which execution context
      -- script evaluation should be performed.
      id :: !ExecutionContextId
      -- | Execution context origin.
    , origin :: !T.Text
      -- | Human readable name describing given context.
    , name :: !T.Text
      -- | Embedder-specific auxiliary data.
    , auxData :: !(P.Maybe A.Object)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ExecutionContextDescription where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "ExecutionContextDescription" $ \_o -> ExecutionContextDescription
            <$> _o .: "id"
            <*> _o .: "origin"
            <*> _o .: "name"
            <*> _o .:? "auxData"
        ago = A.withArray "ExecutionContextDescription" $ \_a -> ExecutionContextDescription
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON ExecutionContextDescription where
    toEncoding (ExecutionContextDescription _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "id" .= _0
        , P.pure $ "origin" .= _1
        , P.pure $ "name" .= _2
        , ("auxData" .=) <$> _3
        ]
    toJSON (ExecutionContextDescription _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "id" .= _0
        , P.pure $ "origin" .= _1
        , P.pure $ "name" .= _2
        , ("auxData" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup ExecutionContextDescription where
    ExecutionContextDescription _0 _1 _2 _3 <> ExecutionContextDescription _ _ _ __3 = ExecutionContextDescription _0 _1 _2 (_3 <|> __3)


------------------------------------------------------------------------------
-- | Detailed information about exception (or error) that was thrown during script compilation or
-- execution.
data ExceptionDetails = ExceptionDetails
    { -- | Exception id.
      exceptionId :: !P.Int
      -- | Exception text, which should be used together with exception object when available.
    , text :: !T.Text
      -- | Line number of the exception location (0-based).
    , lineNumber :: !P.Int
      -- | Column number of the exception location (0-based).
    , columnNumber :: !P.Int
      -- | Script ID of the exception location.
    , scriptId :: !(P.Maybe ScriptId)
      -- | URL of the exception location, to be used when the script was not reported.
    , url :: !(P.Maybe T.Text)
      -- | JavaScript stack trace if available.
    , stackTrace :: !(P.Maybe StackTrace)
      -- | Exception object if available.
    , exception :: !(P.Maybe RemoteObject)
      -- | Identifier of the context where exception happened.
    , executionContextId :: !(P.Maybe ExecutionContextId)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ExceptionDetails where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "ExceptionDetails" $ \_o -> ExceptionDetails
            <$> _o .: "exceptionId"
            <*> _o .: "text"
            <*> _o .: "lineNumber"
            <*> _o .: "columnNumber"
            <*> _o .:? "scriptId"
            <*> _o .:? "url"
            <*> _o .:? "stackTrace"
            <*> _o .:? "exception"
            <*> _o .:? "executionContextId"
        ago = A.withArray "ExceptionDetails" $ \_a -> ExceptionDetails
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)
            <*> P.traverse A.parseJSON (_a !? 7)
            <*> P.traverse A.parseJSON (_a !? 8)


------------------------------------------------------------------------------
instance A.ToJSON ExceptionDetails where
    toEncoding (ExceptionDetails _0 _1 _2 _3 _4 _5 _6 _7 _8) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "exceptionId" .= _0
        , P.pure $ "text" .= _1
        , P.pure $ "lineNumber" .= _2
        , P.pure $ "columnNumber" .= _3
        , ("scriptId" .=) <$> _4
        , ("url" .=) <$> _5
        , ("stackTrace" .=) <$> _6
        , ("exception" .=) <$> _7
        , ("executionContextId" .=) <$> _8
        ]
    toJSON (ExceptionDetails _0 _1 _2 _3 _4 _5 _6 _7 _8) = A.object $ P.catMaybes
        [ P.pure $ "exceptionId" .= _0
        , P.pure $ "text" .= _1
        , P.pure $ "lineNumber" .= _2
        , P.pure $ "columnNumber" .= _3
        , ("scriptId" .=) <$> _4
        , ("url" .=) <$> _5
        , ("stackTrace" .=) <$> _6
        , ("exception" .=) <$> _7
        , ("executionContextId" .=) <$> _8
        ]


------------------------------------------------------------------------------
instance P.Semigroup ExceptionDetails where
    ExceptionDetails _0 _1 _2 _3 _4 _5 _6 _7 _8 <> ExceptionDetails _ _ _ _ __4 __5 __6 __7 __8 = ExceptionDetails _0 _1 _2 _3 (_4 <|> __4) (_5 <|> __5) (_6 <|> __6) (_7 <|> __7) (_8 <|> __8)


------------------------------------------------------------------------------
-- | Number of milliseconds since epoch.
type Timestamp = P.Double


------------------------------------------------------------------------------
-- | Number of milliseconds.
type TimeDelta = P.Double


------------------------------------------------------------------------------
-- | Stack entry for runtime errors and assertions.
data CallFrame = CallFrame
    { -- | JavaScript function name.
      functionName :: !T.Text
      -- | JavaScript script id.
    , scriptId :: !ScriptId
      -- | JavaScript script name or url.
    , url :: !T.Text
      -- | JavaScript script line number (0-based).
    , lineNumber :: !P.Int
      -- | JavaScript script column number (0-based).
    , columnNumber :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CallFrame where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "CallFrame" $ \_o -> CallFrame
            <$> _o .: "functionName"
            <*> _o .: "scriptId"
            <*> _o .: "url"
            <*> _o .: "lineNumber"
            <*> _o .: "columnNumber"
        ago = A.withArray "CallFrame" $ \_a -> CallFrame
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON CallFrame where
    toEncoding (CallFrame _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "functionName" .= _0
        , P.pure $ "scriptId" .= _1
        , P.pure $ "url" .= _2
        , P.pure $ "lineNumber" .= _3
        , P.pure $ "columnNumber" .= _4
        ]
    toJSON (CallFrame _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ P.pure $ "functionName" .= _0
        , P.pure $ "scriptId" .= _1
        , P.pure $ "url" .= _2
        , P.pure $ "lineNumber" .= _3
        , P.pure $ "columnNumber" .= _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup CallFrame where
    CallFrame _0 _1 _2 _3 _4 <> CallFrame _ _ _ _ _ = CallFrame _0 _1 _2 _3 _4


------------------------------------------------------------------------------
-- | Call frames for assertions or error messages.
{-# WARNING parentId "This feature is marked as EXPERIMENTAL." #-}
data StackTrace = StackTrace
    { -- | String label of this stack trace. For async traces this may be a name of the function that
      -- initiated the async call.
      description :: !(P.Maybe T.Text)
      -- | JavaScript function name.
    , callFrames :: ![CallFrame]
      -- | Asynchronous JavaScript stack trace that preceded this stack, if available.
    , parent :: !(P.Maybe StackTrace)
      -- | Asynchronous JavaScript stack trace that preceded this stack, if available.
    , parentId :: !(P.Maybe StackTraceId)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StackTrace where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "StackTrace" $ \_o -> StackTrace
            <$> _o .:? "description"
            <*> _o .: "callFrames"
            <*> _o .:? "parent"
            <*> _o .:? "parentId"
        ago = A.withArray "StackTrace" $ \_a -> StackTrace
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON StackTrace where
    toEncoding (StackTrace _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ ("description" .=) <$> _0
        , P.pure $ "callFrames" .= _1
        , ("parent" .=) <$> _2
        , ("parentId" .=) <$> _3
        ]
    toJSON (StackTrace _0 _1 _2 _3) = A.object $ P.catMaybes
        [ ("description" .=) <$> _0
        , P.pure $ "callFrames" .= _1
        , ("parent" .=) <$> _2
        , ("parentId" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup StackTrace where
    StackTrace _0 _1 _2 _3 <> StackTrace __0 _ __2 __3 = StackTrace (_0 <|> __0) _1 (_2 <|> __2) (_3 <|> __3)


------------------------------------------------------------------------------
-- | Unique identifier of current debugger.
{-# WARNING UniqueDebuggerId "This feature is marked as EXPERIMENTAL." #-}
type UniqueDebuggerId = T.Text


------------------------------------------------------------------------------
-- | If @debuggerId@ is set stack trace comes from another debugger and can be resolved there. This
-- allows to track cross-debugger calls. See @Runtime.StackTrace@ and @Debugger.paused@ for usages.
{-# WARNING StackTraceId "This feature is marked as EXPERIMENTAL." #-}
data StackTraceId = StackTraceId
    { id :: !T.Text
    , debuggerId :: !(P.Maybe UniqueDebuggerId)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StackTraceId where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "StackTraceId" $ \_o -> StackTraceId
            <$> _o .: "id"
            <*> _o .:? "debuggerId"
        ago = A.withArray "StackTraceId" $ \_a -> StackTraceId
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON StackTraceId where
    toEncoding (StackTraceId _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "id" .= _0
        , ("debuggerId" .=) <$> _1
        ]
    toJSON (StackTraceId _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "id" .= _0
        , ("debuggerId" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup StackTraceId where
    StackTraceId _0 _1 <> StackTraceId _ __1 = StackTraceId _0 (_1 <|> __1)

