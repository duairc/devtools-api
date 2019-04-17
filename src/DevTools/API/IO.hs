{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Input\/Output operations for streams produced by DevTools.
module DevTools.API.IO
    ( module DevTools.API.IO.Types
    , module DevTools.API.IO
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
import           DevTools.API.IO.Types
import qualified DevTools.API.Runtime.Types as Runtime


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Close the stream, discard any temporary backing storage.
data Close = Close
    { -- | Handle of the stream to close.
      handle :: !StreamHandle
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Close where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "close" $ \_o -> Close
            <$> _o .: "handle"
        ago = A.withArray "close" $ \_a -> Close
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON Close where
    toEncoding (Close _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "handle" .= _0
        ]
    toJSON (Close _0) = A.object $ P.catMaybes
        [ P.pure $ "handle" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup Close where
    Close _0 <> Close _ = Close _0


------------------------------------------------------------------------------
instance M.Method Close where
    type Result Close = ()
    name _ = "IO.close"


------------------------------------------------------------------------------
-- | Close the stream, discard any temporary backing storage.
close
    :: StreamHandle
    -- ^ Handle of the stream to close.

    -> Close
close _0 = Close _0


------------------------------------------------------------------------------
-- | Read a chunk of the stream
data Read = Read
    { -- | Handle of the stream to read.
      handle :: !StreamHandle
      -- | Seek to the specified offset before reading (if not specificed, proceed with offset
      -- following the last read). Some types of streams may only support sequential reads.
    , offset :: !(P.Maybe P.Int)
      -- | Maximum number of bytes to read (left upon the agent discretion if not specified).
    , size :: !(P.Maybe P.Int)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Read where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "read" $ \_o -> Read
            <$> _o .: "handle"
            <*> _o .:? "offset"
            <*> _o .:? "size"
        ago = A.withArray "read" $ \_a -> Read
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON Read where
    toEncoding (Read _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "handle" .= _0
        , ("offset" .=) <$> _1
        , ("size" .=) <$> _2
        ]
    toJSON (Read _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "handle" .= _0
        , ("offset" .=) <$> _1
        , ("size" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup Read where
    Read _0 _1 _2 <> Read _ __1 __2 = Read _0 (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
-- | Read a chunk of the stream
data ReadResult = ReadResult
    { -- | Set if the data is base64-encoded
      base64Encoded :: !(P.Maybe P.Bool)
      -- | Data that were read.
    , data_ :: !T.Text
      -- | Set if the end-of-file condition occured while reading.
    , eof :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ReadResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "readResult" $ \_o -> ReadResult
            <$> _o .:? "base64Encoded"
            <*> _o .: "data"
            <*> _o .: "eof"
        ago = A.withArray "readResult" $ \_a -> ReadResult
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON ReadResult where
    toEncoding (ReadResult _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ ("base64Encoded" .=) <$> _0
        , P.pure $ "data" .= _1
        , P.pure $ "eof" .= _2
        ]
    toJSON (ReadResult _0 _1 _2) = A.object $ P.catMaybes
        [ ("base64Encoded" .=) <$> _0
        , P.pure $ "data" .= _1
        , P.pure $ "eof" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup ReadResult where
    ReadResult _0 _1 _2 <> ReadResult __0 _ _ = ReadResult (_0 <|> __0) _1 _2


------------------------------------------------------------------------------
instance M.Method Read where
    type Result Read = ReadResult
    name _ = "IO.read"


------------------------------------------------------------------------------
-- | Read a chunk of the stream
read
    :: StreamHandle
    -- ^ Handle of the stream to read.

    -> Read
read _0 = Read _0 P.empty P.empty


------------------------------------------------------------------------------
-- | Return UUID of Blob object specified by a remote object id.
data ResolveBlob = ResolveBlob
    { -- | Object id of a Blob object wrapper.
      objectId :: !Runtime.RemoteObjectId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ResolveBlob where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "resolveBlob" $ \_o -> ResolveBlob
            <$> _o .: "objectId"
        ago = A.withArray "resolveBlob" $ \_a -> ResolveBlob
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON ResolveBlob where
    toEncoding (ResolveBlob _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "objectId" .= _0
        ]
    toJSON (ResolveBlob _0) = A.object $ P.catMaybes
        [ P.pure $ "objectId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup ResolveBlob where
    ResolveBlob _0 <> ResolveBlob _ = ResolveBlob _0


------------------------------------------------------------------------------
-- | Return UUID of Blob object specified by a remote object id.
data ResolveBlobResult = ResolveBlobResult
    { -- | UUID of the specified Blob.
      uuid :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ResolveBlobResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "resolveBlobResult" $ \_o -> ResolveBlobResult
            <$> _o .: "uuid"
        ago = A.withArray "resolveBlobResult" $ \_a -> ResolveBlobResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON ResolveBlobResult where
    toEncoding (ResolveBlobResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "uuid" .= _0
        ]
    toJSON (ResolveBlobResult _0) = A.object $ P.catMaybes
        [ P.pure $ "uuid" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup ResolveBlobResult where
    ResolveBlobResult _0 <> ResolveBlobResult _ = ResolveBlobResult _0


------------------------------------------------------------------------------
instance M.Method ResolveBlob where
    type Result ResolveBlob = ResolveBlobResult
    name _ = "IO.resolveBlob"


------------------------------------------------------------------------------
-- | Return UUID of Blob object specified by a remote object id.
resolveBlob
    :: Runtime.RemoteObjectId
    -- ^ Object id of a Blob object wrapper.

    -> ResolveBlob
resolveBlob _0 = ResolveBlob _0

