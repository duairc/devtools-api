{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Supports additional targets discovery and allows to attach to them.
module DevTools.API.Target.Types
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
type TargetID = T.Text


------------------------------------------------------------------------------
-- | Unique identifier of attached debugging session.
type SessionID = T.Text


------------------------------------------------------------------------------
{-# WARNING BrowserContextID "This feature is marked as EXPERIMENTAL." #-}
type BrowserContextID = T.Text


------------------------------------------------------------------------------
{-# WARNING browserContextId "This feature is marked as EXPERIMENTAL." #-}
data TargetInfo = TargetInfo
    { targetId :: !TargetID
    , type_ :: !T.Text
    , title :: !T.Text
    , url :: !T.Text
      -- | Whether the target has an attached client.
    , attached :: !P.Bool
      -- | Opener target Id
    , openerId :: !(P.Maybe TargetID)
    , browserContextId :: !(P.Maybe BrowserContextID)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TargetInfo where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "TargetInfo" $ \_o -> TargetInfo
            <$> _o .: "targetId"
            <*> _o .: "type"
            <*> _o .: "title"
            <*> _o .: "url"
            <*> _o .: "attached"
            <*> _o .:? "openerId"
            <*> _o .:? "browserContextId"
        ago = A.withArray "TargetInfo" $ \_a -> TargetInfo
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)


------------------------------------------------------------------------------
instance A.ToJSON TargetInfo where
    toEncoding (TargetInfo _0 _1 _2 _3 _4 _5 _6) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "targetId" .= _0
        , P.pure $ "type" .= _1
        , P.pure $ "title" .= _2
        , P.pure $ "url" .= _3
        , P.pure $ "attached" .= _4
        , ("openerId" .=) <$> _5
        , ("browserContextId" .=) <$> _6
        ]
    toJSON (TargetInfo _0 _1 _2 _3 _4 _5 _6) = A.object $ P.catMaybes
        [ P.pure $ "targetId" .= _0
        , P.pure $ "type" .= _1
        , P.pure $ "title" .= _2
        , P.pure $ "url" .= _3
        , P.pure $ "attached" .= _4
        , ("openerId" .=) <$> _5
        , ("browserContextId" .=) <$> _6
        ]


------------------------------------------------------------------------------
instance P.Semigroup TargetInfo where
    TargetInfo _0 _1 _2 _3 _4 _5 _6 <> TargetInfo _ _ _ _ _ __5 __6 = TargetInfo _0 _1 _2 _3 _4 (_5 <|> __5) (_6 <|> __6)


------------------------------------------------------------------------------
{-# WARNING RemoteLocation "This feature is marked as EXPERIMENTAL." #-}
data RemoteLocation = RemoteLocation
    { host :: !T.Text
    , port :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RemoteLocation where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "RemoteLocation" $ \_o -> RemoteLocation
            <$> _o .: "host"
            <*> _o .: "port"
        ago = A.withArray "RemoteLocation" $ \_a -> RemoteLocation
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON RemoteLocation where
    toEncoding (RemoteLocation _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "host" .= _0
        , P.pure $ "port" .= _1
        ]
    toJSON (RemoteLocation _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "host" .= _0
        , P.pure $ "port" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup RemoteLocation where
    RemoteLocation _0 _1 <> RemoteLocation _ _ = RemoteLocation _0 _1

