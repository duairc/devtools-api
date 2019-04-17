{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | The SystemInfo domain defines methods and events for querying low-level system information.
module DevTools.API.SystemInfo{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

    ( module DevTools.API.SystemInfo.Types
    , module DevTools.API.SystemInfo
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
import           DevTools.API.SystemInfo.Types


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Returns information about the system.
data GetInfo = GetInfo
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetInfo where
    parseJSON A.Null = P.pure GetInfo
    parseJSON v = A.withArray "getInfo" go v
        <|> A.withObject "getInfo" go v
      where
        go _ = P.pure GetInfo


------------------------------------------------------------------------------
instance A.ToJSON GetInfo where
    toEncoding GetInfo = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetInfo = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetInfo where
    GetInfo <> GetInfo = GetInfo


------------------------------------------------------------------------------
instance P.Monoid GetInfo where
    mempty = GetInfo


------------------------------------------------------------------------------
-- | Returns information about the system.
data GetInfoResult = GetInfoResult
    { -- | Information about the GPUs on the system.
      gpu :: !GPUInfo
      -- | A platform-dependent description of the model of the machine. On Mac OS, this is, for
      -- example, 'MacBookPro'. Will be the empty string if not supported.
    , modelName :: !T.Text
      -- | A platform-dependent description of the version of the machine. On Mac OS, this is, for
      -- example, '10.1'. Will be the empty string if not supported.
    , modelVersion :: !T.Text
      -- | The command line string used to launch the browser. Will be the empty string if not
      -- supported.
    , commandLine :: !T.Text
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetInfoResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getInfoResult" $ \_o -> GetInfoResult
            <$> _o .: "gpu"
            <*> _o .: "modelName"
            <*> _o .: "modelVersion"
            <*> _o .: "commandLine"
        ago = A.withArray "getInfoResult" $ \_a -> GetInfoResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON GetInfoResult where
    toEncoding (GetInfoResult _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "gpu" .= _0
        , P.pure $ "modelName" .= _1
        , P.pure $ "modelVersion" .= _2
        , P.pure $ "commandLine" .= _3
        ]
    toJSON (GetInfoResult _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "gpu" .= _0
        , P.pure $ "modelName" .= _1
        , P.pure $ "modelVersion" .= _2
        , P.pure $ "commandLine" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetInfoResult where
    GetInfoResult _0 _1 _2 _3 <> GetInfoResult _ _ _ _ = GetInfoResult _0 _1 _2 _3


------------------------------------------------------------------------------
instance M.Method GetInfo where
    type Result GetInfo = GetInfoResult
    name _ = "SystemInfo.getInfo"


------------------------------------------------------------------------------
-- | Returns information about the system.
getInfo
    :: GetInfo
getInfo = GetInfo


------------------------------------------------------------------------------
-- | Returns information about all running processes.
data GetProcessInfo = GetProcessInfo
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetProcessInfo where
    parseJSON A.Null = P.pure GetProcessInfo
    parseJSON v = A.withArray "getProcessInfo" go v
        <|> A.withObject "getProcessInfo" go v
      where
        go _ = P.pure GetProcessInfo


------------------------------------------------------------------------------
instance A.ToJSON GetProcessInfo where
    toEncoding GetProcessInfo = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetProcessInfo = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetProcessInfo where
    GetProcessInfo <> GetProcessInfo = GetProcessInfo


------------------------------------------------------------------------------
instance P.Monoid GetProcessInfo where
    mempty = GetProcessInfo


------------------------------------------------------------------------------
-- | Returns information about all running processes.
data GetProcessInfoResult = GetProcessInfoResult
    { -- | An array of process info blocks.
      processInfo :: ![ProcessInfo]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetProcessInfoResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getProcessInfoResult" $ \_o -> GetProcessInfoResult
            <$> _o .: "processInfo"
        ago = A.withArray "getProcessInfoResult" $ \_a -> GetProcessInfoResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetProcessInfoResult where
    toEncoding (GetProcessInfoResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "processInfo" .= _0
        ]
    toJSON (GetProcessInfoResult _0) = A.object $ P.catMaybes
        [ P.pure $ "processInfo" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetProcessInfoResult where
    GetProcessInfoResult _0 <> GetProcessInfoResult _ = GetProcessInfoResult _0


------------------------------------------------------------------------------
instance M.Method GetProcessInfo where
    type Result GetProcessInfo = GetProcessInfoResult
    name _ = "SystemInfo.getProcessInfo"


------------------------------------------------------------------------------
-- | Returns information about all running processes.
getProcessInfo
    :: GetProcessInfo
getProcessInfo = GetProcessInfo

