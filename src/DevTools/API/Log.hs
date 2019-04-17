{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Provides access to log entries.
module DevTools.API.Log
    ( module DevTools.API.Log.Types
    , module DevTools.API.Log
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
import           DevTools.API.Log.Types


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Clears the log.
data Clear = Clear
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Clear where
    parseJSON A.Null = P.pure Clear
    parseJSON v = A.withArray "clear" go v
        <|> A.withObject "clear" go v
      where
        go _ = P.pure Clear


------------------------------------------------------------------------------
instance A.ToJSON Clear where
    toEncoding Clear = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON Clear = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup Clear where
    Clear <> Clear = Clear


------------------------------------------------------------------------------
instance P.Monoid Clear where
    mempty = Clear


------------------------------------------------------------------------------
instance M.Method Clear where
    type Result Clear = ()
    name _ = "Log.clear"


------------------------------------------------------------------------------
-- | Clears the log.
clear
    :: Clear
clear = Clear


------------------------------------------------------------------------------
-- | Disables log domain, prevents further log entries from being reported to the client.
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
    name _ = "Log.disable"


------------------------------------------------------------------------------
-- | Disables log domain, prevents further log entries from being reported to the client.
disable
    :: Disable
disable = Disable


------------------------------------------------------------------------------
-- | Enables log domain, sends the entries collected so far to the client by means of the
-- @entryAdded@ notification.
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
    name _ = "Log.enable"


------------------------------------------------------------------------------
-- | Enables log domain, sends the entries collected so far to the client by means of the
-- @entryAdded@ notification.
enable
    :: Enable
enable = Enable


------------------------------------------------------------------------------
-- | start violation reporting.
data StartViolationsReport = StartViolationsReport
    { -- | Configuration for violations.
      config :: ![ViolationSetting]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StartViolationsReport where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "startViolationsReport" $ \_o -> StartViolationsReport
            <$> _o .: "config"
        ago = A.withArray "startViolationsReport" $ \_a -> StartViolationsReport
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON StartViolationsReport where
    toEncoding (StartViolationsReport _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "config" .= _0
        ]
    toJSON (StartViolationsReport _0) = A.object $ P.catMaybes
        [ P.pure $ "config" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup StartViolationsReport where
    StartViolationsReport _0 <> StartViolationsReport _ = StartViolationsReport _0


------------------------------------------------------------------------------
instance M.Method StartViolationsReport where
    type Result StartViolationsReport = ()
    name _ = "Log.startViolationsReport"


------------------------------------------------------------------------------
-- | start violation reporting.
startViolationsReport
    :: [ViolationSetting]
    -- ^ Configuration for violations.

    -> StartViolationsReport
startViolationsReport _0 = StartViolationsReport _0


------------------------------------------------------------------------------
-- | Stop violation reporting.
data StopViolationsReport = StopViolationsReport
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StopViolationsReport where
    parseJSON A.Null = P.pure StopViolationsReport
    parseJSON v = A.withArray "stopViolationsReport" go v
        <|> A.withObject "stopViolationsReport" go v
      where
        go _ = P.pure StopViolationsReport


------------------------------------------------------------------------------
instance A.ToJSON StopViolationsReport where
    toEncoding StopViolationsReport = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON StopViolationsReport = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup StopViolationsReport where
    StopViolationsReport <> StopViolationsReport = StopViolationsReport


------------------------------------------------------------------------------
instance P.Monoid StopViolationsReport where
    mempty = StopViolationsReport


------------------------------------------------------------------------------
instance M.Method StopViolationsReport where
    type Result StopViolationsReport = ()
    name _ = "Log.stopViolationsReport"


------------------------------------------------------------------------------
-- | Stop violation reporting.
stopViolationsReport
    :: StopViolationsReport
stopViolationsReport = StopViolationsReport


------------------------------------------------------------------------------
-- | Issued when new message was logged.
data EntryAdded = EntryAdded
    { -- | The entry.
      entry :: !LogEntry
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON EntryAdded where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "entryAdded" $ \_o -> EntryAdded
            <$> _o .: "entry"
        ago = A.withArray "entryAdded" $ \_a -> EntryAdded
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON EntryAdded where
    toEncoding (EntryAdded _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "entry" .= _0
        ]
    toJSON (EntryAdded _0) = A.object $ P.catMaybes
        [ P.pure $ "entry" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup EntryAdded where
    EntryAdded _0 <> EntryAdded _ = EntryAdded _0


------------------------------------------------------------------------------
instance E.Event EntryAdded where
    type Result EntryAdded = EntryAdded
    name _ = "Log.entryAdded"


------------------------------------------------------------------------------
-- | Issued when new message was logged.
entryAdded :: P.Proxy EntryAdded
entryAdded = P.Proxy

