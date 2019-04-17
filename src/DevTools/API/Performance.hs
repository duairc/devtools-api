{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.Performance
    ( module DevTools.API.Performance.Types
    , module DevTools.API.Performance
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
import           DevTools.API.Performance.Types


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Disable collecting and reporting metrics.
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
    name _ = "Performance.disable"


------------------------------------------------------------------------------
-- | Disable collecting and reporting metrics.
disable
    :: Disable
disable = Disable


------------------------------------------------------------------------------
-- | Enable collecting and reporting metrics.
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
    name _ = "Performance.enable"


------------------------------------------------------------------------------
-- | Enable collecting and reporting metrics.
enable
    :: Enable
enable = Enable


------------------------------------------------------------------------------
-- | Sets time domain to use for collecting and reporting duration metrics.
-- Note that this must be called before enabling metrics collection. Calling
-- this method while metrics collection is enabled returns an error.
{-# WARNING SetTimeDomain "This feature is marked as EXPERIMENTAL." #-}
data SetTimeDomain = SetTimeDomain
    { -- | Time domain
      timeDomain :: !TimeDomain
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetTimeDomain where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setTimeDomain" $ \_o -> SetTimeDomain
            <$> _o .: "timeDomain"
        ago = A.withArray "setTimeDomain" $ \_a -> SetTimeDomain
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetTimeDomain where
    toEncoding (SetTimeDomain _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "timeDomain" .= _0
        ]
    toJSON (SetTimeDomain _0) = A.object $ P.catMaybes
        [ P.pure $ "timeDomain" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetTimeDomain where
    SetTimeDomain _0 <> SetTimeDomain _ = SetTimeDomain _0


------------------------------------------------------------------------------
data TimeDomain
    = TimeTicks
    | ThreadTicks
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TimeDomain where
    parseJSON = A.withText "TimeDomain" $ \t -> case t of
        "timeTicks" -> P.pure TimeTicks
        "threadTicks" -> P.pure ThreadTicks
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON TimeDomain where
    toJSON TimeTicks = "timeTicks"
    toJSON ThreadTicks = "threadTicks"


------------------------------------------------------------------------------
instance M.Method SetTimeDomain where
    type Result SetTimeDomain = ()
    name _ = "Performance.setTimeDomain"


------------------------------------------------------------------------------
-- | Sets time domain to use for collecting and reporting duration metrics.
-- Note that this must be called before enabling metrics collection. Calling
-- this method while metrics collection is enabled returns an error.
{-# WARNING setTimeDomain "This feature is marked as EXPERIMENTAL." #-}
setTimeDomain
    :: TimeDomain
    -- ^ Time domain

    -> SetTimeDomain
setTimeDomain _0 = SetTimeDomain _0


------------------------------------------------------------------------------
-- | Retrieve current values of run-time metrics.
data GetMetrics = GetMetrics
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetMetrics where
    parseJSON A.Null = P.pure GetMetrics
    parseJSON v = A.withArray "getMetrics" go v
        <|> A.withObject "getMetrics" go v
      where
        go _ = P.pure GetMetrics


------------------------------------------------------------------------------
instance A.ToJSON GetMetrics where
    toEncoding GetMetrics = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetMetrics = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetMetrics where
    GetMetrics <> GetMetrics = GetMetrics


------------------------------------------------------------------------------
instance P.Monoid GetMetrics where
    mempty = GetMetrics


------------------------------------------------------------------------------
-- | Retrieve current values of run-time metrics.
data GetMetricsResult = GetMetricsResult
    { -- | Current values for run-time metrics.
      metrics_ :: ![Metric]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetMetricsResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getMetricsResult" $ \_o -> GetMetricsResult
            <$> _o .: "metrics"
        ago = A.withArray "getMetricsResult" $ \_a -> GetMetricsResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetMetricsResult where
    toEncoding (GetMetricsResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "metrics" .= _0
        ]
    toJSON (GetMetricsResult _0) = A.object $ P.catMaybes
        [ P.pure $ "metrics" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetMetricsResult where
    GetMetricsResult _0 <> GetMetricsResult _ = GetMetricsResult _0


------------------------------------------------------------------------------
instance M.Method GetMetrics where
    type Result GetMetrics = GetMetricsResult
    name _ = "Performance.getMetrics"


------------------------------------------------------------------------------
-- | Retrieve current values of run-time metrics.
getMetrics
    :: GetMetrics
getMetrics = GetMetrics


------------------------------------------------------------------------------
-- | Current values of the metrics.
data Metrics = Metrics
    { -- | Current values of the metrics.
      metrics_ :: ![Metric]
      -- | Timestamp title.
    , title :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Metrics where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "metrics" $ \_o -> Metrics
            <$> _o .: "metrics"
            <*> _o .: "title"
        ago = A.withArray "metrics" $ \_a -> Metrics
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON Metrics where
    toEncoding (Metrics _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "metrics" .= _0
        , P.pure $ "title" .= _1
        ]
    toJSON (Metrics _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "metrics" .= _0
        , P.pure $ "title" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup Metrics where
    Metrics _0 _1 <> Metrics _ _ = Metrics _0 _1


------------------------------------------------------------------------------
instance E.Event Metrics where
    type Result Metrics = Metrics
    name _ = "Performance.metrics"


------------------------------------------------------------------------------
-- | Current values of the metrics.
metrics :: P.Proxy Metrics
metrics = P.Proxy

