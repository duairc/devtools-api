{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | A domain for interacting with Cast, Presentation API, and Remote Playback API
-- functionalities.
module DevTools.API.Cast{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

    ( module DevTools.API.Cast.Types
    , module DevTools.API.Cast
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
import           DevTools.API.Cast.Types


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Starts observing for sinks that can be used for tab mirroring, and if set,
-- sinks compatible with |presentationUrl| as well. When sinks are found, a
-- |sinksUpdated| event is fired.
-- Also starts observing for issue messages. When an issue is added or removed,
-- an |issueUpdated| event is fired.
data Enable = Enable
    { presentationUrl :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Enable where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "enable" $ \_o -> Enable
            <$> _o .:? "presentationUrl"
        ago = A.withArray "enable" $ \_a -> Enable
            <$> P.traverse A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON Enable where
    toEncoding (Enable _0) = A.pairs $ P.fold $ P.catMaybes
        [ ("presentationUrl" .=) <$> _0
        ]
    toJSON (Enable _0) = A.object $ P.catMaybes
        [ ("presentationUrl" .=) <$> _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup Enable where
    Enable _0 <> Enable __0 = Enable (_0 <|> __0)


------------------------------------------------------------------------------
instance P.Monoid Enable where
    mempty = Enable P.empty


------------------------------------------------------------------------------
instance M.Method Enable where
    type Result Enable = ()
    name _ = "Cast.enable"


------------------------------------------------------------------------------
-- | Starts observing for sinks that can be used for tab mirroring, and if set,
-- sinks compatible with |presentationUrl| as well. When sinks are found, a
-- |sinksUpdated| event is fired.
-- Also starts observing for issue messages. When an issue is added or removed,
-- an |issueUpdated| event is fired.
enable
    :: Enable
enable = Enable P.empty


------------------------------------------------------------------------------
-- | Stops observing for sinks and issues.
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
    name _ = "Cast.disable"


------------------------------------------------------------------------------
-- | Stops observing for sinks and issues.
disable
    :: Disable
disable = Disable


------------------------------------------------------------------------------
-- | Sets a sink to be used when the web page requests the browser to choose a
-- sink via Presentation API, Remote Playback API, or Cast SDK.
data SetSinkToUse = SetSinkToUse
    { sinkName :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetSinkToUse where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setSinkToUse" $ \_o -> SetSinkToUse
            <$> _o .: "sinkName"
        ago = A.withArray "setSinkToUse" $ \_a -> SetSinkToUse
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetSinkToUse where
    toEncoding (SetSinkToUse _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "sinkName" .= _0
        ]
    toJSON (SetSinkToUse _0) = A.object $ P.catMaybes
        [ P.pure $ "sinkName" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetSinkToUse where
    SetSinkToUse _0 <> SetSinkToUse _ = SetSinkToUse _0


------------------------------------------------------------------------------
instance M.Method SetSinkToUse where
    type Result SetSinkToUse = ()
    name _ = "Cast.setSinkToUse"


------------------------------------------------------------------------------
-- | Sets a sink to be used when the web page requests the browser to choose a
-- sink via Presentation API, Remote Playback API, or Cast SDK.
setSinkToUse
    :: T.Text
    -> SetSinkToUse
setSinkToUse _0 = SetSinkToUse _0


------------------------------------------------------------------------------
-- | Starts mirroring the tab to the sink.
data StartTabMirroring = StartTabMirroring
    { sinkName :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StartTabMirroring where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "startTabMirroring" $ \_o -> StartTabMirroring
            <$> _o .: "sinkName"
        ago = A.withArray "startTabMirroring" $ \_a -> StartTabMirroring
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON StartTabMirroring where
    toEncoding (StartTabMirroring _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "sinkName" .= _0
        ]
    toJSON (StartTabMirroring _0) = A.object $ P.catMaybes
        [ P.pure $ "sinkName" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup StartTabMirroring where
    StartTabMirroring _0 <> StartTabMirroring _ = StartTabMirroring _0


------------------------------------------------------------------------------
instance M.Method StartTabMirroring where
    type Result StartTabMirroring = ()
    name _ = "Cast.startTabMirroring"


------------------------------------------------------------------------------
-- | Starts mirroring the tab to the sink.
startTabMirroring
    :: T.Text
    -> StartTabMirroring
startTabMirroring _0 = StartTabMirroring _0


------------------------------------------------------------------------------
-- | Stops the active Cast session on the sink.
data StopCasting = StopCasting
    { sinkName :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StopCasting where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "stopCasting" $ \_o -> StopCasting
            <$> _o .: "sinkName"
        ago = A.withArray "stopCasting" $ \_a -> StopCasting
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON StopCasting where
    toEncoding (StopCasting _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "sinkName" .= _0
        ]
    toJSON (StopCasting _0) = A.object $ P.catMaybes
        [ P.pure $ "sinkName" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup StopCasting where
    StopCasting _0 <> StopCasting _ = StopCasting _0


------------------------------------------------------------------------------
instance M.Method StopCasting where
    type Result StopCasting = ()
    name _ = "Cast.stopCasting"


------------------------------------------------------------------------------
-- | Stops the active Cast session on the sink.
stopCasting
    :: T.Text
    -> StopCasting
stopCasting _0 = StopCasting _0


------------------------------------------------------------------------------
-- | This is fired whenever the list of available sinks changes. A sink is a
-- device or a software surface that you can cast to.
data SinksUpdated = SinksUpdated
    { sinks :: ![Sink]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SinksUpdated where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "sinksUpdated" $ \_o -> SinksUpdated
            <$> _o .: "sinks"
        ago = A.withArray "sinksUpdated" $ \_a -> SinksUpdated
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SinksUpdated where
    toEncoding (SinksUpdated _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "sinks" .= _0
        ]
    toJSON (SinksUpdated _0) = A.object $ P.catMaybes
        [ P.pure $ "sinks" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SinksUpdated where
    SinksUpdated _0 <> SinksUpdated _ = SinksUpdated _0


------------------------------------------------------------------------------
instance E.Event SinksUpdated where
    type Result SinksUpdated = SinksUpdated
    name _ = "Cast.sinksUpdated"


------------------------------------------------------------------------------
-- | This is fired whenever the list of available sinks changes. A sink is a
-- device or a software surface that you can cast to.
sinksUpdated :: P.Proxy SinksUpdated
sinksUpdated = P.Proxy


------------------------------------------------------------------------------
-- | This is fired whenever the outstanding issue\/error message changes.
-- |issueMessage| is empty if there is no issue.
data IssueUpdated = IssueUpdated
    { issueMessage :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON IssueUpdated where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "issueUpdated" $ \_o -> IssueUpdated
            <$> _o .: "issueMessage"
        ago = A.withArray "issueUpdated" $ \_a -> IssueUpdated
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON IssueUpdated where
    toEncoding (IssueUpdated _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "issueMessage" .= _0
        ]
    toJSON (IssueUpdated _0) = A.object $ P.catMaybes
        [ P.pure $ "issueMessage" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup IssueUpdated where
    IssueUpdated _0 <> IssueUpdated _ = IssueUpdated _0


------------------------------------------------------------------------------
instance E.Event IssueUpdated where
    type Result IssueUpdated = IssueUpdated
    name _ = "Cast.issueUpdated"


------------------------------------------------------------------------------
-- | This is fired whenever the outstanding issue\/error message changes.
-- |issueMessage| is empty if there is no issue.
issueUpdated :: P.Proxy IssueUpdated
issueUpdated = P.Proxy

