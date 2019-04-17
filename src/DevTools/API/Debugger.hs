{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Debugger domain exposes JavaScript debugging capabilities. It allows setting and removing
-- breakpoints, stepping through execution, exploring stack traces, etc.
module DevTools.API.Debugger
    ( module DevTools.API.Debugger.Types
    , module DevTools.API.Debugger
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
import           DevTools.API.Debugger.Types
import qualified DevTools.API.Runtime.Types as Runtime


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Continues execution until specific location is reached.
data ContinueToLocation = ContinueToLocation
    { -- | Location to continue to.
      location :: !Location
    , targetCallFrames :: !(P.Maybe TargetCallFrames)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ContinueToLocation where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "continueToLocation" $ \_o -> ContinueToLocation
            <$> _o .: "location"
            <*> _o .:? "targetCallFrames"
        ago = A.withArray "continueToLocation" $ \_a -> ContinueToLocation
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON ContinueToLocation where
    toEncoding (ContinueToLocation _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "location" .= _0
        , ("targetCallFrames" .=) <$> _1
        ]
    toJSON (ContinueToLocation _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "location" .= _0
        , ("targetCallFrames" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup ContinueToLocation where
    ContinueToLocation _0 _1 <> ContinueToLocation _ __1 = ContinueToLocation _0 (_1 <|> __1)


------------------------------------------------------------------------------
data TargetCallFrames
    = Any
    | Current
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TargetCallFrames where
    parseJSON = A.withText "TargetCallFrames" $ \t -> case t of
        "any" -> P.pure Any
        "current" -> P.pure Current
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON TargetCallFrames where
    toJSON Any = "any"
    toJSON Current = "current"


------------------------------------------------------------------------------
instance M.Method ContinueToLocation where
    type Result ContinueToLocation = ()
    name _ = "Debugger.continueToLocation"


------------------------------------------------------------------------------
-- | Continues execution until specific location is reached.
continueToLocation
    :: Location
    -- ^ Location to continue to.

    -> ContinueToLocation
continueToLocation _0 = ContinueToLocation _0 P.empty


------------------------------------------------------------------------------
-- | Disables debugger for given page.
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
    name _ = "Debugger.disable"


------------------------------------------------------------------------------
-- | Disables debugger for given page.
disable
    :: Disable
disable = Disable


------------------------------------------------------------------------------
-- | Enables debugger for the given page. Clients should not assume that the debugging has been
-- enabled until the result for this command is received.
{-# WARNING maxScriptsCacheSize "This feature is marked as EXPERIMENTAL." #-}
data Enable = Enable
    { -- | The maximum size in bytes of collected scripts (not referenced by other heap objects)
      -- the debugger can hold. Puts no limit if paramter is omitted.
      maxScriptsCacheSize :: !(P.Maybe P.Double)
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
            <$> _o .:? "maxScriptsCacheSize"
        ago = A.withArray "enable" $ \_a -> Enable
            <$> P.traverse A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON Enable where
    toEncoding (Enable _0) = A.pairs $ P.fold $ P.catMaybes
        [ ("maxScriptsCacheSize" .=) <$> _0
        ]
    toJSON (Enable _0) = A.object $ P.catMaybes
        [ ("maxScriptsCacheSize" .=) <$> _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup Enable where
    Enable _0 <> Enable __0 = Enable (_0 <|> __0)


------------------------------------------------------------------------------
instance P.Monoid Enable where
    mempty = Enable P.empty


------------------------------------------------------------------------------
-- | Enables debugger for the given page. Clients should not assume that the debugging has been
-- enabled until the result for this command is received.
{-# WARNING debuggerId "This feature is marked as EXPERIMENTAL." #-}
data EnableResult = EnableResult
    { -- | Unique identifier of the debugger.
      debuggerId :: !Runtime.UniqueDebuggerId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON EnableResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "enableResult" $ \_o -> EnableResult
            <$> _o .: "debuggerId"
        ago = A.withArray "enableResult" $ \_a -> EnableResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON EnableResult where
    toEncoding (EnableResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "debuggerId" .= _0
        ]
    toJSON (EnableResult _0) = A.object $ P.catMaybes
        [ P.pure $ "debuggerId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup EnableResult where
    EnableResult _0 <> EnableResult _ = EnableResult _0


------------------------------------------------------------------------------
instance M.Method Enable where
    type Result Enable = EnableResult
    name _ = "Debugger.enable"


------------------------------------------------------------------------------
-- | Enables debugger for the given page. Clients should not assume that the debugging has been
-- enabled until the result for this command is received.
enable
    :: Enable
enable = Enable P.empty


------------------------------------------------------------------------------
-- | Evaluates expression on a given call frame.
{-# WARNING generatePreview, timeout "This feature is marked as EXPERIMENTAL." #-}
data EvaluateOnCallFrame = EvaluateOnCallFrame
    { -- | Call frame identifier to evaluate on.
      callFrameId :: !CallFrameId
      -- | Expression to evaluate.
    , expression :: !T.Text
      -- | String object group name to put result into (allows rapid releasing resulting object handles
      -- using @releaseObjectGroup@).
    , objectGroup :: !(P.Maybe T.Text)
      -- | Specifies whether command line API should be available to the evaluated expression, defaults
      -- to false.
    , includeCommandLineAPI :: !(P.Maybe P.Bool)
      -- | In silent mode exceptions thrown during evaluation are not reported and do not pause
      -- execution. Overrides @setPauseOnException@ state.
    , silent :: !(P.Maybe P.Bool)
      -- | Whether the result is expected to be a JSON object that should be sent by value.
    , returnByValue :: !(P.Maybe P.Bool)
      -- | Whether preview should be generated for the result.
    , generatePreview :: !(P.Maybe P.Bool)
      -- | Whether to throw an exception if side effect cannot be ruled out during evaluation.
    , throwOnSideEffect :: !(P.Maybe P.Bool)
      -- | Terminate execution after timing out (number of milliseconds).
    , timeout :: !(P.Maybe Runtime.TimeDelta)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON EvaluateOnCallFrame where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "evaluateOnCallFrame" $ \_o -> EvaluateOnCallFrame
            <$> _o .: "callFrameId"
            <*> _o .: "expression"
            <*> _o .:? "objectGroup"
            <*> _o .:? "includeCommandLineAPI"
            <*> _o .:? "silent"
            <*> _o .:? "returnByValue"
            <*> _o .:? "generatePreview"
            <*> _o .:? "throwOnSideEffect"
            <*> _o .:? "timeout"
        ago = A.withArray "evaluateOnCallFrame" $ \_a -> EvaluateOnCallFrame
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)
            <*> P.traverse A.parseJSON (_a !? 7)
            <*> P.traverse A.parseJSON (_a !? 8)


------------------------------------------------------------------------------
instance A.ToJSON EvaluateOnCallFrame where
    toEncoding (EvaluateOnCallFrame _0 _1 _2 _3 _4 _5 _6 _7 _8) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "callFrameId" .= _0
        , P.pure $ "expression" .= _1
        , ("objectGroup" .=) <$> _2
        , ("includeCommandLineAPI" .=) <$> _3
        , ("silent" .=) <$> _4
        , ("returnByValue" .=) <$> _5
        , ("generatePreview" .=) <$> _6
        , ("throwOnSideEffect" .=) <$> _7
        , ("timeout" .=) <$> _8
        ]
    toJSON (EvaluateOnCallFrame _0 _1 _2 _3 _4 _5 _6 _7 _8) = A.object $ P.catMaybes
        [ P.pure $ "callFrameId" .= _0
        , P.pure $ "expression" .= _1
        , ("objectGroup" .=) <$> _2
        , ("includeCommandLineAPI" .=) <$> _3
        , ("silent" .=) <$> _4
        , ("returnByValue" .=) <$> _5
        , ("generatePreview" .=) <$> _6
        , ("throwOnSideEffect" .=) <$> _7
        , ("timeout" .=) <$> _8
        ]


------------------------------------------------------------------------------
instance P.Semigroup EvaluateOnCallFrame where
    EvaluateOnCallFrame _0 _1 _2 _3 _4 _5 _6 _7 _8 <> EvaluateOnCallFrame _ _ __2 __3 __4 __5 __6 __7 __8 = EvaluateOnCallFrame _0 _1 (_2 <|> __2) (_3 <|> __3) (_4 <|> __4) (_5 <|> __5) (_6 <|> __6) (_7 <|> __7) (_8 <|> __8)


------------------------------------------------------------------------------
-- | Evaluates expression on a given call frame.
data EvaluateOnCallFrameResult = EvaluateOnCallFrameResult
    { -- | Object wrapper for the evaluation result.
      result :: !Runtime.RemoteObject
      -- | Exception details.
    , exceptionDetails :: !(P.Maybe Runtime.ExceptionDetails)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON EvaluateOnCallFrameResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "evaluateOnCallFrameResult" $ \_o -> EvaluateOnCallFrameResult
            <$> _o .: "result"
            <*> _o .:? "exceptionDetails"
        ago = A.withArray "evaluateOnCallFrameResult" $ \_a -> EvaluateOnCallFrameResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON EvaluateOnCallFrameResult where
    toEncoding (EvaluateOnCallFrameResult _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "result" .= _0
        , ("exceptionDetails" .=) <$> _1
        ]
    toJSON (EvaluateOnCallFrameResult _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "result" .= _0
        , ("exceptionDetails" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup EvaluateOnCallFrameResult where
    EvaluateOnCallFrameResult _0 _1 <> EvaluateOnCallFrameResult _ __1 = EvaluateOnCallFrameResult _0 (_1 <|> __1)


------------------------------------------------------------------------------
instance M.Method EvaluateOnCallFrame where
    type Result EvaluateOnCallFrame = EvaluateOnCallFrameResult
    name _ = "Debugger.evaluateOnCallFrame"


------------------------------------------------------------------------------
-- | Evaluates expression on a given call frame.
evaluateOnCallFrame
    :: CallFrameId
    -- ^ Call frame identifier to evaluate on.

    -> T.Text
    -- ^ Expression to evaluate.

    -> EvaluateOnCallFrame
evaluateOnCallFrame _0 _1 = EvaluateOnCallFrame _0 _1 P.empty P.empty P.empty P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Returns possible locations for breakpoint. scriptId in start and end range locations should be
-- the same.
data GetPossibleBreakpoints = GetPossibleBreakpoints
    { -- | Start of range to search possible breakpoint locations in.
      start :: !Location
      -- | End of range to search possible breakpoint locations in (excluding). When not specified, end
      -- of scripts is used as end of range.
    , end :: !(P.Maybe Location)
      -- | Only consider locations which are in the same (non-nested) function as start.
    , restrictToFunction :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetPossibleBreakpoints where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getPossibleBreakpoints" $ \_o -> GetPossibleBreakpoints
            <$> _o .: "start"
            <*> _o .:? "end"
            <*> _o .:? "restrictToFunction"
        ago = A.withArray "getPossibleBreakpoints" $ \_a -> GetPossibleBreakpoints
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON GetPossibleBreakpoints where
    toEncoding (GetPossibleBreakpoints _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "start" .= _0
        , ("end" .=) <$> _1
        , ("restrictToFunction" .=) <$> _2
        ]
    toJSON (GetPossibleBreakpoints _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "start" .= _0
        , ("end" .=) <$> _1
        , ("restrictToFunction" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetPossibleBreakpoints where
    GetPossibleBreakpoints _0 _1 _2 <> GetPossibleBreakpoints _ __1 __2 = GetPossibleBreakpoints _0 (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
-- | Returns possible locations for breakpoint. scriptId in start and end range locations should be
-- the same.
data GetPossibleBreakpointsResult = GetPossibleBreakpointsResult
    { -- | List of the possible breakpoint locations.
      locations :: ![BreakLocation]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetPossibleBreakpointsResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getPossibleBreakpointsResult" $ \_o -> GetPossibleBreakpointsResult
            <$> _o .: "locations"
        ago = A.withArray "getPossibleBreakpointsResult" $ \_a -> GetPossibleBreakpointsResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetPossibleBreakpointsResult where
    toEncoding (GetPossibleBreakpointsResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "locations" .= _0
        ]
    toJSON (GetPossibleBreakpointsResult _0) = A.object $ P.catMaybes
        [ P.pure $ "locations" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetPossibleBreakpointsResult where
    GetPossibleBreakpointsResult _0 <> GetPossibleBreakpointsResult _ = GetPossibleBreakpointsResult _0


------------------------------------------------------------------------------
instance M.Method GetPossibleBreakpoints where
    type Result GetPossibleBreakpoints = GetPossibleBreakpointsResult
    name _ = "Debugger.getPossibleBreakpoints"


------------------------------------------------------------------------------
-- | Returns possible locations for breakpoint. scriptId in start and end range locations should be
-- the same.
getPossibleBreakpoints
    :: Location
    -- ^ Start of range to search possible breakpoint locations in.

    -> GetPossibleBreakpoints
getPossibleBreakpoints _0 = GetPossibleBreakpoints _0 P.empty P.empty


------------------------------------------------------------------------------
-- | Returns source for the script with given id.
data GetScriptSource = GetScriptSource
    { -- | Id of the script to get source for.
      scriptId :: !Runtime.ScriptId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetScriptSource where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getScriptSource" $ \_o -> GetScriptSource
            <$> _o .: "scriptId"
        ago = A.withArray "getScriptSource" $ \_a -> GetScriptSource
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetScriptSource where
    toEncoding (GetScriptSource _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "scriptId" .= _0
        ]
    toJSON (GetScriptSource _0) = A.object $ P.catMaybes
        [ P.pure $ "scriptId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetScriptSource where
    GetScriptSource _0 <> GetScriptSource _ = GetScriptSource _0


------------------------------------------------------------------------------
-- | Returns source for the script with given id.
data GetScriptSourceResult = GetScriptSourceResult
    { -- | Script source.
      scriptSource :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetScriptSourceResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getScriptSourceResult" $ \_o -> GetScriptSourceResult
            <$> _o .: "scriptSource"
        ago = A.withArray "getScriptSourceResult" $ \_a -> GetScriptSourceResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetScriptSourceResult where
    toEncoding (GetScriptSourceResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "scriptSource" .= _0
        ]
    toJSON (GetScriptSourceResult _0) = A.object $ P.catMaybes
        [ P.pure $ "scriptSource" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetScriptSourceResult where
    GetScriptSourceResult _0 <> GetScriptSourceResult _ = GetScriptSourceResult _0


------------------------------------------------------------------------------
instance M.Method GetScriptSource where
    type Result GetScriptSource = GetScriptSourceResult
    name _ = "Debugger.getScriptSource"


------------------------------------------------------------------------------
-- | Returns source for the script with given id.
getScriptSource
    :: Runtime.ScriptId
    -- ^ Id of the script to get source for.

    -> GetScriptSource
getScriptSource _0 = GetScriptSource _0


------------------------------------------------------------------------------
-- | Returns stack trace with given @stackTraceId@.
{-# WARNING GetStackTrace "This feature is marked as EXPERIMENTAL." #-}
data GetStackTrace = GetStackTrace
    { stackTraceId :: !Runtime.StackTraceId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetStackTrace where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getStackTrace" $ \_o -> GetStackTrace
            <$> _o .: "stackTraceId"
        ago = A.withArray "getStackTrace" $ \_a -> GetStackTrace
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetStackTrace where
    toEncoding (GetStackTrace _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "stackTraceId" .= _0
        ]
    toJSON (GetStackTrace _0) = A.object $ P.catMaybes
        [ P.pure $ "stackTraceId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetStackTrace where
    GetStackTrace _0 <> GetStackTrace _ = GetStackTrace _0


------------------------------------------------------------------------------
-- | Returns stack trace with given @stackTraceId@.
{-# WARNING GetStackTraceResult "This feature is marked as EXPERIMENTAL." #-}
data GetStackTraceResult = GetStackTraceResult
    { stackTrace :: !Runtime.StackTrace
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetStackTraceResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getStackTraceResult" $ \_o -> GetStackTraceResult
            <$> _o .: "stackTrace"
        ago = A.withArray "getStackTraceResult" $ \_a -> GetStackTraceResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetStackTraceResult where
    toEncoding (GetStackTraceResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "stackTrace" .= _0
        ]
    toJSON (GetStackTraceResult _0) = A.object $ P.catMaybes
        [ P.pure $ "stackTrace" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetStackTraceResult where
    GetStackTraceResult _0 <> GetStackTraceResult _ = GetStackTraceResult _0


------------------------------------------------------------------------------
instance M.Method GetStackTrace where
    type Result GetStackTrace = GetStackTraceResult
    name _ = "Debugger.getStackTrace"


------------------------------------------------------------------------------
-- | Returns stack trace with given @stackTraceId@.
{-# WARNING getStackTrace "This feature is marked as EXPERIMENTAL." #-}
getStackTrace
    :: Runtime.StackTraceId
    -> GetStackTrace
getStackTrace _0 = GetStackTrace _0


------------------------------------------------------------------------------
-- | Stops on the next JavaScript statement.
data Pause = Pause
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Pause where
    parseJSON A.Null = P.pure Pause
    parseJSON v = A.withArray "pause" go v
        <|> A.withObject "pause" go v
      where
        go _ = P.pure Pause


------------------------------------------------------------------------------
instance A.ToJSON Pause where
    toEncoding Pause = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON Pause = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup Pause where
    Pause <> Pause = Pause


------------------------------------------------------------------------------
instance P.Monoid Pause where
    mempty = Pause


------------------------------------------------------------------------------
instance M.Method Pause where
    type Result Pause = ()
    name _ = "Debugger.pause"


------------------------------------------------------------------------------
-- | Stops on the next JavaScript statement.
pause
    :: Pause
pause = Pause


------------------------------------------------------------------------------
{-# WARNING PauseOnAsyncCall "This feature is marked as EXPERIMENTAL." #-}
data PauseOnAsyncCall = PauseOnAsyncCall
    { -- | Debugger will pause when async call with given stack trace is started.
      parentStackTraceId :: !Runtime.StackTraceId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PauseOnAsyncCall where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "pauseOnAsyncCall" $ \_o -> PauseOnAsyncCall
            <$> _o .: "parentStackTraceId"
        ago = A.withArray "pauseOnAsyncCall" $ \_a -> PauseOnAsyncCall
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON PauseOnAsyncCall where
    toEncoding (PauseOnAsyncCall _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "parentStackTraceId" .= _0
        ]
    toJSON (PauseOnAsyncCall _0) = A.object $ P.catMaybes
        [ P.pure $ "parentStackTraceId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup PauseOnAsyncCall where
    PauseOnAsyncCall _0 <> PauseOnAsyncCall _ = PauseOnAsyncCall _0


------------------------------------------------------------------------------
instance M.Method PauseOnAsyncCall where
    type Result PauseOnAsyncCall = ()
    name _ = "Debugger.pauseOnAsyncCall"


------------------------------------------------------------------------------
{-# WARNING pauseOnAsyncCall "This feature is marked as EXPERIMENTAL." #-}
pauseOnAsyncCall
    :: Runtime.StackTraceId
    -- ^ Debugger will pause when async call with given stack trace is started.

    -> PauseOnAsyncCall
pauseOnAsyncCall _0 = PauseOnAsyncCall _0


------------------------------------------------------------------------------
-- | Removes JavaScript breakpoint.
data RemoveBreakpoint = RemoveBreakpoint
    { breakpointId :: !BreakpointId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RemoveBreakpoint where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "removeBreakpoint" $ \_o -> RemoveBreakpoint
            <$> _o .: "breakpointId"
        ago = A.withArray "removeBreakpoint" $ \_a -> RemoveBreakpoint
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON RemoveBreakpoint where
    toEncoding (RemoveBreakpoint _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "breakpointId" .= _0
        ]
    toJSON (RemoveBreakpoint _0) = A.object $ P.catMaybes
        [ P.pure $ "breakpointId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup RemoveBreakpoint where
    RemoveBreakpoint _0 <> RemoveBreakpoint _ = RemoveBreakpoint _0


------------------------------------------------------------------------------
instance M.Method RemoveBreakpoint where
    type Result RemoveBreakpoint = ()
    name _ = "Debugger.removeBreakpoint"


------------------------------------------------------------------------------
-- | Removes JavaScript breakpoint.
removeBreakpoint
    :: BreakpointId
    -> RemoveBreakpoint
removeBreakpoint _0 = RemoveBreakpoint _0


------------------------------------------------------------------------------
-- | Restarts particular call frame from the beginning.
data RestartFrame = RestartFrame
    { -- | Call frame identifier to evaluate on.
      callFrameId :: !CallFrameId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RestartFrame where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "restartFrame" $ \_o -> RestartFrame
            <$> _o .: "callFrameId"
        ago = A.withArray "restartFrame" $ \_a -> RestartFrame
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON RestartFrame where
    toEncoding (RestartFrame _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "callFrameId" .= _0
        ]
    toJSON (RestartFrame _0) = A.object $ P.catMaybes
        [ P.pure $ "callFrameId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup RestartFrame where
    RestartFrame _0 <> RestartFrame _ = RestartFrame _0


------------------------------------------------------------------------------
-- | Restarts particular call frame from the beginning.
{-# WARNING asyncStackTraceId "This feature is marked as EXPERIMENTAL." #-}
data RestartFrameResult = RestartFrameResult
    { -- | New stack trace.
      callFrames :: ![CallFrame]
      -- | Async stack trace, if any.
    , asyncStackTrace :: !(P.Maybe Runtime.StackTrace)
      -- | Async stack trace, if any.
    , asyncStackTraceId :: !(P.Maybe Runtime.StackTraceId)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RestartFrameResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "restartFrameResult" $ \_o -> RestartFrameResult
            <$> _o .: "callFrames"
            <*> _o .:? "asyncStackTrace"
            <*> _o .:? "asyncStackTraceId"
        ago = A.withArray "restartFrameResult" $ \_a -> RestartFrameResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON RestartFrameResult where
    toEncoding (RestartFrameResult _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "callFrames" .= _0
        , ("asyncStackTrace" .=) <$> _1
        , ("asyncStackTraceId" .=) <$> _2
        ]
    toJSON (RestartFrameResult _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "callFrames" .= _0
        , ("asyncStackTrace" .=) <$> _1
        , ("asyncStackTraceId" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup RestartFrameResult where
    RestartFrameResult _0 _1 _2 <> RestartFrameResult _ __1 __2 = RestartFrameResult _0 (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
instance M.Method RestartFrame where
    type Result RestartFrame = RestartFrameResult
    name _ = "Debugger.restartFrame"


------------------------------------------------------------------------------
-- | Restarts particular call frame from the beginning.
restartFrame
    :: CallFrameId
    -- ^ Call frame identifier to evaluate on.

    -> RestartFrame
restartFrame _0 = RestartFrame _0


------------------------------------------------------------------------------
-- | Resumes JavaScript execution.
data Resume = Resume
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Resume where
    parseJSON A.Null = P.pure Resume
    parseJSON v = A.withArray "resume" go v
        <|> A.withObject "resume" go v
      where
        go _ = P.pure Resume


------------------------------------------------------------------------------
instance A.ToJSON Resume where
    toEncoding Resume = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON Resume = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup Resume where
    Resume <> Resume = Resume


------------------------------------------------------------------------------
instance P.Monoid Resume where
    mempty = Resume


------------------------------------------------------------------------------
instance M.Method Resume where
    type Result Resume = ()
    name _ = "Debugger.resume"


------------------------------------------------------------------------------
-- | Resumes JavaScript execution.
resume
    :: Resume
resume = Resume


------------------------------------------------------------------------------
-- | Searches for given string in script content.
data SearchInContent = SearchInContent
    { -- | Id of the script to search in.
      scriptId :: !Runtime.ScriptId
      -- | String to search for.
    , query :: !T.Text
      -- | If true, search is case sensitive.
    , caseSensitive :: !(P.Maybe P.Bool)
      -- | If true, treats string parameter as regex.
    , isRegex :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SearchInContent where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "searchInContent" $ \_o -> SearchInContent
            <$> _o .: "scriptId"
            <*> _o .: "query"
            <*> _o .:? "caseSensitive"
            <*> _o .:? "isRegex"
        ago = A.withArray "searchInContent" $ \_a -> SearchInContent
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON SearchInContent where
    toEncoding (SearchInContent _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "scriptId" .= _0
        , P.pure $ "query" .= _1
        , ("caseSensitive" .=) <$> _2
        , ("isRegex" .=) <$> _3
        ]
    toJSON (SearchInContent _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "scriptId" .= _0
        , P.pure $ "query" .= _1
        , ("caseSensitive" .=) <$> _2
        , ("isRegex" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup SearchInContent where
    SearchInContent _0 _1 _2 _3 <> SearchInContent _ _ __2 __3 = SearchInContent _0 _1 (_2 <|> __2) (_3 <|> __3)


------------------------------------------------------------------------------
-- | Searches for given string in script content.
data SearchInContentResult = SearchInContentResult
    { -- | List of search matches.
      result :: ![SearchMatch]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SearchInContentResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "searchInContentResult" $ \_o -> SearchInContentResult
            <$> _o .: "result"
        ago = A.withArray "searchInContentResult" $ \_a -> SearchInContentResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SearchInContentResult where
    toEncoding (SearchInContentResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "result" .= _0
        ]
    toJSON (SearchInContentResult _0) = A.object $ P.catMaybes
        [ P.pure $ "result" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SearchInContentResult where
    SearchInContentResult _0 <> SearchInContentResult _ = SearchInContentResult _0


------------------------------------------------------------------------------
instance M.Method SearchInContent where
    type Result SearchInContent = SearchInContentResult
    name _ = "Debugger.searchInContent"


------------------------------------------------------------------------------
-- | Searches for given string in script content.
searchInContent
    :: Runtime.ScriptId
    -- ^ Id of the script to search in.

    -> T.Text
    -- ^ String to search for.

    -> SearchInContent
searchInContent _0 _1 = SearchInContent _0 _1 P.empty P.empty


------------------------------------------------------------------------------
-- | Enables or disables async call stacks tracking.
data SetAsyncCallStackDepth = SetAsyncCallStackDepth
    { -- | Maximum depth of async call stacks. Setting to @0@ will effectively disable collecting async
      -- call stacks (default).
      maxDepth :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetAsyncCallStackDepth where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setAsyncCallStackDepth" $ \_o -> SetAsyncCallStackDepth
            <$> _o .: "maxDepth"
        ago = A.withArray "setAsyncCallStackDepth" $ \_a -> SetAsyncCallStackDepth
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetAsyncCallStackDepth where
    toEncoding (SetAsyncCallStackDepth _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "maxDepth" .= _0
        ]
    toJSON (SetAsyncCallStackDepth _0) = A.object $ P.catMaybes
        [ P.pure $ "maxDepth" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetAsyncCallStackDepth where
    SetAsyncCallStackDepth _0 <> SetAsyncCallStackDepth _ = SetAsyncCallStackDepth _0


------------------------------------------------------------------------------
instance M.Method SetAsyncCallStackDepth where
    type Result SetAsyncCallStackDepth = ()
    name _ = "Debugger.setAsyncCallStackDepth"


------------------------------------------------------------------------------
-- | Enables or disables async call stacks tracking.
setAsyncCallStackDepth
    :: P.Int
    -- ^ Maximum depth of async call stacks. Setting to @0@ will effectively disable collecting async

    -- call stacks (default).

    -> SetAsyncCallStackDepth
setAsyncCallStackDepth _0 = SetAsyncCallStackDepth _0


------------------------------------------------------------------------------
-- | Replace previous blackbox patterns with passed ones. Forces backend to skip stepping\/pausing in
-- scripts with url matching one of the patterns. VM will try to leave blackboxed script by
-- performing 'step in' several times, finally resorting to 'step out' if unsuccessful.
{-# WARNING SetBlackboxPatterns "This feature is marked as EXPERIMENTAL." #-}
data SetBlackboxPatterns = SetBlackboxPatterns
    { -- | Array of regexps that will be used to check script url for blackbox state.
      patterns :: ![T.Text]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetBlackboxPatterns where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setBlackboxPatterns" $ \_o -> SetBlackboxPatterns
            <$> _o .: "patterns"
        ago = A.withArray "setBlackboxPatterns" $ \_a -> SetBlackboxPatterns
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetBlackboxPatterns where
    toEncoding (SetBlackboxPatterns _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "patterns" .= _0
        ]
    toJSON (SetBlackboxPatterns _0) = A.object $ P.catMaybes
        [ P.pure $ "patterns" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetBlackboxPatterns where
    SetBlackboxPatterns _0 <> SetBlackboxPatterns _ = SetBlackboxPatterns _0


------------------------------------------------------------------------------
instance M.Method SetBlackboxPatterns where
    type Result SetBlackboxPatterns = ()
    name _ = "Debugger.setBlackboxPatterns"


------------------------------------------------------------------------------
-- | Replace previous blackbox patterns with passed ones. Forces backend to skip stepping\/pausing in
-- scripts with url matching one of the patterns. VM will try to leave blackboxed script by
-- performing 'step in' several times, finally resorting to 'step out' if unsuccessful.
{-# WARNING setBlackboxPatterns "This feature is marked as EXPERIMENTAL." #-}
setBlackboxPatterns
    :: [T.Text]
    -- ^ Array of regexps that will be used to check script url for blackbox state.

    -> SetBlackboxPatterns
setBlackboxPatterns _0 = SetBlackboxPatterns _0


------------------------------------------------------------------------------
-- | Makes backend skip steps in the script in blackboxed ranges. VM will try leave blacklisted
-- scripts by performing 'step in' several times, finally resorting to 'step out' if unsuccessful.
-- Positions array contains positions where blackbox state is changed. First interval isn't
-- blackboxed. Array should be sorted.
{-# WARNING SetBlackboxedRanges "This feature is marked as EXPERIMENTAL." #-}
data SetBlackboxedRanges = SetBlackboxedRanges
    { -- | Id of the script.
      scriptId :: !Runtime.ScriptId
    , positions :: ![ScriptPosition]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetBlackboxedRanges where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setBlackboxedRanges" $ \_o -> SetBlackboxedRanges
            <$> _o .: "scriptId"
            <*> _o .: "positions"
        ago = A.withArray "setBlackboxedRanges" $ \_a -> SetBlackboxedRanges
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SetBlackboxedRanges where
    toEncoding (SetBlackboxedRanges _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "scriptId" .= _0
        , P.pure $ "positions" .= _1
        ]
    toJSON (SetBlackboxedRanges _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "scriptId" .= _0
        , P.pure $ "positions" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetBlackboxedRanges where
    SetBlackboxedRanges _0 _1 <> SetBlackboxedRanges _ _ = SetBlackboxedRanges _0 _1


------------------------------------------------------------------------------
instance M.Method SetBlackboxedRanges where
    type Result SetBlackboxedRanges = ()
    name _ = "Debugger.setBlackboxedRanges"


------------------------------------------------------------------------------
-- | Makes backend skip steps in the script in blackboxed ranges. VM will try leave blacklisted
-- scripts by performing 'step in' several times, finally resorting to 'step out' if unsuccessful.
-- Positions array contains positions where blackbox state is changed. First interval isn't
-- blackboxed. Array should be sorted.
{-# WARNING setBlackboxedRanges "This feature is marked as EXPERIMENTAL." #-}
setBlackboxedRanges
    :: Runtime.ScriptId
    -- ^ Id of the script.

    -> [ScriptPosition]
    -> SetBlackboxedRanges
setBlackboxedRanges _0 _1 = SetBlackboxedRanges _0 _1


------------------------------------------------------------------------------
-- | Sets JavaScript breakpoint at a given location.
data SetBreakpoint = SetBreakpoint
    { -- | Location to set breakpoint in.
      location :: !Location
      -- | Expression to use as a breakpoint condition. When specified, debugger will only stop on the
      -- breakpoint if this expression evaluates to true.
    , condition :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetBreakpoint where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setBreakpoint" $ \_o -> SetBreakpoint
            <$> _o .: "location"
            <*> _o .:? "condition"
        ago = A.withArray "setBreakpoint" $ \_a -> SetBreakpoint
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SetBreakpoint where
    toEncoding (SetBreakpoint _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "location" .= _0
        , ("condition" .=) <$> _1
        ]
    toJSON (SetBreakpoint _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "location" .= _0
        , ("condition" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetBreakpoint where
    SetBreakpoint _0 _1 <> SetBreakpoint _ __1 = SetBreakpoint _0 (_1 <|> __1)


------------------------------------------------------------------------------
-- | Sets JavaScript breakpoint at a given location.
data SetBreakpointResult = SetBreakpointResult
    { -- | Id of the created breakpoint for further reference.
      breakpointId :: !BreakpointId
      -- | Location this breakpoint resolved into.
    , actualLocation :: !Location
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetBreakpointResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setBreakpointResult" $ \_o -> SetBreakpointResult
            <$> _o .: "breakpointId"
            <*> _o .: "actualLocation"
        ago = A.withArray "setBreakpointResult" $ \_a -> SetBreakpointResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SetBreakpointResult where
    toEncoding (SetBreakpointResult _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "breakpointId" .= _0
        , P.pure $ "actualLocation" .= _1
        ]
    toJSON (SetBreakpointResult _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "breakpointId" .= _0
        , P.pure $ "actualLocation" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetBreakpointResult where
    SetBreakpointResult _0 _1 <> SetBreakpointResult _ _ = SetBreakpointResult _0 _1


------------------------------------------------------------------------------
instance M.Method SetBreakpoint where
    type Result SetBreakpoint = SetBreakpointResult
    name _ = "Debugger.setBreakpoint"


------------------------------------------------------------------------------
-- | Sets JavaScript breakpoint at a given location.
setBreakpoint
    :: Location
    -- ^ Location to set breakpoint in.

    -> SetBreakpoint
setBreakpoint _0 = SetBreakpoint _0 P.empty


------------------------------------------------------------------------------
-- | Sets JavaScript breakpoint at given location specified either by URL or URL regex. Once this
-- command is issued, all existing parsed scripts will have breakpoints resolved and returned in
-- @locations@ property. Further matching script parsing will result in subsequent
-- @breakpointResolved@ events issued. This logical breakpoint will survive page reloads.
data SetBreakpointByUrl = SetBreakpointByUrl
    { -- | Line number to set breakpoint at.
      lineNumber :: !P.Int
      -- | URL of the resources to set breakpoint on.
    , url :: !(P.Maybe T.Text)
      -- | Regex pattern for the URLs of the resources to set breakpoints on. Either @url@ or
      -- @urlRegex@ must be specified.
    , urlRegex :: !(P.Maybe T.Text)
      -- | Script hash of the resources to set breakpoint on.
    , scriptHash :: !(P.Maybe T.Text)
      -- | Offset in the line to set breakpoint at.
    , columnNumber :: !(P.Maybe P.Int)
      -- | Expression to use as a breakpoint condition. When specified, debugger will only stop on the
      -- breakpoint if this expression evaluates to true.
    , condition :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetBreakpointByUrl where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setBreakpointByUrl" $ \_o -> SetBreakpointByUrl
            <$> _o .: "lineNumber"
            <*> _o .:? "url"
            <*> _o .:? "urlRegex"
            <*> _o .:? "scriptHash"
            <*> _o .:? "columnNumber"
            <*> _o .:? "condition"
        ago = A.withArray "setBreakpointByUrl" $ \_a -> SetBreakpointByUrl
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)


------------------------------------------------------------------------------
instance A.ToJSON SetBreakpointByUrl where
    toEncoding (SetBreakpointByUrl _0 _1 _2 _3 _4 _5) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "lineNumber" .= _0
        , ("url" .=) <$> _1
        , ("urlRegex" .=) <$> _2
        , ("scriptHash" .=) <$> _3
        , ("columnNumber" .=) <$> _4
        , ("condition" .=) <$> _5
        ]
    toJSON (SetBreakpointByUrl _0 _1 _2 _3 _4 _5) = A.object $ P.catMaybes
        [ P.pure $ "lineNumber" .= _0
        , ("url" .=) <$> _1
        , ("urlRegex" .=) <$> _2
        , ("scriptHash" .=) <$> _3
        , ("columnNumber" .=) <$> _4
        , ("condition" .=) <$> _5
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetBreakpointByUrl where
    SetBreakpointByUrl _0 _1 _2 _3 _4 _5 <> SetBreakpointByUrl _ __1 __2 __3 __4 __5 = SetBreakpointByUrl _0 (_1 <|> __1) (_2 <|> __2) (_3 <|> __3) (_4 <|> __4) (_5 <|> __5)


------------------------------------------------------------------------------
-- | Sets JavaScript breakpoint at given location specified either by URL or URL regex. Once this
-- command is issued, all existing parsed scripts will have breakpoints resolved and returned in
-- @locations@ property. Further matching script parsing will result in subsequent
-- @breakpointResolved@ events issued. This logical breakpoint will survive page reloads.
data SetBreakpointByUrlResult = SetBreakpointByUrlResult
    { -- | Id of the created breakpoint for further reference.
      breakpointId :: !BreakpointId
      -- | List of the locations this breakpoint resolved into upon addition.
    , locations :: ![Location]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetBreakpointByUrlResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setBreakpointByUrlResult" $ \_o -> SetBreakpointByUrlResult
            <$> _o .: "breakpointId"
            <*> _o .: "locations"
        ago = A.withArray "setBreakpointByUrlResult" $ \_a -> SetBreakpointByUrlResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SetBreakpointByUrlResult where
    toEncoding (SetBreakpointByUrlResult _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "breakpointId" .= _0
        , P.pure $ "locations" .= _1
        ]
    toJSON (SetBreakpointByUrlResult _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "breakpointId" .= _0
        , P.pure $ "locations" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetBreakpointByUrlResult where
    SetBreakpointByUrlResult _0 _1 <> SetBreakpointByUrlResult _ _ = SetBreakpointByUrlResult _0 _1


------------------------------------------------------------------------------
instance M.Method SetBreakpointByUrl where
    type Result SetBreakpointByUrl = SetBreakpointByUrlResult
    name _ = "Debugger.setBreakpointByUrl"


------------------------------------------------------------------------------
-- | Sets JavaScript breakpoint at given location specified either by URL or URL regex. Once this
-- command is issued, all existing parsed scripts will have breakpoints resolved and returned in
-- @locations@ property. Further matching script parsing will result in subsequent
-- @breakpointResolved@ events issued. This logical breakpoint will survive page reloads.
setBreakpointByUrl
    :: P.Int
    -- ^ Line number to set breakpoint at.

    -> SetBreakpointByUrl
setBreakpointByUrl _0 = SetBreakpointByUrl _0 P.empty P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Sets JavaScript breakpoint before each call to the given function.
-- If another function was created from the same source as a given one,
-- calling it will also trigger the breakpoint.
{-# WARNING SetBreakpointOnFunctionCall "This feature is marked as EXPERIMENTAL." #-}
data SetBreakpointOnFunctionCall = SetBreakpointOnFunctionCall
    { -- | Function object id.
      objectId :: !Runtime.RemoteObjectId
      -- | Expression to use as a breakpoint condition. When specified, debugger will
      -- stop on the breakpoint if this expression evaluates to true.
    , condition :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetBreakpointOnFunctionCall where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setBreakpointOnFunctionCall" $ \_o -> SetBreakpointOnFunctionCall
            <$> _o .: "objectId"
            <*> _o .:? "condition"
        ago = A.withArray "setBreakpointOnFunctionCall" $ \_a -> SetBreakpointOnFunctionCall
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SetBreakpointOnFunctionCall where
    toEncoding (SetBreakpointOnFunctionCall _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "objectId" .= _0
        , ("condition" .=) <$> _1
        ]
    toJSON (SetBreakpointOnFunctionCall _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "objectId" .= _0
        , ("condition" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetBreakpointOnFunctionCall where
    SetBreakpointOnFunctionCall _0 _1 <> SetBreakpointOnFunctionCall _ __1 = SetBreakpointOnFunctionCall _0 (_1 <|> __1)


------------------------------------------------------------------------------
-- | Sets JavaScript breakpoint before each call to the given function.
-- If another function was created from the same source as a given one,
-- calling it will also trigger the breakpoint.
{-# WARNING SetBreakpointOnFunctionCallResult "This feature is marked as EXPERIMENTAL." #-}
data SetBreakpointOnFunctionCallResult = SetBreakpointOnFunctionCallResult
    { -- | Id of the created breakpoint for further reference.
      breakpointId :: !BreakpointId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetBreakpointOnFunctionCallResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setBreakpointOnFunctionCallResult" $ \_o -> SetBreakpointOnFunctionCallResult
            <$> _o .: "breakpointId"
        ago = A.withArray "setBreakpointOnFunctionCallResult" $ \_a -> SetBreakpointOnFunctionCallResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetBreakpointOnFunctionCallResult where
    toEncoding (SetBreakpointOnFunctionCallResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "breakpointId" .= _0
        ]
    toJSON (SetBreakpointOnFunctionCallResult _0) = A.object $ P.catMaybes
        [ P.pure $ "breakpointId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetBreakpointOnFunctionCallResult where
    SetBreakpointOnFunctionCallResult _0 <> SetBreakpointOnFunctionCallResult _ = SetBreakpointOnFunctionCallResult _0


------------------------------------------------------------------------------
instance M.Method SetBreakpointOnFunctionCall where
    type Result SetBreakpointOnFunctionCall = SetBreakpointOnFunctionCallResult
    name _ = "Debugger.setBreakpointOnFunctionCall"


------------------------------------------------------------------------------
-- | Sets JavaScript breakpoint before each call to the given function.
-- If another function was created from the same source as a given one,
-- calling it will also trigger the breakpoint.
{-# WARNING setBreakpointOnFunctionCall "This feature is marked as EXPERIMENTAL." #-}
setBreakpointOnFunctionCall
    :: Runtime.RemoteObjectId
    -- ^ Function object id.

    -> SetBreakpointOnFunctionCall
setBreakpointOnFunctionCall _0 = SetBreakpointOnFunctionCall _0 P.empty


------------------------------------------------------------------------------
-- | Activates \/ deactivates all breakpoints on the page.
data SetBreakpointsActive = SetBreakpointsActive
    { -- | New value for breakpoints active state.
      active :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetBreakpointsActive where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setBreakpointsActive" $ \_o -> SetBreakpointsActive
            <$> _o .: "active"
        ago = A.withArray "setBreakpointsActive" $ \_a -> SetBreakpointsActive
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetBreakpointsActive where
    toEncoding (SetBreakpointsActive _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "active" .= _0
        ]
    toJSON (SetBreakpointsActive _0) = A.object $ P.catMaybes
        [ P.pure $ "active" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetBreakpointsActive where
    SetBreakpointsActive _0 <> SetBreakpointsActive _ = SetBreakpointsActive _0


------------------------------------------------------------------------------
instance M.Method SetBreakpointsActive where
    type Result SetBreakpointsActive = ()
    name _ = "Debugger.setBreakpointsActive"


------------------------------------------------------------------------------
-- | Activates \/ deactivates all breakpoints on the page.
setBreakpointsActive
    :: P.Bool
    -- ^ New value for breakpoints active state.

    -> SetBreakpointsActive
setBreakpointsActive _0 = SetBreakpointsActive _0


------------------------------------------------------------------------------
-- | Defines pause on exceptions state. Can be set to stop on all exceptions, uncaught exceptions or
-- no exceptions. Initial pause on exceptions state is @none@.
data SetPauseOnExceptions = SetPauseOnExceptions
    { -- | Pause on exceptions mode.
      state :: !State
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetPauseOnExceptions where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setPauseOnExceptions" $ \_o -> SetPauseOnExceptions
            <$> _o .: "state"
        ago = A.withArray "setPauseOnExceptions" $ \_a -> SetPauseOnExceptions
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetPauseOnExceptions where
    toEncoding (SetPauseOnExceptions _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "state" .= _0
        ]
    toJSON (SetPauseOnExceptions _0) = A.object $ P.catMaybes
        [ P.pure $ "state" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetPauseOnExceptions where
    SetPauseOnExceptions _0 <> SetPauseOnExceptions _ = SetPauseOnExceptions _0


------------------------------------------------------------------------------
data State
    = None
    | Uncaught
    | All
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON State where
    parseJSON = A.withText "State" $ \t -> case t of
        "none" -> P.pure None
        "uncaught" -> P.pure Uncaught
        "all" -> P.pure All
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON State where
    toJSON None = "none"
    toJSON Uncaught = "uncaught"
    toJSON All = "all"


------------------------------------------------------------------------------
instance M.Method SetPauseOnExceptions where
    type Result SetPauseOnExceptions = ()
    name _ = "Debugger.setPauseOnExceptions"


------------------------------------------------------------------------------
-- | Defines pause on exceptions state. Can be set to stop on all exceptions, uncaught exceptions or
-- no exceptions. Initial pause on exceptions state is @none@.
setPauseOnExceptions
    :: State
    -- ^ Pause on exceptions mode.

    -> SetPauseOnExceptions
setPauseOnExceptions _0 = SetPauseOnExceptions _0


------------------------------------------------------------------------------
-- | Changes return value in top frame. Available only at return break position.
{-# WARNING SetReturnValue "This feature is marked as EXPERIMENTAL." #-}
data SetReturnValue = SetReturnValue
    { -- | New return value.
      newValue :: !Runtime.CallArgument
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetReturnValue where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setReturnValue" $ \_o -> SetReturnValue
            <$> _o .: "newValue"
        ago = A.withArray "setReturnValue" $ \_a -> SetReturnValue
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetReturnValue where
    toEncoding (SetReturnValue _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "newValue" .= _0
        ]
    toJSON (SetReturnValue _0) = A.object $ P.catMaybes
        [ P.pure $ "newValue" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetReturnValue where
    SetReturnValue _0 <> SetReturnValue _ = SetReturnValue _0


------------------------------------------------------------------------------
instance M.Method SetReturnValue where
    type Result SetReturnValue = ()
    name _ = "Debugger.setReturnValue"


------------------------------------------------------------------------------
-- | Changes return value in top frame. Available only at return break position.
{-# WARNING setReturnValue "This feature is marked as EXPERIMENTAL." #-}
setReturnValue
    :: Runtime.CallArgument
    -- ^ New return value.

    -> SetReturnValue
setReturnValue _0 = SetReturnValue _0


------------------------------------------------------------------------------
-- | Edits JavaScript source live.
data SetScriptSource = SetScriptSource
    { -- | Id of the script to edit.
      scriptId :: !Runtime.ScriptId
      -- | New content of the script.
    , scriptSource :: !T.Text
      -- | If true the change will not actually be applied. Dry run may be used to get result
      -- description without actually modifying the code.
    , dryRun :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetScriptSource where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setScriptSource" $ \_o -> SetScriptSource
            <$> _o .: "scriptId"
            <*> _o .: "scriptSource"
            <*> _o .:? "dryRun"
        ago = A.withArray "setScriptSource" $ \_a -> SetScriptSource
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON SetScriptSource where
    toEncoding (SetScriptSource _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "scriptId" .= _0
        , P.pure $ "scriptSource" .= _1
        , ("dryRun" .=) <$> _2
        ]
    toJSON (SetScriptSource _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "scriptId" .= _0
        , P.pure $ "scriptSource" .= _1
        , ("dryRun" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetScriptSource where
    SetScriptSource _0 _1 _2 <> SetScriptSource _ _ __2 = SetScriptSource _0 _1 (_2 <|> __2)


------------------------------------------------------------------------------
-- | Edits JavaScript source live.
{-{-# WARNING asyncStackTraceId "This feature is marked as EXPERIMENTAL." #-}-}
data SetScriptSourceResult = SetScriptSourceResult
    { -- | New stack trace in case editing has happened while VM was stopped.
      callFrames :: !(P.Maybe [CallFrame])
      -- | Whether current call stack  was modified after applying the changes.
    , stackChanged :: !(P.Maybe P.Bool)
      -- | Async stack trace, if any.
    , asyncStackTrace :: !(P.Maybe Runtime.StackTrace)
      -- | Async stack trace, if any.
    , asyncStackTraceId :: !(P.Maybe Runtime.StackTraceId)
      -- | Exception details if any.
    , exceptionDetails :: !(P.Maybe Runtime.ExceptionDetails)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetScriptSourceResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setScriptSourceResult" $ \_o -> SetScriptSourceResult
            <$> _o .:? "callFrames"
            <*> _o .:? "stackChanged"
            <*> _o .:? "asyncStackTrace"
            <*> _o .:? "asyncStackTraceId"
            <*> _o .:? "exceptionDetails"
        ago = A.withArray "setScriptSourceResult" $ \_a -> SetScriptSourceResult
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON SetScriptSourceResult where
    toEncoding (SetScriptSourceResult _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ ("callFrames" .=) <$> _0
        , ("stackChanged" .=) <$> _1
        , ("asyncStackTrace" .=) <$> _2
        , ("asyncStackTraceId" .=) <$> _3
        , ("exceptionDetails" .=) <$> _4
        ]
    toJSON (SetScriptSourceResult _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ ("callFrames" .=) <$> _0
        , ("stackChanged" .=) <$> _1
        , ("asyncStackTrace" .=) <$> _2
        , ("asyncStackTraceId" .=) <$> _3
        , ("exceptionDetails" .=) <$> _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetScriptSourceResult where
    SetScriptSourceResult _0 _1 _2 _3 _4 <> SetScriptSourceResult __0 __1 __2 __3 __4 = SetScriptSourceResult (_0 <|> __0) (_1 <|> __1) (_2 <|> __2) (_3 <|> __3) (_4 <|> __4)


------------------------------------------------------------------------------
instance P.Monoid SetScriptSourceResult where
    mempty = SetScriptSourceResult P.empty P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
instance M.Method SetScriptSource where
    type Result SetScriptSource = SetScriptSourceResult
    name _ = "Debugger.setScriptSource"


------------------------------------------------------------------------------
-- | Edits JavaScript source live.
setScriptSource
    :: Runtime.ScriptId
    -- ^ Id of the script to edit.

    -> T.Text
    -- ^ New content of the script.

    -> SetScriptSource
setScriptSource _0 _1 = SetScriptSource _0 _1 P.empty


------------------------------------------------------------------------------
-- | Makes page not interrupt on any pauses (breakpoint, exception, dom exception etc).
data SetSkipAllPauses = SetSkipAllPauses
    { -- | New value for skip pauses state.
      skip :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetSkipAllPauses where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setSkipAllPauses" $ \_o -> SetSkipAllPauses
            <$> _o .: "skip"
        ago = A.withArray "setSkipAllPauses" $ \_a -> SetSkipAllPauses
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetSkipAllPauses where
    toEncoding (SetSkipAllPauses _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "skip" .= _0
        ]
    toJSON (SetSkipAllPauses _0) = A.object $ P.catMaybes
        [ P.pure $ "skip" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetSkipAllPauses where
    SetSkipAllPauses _0 <> SetSkipAllPauses _ = SetSkipAllPauses _0


------------------------------------------------------------------------------
instance M.Method SetSkipAllPauses where
    type Result SetSkipAllPauses = ()
    name _ = "Debugger.setSkipAllPauses"


------------------------------------------------------------------------------
-- | Makes page not interrupt on any pauses (breakpoint, exception, dom exception etc).
setSkipAllPauses
    :: P.Bool
    -- ^ New value for skip pauses state.

    -> SetSkipAllPauses
setSkipAllPauses _0 = SetSkipAllPauses _0


------------------------------------------------------------------------------
-- | Changes value of variable in a callframe. Object-based scopes are not supported and must be
-- mutated manually.
data SetVariableValue = SetVariableValue
    { -- | 0-based number of scope as was listed in scope chain. Only 'local', 'closure' and 'catch'
      -- scope types are allowed. Other scopes could be manipulated manually.
      scopeNumber :: !P.Int
      -- | Variable name.
    , variableName :: !T.Text
      -- | New variable value.
    , newValue :: !Runtime.CallArgument
      -- | Id of callframe that holds variable.
    , callFrameId :: !CallFrameId
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetVariableValue where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setVariableValue" $ \_o -> SetVariableValue
            <$> _o .: "scopeNumber"
            <*> _o .: "variableName"
            <*> _o .: "newValue"
            <*> _o .: "callFrameId"
        ago = A.withArray "setVariableValue" $ \_a -> SetVariableValue
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON SetVariableValue where
    toEncoding (SetVariableValue _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "scopeNumber" .= _0
        , P.pure $ "variableName" .= _1
        , P.pure $ "newValue" .= _2
        , P.pure $ "callFrameId" .= _3
        ]
    toJSON (SetVariableValue _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "scopeNumber" .= _0
        , P.pure $ "variableName" .= _1
        , P.pure $ "newValue" .= _2
        , P.pure $ "callFrameId" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetVariableValue where
    SetVariableValue _0 _1 _2 _3 <> SetVariableValue _ _ _ _ = SetVariableValue _0 _1 _2 _3


------------------------------------------------------------------------------
instance M.Method SetVariableValue where
    type Result SetVariableValue = ()
    name _ = "Debugger.setVariableValue"


------------------------------------------------------------------------------
-- | Changes value of variable in a callframe. Object-based scopes are not supported and must be
-- mutated manually.
setVariableValue
    :: P.Int
    -- ^ 0-based number of scope as was listed in scope chain. Only 'local', 'closure' and 'catch'

    -- scope types are allowed. Other scopes could be manipulated manually.

    -> T.Text
    -- ^ Variable name.

    -> Runtime.CallArgument
    -- ^ New variable value.

    -> CallFrameId
    -- ^ Id of callframe that holds variable.

    -> SetVariableValue
setVariableValue _0 _1 _2 _3 = SetVariableValue _0 _1 _2 _3


------------------------------------------------------------------------------
-- | Steps into the function call.
{-# WARNING breakOnAsyncCall "This feature is marked as EXPERIMENTAL." #-}
data StepInto = StepInto
    { -- | Debugger will issue additional Debugger.paused notification if any async task is scheduled
      -- before next pause.
      breakOnAsyncCall :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StepInto where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "stepInto" $ \_o -> StepInto
            <$> _o .:? "breakOnAsyncCall"
        ago = A.withArray "stepInto" $ \_a -> StepInto
            <$> P.traverse A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON StepInto where
    toEncoding (StepInto _0) = A.pairs $ P.fold $ P.catMaybes
        [ ("breakOnAsyncCall" .=) <$> _0
        ]
    toJSON (StepInto _0) = A.object $ P.catMaybes
        [ ("breakOnAsyncCall" .=) <$> _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup StepInto where
    StepInto _0 <> StepInto __0 = StepInto (_0 <|> __0)


------------------------------------------------------------------------------
instance P.Monoid StepInto where
    mempty = StepInto P.empty


------------------------------------------------------------------------------
instance M.Method StepInto where
    type Result StepInto = ()
    name _ = "Debugger.stepInto"


------------------------------------------------------------------------------
-- | Steps into the function call.
stepInto
    :: StepInto
stepInto = StepInto P.empty


------------------------------------------------------------------------------
-- | Steps out of the function call.
data StepOut = StepOut
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StepOut where
    parseJSON A.Null = P.pure StepOut
    parseJSON v = A.withArray "stepOut" go v
        <|> A.withObject "stepOut" go v
      where
        go _ = P.pure StepOut


------------------------------------------------------------------------------
instance A.ToJSON StepOut where
    toEncoding StepOut = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON StepOut = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup StepOut where
    StepOut <> StepOut = StepOut


------------------------------------------------------------------------------
instance P.Monoid StepOut where
    mempty = StepOut


------------------------------------------------------------------------------
instance M.Method StepOut where
    type Result StepOut = ()
    name _ = "Debugger.stepOut"


------------------------------------------------------------------------------
-- | Steps out of the function call.
stepOut
    :: StepOut
stepOut = StepOut


------------------------------------------------------------------------------
-- | Steps over the statement.
data StepOver = StepOver
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StepOver where
    parseJSON A.Null = P.pure StepOver
    parseJSON v = A.withArray "stepOver" go v
        <|> A.withObject "stepOver" go v
      where
        go _ = P.pure StepOver


------------------------------------------------------------------------------
instance A.ToJSON StepOver where
    toEncoding StepOver = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON StepOver = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup StepOver where
    StepOver <> StepOver = StepOver


------------------------------------------------------------------------------
instance P.Monoid StepOver where
    mempty = StepOver


------------------------------------------------------------------------------
instance M.Method StepOver where
    type Result StepOver = ()
    name _ = "Debugger.stepOver"


------------------------------------------------------------------------------
-- | Steps over the statement.
stepOver
    :: StepOver
stepOver = StepOver


------------------------------------------------------------------------------
-- | Fired when breakpoint is resolved to an actual script and location.
data BreakpointResolved = BreakpointResolved
    { -- | Breakpoint unique identifier.
      breakpointId :: !BreakpointId
      -- | Actual breakpoint location.
    , location :: !Location
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON BreakpointResolved where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "breakpointResolved" $ \_o -> BreakpointResolved
            <$> _o .: "breakpointId"
            <*> _o .: "location"
        ago = A.withArray "breakpointResolved" $ \_a -> BreakpointResolved
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON BreakpointResolved where
    toEncoding (BreakpointResolved _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "breakpointId" .= _0
        , P.pure $ "location" .= _1
        ]
    toJSON (BreakpointResolved _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "breakpointId" .= _0
        , P.pure $ "location" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup BreakpointResolved where
    BreakpointResolved _0 _1 <> BreakpointResolved _ _ = BreakpointResolved _0 _1


------------------------------------------------------------------------------
instance E.Event BreakpointResolved where
    type Result BreakpointResolved = BreakpointResolved
    name _ = "Debugger.breakpointResolved"


------------------------------------------------------------------------------
-- | Fired when breakpoint is resolved to an actual script and location.
breakpointResolved :: P.Proxy BreakpointResolved
breakpointResolved = P.Proxy


------------------------------------------------------------------------------
-- | Fired when the virtual machine stopped on breakpoint or exception or any other stop criteria.
{-# WARNING {-asyncStackTraceId, -}asyncCallStackTraceId "This feature is marked as EXPERIMENTAL." #-}
data Paused = Paused
    { -- | Call stack the virtual machine stopped on.
      callFrames :: ![CallFrame]
      -- | Pause reason.
    , reason :: !Reason
      -- | Object containing break-specific auxiliary properties.
    , data_ :: !(P.Maybe A.Object)
      -- | Hit breakpoints IDs
    , hitBreakpoints :: !(P.Maybe [T.Text])
      -- | Async stack trace, if any.
    , asyncStackTrace :: !(P.Maybe Runtime.StackTrace)
      -- | Async stack trace, if any.
    , asyncStackTraceId :: !(P.Maybe Runtime.StackTraceId)
      -- | Just scheduled async call will have this stack trace as parent stack during async execution.
      -- This field is available only after @Debugger.stepInto@ call with @breakOnAsynCall@ flag.
    , asyncCallStackTraceId :: !(P.Maybe Runtime.StackTraceId)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Paused where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "paused" $ \_o -> Paused
            <$> _o .: "callFrames"
            <*> _o .: "reason"
            <*> _o .:? "data"
            <*> _o .:? "hitBreakpoints"
            <*> _o .:? "asyncStackTrace"
            <*> _o .:? "asyncStackTraceId"
            <*> _o .:? "asyncCallStackTraceId"
        ago = A.withArray "paused" $ \_a -> Paused
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)


------------------------------------------------------------------------------
instance A.ToJSON Paused where
    toEncoding (Paused _0 _1 _2 _3 _4 _5 _6) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "callFrames" .= _0
        , P.pure $ "reason" .= _1
        , ("data" .=) <$> _2
        , ("hitBreakpoints" .=) <$> _3
        , ("asyncStackTrace" .=) <$> _4
        , ("asyncStackTraceId" .=) <$> _5
        , ("asyncCallStackTraceId" .=) <$> _6
        ]
    toJSON (Paused _0 _1 _2 _3 _4 _5 _6) = A.object $ P.catMaybes
        [ P.pure $ "callFrames" .= _0
        , P.pure $ "reason" .= _1
        , ("data" .=) <$> _2
        , ("hitBreakpoints" .=) <$> _3
        , ("asyncStackTrace" .=) <$> _4
        , ("asyncStackTraceId" .=) <$> _5
        , ("asyncCallStackTraceId" .=) <$> _6
        ]


------------------------------------------------------------------------------
instance P.Semigroup Paused where
    Paused _0 _1 _2 _3 _4 _5 _6 <> Paused _ _ __2 __3 __4 __5 __6 = Paused _0 _1 (_2 <|> __2) (_3 <|> __3) (_4 <|> __4) (_5 <|> __5) (_6 <|> __6)


------------------------------------------------------------------------------
data Reason
    = XHR
    | DOM
    | EventListener
    | Exception
    | Assert
    | DebugCommand
    | PromiseRejection
    | OOM
    | Other
    | Ambiguous
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Reason where
    parseJSON = A.withText "Reason" $ \t -> case t of
        "XHR" -> P.pure XHR
        "DOM" -> P.pure DOM
        "EventListener" -> P.pure EventListener
        "exception" -> P.pure Exception
        "assert" -> P.pure Assert
        "debugCommand" -> P.pure DebugCommand
        "promiseRejection" -> P.pure PromiseRejection
        "OOM" -> P.pure OOM
        "other" -> P.pure Other
        "ambiguous" -> P.pure Ambiguous
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Reason where
    toJSON XHR = "XHR"
    toJSON DOM = "DOM"
    toJSON EventListener = "EventListener"
    toJSON Exception = "exception"
    toJSON Assert = "assert"
    toJSON DebugCommand = "debugCommand"
    toJSON PromiseRejection = "promiseRejection"
    toJSON OOM = "OOM"
    toJSON Other = "other"
    toJSON Ambiguous = "ambiguous"


------------------------------------------------------------------------------
instance E.Event Paused where
    type Result Paused = Paused
    name _ = "Debugger.paused"


------------------------------------------------------------------------------
-- | Fired when the virtual machine stopped on breakpoint or exception or any other stop criteria.
paused :: P.Proxy Paused
paused = P.Proxy


------------------------------------------------------------------------------
-- | Fired when the virtual machine resumed execution.
data Resumed = Resumed
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Resumed where
    parseJSON A.Null = P.pure Resumed
    parseJSON v = A.withArray "resumed" go v
        <|> A.withObject "resumed" go v
      where
        go _ = P.pure Resumed


------------------------------------------------------------------------------
instance A.ToJSON Resumed where
    toEncoding Resumed = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON Resumed = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup Resumed where
    Resumed <> Resumed = Resumed


------------------------------------------------------------------------------
instance P.Monoid Resumed where
    mempty = Resumed


------------------------------------------------------------------------------
instance E.Event Resumed where
    type Result Resumed = ()
    name _ = "Debugger.resumed"


------------------------------------------------------------------------------
-- | Fired when the virtual machine resumed execution.
resumed :: P.Proxy Resumed
resumed = P.Proxy


------------------------------------------------------------------------------
-- | Fired when virtual machine fails to parse the script.
{-# WARNING stackTrace "This feature is marked as EXPERIMENTAL." #-}
data ScriptFailedToParse = ScriptFailedToParse
    { -- | Identifier of the script parsed.
      scriptId :: !Runtime.ScriptId
      -- | URL or name of the script parsed (if any).
    , url :: !T.Text
      -- | Line offset of the script within the resource with given URL (for script tags).
    , startLine :: !P.Int
      -- | Column offset of the script within the resource with given URL.
    , startColumn :: !P.Int
      -- | Last line of the script.
    , endLine :: !P.Int
      -- | Length of the last line of the script.
    , endColumn :: !P.Int
      -- | Specifies script creation context.
    , executionContextId :: !Runtime.ExecutionContextId
      -- | Content hash of the script.
    , hash :: !T.Text
      -- | Embedder-specific auxiliary data.
    , executionContextAuxData :: !(P.Maybe A.Object)
      -- | URL of source map associated with script (if any).
    , sourceMapURL :: !(P.Maybe T.Text)
      -- | True, if this script has sourceURL.
    , hasSourceURL :: !(P.Maybe P.Bool)
      -- | True, if this script is ES6 module.
    , isModule :: !(P.Maybe P.Bool)
      -- | This script length.
    , length :: !(P.Maybe P.Int)
      -- | JavaScript top stack frame of where the script parsed event was triggered if available.
    , stackTrace :: !(P.Maybe Runtime.StackTrace)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ScriptFailedToParse where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "scriptFailedToParse" $ \_o -> ScriptFailedToParse
            <$> _o .: "scriptId"
            <*> _o .: "url"
            <*> _o .: "startLine"
            <*> _o .: "startColumn"
            <*> _o .: "endLine"
            <*> _o .: "endColumn"
            <*> _o .: "executionContextId"
            <*> _o .: "hash"
            <*> _o .:? "executionContextAuxData"
            <*> _o .:? "sourceMapURL"
            <*> _o .:? "hasSourceURL"
            <*> _o .:? "isModule"
            <*> _o .:? "length"
            <*> _o .:? "stackTrace"
        ago = A.withArray "scriptFailedToParse" $ \_a -> ScriptFailedToParse
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.maybe P.empty A.parseJSON (_a !? 6)
            <*> P.maybe P.empty A.parseJSON (_a !? 7)
            <*> P.traverse A.parseJSON (_a !? 8)
            <*> P.traverse A.parseJSON (_a !? 9)
            <*> P.traverse A.parseJSON (_a !? 10)
            <*> P.traverse A.parseJSON (_a !? 11)
            <*> P.traverse A.parseJSON (_a !? 12)
            <*> P.traverse A.parseJSON (_a !? 13)


------------------------------------------------------------------------------
instance A.ToJSON ScriptFailedToParse where
    toEncoding (ScriptFailedToParse _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "scriptId" .= _0
        , P.pure $ "url" .= _1
        , P.pure $ "startLine" .= _2
        , P.pure $ "startColumn" .= _3
        , P.pure $ "endLine" .= _4
        , P.pure $ "endColumn" .= _5
        , P.pure $ "executionContextId" .= _6
        , P.pure $ "hash" .= _7
        , ("executionContextAuxData" .=) <$> _8
        , ("sourceMapURL" .=) <$> _9
        , ("hasSourceURL" .=) <$> _10
        , ("isModule" .=) <$> _11
        , ("length" .=) <$> _12
        , ("stackTrace" .=) <$> _13
        ]
    toJSON (ScriptFailedToParse _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13) = A.object $ P.catMaybes
        [ P.pure $ "scriptId" .= _0
        , P.pure $ "url" .= _1
        , P.pure $ "startLine" .= _2
        , P.pure $ "startColumn" .= _3
        , P.pure $ "endLine" .= _4
        , P.pure $ "endColumn" .= _5
        , P.pure $ "executionContextId" .= _6
        , P.pure $ "hash" .= _7
        , ("executionContextAuxData" .=) <$> _8
        , ("sourceMapURL" .=) <$> _9
        , ("hasSourceURL" .=) <$> _10
        , ("isModule" .=) <$> _11
        , ("length" .=) <$> _12
        , ("stackTrace" .=) <$> _13
        ]


------------------------------------------------------------------------------
instance P.Semigroup ScriptFailedToParse where
    ScriptFailedToParse _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 <> ScriptFailedToParse _ _ _ _ _ _ _ _ __8 __9 __10 __11 __12 __13 = ScriptFailedToParse _0 _1 _2 _3 _4 _5 _6 _7 (_8 <|> __8) (_9 <|> __9) (_10 <|> __10) (_11 <|> __11) (_12 <|> __12) (_13 <|> __13)


------------------------------------------------------------------------------
instance E.Event ScriptFailedToParse where
    type Result ScriptFailedToParse = ScriptFailedToParse
    name _ = "Debugger.scriptFailedToParse"


------------------------------------------------------------------------------
-- | Fired when virtual machine fails to parse the script.
scriptFailedToParse :: P.Proxy ScriptFailedToParse
scriptFailedToParse = P.Proxy


------------------------------------------------------------------------------
-- | Fired when virtual machine parses script. This event is also fired for all known and uncollected
-- scripts upon enabling debugger.
{-# WARNING isLiveEdit{-, stackTrace-} "This feature is marked as EXPERIMENTAL." #-}
data ScriptParsed = ScriptParsed
    { -- | Identifier of the script parsed.
      scriptId :: !Runtime.ScriptId
      -- | URL or name of the script parsed (if any).
    , url :: !T.Text
      -- | Line offset of the script within the resource with given URL (for script tags).
    , startLine :: !P.Int
      -- | Column offset of the script within the resource with given URL.
    , startColumn :: !P.Int
      -- | Last line of the script.
    , endLine :: !P.Int
      -- | Length of the last line of the script.
    , endColumn :: !P.Int
      -- | Specifies script creation context.
    , executionContextId :: !Runtime.ExecutionContextId
      -- | Content hash of the script.
    , hash :: !T.Text
      -- | Embedder-specific auxiliary data.
    , executionContextAuxData :: !(P.Maybe A.Object)
      -- | True, if this script is generated as a result of the live edit operation.
    , isLiveEdit :: !(P.Maybe P.Bool)
      -- | URL of source map associated with script (if any).
    , sourceMapURL :: !(P.Maybe T.Text)
      -- | True, if this script has sourceURL.
    , hasSourceURL :: !(P.Maybe P.Bool)
      -- | True, if this script is ES6 module.
    , isModule :: !(P.Maybe P.Bool)
      -- | This script length.
    , length :: !(P.Maybe P.Int)
      -- | JavaScript top stack frame of where the script parsed event was triggered if available.
    , stackTrace :: !(P.Maybe Runtime.StackTrace)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ScriptParsed where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "scriptParsed" $ \_o -> ScriptParsed
            <$> _o .: "scriptId"
            <*> _o .: "url"
            <*> _o .: "startLine"
            <*> _o .: "startColumn"
            <*> _o .: "endLine"
            <*> _o .: "endColumn"
            <*> _o .: "executionContextId"
            <*> _o .: "hash"
            <*> _o .:? "executionContextAuxData"
            <*> _o .:? "isLiveEdit"
            <*> _o .:? "sourceMapURL"
            <*> _o .:? "hasSourceURL"
            <*> _o .:? "isModule"
            <*> _o .:? "length"
            <*> _o .:? "stackTrace"
        ago = A.withArray "scriptParsed" $ \_a -> ScriptParsed
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.maybe P.empty A.parseJSON (_a !? 6)
            <*> P.maybe P.empty A.parseJSON (_a !? 7)
            <*> P.traverse A.parseJSON (_a !? 8)
            <*> P.traverse A.parseJSON (_a !? 9)
            <*> P.traverse A.parseJSON (_a !? 10)
            <*> P.traverse A.parseJSON (_a !? 11)
            <*> P.traverse A.parseJSON (_a !? 12)
            <*> P.traverse A.parseJSON (_a !? 13)
            <*> P.traverse A.parseJSON (_a !? 14)


------------------------------------------------------------------------------
instance A.ToJSON ScriptParsed where
    toEncoding (ScriptParsed _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "scriptId" .= _0
        , P.pure $ "url" .= _1
        , P.pure $ "startLine" .= _2
        , P.pure $ "startColumn" .= _3
        , P.pure $ "endLine" .= _4
        , P.pure $ "endColumn" .= _5
        , P.pure $ "executionContextId" .= _6
        , P.pure $ "hash" .= _7
        , ("executionContextAuxData" .=) <$> _8
        , ("isLiveEdit" .=) <$> _9
        , ("sourceMapURL" .=) <$> _10
        , ("hasSourceURL" .=) <$> _11
        , ("isModule" .=) <$> _12
        , ("length" .=) <$> _13
        , ("stackTrace" .=) <$> _14
        ]
    toJSON (ScriptParsed _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14) = A.object $ P.catMaybes
        [ P.pure $ "scriptId" .= _0
        , P.pure $ "url" .= _1
        , P.pure $ "startLine" .= _2
        , P.pure $ "startColumn" .= _3
        , P.pure $ "endLine" .= _4
        , P.pure $ "endColumn" .= _5
        , P.pure $ "executionContextId" .= _6
        , P.pure $ "hash" .= _7
        , ("executionContextAuxData" .=) <$> _8
        , ("isLiveEdit" .=) <$> _9
        , ("sourceMapURL" .=) <$> _10
        , ("hasSourceURL" .=) <$> _11
        , ("isModule" .=) <$> _12
        , ("length" .=) <$> _13
        , ("stackTrace" .=) <$> _14
        ]


------------------------------------------------------------------------------
instance P.Semigroup ScriptParsed where
    ScriptParsed _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 <> ScriptParsed _ _ _ _ _ _ _ _ __8 __9 __10 __11 __12 __13 __14 = ScriptParsed _0 _1 _2 _3 _4 _5 _6 _7 (_8 <|> __8) (_9 <|> __9) (_10 <|> __10) (_11 <|> __11) (_12 <|> __12) (_13 <|> __13) (_14 <|> __14)


------------------------------------------------------------------------------
instance E.Event ScriptParsed where
    type Result ScriptParsed = ScriptParsed
    name _ = "Debugger.scriptParsed"


------------------------------------------------------------------------------
-- | Fired when virtual machine parses script. This event is also fired for all known and uncollected
-- scripts upon enabling debugger.
scriptParsed :: P.Proxy ScriptParsed
scriptParsed = P.Proxy

