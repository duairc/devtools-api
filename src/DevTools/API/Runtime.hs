{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Runtime domain exposes JavaScript runtime by means of remote evaluation and mirror objects.
-- Evaluation results are returned as mirror object that expose object type, string representation
-- and unique identifier that can be used for further object reference. Original objects are
-- maintained in memory unless they are either explicitly released or are released along with the
-- other objects in their object group.
module DevTools.API.Runtime
    ( module DevTools.API.Runtime.Types
    , module DevTools.API.Runtime
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
import           DevTools.API.Runtime.Types


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Add handler to promise with given promise object id.
data AwaitPromise = AwaitPromise
    { -- | Identifier of the promise.
      promiseObjectId :: !RemoteObjectId
      -- | Whether the result is expected to be a JSON object that should be sent by value.
    , returnByValue :: !(P.Maybe P.Bool)
      -- | Whether preview should be generated for the result.
    , generatePreview :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AwaitPromise where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "awaitPromise" $ \_o -> AwaitPromise
            <$> _o .: "promiseObjectId"
            <*> _o .:? "returnByValue"
            <*> _o .:? "generatePreview"
        ago = A.withArray "awaitPromise" $ \_a -> AwaitPromise
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON AwaitPromise where
    toEncoding (AwaitPromise _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "promiseObjectId" .= _0
        , ("returnByValue" .=) <$> _1
        , ("generatePreview" .=) <$> _2
        ]
    toJSON (AwaitPromise _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "promiseObjectId" .= _0
        , ("returnByValue" .=) <$> _1
        , ("generatePreview" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup AwaitPromise where
    AwaitPromise _0 _1 _2 <> AwaitPromise _ __1 __2 = AwaitPromise _0 (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
-- | Add handler to promise with given promise object id.
data AwaitPromiseResult = AwaitPromiseResult
    { -- | Promise result. Will contain rejected value if promise was rejected.
      result :: !RemoteObject
      -- | Exception details if stack strace is available.
    , exceptionDetails :: !(P.Maybe ExceptionDetails)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AwaitPromiseResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "awaitPromiseResult" $ \_o -> AwaitPromiseResult
            <$> _o .: "result"
            <*> _o .:? "exceptionDetails"
        ago = A.withArray "awaitPromiseResult" $ \_a -> AwaitPromiseResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON AwaitPromiseResult where
    toEncoding (AwaitPromiseResult _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "result" .= _0
        , ("exceptionDetails" .=) <$> _1
        ]
    toJSON (AwaitPromiseResult _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "result" .= _0
        , ("exceptionDetails" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup AwaitPromiseResult where
    AwaitPromiseResult _0 _1 <> AwaitPromiseResult _ __1 = AwaitPromiseResult _0 (_1 <|> __1)


------------------------------------------------------------------------------
instance M.Method AwaitPromise where
    type Result AwaitPromise = AwaitPromiseResult
    name _ = "Runtime.awaitPromise"


------------------------------------------------------------------------------
-- | Add handler to promise with given promise object id.
awaitPromise
    :: RemoteObjectId
    -- ^ Identifier of the promise.

    -> AwaitPromise
awaitPromise _0 = AwaitPromise _0 P.empty P.empty


------------------------------------------------------------------------------
-- | Calls function with given declaration on the given object. Object group of the result is
-- inherited from the target object.
{-# WARNING generatePreview "This feature is marked as EXPERIMENTAL." #-}
data CallFunctionOn = CallFunctionOn
    { -- | Declaration of the function to call.
      functionDeclaration :: !T.Text
      -- | Identifier of the object to call function on. Either objectId or executionContextId should
      -- be specified.
    , objectId :: !(P.Maybe RemoteObjectId)
      -- | Call arguments. All call arguments must belong to the same JavaScript world as the target
      -- object.
    , arguments :: !(P.Maybe [CallArgument])
      -- | In silent mode exceptions thrown during evaluation are not reported and do not pause
      -- execution. Overrides @setPauseOnException@ state.
    , silent :: !(P.Maybe P.Bool)
      -- | Whether the result is expected to be a JSON object which should be sent by value.
    , returnByValue :: !(P.Maybe P.Bool)
      -- | Whether preview should be generated for the result.
    , generatePreview :: !(P.Maybe P.Bool)
      -- | Whether execution should be treated as initiated by user in the UI.
    , userGesture :: !(P.Maybe P.Bool)
      -- | Whether execution should @await@ for resulting value and return once awaited promise is
      -- resolved.
    , awaitPromise_ :: !(P.Maybe P.Bool)
      -- | Specifies execution context which global object will be used to call function on. Either
      -- executionContextId or objectId should be specified.
    , executionContextId :: !(P.Maybe ExecutionContextId)
      -- | Symbolic group name that can be used to release multiple objects. If objectGroup is not
      -- specified and objectId is, objectGroup will be inherited from object.
    , objectGroup :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CallFunctionOn where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "callFunctionOn" $ \_o -> CallFunctionOn
            <$> _o .: "functionDeclaration"
            <*> _o .:? "objectId"
            <*> _o .:? "arguments"
            <*> _o .:? "silent"
            <*> _o .:? "returnByValue"
            <*> _o .:? "generatePreview"
            <*> _o .:? "userGesture"
            <*> _o .:? "awaitPromise"
            <*> _o .:? "executionContextId"
            <*> _o .:? "objectGroup"
        ago = A.withArray "callFunctionOn" $ \_a -> CallFunctionOn
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)
            <*> P.traverse A.parseJSON (_a !? 7)
            <*> P.traverse A.parseJSON (_a !? 8)
            <*> P.traverse A.parseJSON (_a !? 9)


------------------------------------------------------------------------------
instance A.ToJSON CallFunctionOn where
    toEncoding (CallFunctionOn _0 _1 _2 _3 _4 _5 _6 _7 _8 _9) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "functionDeclaration" .= _0
        , ("objectId" .=) <$> _1
        , ("arguments" .=) <$> _2
        , ("silent" .=) <$> _3
        , ("returnByValue" .=) <$> _4
        , ("generatePreview" .=) <$> _5
        , ("userGesture" .=) <$> _6
        , ("awaitPromise" .=) <$> _7
        , ("executionContextId" .=) <$> _8
        , ("objectGroup" .=) <$> _9
        ]
    toJSON (CallFunctionOn _0 _1 _2 _3 _4 _5 _6 _7 _8 _9) = A.object $ P.catMaybes
        [ P.pure $ "functionDeclaration" .= _0
        , ("objectId" .=) <$> _1
        , ("arguments" .=) <$> _2
        , ("silent" .=) <$> _3
        , ("returnByValue" .=) <$> _4
        , ("generatePreview" .=) <$> _5
        , ("userGesture" .=) <$> _6
        , ("awaitPromise" .=) <$> _7
        , ("executionContextId" .=) <$> _8
        , ("objectGroup" .=) <$> _9
        ]


------------------------------------------------------------------------------
instance P.Semigroup CallFunctionOn where
    CallFunctionOn _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 <> CallFunctionOn _ __1 __2 __3 __4 __5 __6 __7 __8 __9 = CallFunctionOn _0 (_1 <|> __1) (_2 <|> __2) (_3 <|> __3) (_4 <|> __4) (_5 <|> __5) (_6 <|> __6) (_7 <|> __7) (_8 <|> __8) (_9 <|> __9)


------------------------------------------------------------------------------
-- | Calls function with given declaration on the given object. Object group of the result is
-- inherited from the target object.
data CallFunctionOnResult = CallFunctionOnResult
    { -- | Call result.
      result :: !RemoteObject
      -- | Exception details.
    , exceptionDetails :: !(P.Maybe ExceptionDetails)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CallFunctionOnResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "callFunctionOnResult" $ \_o -> CallFunctionOnResult
            <$> _o .: "result"
            <*> _o .:? "exceptionDetails"
        ago = A.withArray "callFunctionOnResult" $ \_a -> CallFunctionOnResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON CallFunctionOnResult where
    toEncoding (CallFunctionOnResult _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "result" .= _0
        , ("exceptionDetails" .=) <$> _1
        ]
    toJSON (CallFunctionOnResult _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "result" .= _0
        , ("exceptionDetails" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup CallFunctionOnResult where
    CallFunctionOnResult _0 _1 <> CallFunctionOnResult _ __1 = CallFunctionOnResult _0 (_1 <|> __1)


------------------------------------------------------------------------------
instance M.Method CallFunctionOn where
    type Result CallFunctionOn = CallFunctionOnResult
    name _ = "Runtime.callFunctionOn"


------------------------------------------------------------------------------
-- | Calls function with given declaration on the given object. Object group of the result is
-- inherited from the target object.
callFunctionOn
    :: T.Text
    -- ^ Declaration of the function to call.

    -> CallFunctionOn
callFunctionOn _0 = CallFunctionOn _0 P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Compiles expression.
data CompileScript = CompileScript
    { -- | Expression to compile.
      expression :: !T.Text
      -- | Source url to be set for the script.
    , sourceURL :: !T.Text
      -- | Specifies whether the compiled script should be persisted.
    , persistScript :: !P.Bool
      -- | Specifies in which execution context to perform script run. If the parameter is omitted the
      -- evaluation will be performed in the context of the inspected page.
    , executionContextId :: !(P.Maybe ExecutionContextId)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CompileScript where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "compileScript" $ \_o -> CompileScript
            <$> _o .: "expression"
            <*> _o .: "sourceURL"
            <*> _o .: "persistScript"
            <*> _o .:? "executionContextId"
        ago = A.withArray "compileScript" $ \_a -> CompileScript
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON CompileScript where
    toEncoding (CompileScript _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "expression" .= _0
        , P.pure $ "sourceURL" .= _1
        , P.pure $ "persistScript" .= _2
        , ("executionContextId" .=) <$> _3
        ]
    toJSON (CompileScript _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "expression" .= _0
        , P.pure $ "sourceURL" .= _1
        , P.pure $ "persistScript" .= _2
        , ("executionContextId" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup CompileScript where
    CompileScript _0 _1 _2 _3 <> CompileScript _ _ _ __3 = CompileScript _0 _1 _2 (_3 <|> __3)


------------------------------------------------------------------------------
-- | Compiles expression.
data CompileScriptResult = CompileScriptResult
    { -- | Id of the script.
      scriptId :: !(P.Maybe ScriptId)
      -- | Exception details.
    , exceptionDetails :: !(P.Maybe ExceptionDetails)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CompileScriptResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "compileScriptResult" $ \_o -> CompileScriptResult
            <$> _o .:? "scriptId"
            <*> _o .:? "exceptionDetails"
        ago = A.withArray "compileScriptResult" $ \_a -> CompileScriptResult
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON CompileScriptResult where
    toEncoding (CompileScriptResult _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ ("scriptId" .=) <$> _0
        , ("exceptionDetails" .=) <$> _1
        ]
    toJSON (CompileScriptResult _0 _1) = A.object $ P.catMaybes
        [ ("scriptId" .=) <$> _0
        , ("exceptionDetails" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup CompileScriptResult where
    CompileScriptResult _0 _1 <> CompileScriptResult __0 __1 = CompileScriptResult (_0 <|> __0) (_1 <|> __1)


------------------------------------------------------------------------------
instance P.Monoid CompileScriptResult where
    mempty = CompileScriptResult P.empty P.empty


------------------------------------------------------------------------------
instance M.Method CompileScript where
    type Result CompileScript = CompileScriptResult
    name _ = "Runtime.compileScript"


------------------------------------------------------------------------------
-- | Compiles expression.
compileScript
    :: T.Text
    -- ^ Expression to compile.

    -> T.Text
    -- ^ Source url to be set for the script.

    -> P.Bool
    -- ^ Specifies whether the compiled script should be persisted.

    -> CompileScript
compileScript _0 _1 _2 = CompileScript _0 _1 _2 P.empty


------------------------------------------------------------------------------
-- | Disables reporting of execution contexts creation.
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
    name _ = "Runtime.disable"


------------------------------------------------------------------------------
-- | Disables reporting of execution contexts creation.
disable
    :: Disable
disable = Disable


------------------------------------------------------------------------------
-- | Discards collected exceptions and console API calls.
data DiscardConsoleEntries = DiscardConsoleEntries
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DiscardConsoleEntries where
    parseJSON A.Null = P.pure DiscardConsoleEntries
    parseJSON v = A.withArray "discardConsoleEntries" go v
        <|> A.withObject "discardConsoleEntries" go v
      where
        go _ = P.pure DiscardConsoleEntries


------------------------------------------------------------------------------
instance A.ToJSON DiscardConsoleEntries where
    toEncoding DiscardConsoleEntries = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON DiscardConsoleEntries = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup DiscardConsoleEntries where
    DiscardConsoleEntries <> DiscardConsoleEntries = DiscardConsoleEntries


------------------------------------------------------------------------------
instance P.Monoid DiscardConsoleEntries where
    mempty = DiscardConsoleEntries


------------------------------------------------------------------------------
instance M.Method DiscardConsoleEntries where
    type Result DiscardConsoleEntries = ()
    name _ = "Runtime.discardConsoleEntries"


------------------------------------------------------------------------------
-- | Discards collected exceptions and console API calls.
discardConsoleEntries
    :: DiscardConsoleEntries
discardConsoleEntries = DiscardConsoleEntries


------------------------------------------------------------------------------
-- | Enables reporting of execution contexts creation by means of @executionContextCreated@ event.
-- When the reporting gets enabled the event will be sent immediately for each existing execution
-- context.
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
    name _ = "Runtime.enable"


------------------------------------------------------------------------------
-- | Enables reporting of execution contexts creation by means of @executionContextCreated@ event.
-- When the reporting gets enabled the event will be sent immediately for each existing execution
-- context.
enable
    :: Enable
enable = Enable


------------------------------------------------------------------------------
-- | Evaluates expression on global object.
{-# WARNING {-generatePreview, -}throwOnSideEffect, timeout "This feature is marked as EXPERIMENTAL." #-}
data Evaluate = Evaluate
    { -- | Expression to evaluate.
      expression :: !T.Text
      -- | Symbolic group name that can be used to release multiple objects.
    , objectGroup :: !(P.Maybe T.Text)
      -- | Determines whether Command Line API should be available during the evaluation.
    , includeCommandLineAPI :: !(P.Maybe P.Bool)
      -- | In silent mode exceptions thrown during evaluation are not reported and do not pause
      -- execution. Overrides @setPauseOnException@ state.
    , silent :: !(P.Maybe P.Bool)
      -- | Specifies in which execution context to perform evaluation. If the parameter is omitted the
      -- evaluation will be performed in the context of the inspected page.
    , contextId :: !(P.Maybe ExecutionContextId)
      -- | Whether the result is expected to be a JSON object that should be sent by value.
    , returnByValue :: !(P.Maybe P.Bool)
      -- | Whether preview should be generated for the result.
    , generatePreview :: !(P.Maybe P.Bool)
      -- | Whether execution should be treated as initiated by user in the UI.
    , userGesture :: !(P.Maybe P.Bool)
      -- | Whether execution should @await@ for resulting value and return once awaited promise is
      -- resolved.
    , awaitPromise_ :: !(P.Maybe P.Bool)
      -- | Whether to throw an exception if side effect cannot be ruled out during evaluation.
    , throwOnSideEffect :: !(P.Maybe P.Bool)
      -- | Terminate execution after timing out (number of milliseconds).
    , timeout :: !(P.Maybe TimeDelta)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Evaluate where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "evaluate" $ \_o -> Evaluate
            <$> _o .: "expression"
            <*> _o .:? "objectGroup"
            <*> _o .:? "includeCommandLineAPI"
            <*> _o .:? "silent"
            <*> _o .:? "contextId"
            <*> _o .:? "returnByValue"
            <*> _o .:? "generatePreview"
            <*> _o .:? "userGesture"
            <*> _o .:? "awaitPromise"
            <*> _o .:? "throwOnSideEffect"
            <*> _o .:? "timeout"
        ago = A.withArray "evaluate" $ \_a -> Evaluate
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)
            <*> P.traverse A.parseJSON (_a !? 7)
            <*> P.traverse A.parseJSON (_a !? 8)
            <*> P.traverse A.parseJSON (_a !? 9)
            <*> P.traverse A.parseJSON (_a !? 10)


------------------------------------------------------------------------------
instance A.ToJSON Evaluate where
    toEncoding (Evaluate _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "expression" .= _0
        , ("objectGroup" .=) <$> _1
        , ("includeCommandLineAPI" .=) <$> _2
        , ("silent" .=) <$> _3
        , ("contextId" .=) <$> _4
        , ("returnByValue" .=) <$> _5
        , ("generatePreview" .=) <$> _6
        , ("userGesture" .=) <$> _7
        , ("awaitPromise" .=) <$> _8
        , ("throwOnSideEffect" .=) <$> _9
        , ("timeout" .=) <$> _10
        ]
    toJSON (Evaluate _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10) = A.object $ P.catMaybes
        [ P.pure $ "expression" .= _0
        , ("objectGroup" .=) <$> _1
        , ("includeCommandLineAPI" .=) <$> _2
        , ("silent" .=) <$> _3
        , ("contextId" .=) <$> _4
        , ("returnByValue" .=) <$> _5
        , ("generatePreview" .=) <$> _6
        , ("userGesture" .=) <$> _7
        , ("awaitPromise" .=) <$> _8
        , ("throwOnSideEffect" .=) <$> _9
        , ("timeout" .=) <$> _10
        ]


------------------------------------------------------------------------------
instance P.Semigroup Evaluate where
    Evaluate _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 <> Evaluate _ __1 __2 __3 __4 __5 __6 __7 __8 __9 __10 = Evaluate _0 (_1 <|> __1) (_2 <|> __2) (_3 <|> __3) (_4 <|> __4) (_5 <|> __5) (_6 <|> __6) (_7 <|> __7) (_8 <|> __8) (_9 <|> __9) (_10 <|> __10)


------------------------------------------------------------------------------
-- | Evaluates expression on global object.
data EvaluateResult = EvaluateResult
    { -- | Evaluation result.
      result :: !RemoteObject
      -- | Exception details.
    , exceptionDetails :: !(P.Maybe ExceptionDetails)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON EvaluateResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "evaluateResult" $ \_o -> EvaluateResult
            <$> _o .: "result"
            <*> _o .:? "exceptionDetails"
        ago = A.withArray "evaluateResult" $ \_a -> EvaluateResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON EvaluateResult where
    toEncoding (EvaluateResult _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "result" .= _0
        , ("exceptionDetails" .=) <$> _1
        ]
    toJSON (EvaluateResult _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "result" .= _0
        , ("exceptionDetails" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup EvaluateResult where
    EvaluateResult _0 _1 <> EvaluateResult _ __1 = EvaluateResult _0 (_1 <|> __1)


------------------------------------------------------------------------------
instance M.Method Evaluate where
    type Result Evaluate = EvaluateResult
    name _ = "Runtime.evaluate"


------------------------------------------------------------------------------
-- | Evaluates expression on global object.
evaluate
    :: T.Text
    -- ^ Expression to evaluate.

    -> Evaluate
evaluate _0 = Evaluate _0 P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Returns the isolate id.
{-# WARNING GetIsolateId "This feature is marked as EXPERIMENTAL." #-}
data GetIsolateId = GetIsolateId
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetIsolateId where
    parseJSON A.Null = P.pure GetIsolateId
    parseJSON v = A.withArray "getIsolateId" go v
        <|> A.withObject "getIsolateId" go v
      where
        go _ = P.pure GetIsolateId


------------------------------------------------------------------------------
instance A.ToJSON GetIsolateId where
    toEncoding GetIsolateId = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetIsolateId = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetIsolateId where
    GetIsolateId <> GetIsolateId = GetIsolateId


------------------------------------------------------------------------------
instance P.Monoid GetIsolateId where
    mempty = GetIsolateId


------------------------------------------------------------------------------
-- | Returns the isolate id.
{-# WARNING GetIsolateIdResult "This feature is marked as EXPERIMENTAL." #-}
data GetIsolateIdResult = GetIsolateIdResult
    { -- | The isolate id.
      id :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetIsolateIdResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getIsolateIdResult" $ \_o -> GetIsolateIdResult
            <$> _o .: "id"
        ago = A.withArray "getIsolateIdResult" $ \_a -> GetIsolateIdResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetIsolateIdResult where
    toEncoding (GetIsolateIdResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "id" .= _0
        ]
    toJSON (GetIsolateIdResult _0) = A.object $ P.catMaybes
        [ P.pure $ "id" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetIsolateIdResult where
    GetIsolateIdResult _0 <> GetIsolateIdResult _ = GetIsolateIdResult _0


------------------------------------------------------------------------------
instance M.Method GetIsolateId where
    type Result GetIsolateId = GetIsolateIdResult
    name _ = "Runtime.getIsolateId"


------------------------------------------------------------------------------
-- | Returns the isolate id.
{-# WARNING getIsolateId "This feature is marked as EXPERIMENTAL." #-}
getIsolateId
    :: GetIsolateId
getIsolateId = GetIsolateId


------------------------------------------------------------------------------
-- | Returns the JavaScript heap usage.
-- It is the total usage of the corresponding isolate not scoped to a particular Runtime.
{-# WARNING GetHeapUsage "This feature is marked as EXPERIMENTAL." #-}
data GetHeapUsage = GetHeapUsage
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetHeapUsage where
    parseJSON A.Null = P.pure GetHeapUsage
    parseJSON v = A.withArray "getHeapUsage" go v
        <|> A.withObject "getHeapUsage" go v
      where
        go _ = P.pure GetHeapUsage


------------------------------------------------------------------------------
instance A.ToJSON GetHeapUsage where
    toEncoding GetHeapUsage = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetHeapUsage = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetHeapUsage where
    GetHeapUsage <> GetHeapUsage = GetHeapUsage


------------------------------------------------------------------------------
instance P.Monoid GetHeapUsage where
    mempty = GetHeapUsage


------------------------------------------------------------------------------
-- | Returns the JavaScript heap usage.
-- It is the total usage of the corresponding isolate not scoped to a particular Runtime.
{-# WARNING GetHeapUsageResult "This feature is marked as EXPERIMENTAL." #-}
data GetHeapUsageResult = GetHeapUsageResult
    { -- | Used heap size in bytes.
      usedSize :: !P.Double
      -- | Allocated heap size in bytes.
    , totalSize :: !P.Double
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetHeapUsageResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getHeapUsageResult" $ \_o -> GetHeapUsageResult
            <$> _o .: "usedSize"
            <*> _o .: "totalSize"
        ago = A.withArray "getHeapUsageResult" $ \_a -> GetHeapUsageResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON GetHeapUsageResult where
    toEncoding (GetHeapUsageResult _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "usedSize" .= _0
        , P.pure $ "totalSize" .= _1
        ]
    toJSON (GetHeapUsageResult _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "usedSize" .= _0
        , P.pure $ "totalSize" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetHeapUsageResult where
    GetHeapUsageResult _0 _1 <> GetHeapUsageResult _ _ = GetHeapUsageResult _0 _1


------------------------------------------------------------------------------
instance M.Method GetHeapUsage where
    type Result GetHeapUsage = GetHeapUsageResult
    name _ = "Runtime.getHeapUsage"


------------------------------------------------------------------------------
-- | Returns the JavaScript heap usage.
-- It is the total usage of the corresponding isolate not scoped to a particular Runtime.
{-# WARNING getHeapUsage "This feature is marked as EXPERIMENTAL." #-}
getHeapUsage
    :: GetHeapUsage
getHeapUsage = GetHeapUsage


------------------------------------------------------------------------------
-- | Returns properties of a given object. Object group of the result is inherited from the target
-- object.
{-# WARNING accessorPropertiesOnly{-, generatePreview-} "This feature is marked as EXPERIMENTAL." #-}
data GetProperties = GetProperties
    { -- | Identifier of the object to return properties for.
      objectId :: !RemoteObjectId
      -- | If true, returns properties belonging only to the element itself, not to its prototype
      -- chain.
    , ownProperties :: !(P.Maybe P.Bool)
      -- | If true, returns accessor properties (with getter\/setter) only; internal properties are not
      -- returned either.
    , accessorPropertiesOnly :: !(P.Maybe P.Bool)
      -- | Whether preview should be generated for the results.
    , generatePreview :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetProperties where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getProperties" $ \_o -> GetProperties
            <$> _o .: "objectId"
            <*> _o .:? "ownProperties"
            <*> _o .:? "accessorPropertiesOnly"
            <*> _o .:? "generatePreview"
        ago = A.withArray "getProperties" $ \_a -> GetProperties
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON GetProperties where
    toEncoding (GetProperties _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "objectId" .= _0
        , ("ownProperties" .=) <$> _1
        , ("accessorPropertiesOnly" .=) <$> _2
        , ("generatePreview" .=) <$> _3
        ]
    toJSON (GetProperties _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "objectId" .= _0
        , ("ownProperties" .=) <$> _1
        , ("accessorPropertiesOnly" .=) <$> _2
        , ("generatePreview" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetProperties where
    GetProperties _0 _1 _2 _3 <> GetProperties _ __1 __2 __3 = GetProperties _0 (_1 <|> __1) (_2 <|> __2) (_3 <|> __3)


------------------------------------------------------------------------------
-- | Returns properties of a given object. Object group of the result is inherited from the target
-- object.
{-# WARNING privateProperties "This feature is marked as EXPERIMENTAL." #-}
data GetPropertiesResult = GetPropertiesResult
    { -- | Object properties.
      result :: ![PropertyDescriptor]
      -- | Internal object properties (only of the element itself).
    , internalProperties :: !(P.Maybe [InternalPropertyDescriptor])
      -- | Object private properties.
    , privateProperties :: !(P.Maybe [PrivatePropertyDescriptor])
      -- | Exception details.
    , exceptionDetails :: !(P.Maybe ExceptionDetails)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetPropertiesResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getPropertiesResult" $ \_o -> GetPropertiesResult
            <$> _o .: "result"
            <*> _o .:? "internalProperties"
            <*> _o .:? "privateProperties"
            <*> _o .:? "exceptionDetails"
        ago = A.withArray "getPropertiesResult" $ \_a -> GetPropertiesResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON GetPropertiesResult where
    toEncoding (GetPropertiesResult _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "result" .= _0
        , ("internalProperties" .=) <$> _1
        , ("privateProperties" .=) <$> _2
        , ("exceptionDetails" .=) <$> _3
        ]
    toJSON (GetPropertiesResult _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "result" .= _0
        , ("internalProperties" .=) <$> _1
        , ("privateProperties" .=) <$> _2
        , ("exceptionDetails" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetPropertiesResult where
    GetPropertiesResult _0 _1 _2 _3 <> GetPropertiesResult _ __1 __2 __3 = GetPropertiesResult _0 (_1 <|> __1) (_2 <|> __2) (_3 <|> __3)


------------------------------------------------------------------------------
instance M.Method GetProperties where
    type Result GetProperties = GetPropertiesResult
    name _ = "Runtime.getProperties"


------------------------------------------------------------------------------
-- | Returns properties of a given object. Object group of the result is inherited from the target
-- object.
getProperties
    :: RemoteObjectId
    -- ^ Identifier of the object to return properties for.

    -> GetProperties
getProperties _0 = GetProperties _0 P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Returns all let, const and class variables from global scope.
data GlobalLexicalScopeNames = GlobalLexicalScopeNames
    { -- | Specifies in which execution context to lookup global scope variables.
      executionContextId :: !(P.Maybe ExecutionContextId)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GlobalLexicalScopeNames where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "globalLexicalScopeNames" $ \_o -> GlobalLexicalScopeNames
            <$> _o .:? "executionContextId"
        ago = A.withArray "globalLexicalScopeNames" $ \_a -> GlobalLexicalScopeNames
            <$> P.traverse A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GlobalLexicalScopeNames where
    toEncoding (GlobalLexicalScopeNames _0) = A.pairs $ P.fold $ P.catMaybes
        [ ("executionContextId" .=) <$> _0
        ]
    toJSON (GlobalLexicalScopeNames _0) = A.object $ P.catMaybes
        [ ("executionContextId" .=) <$> _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GlobalLexicalScopeNames where
    GlobalLexicalScopeNames _0 <> GlobalLexicalScopeNames __0 = GlobalLexicalScopeNames (_0 <|> __0)


------------------------------------------------------------------------------
instance P.Monoid GlobalLexicalScopeNames where
    mempty = GlobalLexicalScopeNames P.empty


------------------------------------------------------------------------------
-- | Returns all let, const and class variables from global scope.
data GlobalLexicalScopeNamesResult = GlobalLexicalScopeNamesResult
    { names :: ![T.Text]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GlobalLexicalScopeNamesResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "globalLexicalScopeNamesResult" $ \_o -> GlobalLexicalScopeNamesResult
            <$> _o .: "names"
        ago = A.withArray "globalLexicalScopeNamesResult" $ \_a -> GlobalLexicalScopeNamesResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GlobalLexicalScopeNamesResult where
    toEncoding (GlobalLexicalScopeNamesResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "names" .= _0
        ]
    toJSON (GlobalLexicalScopeNamesResult _0) = A.object $ P.catMaybes
        [ P.pure $ "names" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GlobalLexicalScopeNamesResult where
    GlobalLexicalScopeNamesResult _0 <> GlobalLexicalScopeNamesResult _ = GlobalLexicalScopeNamesResult _0


------------------------------------------------------------------------------
instance M.Method GlobalLexicalScopeNames where
    type Result GlobalLexicalScopeNames = GlobalLexicalScopeNamesResult
    name _ = "Runtime.globalLexicalScopeNames"


------------------------------------------------------------------------------
-- | Returns all let, const and class variables from global scope.
globalLexicalScopeNames
    :: GlobalLexicalScopeNames
globalLexicalScopeNames = GlobalLexicalScopeNames P.empty


------------------------------------------------------------------------------
data QueryObjects = QueryObjects
    { -- | Identifier of the prototype to return objects for.
      prototypeObjectId :: !RemoteObjectId
      -- | Symbolic group name that can be used to release the results.
    , objectGroup :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON QueryObjects where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "queryObjects" $ \_o -> QueryObjects
            <$> _o .: "prototypeObjectId"
            <*> _o .:? "objectGroup"
        ago = A.withArray "queryObjects" $ \_a -> QueryObjects
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON QueryObjects where
    toEncoding (QueryObjects _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "prototypeObjectId" .= _0
        , ("objectGroup" .=) <$> _1
        ]
    toJSON (QueryObjects _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "prototypeObjectId" .= _0
        , ("objectGroup" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup QueryObjects where
    QueryObjects _0 _1 <> QueryObjects _ __1 = QueryObjects _0 (_1 <|> __1)


------------------------------------------------------------------------------
data QueryObjectsResult = QueryObjectsResult
    { -- | Array with objects.
      objects :: !RemoteObject
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON QueryObjectsResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "queryObjectsResult" $ \_o -> QueryObjectsResult
            <$> _o .: "objects"
        ago = A.withArray "queryObjectsResult" $ \_a -> QueryObjectsResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON QueryObjectsResult where
    toEncoding (QueryObjectsResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "objects" .= _0
        ]
    toJSON (QueryObjectsResult _0) = A.object $ P.catMaybes
        [ P.pure $ "objects" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup QueryObjectsResult where
    QueryObjectsResult _0 <> QueryObjectsResult _ = QueryObjectsResult _0


------------------------------------------------------------------------------
instance M.Method QueryObjects where
    type Result QueryObjects = QueryObjectsResult
    name _ = "Runtime.queryObjects"


------------------------------------------------------------------------------
queryObjects
    :: RemoteObjectId
    -- ^ Identifier of the prototype to return objects for.

    -> QueryObjects
queryObjects _0 = QueryObjects _0 P.empty


------------------------------------------------------------------------------
-- | Releases remote object with given id.
data ReleaseObject = ReleaseObject
    { -- | Identifier of the object to release.
      objectId :: !RemoteObjectId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ReleaseObject where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "releaseObject" $ \_o -> ReleaseObject
            <$> _o .: "objectId"
        ago = A.withArray "releaseObject" $ \_a -> ReleaseObject
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON ReleaseObject where
    toEncoding (ReleaseObject _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "objectId" .= _0
        ]
    toJSON (ReleaseObject _0) = A.object $ P.catMaybes
        [ P.pure $ "objectId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup ReleaseObject where
    ReleaseObject _0 <> ReleaseObject _ = ReleaseObject _0


------------------------------------------------------------------------------
instance M.Method ReleaseObject where
    type Result ReleaseObject = ()
    name _ = "Runtime.releaseObject"


------------------------------------------------------------------------------
-- | Releases remote object with given id.
releaseObject
    :: RemoteObjectId
    -- ^ Identifier of the object to release.

    -> ReleaseObject
releaseObject _0 = ReleaseObject _0


------------------------------------------------------------------------------
-- | Releases all remote objects that belong to a given group.
data ReleaseObjectGroup = ReleaseObjectGroup
    { -- | Symbolic object group name.
      objectGroup :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ReleaseObjectGroup where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "releaseObjectGroup" $ \_o -> ReleaseObjectGroup
            <$> _o .: "objectGroup"
        ago = A.withArray "releaseObjectGroup" $ \_a -> ReleaseObjectGroup
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON ReleaseObjectGroup where
    toEncoding (ReleaseObjectGroup _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "objectGroup" .= _0
        ]
    toJSON (ReleaseObjectGroup _0) = A.object $ P.catMaybes
        [ P.pure $ "objectGroup" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup ReleaseObjectGroup where
    ReleaseObjectGroup _0 <> ReleaseObjectGroup _ = ReleaseObjectGroup _0


------------------------------------------------------------------------------
instance M.Method ReleaseObjectGroup where
    type Result ReleaseObjectGroup = ()
    name _ = "Runtime.releaseObjectGroup"


------------------------------------------------------------------------------
-- | Releases all remote objects that belong to a given group.
releaseObjectGroup
    :: T.Text
    -- ^ Symbolic object group name.

    -> ReleaseObjectGroup
releaseObjectGroup _0 = ReleaseObjectGroup _0


------------------------------------------------------------------------------
-- | Tells inspected instance to run if it was waiting for debugger to attach.
data RunIfWaitingForDebugger = RunIfWaitingForDebugger
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RunIfWaitingForDebugger where
    parseJSON A.Null = P.pure RunIfWaitingForDebugger
    parseJSON v = A.withArray "runIfWaitingForDebugger" go v
        <|> A.withObject "runIfWaitingForDebugger" go v
      where
        go _ = P.pure RunIfWaitingForDebugger


------------------------------------------------------------------------------
instance A.ToJSON RunIfWaitingForDebugger where
    toEncoding RunIfWaitingForDebugger = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON RunIfWaitingForDebugger = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup RunIfWaitingForDebugger where
    RunIfWaitingForDebugger <> RunIfWaitingForDebugger = RunIfWaitingForDebugger


------------------------------------------------------------------------------
instance P.Monoid RunIfWaitingForDebugger where
    mempty = RunIfWaitingForDebugger


------------------------------------------------------------------------------
instance M.Method RunIfWaitingForDebugger where
    type Result RunIfWaitingForDebugger = ()
    name _ = "Runtime.runIfWaitingForDebugger"


------------------------------------------------------------------------------
-- | Tells inspected instance to run if it was waiting for debugger to attach.
runIfWaitingForDebugger
    :: RunIfWaitingForDebugger
runIfWaitingForDebugger = RunIfWaitingForDebugger


------------------------------------------------------------------------------
-- | Runs script with given id in a given context.
data RunScript = RunScript
    { -- | Id of the script to run.
      scriptId :: !ScriptId
      -- | Specifies in which execution context to perform script run. If the parameter is omitted the
      -- evaluation will be performed in the context of the inspected page.
    , executionContextId :: !(P.Maybe ExecutionContextId)
      -- | Symbolic group name that can be used to release multiple objects.
    , objectGroup :: !(P.Maybe T.Text)
      -- | In silent mode exceptions thrown during evaluation are not reported and do not pause
      -- execution. Overrides @setPauseOnException@ state.
    , silent :: !(P.Maybe P.Bool)
      -- | Determines whether Command Line API should be available during the evaluation.
    , includeCommandLineAPI :: !(P.Maybe P.Bool)
      -- | Whether the result is expected to be a JSON object which should be sent by value.
    , returnByValue :: !(P.Maybe P.Bool)
      -- | Whether preview should be generated for the result.
    , generatePreview :: !(P.Maybe P.Bool)
      -- | Whether execution should @await@ for resulting value and return once awaited promise is
      -- resolved.
    , awaitPromise_ :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RunScript where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "runScript" $ \_o -> RunScript
            <$> _o .: "scriptId"
            <*> _o .:? "executionContextId"
            <*> _o .:? "objectGroup"
            <*> _o .:? "silent"
            <*> _o .:? "includeCommandLineAPI"
            <*> _o .:? "returnByValue"
            <*> _o .:? "generatePreview"
            <*> _o .:? "awaitPromise"
        ago = A.withArray "runScript" $ \_a -> RunScript
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)
            <*> P.traverse A.parseJSON (_a !? 7)


------------------------------------------------------------------------------
instance A.ToJSON RunScript where
    toEncoding (RunScript _0 _1 _2 _3 _4 _5 _6 _7) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "scriptId" .= _0
        , ("executionContextId" .=) <$> _1
        , ("objectGroup" .=) <$> _2
        , ("silent" .=) <$> _3
        , ("includeCommandLineAPI" .=) <$> _4
        , ("returnByValue" .=) <$> _5
        , ("generatePreview" .=) <$> _6
        , ("awaitPromise" .=) <$> _7
        ]
    toJSON (RunScript _0 _1 _2 _3 _4 _5 _6 _7) = A.object $ P.catMaybes
        [ P.pure $ "scriptId" .= _0
        , ("executionContextId" .=) <$> _1
        , ("objectGroup" .=) <$> _2
        , ("silent" .=) <$> _3
        , ("includeCommandLineAPI" .=) <$> _4
        , ("returnByValue" .=) <$> _5
        , ("generatePreview" .=) <$> _6
        , ("awaitPromise" .=) <$> _7
        ]


------------------------------------------------------------------------------
instance P.Semigroup RunScript where
    RunScript _0 _1 _2 _3 _4 _5 _6 _7 <> RunScript _ __1 __2 __3 __4 __5 __6 __7 = RunScript _0 (_1 <|> __1) (_2 <|> __2) (_3 <|> __3) (_4 <|> __4) (_5 <|> __5) (_6 <|> __6) (_7 <|> __7)


------------------------------------------------------------------------------
-- | Runs script with given id in a given context.
data RunScriptResult = RunScriptResult
    { -- | Run result.
      result :: !RemoteObject
      -- | Exception details.
    , exceptionDetails :: !(P.Maybe ExceptionDetails)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RunScriptResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "runScriptResult" $ \_o -> RunScriptResult
            <$> _o .: "result"
            <*> _o .:? "exceptionDetails"
        ago = A.withArray "runScriptResult" $ \_a -> RunScriptResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON RunScriptResult where
    toEncoding (RunScriptResult _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "result" .= _0
        , ("exceptionDetails" .=) <$> _1
        ]
    toJSON (RunScriptResult _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "result" .= _0
        , ("exceptionDetails" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup RunScriptResult where
    RunScriptResult _0 _1 <> RunScriptResult _ __1 = RunScriptResult _0 (_1 <|> __1)


------------------------------------------------------------------------------
instance M.Method RunScript where
    type Result RunScript = RunScriptResult
    name _ = "Runtime.runScript"


------------------------------------------------------------------------------
-- | Runs script with given id in a given context.
runScript
    :: ScriptId
    -- ^ Id of the script to run.

    -> RunScript
runScript _0 = RunScript _0 P.empty P.empty P.empty P.empty P.empty P.empty P.empty


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
    name _ = "Runtime.setAsyncCallStackDepth"


------------------------------------------------------------------------------
-- | Enables or disables async call stacks tracking.
setAsyncCallStackDepth
    :: P.Int
    -- ^ Maximum depth of async call stacks. Setting to @0@ will effectively disable collecting async

    -- call stacks (default).

    -> SetAsyncCallStackDepth
setAsyncCallStackDepth _0 = SetAsyncCallStackDepth _0


------------------------------------------------------------------------------
{-# WARNING SetCustomObjectFormatterEnabled "This feature is marked as EXPERIMENTAL." #-}
data SetCustomObjectFormatterEnabled = SetCustomObjectFormatterEnabled
    { enabled :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetCustomObjectFormatterEnabled where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setCustomObjectFormatterEnabled" $ \_o -> SetCustomObjectFormatterEnabled
            <$> _o .: "enabled"
        ago = A.withArray "setCustomObjectFormatterEnabled" $ \_a -> SetCustomObjectFormatterEnabled
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetCustomObjectFormatterEnabled where
    toEncoding (SetCustomObjectFormatterEnabled _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "enabled" .= _0
        ]
    toJSON (SetCustomObjectFormatterEnabled _0) = A.object $ P.catMaybes
        [ P.pure $ "enabled" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetCustomObjectFormatterEnabled where
    SetCustomObjectFormatterEnabled _0 <> SetCustomObjectFormatterEnabled _ = SetCustomObjectFormatterEnabled _0


------------------------------------------------------------------------------
instance M.Method SetCustomObjectFormatterEnabled where
    type Result SetCustomObjectFormatterEnabled = ()
    name _ = "Runtime.setCustomObjectFormatterEnabled"


------------------------------------------------------------------------------
{-# WARNING setCustomObjectFormatterEnabled "This feature is marked as EXPERIMENTAL." #-}
setCustomObjectFormatterEnabled
    :: P.Bool
    -> SetCustomObjectFormatterEnabled
setCustomObjectFormatterEnabled _0 = SetCustomObjectFormatterEnabled _0


------------------------------------------------------------------------------
{-# WARNING SetMaxCallStackSizeToCapture "This feature is marked as EXPERIMENTAL." #-}
data SetMaxCallStackSizeToCapture = SetMaxCallStackSizeToCapture
    { size :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetMaxCallStackSizeToCapture where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setMaxCallStackSizeToCapture" $ \_o -> SetMaxCallStackSizeToCapture
            <$> _o .: "size"
        ago = A.withArray "setMaxCallStackSizeToCapture" $ \_a -> SetMaxCallStackSizeToCapture
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetMaxCallStackSizeToCapture where
    toEncoding (SetMaxCallStackSizeToCapture _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "size" .= _0
        ]
    toJSON (SetMaxCallStackSizeToCapture _0) = A.object $ P.catMaybes
        [ P.pure $ "size" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetMaxCallStackSizeToCapture where
    SetMaxCallStackSizeToCapture _0 <> SetMaxCallStackSizeToCapture _ = SetMaxCallStackSizeToCapture _0


------------------------------------------------------------------------------
instance M.Method SetMaxCallStackSizeToCapture where
    type Result SetMaxCallStackSizeToCapture = ()
    name _ = "Runtime.setMaxCallStackSizeToCapture"


------------------------------------------------------------------------------
{-# WARNING setMaxCallStackSizeToCapture "This feature is marked as EXPERIMENTAL." #-}
setMaxCallStackSizeToCapture
    :: P.Int
    -> SetMaxCallStackSizeToCapture
setMaxCallStackSizeToCapture _0 = SetMaxCallStackSizeToCapture _0


------------------------------------------------------------------------------
-- | Terminate current or next JavaScript execution.
-- Will cancel the termination when the outer-most script execution ends.
{-# WARNING TerminateExecution "This feature is marked as EXPERIMENTAL." #-}
data TerminateExecution = TerminateExecution
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TerminateExecution where
    parseJSON A.Null = P.pure TerminateExecution
    parseJSON v = A.withArray "terminateExecution" go v
        <|> A.withObject "terminateExecution" go v
      where
        go _ = P.pure TerminateExecution


------------------------------------------------------------------------------
instance A.ToJSON TerminateExecution where
    toEncoding TerminateExecution = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON TerminateExecution = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup TerminateExecution where
    TerminateExecution <> TerminateExecution = TerminateExecution


------------------------------------------------------------------------------
instance P.Monoid TerminateExecution where
    mempty = TerminateExecution


------------------------------------------------------------------------------
instance M.Method TerminateExecution where
    type Result TerminateExecution = ()
    name _ = "Runtime.terminateExecution"


------------------------------------------------------------------------------
-- | Terminate current or next JavaScript execution.
-- Will cancel the termination when the outer-most script execution ends.
{-# WARNING terminateExecution "This feature is marked as EXPERIMENTAL." #-}
terminateExecution
    :: TerminateExecution
terminateExecution = TerminateExecution


------------------------------------------------------------------------------
-- | If executionContextId is empty, adds binding with the given name on the
-- global objects of all inspected contexts, including those created later,
-- bindings survive reloads.
-- If executionContextId is specified, adds binding only on global object of
-- given execution context.
-- Binding function takes exactly one argument, this argument should be string,
-- in case of any other input, function throws an exception.
-- Each binding function call produces Runtime.bindingCalled notification.
{-# WARNING AddBinding "This feature is marked as EXPERIMENTAL." #-}
data AddBinding = AddBinding
    { name :: !T.Text
    , executionContextId :: !(P.Maybe ExecutionContextId)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AddBinding where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "addBinding" $ \_o -> AddBinding
            <$> _o .: "name"
            <*> _o .:? "executionContextId"
        ago = A.withArray "addBinding" $ \_a -> AddBinding
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON AddBinding where
    toEncoding (AddBinding _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "name" .= _0
        , ("executionContextId" .=) <$> _1
        ]
    toJSON (AddBinding _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "name" .= _0
        , ("executionContextId" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup AddBinding where
    AddBinding _0 _1 <> AddBinding _ __1 = AddBinding _0 (_1 <|> __1)


------------------------------------------------------------------------------
instance M.Method AddBinding where
    type Result AddBinding = ()
    name _ = "Runtime.addBinding"


------------------------------------------------------------------------------
-- | If executionContextId is empty, adds binding with the given name on the
-- global objects of all inspected contexts, including those created later,
-- bindings survive reloads.
-- If executionContextId is specified, adds binding only on global object of
-- given execution context.
-- Binding function takes exactly one argument, this argument should be string,
-- in case of any other input, function throws an exception.
-- Each binding function call produces Runtime.bindingCalled notification.
{-# WARNING addBinding "This feature is marked as EXPERIMENTAL." #-}
addBinding
    :: T.Text
    -> AddBinding
addBinding _0 = AddBinding _0 P.empty


------------------------------------------------------------------------------
-- | This method does not remove binding function from global object but
-- unsubscribes current runtime agent from Runtime.bindingCalled notifications.
{-# WARNING RemoveBinding "This feature is marked as EXPERIMENTAL." #-}
data RemoveBinding = RemoveBinding
    { name :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RemoveBinding where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "removeBinding" $ \_o -> RemoveBinding
            <$> _o .: "name"
        ago = A.withArray "removeBinding" $ \_a -> RemoveBinding
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON RemoveBinding where
    toEncoding (RemoveBinding _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "name" .= _0
        ]
    toJSON (RemoveBinding _0) = A.object $ P.catMaybes
        [ P.pure $ "name" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup RemoveBinding where
    RemoveBinding _0 <> RemoveBinding _ = RemoveBinding _0


------------------------------------------------------------------------------
instance M.Method RemoveBinding where
    type Result RemoveBinding = ()
    name _ = "Runtime.removeBinding"


------------------------------------------------------------------------------
-- | This method does not remove binding function from global object but
-- unsubscribes current runtime agent from Runtime.bindingCalled notifications.
{-# WARNING removeBinding "This feature is marked as EXPERIMENTAL." #-}
removeBinding
    :: T.Text
    -> RemoveBinding
removeBinding _0 = RemoveBinding _0


------------------------------------------------------------------------------
-- | Notification is issued every time when binding is called.
{-# WARNING BindingCalled "This feature is marked as EXPERIMENTAL." #-}
data BindingCalled = BindingCalled
    { name :: !T.Text
    , payload :: !T.Text
      -- | Identifier of the context where the call was made.
    , executionContextId :: !ExecutionContextId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON BindingCalled where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "bindingCalled" $ \_o -> BindingCalled
            <$> _o .: "name"
            <*> _o .: "payload"
            <*> _o .: "executionContextId"
        ago = A.withArray "bindingCalled" $ \_a -> BindingCalled
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON BindingCalled where
    toEncoding (BindingCalled _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "payload" .= _1
        , P.pure $ "executionContextId" .= _2
        ]
    toJSON (BindingCalled _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "payload" .= _1
        , P.pure $ "executionContextId" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup BindingCalled where
    BindingCalled _0 _1 _2 <> BindingCalled _ _ _ = BindingCalled _0 _1 _2


------------------------------------------------------------------------------
instance E.Event BindingCalled where
    type Result BindingCalled = BindingCalled
    name _ = "Runtime.bindingCalled"


------------------------------------------------------------------------------
-- | Notification is issued every time when binding is called.
{-# WARNING bindingCalled "This feature is marked as EXPERIMENTAL." #-}
bindingCalled :: P.Proxy BindingCalled
bindingCalled = P.Proxy


------------------------------------------------------------------------------
-- | Issued when console API was called.
{-# WARNING context "This feature is marked as EXPERIMENTAL." #-}
data ConsoleAPICalled = ConsoleAPICalled
    { -- | Type of the call.
      type_ :: !Type__
      -- | Call arguments.
    , args :: ![RemoteObject]
      -- | Identifier of the context where the call was made.
    , executionContextId :: !ExecutionContextId
      -- | Call timestamp.
    , timestamp :: !Timestamp
      -- | Stack trace captured when the call was made. The async stack chain is automatically reported for
      -- the following call types: @assert@, @error@, @trace@, @warning@. For other types the async call
      -- chain can be retrieved using @Debugger.getStackTrace@ and @stackTrace.parentId@ field.
    , stackTrace :: !(P.Maybe StackTrace)
      -- | Console context descriptor for calls on non-default console context (not console.*):
      -- 'anonymous#unique-logger-id' for call on unnamed context, 'name#unique-logger-id' for call
      -- on named context.
    , context :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ConsoleAPICalled where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "consoleAPICalled" $ \_o -> ConsoleAPICalled
            <$> _o .: "type"
            <*> _o .: "args"
            <*> _o .: "executionContextId"
            <*> _o .: "timestamp"
            <*> _o .:? "stackTrace"
            <*> _o .:? "context"
        ago = A.withArray "consoleAPICalled" $ \_a -> ConsoleAPICalled
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)


------------------------------------------------------------------------------
instance A.ToJSON ConsoleAPICalled where
    toEncoding (ConsoleAPICalled _0 _1 _2 _3 _4 _5) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "type" .= _0
        , P.pure $ "args" .= _1
        , P.pure $ "executionContextId" .= _2
        , P.pure $ "timestamp" .= _3
        , ("stackTrace" .=) <$> _4
        , ("context" .=) <$> _5
        ]
    toJSON (ConsoleAPICalled _0 _1 _2 _3 _4 _5) = A.object $ P.catMaybes
        [ P.pure $ "type" .= _0
        , P.pure $ "args" .= _1
        , P.pure $ "executionContextId" .= _2
        , P.pure $ "timestamp" .= _3
        , ("stackTrace" .=) <$> _4
        , ("context" .=) <$> _5
        ]


------------------------------------------------------------------------------
instance P.Semigroup ConsoleAPICalled where
    ConsoleAPICalled _0 _1 _2 _3 _4 _5 <> ConsoleAPICalled _ _ _ _ __4 __5 = ConsoleAPICalled _0 _1 _2 _3 (_4 <|> __4) (_5 <|> __5)


------------------------------------------------------------------------------
data Type__
    = Log
    | Debug
    | Info
    | Error__
    | Warning
    | Dir
    | Dirxml
    | Table
    | Trace
    | Clear
    | StartGroup
    | StartGroupCollapsed
    | EndGroup
    | Assert
    | Profile
    | ProfileEnd
    | Count
    | TimeEnd
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Type__ where
    parseJSON = A.withText "Type" $ \t -> case t of
        "log" -> P.pure Log
        "debug" -> P.pure Debug
        "info" -> P.pure Info
        "error" -> P.pure Error__
        "warning" -> P.pure Warning
        "dir" -> P.pure Dir
        "dirxml" -> P.pure Dirxml
        "table" -> P.pure Table
        "trace" -> P.pure Trace
        "clear" -> P.pure Clear
        "startGroup" -> P.pure StartGroup
        "startGroupCollapsed" -> P.pure StartGroupCollapsed
        "endGroup" -> P.pure EndGroup
        "assert" -> P.pure Assert
        "profile" -> P.pure Profile
        "profileEnd" -> P.pure ProfileEnd
        "count" -> P.pure Count
        "timeEnd" -> P.pure TimeEnd
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Type__ where
    toJSON Log = "log"
    toJSON Debug = "debug"
    toJSON Info = "info"
    toJSON Error__ = "error"
    toJSON Warning = "warning"
    toJSON Dir = "dir"
    toJSON Dirxml = "dirxml"
    toJSON Table = "table"
    toJSON Trace = "trace"
    toJSON Clear = "clear"
    toJSON StartGroup = "startGroup"
    toJSON StartGroupCollapsed = "startGroupCollapsed"
    toJSON EndGroup = "endGroup"
    toJSON Assert = "assert"
    toJSON Profile = "profile"
    toJSON ProfileEnd = "profileEnd"
    toJSON Count = "count"
    toJSON TimeEnd = "timeEnd"


------------------------------------------------------------------------------
instance E.Event ConsoleAPICalled where
    type Result ConsoleAPICalled = ConsoleAPICalled
    name _ = "Runtime.consoleAPICalled"


------------------------------------------------------------------------------
-- | Issued when console API was called.
consoleAPICalled :: P.Proxy ConsoleAPICalled
consoleAPICalled = P.Proxy


------------------------------------------------------------------------------
-- | Issued when unhandled exception was revoked.
data ExceptionRevoked = ExceptionRevoked
    { -- | Reason describing why exception was revoked.
      reason :: !T.Text
      -- | The id of revoked exception, as reported in @exceptionThrown@.
    , exceptionId :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ExceptionRevoked where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "exceptionRevoked" $ \_o -> ExceptionRevoked
            <$> _o .: "reason"
            <*> _o .: "exceptionId"
        ago = A.withArray "exceptionRevoked" $ \_a -> ExceptionRevoked
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON ExceptionRevoked where
    toEncoding (ExceptionRevoked _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "reason" .= _0
        , P.pure $ "exceptionId" .= _1
        ]
    toJSON (ExceptionRevoked _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "reason" .= _0
        , P.pure $ "exceptionId" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup ExceptionRevoked where
    ExceptionRevoked _0 _1 <> ExceptionRevoked _ _ = ExceptionRevoked _0 _1


------------------------------------------------------------------------------
instance E.Event ExceptionRevoked where
    type Result ExceptionRevoked = ExceptionRevoked
    name _ = "Runtime.exceptionRevoked"


------------------------------------------------------------------------------
-- | Issued when unhandled exception was revoked.
exceptionRevoked :: P.Proxy ExceptionRevoked
exceptionRevoked = P.Proxy


------------------------------------------------------------------------------
-- | Issued when exception was thrown and unhandled.
data ExceptionThrown = ExceptionThrown
    { -- | Timestamp of the exception.
      timestamp :: !Timestamp
    , exceptionDetails :: !ExceptionDetails
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ExceptionThrown where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "exceptionThrown" $ \_o -> ExceptionThrown
            <$> _o .: "timestamp"
            <*> _o .: "exceptionDetails"
        ago = A.withArray "exceptionThrown" $ \_a -> ExceptionThrown
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON ExceptionThrown where
    toEncoding (ExceptionThrown _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "timestamp" .= _0
        , P.pure $ "exceptionDetails" .= _1
        ]
    toJSON (ExceptionThrown _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "timestamp" .= _0
        , P.pure $ "exceptionDetails" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup ExceptionThrown where
    ExceptionThrown _0 _1 <> ExceptionThrown _ _ = ExceptionThrown _0 _1


------------------------------------------------------------------------------
instance E.Event ExceptionThrown where
    type Result ExceptionThrown = ExceptionThrown
    name _ = "Runtime.exceptionThrown"


------------------------------------------------------------------------------
-- | Issued when exception was thrown and unhandled.
exceptionThrown :: P.Proxy ExceptionThrown
exceptionThrown = P.Proxy


------------------------------------------------------------------------------
-- | Issued when new execution context is created.
data ExecutionContextCreated = ExecutionContextCreated
    { -- | A newly created execution context.
      context :: !ExecutionContextDescription
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ExecutionContextCreated where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "executionContextCreated" $ \_o -> ExecutionContextCreated
            <$> _o .: "context"
        ago = A.withArray "executionContextCreated" $ \_a -> ExecutionContextCreated
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON ExecutionContextCreated where
    toEncoding (ExecutionContextCreated _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "context" .= _0
        ]
    toJSON (ExecutionContextCreated _0) = A.object $ P.catMaybes
        [ P.pure $ "context" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup ExecutionContextCreated where
    ExecutionContextCreated _0 <> ExecutionContextCreated _ = ExecutionContextCreated _0


------------------------------------------------------------------------------
instance E.Event ExecutionContextCreated where
    type Result ExecutionContextCreated = ExecutionContextCreated
    name _ = "Runtime.executionContextCreated"


------------------------------------------------------------------------------
-- | Issued when new execution context is created.
executionContextCreated :: P.Proxy ExecutionContextCreated
executionContextCreated = P.Proxy


------------------------------------------------------------------------------
-- | Issued when execution context is destroyed.
data ExecutionContextDestroyed = ExecutionContextDestroyed
    { -- | Id of the destroyed context
      executionContextId :: !ExecutionContextId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ExecutionContextDestroyed where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "executionContextDestroyed" $ \_o -> ExecutionContextDestroyed
            <$> _o .: "executionContextId"
        ago = A.withArray "executionContextDestroyed" $ \_a -> ExecutionContextDestroyed
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON ExecutionContextDestroyed where
    toEncoding (ExecutionContextDestroyed _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "executionContextId" .= _0
        ]
    toJSON (ExecutionContextDestroyed _0) = A.object $ P.catMaybes
        [ P.pure $ "executionContextId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup ExecutionContextDestroyed where
    ExecutionContextDestroyed _0 <> ExecutionContextDestroyed _ = ExecutionContextDestroyed _0


------------------------------------------------------------------------------
instance E.Event ExecutionContextDestroyed where
    type Result ExecutionContextDestroyed = ExecutionContextDestroyed
    name _ = "Runtime.executionContextDestroyed"


------------------------------------------------------------------------------
-- | Issued when execution context is destroyed.
executionContextDestroyed :: P.Proxy ExecutionContextDestroyed
executionContextDestroyed = P.Proxy


------------------------------------------------------------------------------
-- | Issued when all executionContexts were cleared in browser
data ExecutionContextsCleared = ExecutionContextsCleared
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ExecutionContextsCleared where
    parseJSON A.Null = P.pure ExecutionContextsCleared
    parseJSON v = A.withArray "executionContextsCleared" go v
        <|> A.withObject "executionContextsCleared" go v
      where
        go _ = P.pure ExecutionContextsCleared


------------------------------------------------------------------------------
instance A.ToJSON ExecutionContextsCleared where
    toEncoding ExecutionContextsCleared = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON ExecutionContextsCleared = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup ExecutionContextsCleared where
    ExecutionContextsCleared <> ExecutionContextsCleared = ExecutionContextsCleared


------------------------------------------------------------------------------
instance P.Monoid ExecutionContextsCleared where
    mempty = ExecutionContextsCleared


------------------------------------------------------------------------------
instance E.Event ExecutionContextsCleared where
    type Result ExecutionContextsCleared = ()
    name _ = "Runtime.executionContextsCleared"


------------------------------------------------------------------------------
-- | Issued when all executionContexts were cleared in browser
executionContextsCleared :: P.Proxy ExecutionContextsCleared
executionContextsCleared = P.Proxy


------------------------------------------------------------------------------
-- | Issued when object should be inspected (for example, as a result of inspect() command line API
-- call).
data InspectRequested = InspectRequested
    { object :: !RemoteObject
    , hints :: !A.Object
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON InspectRequested where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "inspectRequested" $ \_o -> InspectRequested
            <$> _o .: "object"
            <*> _o .: "hints"
        ago = A.withArray "inspectRequested" $ \_a -> InspectRequested
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON InspectRequested where
    toEncoding (InspectRequested _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "object" .= _0
        , P.pure $ "hints" .= _1
        ]
    toJSON (InspectRequested _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "object" .= _0
        , P.pure $ "hints" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup InspectRequested where
    InspectRequested _0 _1 <> InspectRequested _ _ = InspectRequested _0 _1


------------------------------------------------------------------------------
instance E.Event InspectRequested where
    type Result InspectRequested = InspectRequested
    name _ = "Runtime.inspectRequested"


------------------------------------------------------------------------------
-- | Issued when object should be inspected (for example, as a result of inspect() command line API
-- call).
inspectRequested :: P.Proxy InspectRequested
inspectRequested = P.Proxy

