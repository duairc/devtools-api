{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Debugger domain exposes JavaScript debugging capabilities. It allows setting and removing
-- breakpoints, stepping through execution, exploring stack traces, etc.
module DevTools.API.Debugger.Types
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


-- devtools-api---------------------------------------------------------------
import qualified DevTools.API.Runtime.Types as Runtime


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Breakpoint identifier.
type BreakpointId = T.Text


------------------------------------------------------------------------------
-- | Call frame identifier.
type CallFrameId = T.Text


------------------------------------------------------------------------------
-- | Location in the source code.
data Location = Location
    { -- | Script identifier as reported in the @Debugger.scriptParsed@.
      scriptId :: !Runtime.ScriptId
      -- | Line number in the script (0-based).
    , lineNumber :: !P.Int
      -- | Column number in the script (0-based).
    , columnNumber :: !(P.Maybe P.Int)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Location where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "Location" $ \_o -> Location
            <$> _o .: "scriptId"
            <*> _o .: "lineNumber"
            <*> _o .:? "columnNumber"
        ago = A.withArray "Location" $ \_a -> Location
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON Location where
    toEncoding (Location _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "scriptId" .= _0
        , P.pure $ "lineNumber" .= _1
        , ("columnNumber" .=) <$> _2
        ]
    toJSON (Location _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "scriptId" .= _0
        , P.pure $ "lineNumber" .= _1
        , ("columnNumber" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup Location where
    Location _0 _1 _2 <> Location _ _ __2 = Location _0 _1 (_2 <|> __2)


------------------------------------------------------------------------------
-- | Location in the source code.
{-# WARNING ScriptPosition "This feature is marked as EXPERIMENTAL." #-}
data ScriptPosition = ScriptPosition
    { lineNumber :: !P.Int
    , columnNumber :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ScriptPosition where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "ScriptPosition" $ \_o -> ScriptPosition
            <$> _o .: "lineNumber"
            <*> _o .: "columnNumber"
        ago = A.withArray "ScriptPosition" $ \_a -> ScriptPosition
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON ScriptPosition where
    toEncoding (ScriptPosition _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "lineNumber" .= _0
        , P.pure $ "columnNumber" .= _1
        ]
    toJSON (ScriptPosition _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "lineNumber" .= _0
        , P.pure $ "columnNumber" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup ScriptPosition where
    ScriptPosition _0 _1 <> ScriptPosition _ _ = ScriptPosition _0 _1


------------------------------------------------------------------------------
-- | JavaScript call frame. Array of call frames form the call stack.
data CallFrame = CallFrame
    { -- | Call frame identifier. This identifier is only valid while the virtual machine is paused.
      callFrameId :: !CallFrameId
      -- | Name of the JavaScript function called on this call frame.
    , functionName :: !T.Text
      -- | Location in the source code.
    , functionLocation :: !(P.Maybe Location)
      -- | Location in the source code.
    , location :: !Location
      -- | JavaScript script name or url.
    , url :: !T.Text
      -- | Scope chain for this call frame.
    , scopeChain :: ![Scope]
      -- | @this@ object for this call frame.
    , this :: !Runtime.RemoteObject
      -- | The value being returned, if the function is at return point.
    , returnValue :: !(P.Maybe Runtime.RemoteObject)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CallFrame where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "CallFrame" $ \_o -> CallFrame
            <$> _o .: "callFrameId"
            <*> _o .: "functionName"
            <*> _o .:? "functionLocation"
            <*> _o .: "location"
            <*> _o .: "url"
            <*> _o .: "scopeChain"
            <*> _o .: "this"
            <*> _o .:? "returnValue"
        ago = A.withArray "CallFrame" $ \_a -> CallFrame
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.maybe P.empty A.parseJSON (_a !? 6)
            <*> P.traverse A.parseJSON (_a !? 7)


------------------------------------------------------------------------------
instance A.ToJSON CallFrame where
    toEncoding (CallFrame _0 _1 _2 _3 _4 _5 _6 _7) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "callFrameId" .= _0
        , P.pure $ "functionName" .= _1
        , ("functionLocation" .=) <$> _2
        , P.pure $ "location" .= _3
        , P.pure $ "url" .= _4
        , P.pure $ "scopeChain" .= _5
        , P.pure $ "this" .= _6
        , ("returnValue" .=) <$> _7
        ]
    toJSON (CallFrame _0 _1 _2 _3 _4 _5 _6 _7) = A.object $ P.catMaybes
        [ P.pure $ "callFrameId" .= _0
        , P.pure $ "functionName" .= _1
        , ("functionLocation" .=) <$> _2
        , P.pure $ "location" .= _3
        , P.pure $ "url" .= _4
        , P.pure $ "scopeChain" .= _5
        , P.pure $ "this" .= _6
        , ("returnValue" .=) <$> _7
        ]


------------------------------------------------------------------------------
instance P.Semigroup CallFrame where
    CallFrame _0 _1 _2 _3 _4 _5 _6 _7 <> CallFrame _ _ __2 _ _ _ _ __7 = CallFrame _0 _1 (_2 <|> __2) _3 _4 _5 _6 (_7 <|> __7)


------------------------------------------------------------------------------
-- | Scope description.
data Scope = Scope
    { -- | Scope type.
      type_ :: !Type
      -- | Object representing the scope. For @global@ and @with@ scopes it represents the actual
      -- object; for the rest of the scopes, it is artificial transient object enumerating scope
      -- variables as its properties.
    , object :: !Runtime.RemoteObject
    , name :: !(P.Maybe T.Text)
      -- | Location in the source code where scope starts
    , startLocation :: !(P.Maybe Location)
      -- | Location in the source code where scope ends
    , endLocation :: !(P.Maybe Location)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Scope where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "Scope" $ \_o -> Scope
            <$> _o .: "type"
            <*> _o .: "object"
            <*> _o .:? "name"
            <*> _o .:? "startLocation"
            <*> _o .:? "endLocation"
        ago = A.withArray "Scope" $ \_a -> Scope
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON Scope where
    toEncoding (Scope _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "type" .= _0
        , P.pure $ "object" .= _1
        , ("name" .=) <$> _2
        , ("startLocation" .=) <$> _3
        , ("endLocation" .=) <$> _4
        ]
    toJSON (Scope _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ P.pure $ "type" .= _0
        , P.pure $ "object" .= _1
        , ("name" .=) <$> _2
        , ("startLocation" .=) <$> _3
        , ("endLocation" .=) <$> _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup Scope where
    Scope _0 _1 _2 _3 _4 <> Scope _ _ __2 __3 __4 = Scope _0 _1 (_2 <|> __2) (_3 <|> __3) (_4 <|> __4)


------------------------------------------------------------------------------
data Type
    = Global
    | Local
    | With
    | Closure
    | Catch
    | Block
    | Script
    | Eval
    | Module
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Type where
    parseJSON = A.withText "Type" $ \t -> case t of
        "global" -> P.pure Global
        "local" -> P.pure Local
        "with" -> P.pure With
        "closure" -> P.pure Closure
        "catch" -> P.pure Catch
        "block" -> P.pure Block
        "script" -> P.pure Script
        "eval" -> P.pure Eval
        "module" -> P.pure Module
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Type where
    toJSON Global = "global"
    toJSON Local = "local"
    toJSON With = "with"
    toJSON Closure = "closure"
    toJSON Catch = "catch"
    toJSON Block = "block"
    toJSON Script = "script"
    toJSON Eval = "eval"
    toJSON Module = "module"


------------------------------------------------------------------------------
-- | Search match for resource.
data SearchMatch = SearchMatch
    { -- | Line number in resource content.
      lineNumber :: !P.Double
      -- | Line with match content.
    , lineContent :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SearchMatch where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "SearchMatch" $ \_o -> SearchMatch
            <$> _o .: "lineNumber"
            <*> _o .: "lineContent"
        ago = A.withArray "SearchMatch" $ \_a -> SearchMatch
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SearchMatch where
    toEncoding (SearchMatch _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "lineNumber" .= _0
        , P.pure $ "lineContent" .= _1
        ]
    toJSON (SearchMatch _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "lineNumber" .= _0
        , P.pure $ "lineContent" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SearchMatch where
    SearchMatch _0 _1 <> SearchMatch _ _ = SearchMatch _0 _1


------------------------------------------------------------------------------
data BreakLocation = BreakLocation
    { -- | Script identifier as reported in the @Debugger.scriptParsed@.
      scriptId :: !Runtime.ScriptId
      -- | Line number in the script (0-based).
    , lineNumber :: !P.Int
      -- | Column number in the script (0-based).
    , columnNumber :: !(P.Maybe P.Int)
    , type_ :: !(P.Maybe Type_)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON BreakLocation where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "BreakLocation" $ \_o -> BreakLocation
            <$> _o .: "scriptId"
            <*> _o .: "lineNumber"
            <*> _o .:? "columnNumber"
            <*> _o .:? "type"
        ago = A.withArray "BreakLocation" $ \_a -> BreakLocation
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON BreakLocation where
    toEncoding (BreakLocation _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "scriptId" .= _0
        , P.pure $ "lineNumber" .= _1
        , ("columnNumber" .=) <$> _2
        , ("type" .=) <$> _3
        ]
    toJSON (BreakLocation _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "scriptId" .= _0
        , P.pure $ "lineNumber" .= _1
        , ("columnNumber" .=) <$> _2
        , ("type" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup BreakLocation where
    BreakLocation _0 _1 _2 _3 <> BreakLocation _ _ __2 __3 = BreakLocation _0 _1 (_2 <|> __2) (_3 <|> __3)


------------------------------------------------------------------------------
data Type_
    = DebuggerStatement
    | Call
    | Return
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Type_ where
    parseJSON = A.withText "Type" $ \t -> case t of
        "debuggerStatement" -> P.pure DebuggerStatement
        "call" -> P.pure Call
        "return" -> P.pure Return
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Type_ where
    toJSON DebuggerStatement = "debuggerStatement"
    toJSON Call = "call"
    toJSON Return = "return"

