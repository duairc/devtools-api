{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | DOM debugging allows setting breakpoints on particular DOM operations and events. JavaScript
-- execution will stop on these operations as if there was a regular breakpoint set.
module DevTools.API.DOMDebugger.Types
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
import qualified DevTools.API.DOM.Types as DOM
import qualified DevTools.API.Runtime.Types as Runtime


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | DOM breakpoint type.
data DOMBreakpointType
    = SubtreeModified
    | AttributeModified
    | NodeRemoved
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DOMBreakpointType where
    parseJSON = A.withText "DOMBreakpointType" $ \t -> case t of
        "subtree-modified" -> P.pure SubtreeModified
        "attribute-modified" -> P.pure AttributeModified
        "node-removed" -> P.pure NodeRemoved
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON DOMBreakpointType where
    toJSON SubtreeModified = "subtree-modified"
    toJSON AttributeModified = "attribute-modified"
    toJSON NodeRemoved = "node-removed"


------------------------------------------------------------------------------
-- | Object event listener.
data EventListener = EventListener
    { -- | @EventListener@'s type.
      type_ :: !T.Text
      -- | @EventListener@'s useCapture.
    , useCapture :: !P.Bool
      -- | @EventListener@'s passive flag.
    , passive :: !P.Bool
      -- | @EventListener@'s once flag.
    , once :: !P.Bool
      -- | Script id of the handler code.
    , scriptId :: !Runtime.ScriptId
      -- | Line number in the script (0-based).
    , lineNumber :: !P.Int
      -- | Column number in the script (0-based).
    , columnNumber :: !P.Int
      -- | Event handler function value.
    , handler :: !(P.Maybe Runtime.RemoteObject)
      -- | Event original handler function value.
    , originalHandler :: !(P.Maybe Runtime.RemoteObject)
      -- | Node the listener is added to (if any).
    , backendNodeId :: !(P.Maybe DOM.BackendNodeId)
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON EventListener where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "EventListener" $ \_o -> EventListener
            <$> _o .: "type"
            <*> _o .: "useCapture"
            <*> _o .: "passive"
            <*> _o .: "once"
            <*> _o .: "scriptId"
            <*> _o .: "lineNumber"
            <*> _o .: "columnNumber"
            <*> _o .:? "handler"
            <*> _o .:? "originalHandler"
            <*> _o .:? "backendNodeId"
        ago = A.withArray "EventListener" $ \_a -> EventListener
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.maybe P.empty A.parseJSON (_a !? 6)
            <*> P.traverse A.parseJSON (_a !? 7)
            <*> P.traverse A.parseJSON (_a !? 8)
            <*> P.traverse A.parseJSON (_a !? 9)


------------------------------------------------------------------------------
instance A.ToJSON EventListener where
    toEncoding (EventListener _0 _1 _2 _3 _4 _5 _6 _7 _8 _9) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "type" .= _0
        , P.pure $ "useCapture" .= _1
        , P.pure $ "passive" .= _2
        , P.pure $ "once" .= _3
        , P.pure $ "scriptId" .= _4
        , P.pure $ "lineNumber" .= _5
        , P.pure $ "columnNumber" .= _6
        , ("handler" .=) <$> _7
        , ("originalHandler" .=) <$> _8
        , ("backendNodeId" .=) <$> _9
        ]
    toJSON (EventListener _0 _1 _2 _3 _4 _5 _6 _7 _8 _9) = A.object $ P.catMaybes
        [ P.pure $ "type" .= _0
        , P.pure $ "useCapture" .= _1
        , P.pure $ "passive" .= _2
        , P.pure $ "once" .= _3
        , P.pure $ "scriptId" .= _4
        , P.pure $ "lineNumber" .= _5
        , P.pure $ "columnNumber" .= _6
        , ("handler" .=) <$> _7
        , ("originalHandler" .=) <$> _8
        , ("backendNodeId" .=) <$> _9
        ]


------------------------------------------------------------------------------
instance P.Semigroup EventListener where
    EventListener _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 <> EventListener _ _ _ _ _ _ _ __7 __8 __9 = EventListener _0 _1 _2 _3 _4 _5 _6 (_7 <|> __7) (_8 <|> __8) (_9 <|> __9)

