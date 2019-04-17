{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.Accessibility{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

    ( module DevTools.API.Accessibility.Types
    , module DevTools.API.Accessibility
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
import           DevTools.API.Accessibility.Types
import qualified DevTools.API.DOM.Types as DOM
import qualified DevTools.API.Runtime.Types as Runtime


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Disables the accessibility domain.
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
    name _ = "Accessibility.disable"


------------------------------------------------------------------------------
-- | Disables the accessibility domain.
disable
    :: Disable
disable = Disable


------------------------------------------------------------------------------
-- | Enables the accessibility domain which causes @AXNodeId@s to remain consistent between method calls.
-- This turns on accessibility for the page, which can impact performance until accessibility is disabled.
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
    name _ = "Accessibility.enable"


------------------------------------------------------------------------------
-- | Enables the accessibility domain which causes @AXNodeId@s to remain consistent between method calls.
-- This turns on accessibility for the page, which can impact performance until accessibility is disabled.
enable
    :: Enable
enable = Enable


------------------------------------------------------------------------------
-- | Fetches the accessibility node and partial accessibility tree for this DOM node, if it exists.
{-# WARNING GetPartialAXTree "This feature is marked as EXPERIMENTAL." #-}
data GetPartialAXTree = GetPartialAXTree
    { -- | Identifier of the node to get the partial accessibility tree for.
      nodeId :: !(P.Maybe DOM.NodeId)
      -- | Identifier of the backend node to get the partial accessibility tree for.
    , backendNodeId :: !(P.Maybe DOM.BackendNodeId)
      -- | JavaScript object id of the node wrapper to get the partial accessibility tree for.
    , objectId :: !(P.Maybe Runtime.RemoteObjectId)
      -- | Whether to fetch this nodes ancestors, siblings and children. Defaults to true.
    , fetchRelatives :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetPartialAXTree where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getPartialAXTree" $ \_o -> GetPartialAXTree
            <$> _o .:? "nodeId"
            <*> _o .:? "backendNodeId"
            <*> _o .:? "objectId"
            <*> _o .:? "fetchRelatives"
        ago = A.withArray "getPartialAXTree" $ \_a -> GetPartialAXTree
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON GetPartialAXTree where
    toEncoding (GetPartialAXTree _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ ("nodeId" .=) <$> _0
        , ("backendNodeId" .=) <$> _1
        , ("objectId" .=) <$> _2
        , ("fetchRelatives" .=) <$> _3
        ]
    toJSON (GetPartialAXTree _0 _1 _2 _3) = A.object $ P.catMaybes
        [ ("nodeId" .=) <$> _0
        , ("backendNodeId" .=) <$> _1
        , ("objectId" .=) <$> _2
        , ("fetchRelatives" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetPartialAXTree where
    GetPartialAXTree _0 _1 _2 _3 <> GetPartialAXTree __0 __1 __2 __3 = GetPartialAXTree (_0 <|> __0) (_1 <|> __1) (_2 <|> __2) (_3 <|> __3)


------------------------------------------------------------------------------
instance P.Monoid GetPartialAXTree where
    mempty = GetPartialAXTree P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Fetches the accessibility node and partial accessibility tree for this DOM node, if it exists.
{-# WARNING GetPartialAXTreeResult "This feature is marked as EXPERIMENTAL." #-}
data GetPartialAXTreeResult = GetPartialAXTreeResult
    { -- | The @Accessibility.AXNode@ for this DOM node, if it exists, plus its ancestors, siblings and
      -- children, if requested.
      nodes :: ![AXNode]
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetPartialAXTreeResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getPartialAXTreeResult" $ \_o -> GetPartialAXTreeResult
            <$> _o .: "nodes"
        ago = A.withArray "getPartialAXTreeResult" $ \_a -> GetPartialAXTreeResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetPartialAXTreeResult where
    toEncoding (GetPartialAXTreeResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodes" .= _0
        ]
    toJSON (GetPartialAXTreeResult _0) = A.object $ P.catMaybes
        [ P.pure $ "nodes" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetPartialAXTreeResult where
    GetPartialAXTreeResult _0 <> GetPartialAXTreeResult _ = GetPartialAXTreeResult _0


------------------------------------------------------------------------------
instance M.Method GetPartialAXTree where
    type Result GetPartialAXTree = GetPartialAXTreeResult
    name _ = "Accessibility.getPartialAXTree"


------------------------------------------------------------------------------
-- | Fetches the accessibility node and partial accessibility tree for this DOM node, if it exists.
{-# WARNING getPartialAXTree "This feature is marked as EXPERIMENTAL." #-}
getPartialAXTree
    :: GetPartialAXTree
getPartialAXTree = GetPartialAXTree P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Fetches the entire accessibility tree
{-# WARNING GetFullAXTree "This feature is marked as EXPERIMENTAL." #-}
data GetFullAXTree = GetFullAXTree
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetFullAXTree where
    parseJSON A.Null = P.pure GetFullAXTree
    parseJSON v = A.withArray "getFullAXTree" go v
        <|> A.withObject "getFullAXTree" go v
      where
        go _ = P.pure GetFullAXTree


------------------------------------------------------------------------------
instance A.ToJSON GetFullAXTree where
    toEncoding GetFullAXTree = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetFullAXTree = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetFullAXTree where
    GetFullAXTree <> GetFullAXTree = GetFullAXTree


------------------------------------------------------------------------------
instance P.Monoid GetFullAXTree where
    mempty = GetFullAXTree


------------------------------------------------------------------------------
-- | Fetches the entire accessibility tree
{-# WARNING GetFullAXTreeResult "This feature is marked as EXPERIMENTAL." #-}
data GetFullAXTreeResult = GetFullAXTreeResult
    { nodes :: ![AXNode]
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetFullAXTreeResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getFullAXTreeResult" $ \_o -> GetFullAXTreeResult
            <$> _o .: "nodes"
        ago = A.withArray "getFullAXTreeResult" $ \_a -> GetFullAXTreeResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetFullAXTreeResult where
    toEncoding (GetFullAXTreeResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodes" .= _0
        ]
    toJSON (GetFullAXTreeResult _0) = A.object $ P.catMaybes
        [ P.pure $ "nodes" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetFullAXTreeResult where
    GetFullAXTreeResult _0 <> GetFullAXTreeResult _ = GetFullAXTreeResult _0


------------------------------------------------------------------------------
instance M.Method GetFullAXTree where
    type Result GetFullAXTree = GetFullAXTreeResult
    name _ = "Accessibility.getFullAXTree"


------------------------------------------------------------------------------
-- | Fetches the entire accessibility tree
{-# WARNING getFullAXTree "This feature is marked as EXPERIMENTAL." #-}
getFullAXTree
    :: GetFullAXTree
getFullAXTree = GetFullAXTree

