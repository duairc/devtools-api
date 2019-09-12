{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | This domain exposes DOM read\/write operations. Each DOM Node is represented with its mirror object
-- that has an @id@. This @id@ can be used to get additional information on the Node, resolve it into
-- the JavaScript object wrapper, etc. It is important that client receives DOM events only for the
-- nodes that are known to the client. Backend keeps track of the nodes that were sent to the client
-- and never sends the same node twice. It is client's responsibility to collect information about
-- the nodes that were sent to the client.<p>Note that @iframe@ owner elements will return
-- corresponding document elements as their child nodes.<\/p>
module DevTools.API.DOM
    ( module DevTools.API.DOM.Types
    , module DevTools.API.DOM
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
import qualified DevTools.API.DOM.Types as DOM
import           DevTools.API.DOM.Types
import qualified DevTools.API.Page.Types as Page
import qualified DevTools.API.Runtime.Types as Runtime


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Collects class names for the node with given id and all of it's child nodes.
{-# WARNING CollectClassNamesFromSubtree "This feature is marked as EXPERIMENTAL." #-}
data CollectClassNamesFromSubtree = CollectClassNamesFromSubtree
    { -- | Id of the node to collect class names.
      nodeId :: !NodeId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CollectClassNamesFromSubtree where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "collectClassNamesFromSubtree" $ \_o -> CollectClassNamesFromSubtree
            <$> _o .: "nodeId"
        ago = A.withArray "collectClassNamesFromSubtree" $ \_a -> CollectClassNamesFromSubtree
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON CollectClassNamesFromSubtree where
    toEncoding (CollectClassNamesFromSubtree _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]
    toJSON (CollectClassNamesFromSubtree _0) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup CollectClassNamesFromSubtree where
    CollectClassNamesFromSubtree _0 <> CollectClassNamesFromSubtree _ = CollectClassNamesFromSubtree _0


------------------------------------------------------------------------------
-- | Collects class names for the node with given id and all of it's child nodes.
{-# WARNING CollectClassNamesFromSubtreeResult "This feature is marked as EXPERIMENTAL." #-}
data CollectClassNamesFromSubtreeResult = CollectClassNamesFromSubtreeResult
    { -- | Class name list.
      classNames :: ![T.Text]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CollectClassNamesFromSubtreeResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "collectClassNamesFromSubtreeResult" $ \_o -> CollectClassNamesFromSubtreeResult
            <$> _o .: "classNames"
        ago = A.withArray "collectClassNamesFromSubtreeResult" $ \_a -> CollectClassNamesFromSubtreeResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON CollectClassNamesFromSubtreeResult where
    toEncoding (CollectClassNamesFromSubtreeResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "classNames" .= _0
        ]
    toJSON (CollectClassNamesFromSubtreeResult _0) = A.object $ P.catMaybes
        [ P.pure $ "classNames" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup CollectClassNamesFromSubtreeResult where
    CollectClassNamesFromSubtreeResult _0 <> CollectClassNamesFromSubtreeResult _ = CollectClassNamesFromSubtreeResult _0


------------------------------------------------------------------------------
instance M.Method CollectClassNamesFromSubtree where
    type Result CollectClassNamesFromSubtree = CollectClassNamesFromSubtreeResult
    name _ = "DOM.collectClassNamesFromSubtree"


------------------------------------------------------------------------------
-- | Collects class names for the node with given id and all of it's child nodes.
{-# WARNING collectClassNamesFromSubtree "This feature is marked as EXPERIMENTAL." #-}
collectClassNamesFromSubtree
    :: NodeId
    -- ^ Id of the node to collect class names.

    -> CollectClassNamesFromSubtree
collectClassNamesFromSubtree _0 = CollectClassNamesFromSubtree _0


------------------------------------------------------------------------------
-- | Creates a deep copy of the specified node and places it into the target container before the
-- given anchor.
{-# WARNING CopyTo "This feature is marked as EXPERIMENTAL." #-}
data CopyTo = CopyTo
    { -- | Id of the node to copy.
      nodeId :: !NodeId
      -- | Id of the element to drop the copy into.
    , targetNodeId :: !NodeId
      -- | Drop the copy before this node (if absent, the copy becomes the last child of
      -- @targetNodeId@).
    , insertBeforeNodeId :: !(P.Maybe NodeId)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CopyTo where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "copyTo" $ \_o -> CopyTo
            <$> _o .: "nodeId"
            <*> _o .: "targetNodeId"
            <*> _o .:? "insertBeforeNodeId"
        ago = A.withArray "copyTo" $ \_a -> CopyTo
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON CopyTo where
    toEncoding (CopyTo _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "targetNodeId" .= _1
        , ("insertBeforeNodeId" .=) <$> _2
        ]
    toJSON (CopyTo _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "targetNodeId" .= _1
        , ("insertBeforeNodeId" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup CopyTo where
    CopyTo _0 _1 _2 <> CopyTo _ _ __2 = CopyTo _0 _1 (_2 <|> __2)


------------------------------------------------------------------------------
-- | Creates a deep copy of the specified node and places it into the target container before the
-- given anchor.
{-# WARNING CopyToResult "This feature is marked as EXPERIMENTAL." #-}
data CopyToResult = CopyToResult
    { -- | Id of the node clone.
      nodeId :: !NodeId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CopyToResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "copyToResult" $ \_o -> CopyToResult
            <$> _o .: "nodeId"
        ago = A.withArray "copyToResult" $ \_a -> CopyToResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON CopyToResult where
    toEncoding (CopyToResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]
    toJSON (CopyToResult _0) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup CopyToResult where
    CopyToResult _0 <> CopyToResult _ = CopyToResult _0


------------------------------------------------------------------------------
instance M.Method CopyTo where
    type Result CopyTo = CopyToResult
    name _ = "DOM.copyTo"


------------------------------------------------------------------------------
-- | Creates a deep copy of the specified node and places it into the target container before the
-- given anchor.
{-# WARNING copyTo "This feature is marked as EXPERIMENTAL." #-}
copyTo
    :: NodeId
    -- ^ Id of the node to copy.

    -> NodeId
    -- ^ Id of the element to drop the copy into.

    -> CopyTo
copyTo _0 _1 = CopyTo _0 _1 P.empty


------------------------------------------------------------------------------
-- | Describes node given its id, does not require domain to be enabled. Does not start tracking any
-- objects, can be used for automation.
data DescribeNode = DescribeNode
    { -- | Identifier of the node.
      nodeId :: !(P.Maybe NodeId)
      -- | Identifier of the backend node.
    , backendNodeId :: !(P.Maybe BackendNodeId)
      -- | JavaScript object id of the node wrapper.
    , objectId :: !(P.Maybe Runtime.RemoteObjectId)
      -- | The maximum depth at which children should be retrieved, defaults to 1. Use -1 for the
      -- entire subtree or provide an integer larger than 0.
    , depth :: !(P.Maybe P.Int)
      -- | Whether or not iframes and shadow roots should be traversed when returning the subtree
      -- (default is false).
    , pierce :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DescribeNode where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "describeNode" $ \_o -> DescribeNode
            <$> _o .:? "nodeId"
            <*> _o .:? "backendNodeId"
            <*> _o .:? "objectId"
            <*> _o .:? "depth"
            <*> _o .:? "pierce"
        ago = A.withArray "describeNode" $ \_a -> DescribeNode
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON DescribeNode where
    toEncoding (DescribeNode _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ ("nodeId" .=) <$> _0
        , ("backendNodeId" .=) <$> _1
        , ("objectId" .=) <$> _2
        , ("depth" .=) <$> _3
        , ("pierce" .=) <$> _4
        ]
    toJSON (DescribeNode _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ ("nodeId" .=) <$> _0
        , ("backendNodeId" .=) <$> _1
        , ("objectId" .=) <$> _2
        , ("depth" .=) <$> _3
        , ("pierce" .=) <$> _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup DescribeNode where
    DescribeNode _0 _1 _2 _3 _4 <> DescribeNode __0 __1 __2 __3 __4 = DescribeNode (_0 <|> __0) (_1 <|> __1) (_2 <|> __2) (_3 <|> __3) (_4 <|> __4)


------------------------------------------------------------------------------
instance P.Monoid DescribeNode where
    mempty = DescribeNode P.empty P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Describes node given its id, does not require domain to be enabled. Does not start tracking any
-- objects, can be used for automation.
data DescribeNodeResult = DescribeNodeResult
    { -- | Node description.
      node :: !Node
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DescribeNodeResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "describeNodeResult" $ \_o -> DescribeNodeResult
            <$> _o .: "node"
        ago = A.withArray "describeNodeResult" $ \_a -> DescribeNodeResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON DescribeNodeResult where
    toEncoding (DescribeNodeResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "node" .= _0
        ]
    toJSON (DescribeNodeResult _0) = A.object $ P.catMaybes
        [ P.pure $ "node" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup DescribeNodeResult where
    DescribeNodeResult _0 <> DescribeNodeResult _ = DescribeNodeResult _0


------------------------------------------------------------------------------
instance M.Method DescribeNode where
    type Result DescribeNode = DescribeNodeResult
    name _ = "DOM.describeNode"


------------------------------------------------------------------------------
-- | Describes node given its id, does not require domain to be enabled. Does not start tracking any
-- objects, can be used for automation.
describeNode
    :: DescribeNode
describeNode = DescribeNode P.empty P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Disables DOM agent for the given page.
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
    name _ = "DOM.disable"


------------------------------------------------------------------------------
-- | Disables DOM agent for the given page.
disable
    :: Disable
disable = Disable


------------------------------------------------------------------------------
-- | Discards search results from the session with the given id. @getSearchResults@ should no longer
-- be called for that search.
{-# WARNING DiscardSearchResults "This feature is marked as EXPERIMENTAL." #-}
data DiscardSearchResults = DiscardSearchResults
    { -- | Unique search session identifier.
      searchId :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DiscardSearchResults where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "discardSearchResults" $ \_o -> DiscardSearchResults
            <$> _o .: "searchId"
        ago = A.withArray "discardSearchResults" $ \_a -> DiscardSearchResults
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON DiscardSearchResults where
    toEncoding (DiscardSearchResults _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "searchId" .= _0
        ]
    toJSON (DiscardSearchResults _0) = A.object $ P.catMaybes
        [ P.pure $ "searchId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup DiscardSearchResults where
    DiscardSearchResults _0 <> DiscardSearchResults _ = DiscardSearchResults _0


------------------------------------------------------------------------------
instance M.Method DiscardSearchResults where
    type Result DiscardSearchResults = ()
    name _ = "DOM.discardSearchResults"


------------------------------------------------------------------------------
-- | Discards search results from the session with the given id. @getSearchResults@ should no longer
-- be called for that search.
{-# WARNING discardSearchResults "This feature is marked as EXPERIMENTAL." #-}
discardSearchResults
    :: T.Text
    -- ^ Unique search session identifier.

    -> DiscardSearchResults
discardSearchResults _0 = DiscardSearchResults _0


------------------------------------------------------------------------------
-- | Enables DOM agent for the given page.
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
    name _ = "DOM.enable"


------------------------------------------------------------------------------
-- | Enables DOM agent for the given page.
enable
    :: Enable
enable = Enable


------------------------------------------------------------------------------
-- | Focuses the given element.
data Focus = Focus
    { -- | Identifier of the node.
      nodeId :: !(P.Maybe NodeId)
      -- | Identifier of the backend node.
    , backendNodeId :: !(P.Maybe BackendNodeId)
      -- | JavaScript object id of the node wrapper.
    , objectId :: !(P.Maybe Runtime.RemoteObjectId)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Focus where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "focus" $ \_o -> Focus
            <$> _o .:? "nodeId"
            <*> _o .:? "backendNodeId"
            <*> _o .:? "objectId"
        ago = A.withArray "focus" $ \_a -> Focus
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON Focus where
    toEncoding (Focus _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ ("nodeId" .=) <$> _0
        , ("backendNodeId" .=) <$> _1
        , ("objectId" .=) <$> _2
        ]
    toJSON (Focus _0 _1 _2) = A.object $ P.catMaybes
        [ ("nodeId" .=) <$> _0
        , ("backendNodeId" .=) <$> _1
        , ("objectId" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup Focus where
    Focus _0 _1 _2 <> Focus __0 __1 __2 = Focus (_0 <|> __0) (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
instance P.Monoid Focus where
    mempty = Focus P.empty P.empty P.empty


------------------------------------------------------------------------------
instance M.Method Focus where
    type Result Focus = ()
    name _ = "DOM.focus"


------------------------------------------------------------------------------
-- | Focuses the given element.
focus
    :: Focus
focus = Focus P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Returns attributes for the specified node.
data GetAttributes = GetAttributes
    { -- | Id of the node to retrieve attibutes for.
      nodeId :: !NodeId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetAttributes where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getAttributes" $ \_o -> GetAttributes
            <$> _o .: "nodeId"
        ago = A.withArray "getAttributes" $ \_a -> GetAttributes
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetAttributes where
    toEncoding (GetAttributes _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]
    toJSON (GetAttributes _0) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetAttributes where
    GetAttributes _0 <> GetAttributes _ = GetAttributes _0


------------------------------------------------------------------------------
-- | Returns attributes for the specified node.
data GetAttributesResult = GetAttributesResult
    { -- | An interleaved array of node attribute names and values.
      attributes :: ![T.Text]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetAttributesResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getAttributesResult" $ \_o -> GetAttributesResult
            <$> _o .: "attributes"
        ago = A.withArray "getAttributesResult" $ \_a -> GetAttributesResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetAttributesResult where
    toEncoding (GetAttributesResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "attributes" .= _0
        ]
    toJSON (GetAttributesResult _0) = A.object $ P.catMaybes
        [ P.pure $ "attributes" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetAttributesResult where
    GetAttributesResult _0 <> GetAttributesResult _ = GetAttributesResult _0


------------------------------------------------------------------------------
instance M.Method GetAttributes where
    type Result GetAttributes = GetAttributesResult
    name _ = "DOM.getAttributes"


------------------------------------------------------------------------------
-- | Returns attributes for the specified node.
getAttributes
    :: NodeId
    -- ^ Id of the node to retrieve attibutes for.

    -> GetAttributes
getAttributes _0 = GetAttributes _0


------------------------------------------------------------------------------
-- | Returns boxes for the given node.
data GetBoxModel = GetBoxModel
    { -- | Identifier of the node.
      nodeId :: !(P.Maybe NodeId)
      -- | Identifier of the backend node.
    , backendNodeId :: !(P.Maybe BackendNodeId)
      -- | JavaScript object id of the node wrapper.
    , objectId :: !(P.Maybe Runtime.RemoteObjectId)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetBoxModel where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getBoxModel" $ \_o -> GetBoxModel
            <$> _o .:? "nodeId"
            <*> _o .:? "backendNodeId"
            <*> _o .:? "objectId"
        ago = A.withArray "getBoxModel" $ \_a -> GetBoxModel
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON GetBoxModel where
    toEncoding (GetBoxModel _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ ("nodeId" .=) <$> _0
        , ("backendNodeId" .=) <$> _1
        , ("objectId" .=) <$> _2
        ]
    toJSON (GetBoxModel _0 _1 _2) = A.object $ P.catMaybes
        [ ("nodeId" .=) <$> _0
        , ("backendNodeId" .=) <$> _1
        , ("objectId" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetBoxModel where
    GetBoxModel _0 _1 _2 <> GetBoxModel __0 __1 __2 = GetBoxModel (_0 <|> __0) (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
instance P.Monoid GetBoxModel where
    mempty = GetBoxModel P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Returns boxes for the given node.
data GetBoxModelResult = GetBoxModelResult
    { -- | Box model for the node.
      model :: !BoxModel
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetBoxModelResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getBoxModelResult" $ \_o -> GetBoxModelResult
            <$> _o .: "model"
        ago = A.withArray "getBoxModelResult" $ \_a -> GetBoxModelResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetBoxModelResult where
    toEncoding (GetBoxModelResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "model" .= _0
        ]
    toJSON (GetBoxModelResult _0) = A.object $ P.catMaybes
        [ P.pure $ "model" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetBoxModelResult where
    GetBoxModelResult _0 <> GetBoxModelResult _ = GetBoxModelResult _0


------------------------------------------------------------------------------
instance M.Method GetBoxModel where
    type Result GetBoxModel = GetBoxModelResult
    name _ = "DOM.getBoxModel"


------------------------------------------------------------------------------
-- | Returns boxes for the given node.
getBoxModel
    :: GetBoxModel
getBoxModel = GetBoxModel P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Returns quads that describe node position on the page. This method
-- might return multiple quads for inline nodes.
{-# WARNING GetContentQuads "This feature is marked as EXPERIMENTAL." #-}
data GetContentQuads = GetContentQuads
    { -- | Identifier of the node.
      nodeId :: !(P.Maybe NodeId)
      -- | Identifier of the backend node.
    , backendNodeId :: !(P.Maybe BackendNodeId)
      -- | JavaScript object id of the node wrapper.
    , objectId :: !(P.Maybe Runtime.RemoteObjectId)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetContentQuads where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getContentQuads" $ \_o -> GetContentQuads
            <$> _o .:? "nodeId"
            <*> _o .:? "backendNodeId"
            <*> _o .:? "objectId"
        ago = A.withArray "getContentQuads" $ \_a -> GetContentQuads
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON GetContentQuads where
    toEncoding (GetContentQuads _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ ("nodeId" .=) <$> _0
        , ("backendNodeId" .=) <$> _1
        , ("objectId" .=) <$> _2
        ]
    toJSON (GetContentQuads _0 _1 _2) = A.object $ P.catMaybes
        [ ("nodeId" .=) <$> _0
        , ("backendNodeId" .=) <$> _1
        , ("objectId" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetContentQuads where
    GetContentQuads _0 _1 _2 <> GetContentQuads __0 __1 __2 = GetContentQuads (_0 <|> __0) (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
instance P.Monoid GetContentQuads where
    mempty = GetContentQuads P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Returns quads that describe node position on the page. This method
-- might return multiple quads for inline nodes.
{-# WARNING GetContentQuadsResult "This feature is marked as EXPERIMENTAL." #-}
data GetContentQuadsResult = GetContentQuadsResult
    { -- | Quads that describe node layout relative to viewport.
      quads :: ![Quad]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetContentQuadsResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getContentQuadsResult" $ \_o -> GetContentQuadsResult
            <$> _o .: "quads"
        ago = A.withArray "getContentQuadsResult" $ \_a -> GetContentQuadsResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetContentQuadsResult where
    toEncoding (GetContentQuadsResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "quads" .= _0
        ]
    toJSON (GetContentQuadsResult _0) = A.object $ P.catMaybes
        [ P.pure $ "quads" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetContentQuadsResult where
    GetContentQuadsResult _0 <> GetContentQuadsResult _ = GetContentQuadsResult _0


------------------------------------------------------------------------------
instance M.Method GetContentQuads where
    type Result GetContentQuads = GetContentQuadsResult
    name _ = "DOM.getContentQuads"


------------------------------------------------------------------------------
-- | Returns quads that describe node position on the page. This method
-- might return multiple quads for inline nodes.
{-# WARNING getContentQuads "This feature is marked as EXPERIMENTAL." #-}
getContentQuads
    :: GetContentQuads
getContentQuads = GetContentQuads P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Returns the root DOM node (and optionally the subtree) to the caller.
data GetDocument = GetDocument
    { -- | The maximum depth at which children should be retrieved, defaults to 1. Use -1 for the
      -- entire subtree or provide an integer larger than 0.
      depth :: !(P.Maybe P.Int)
      -- | Whether or not iframes and shadow roots should be traversed when returning the subtree
      -- (default is false).
    , pierce :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetDocument where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getDocument" $ \_o -> GetDocument
            <$> _o .:? "depth"
            <*> _o .:? "pierce"
        ago = A.withArray "getDocument" $ \_a -> GetDocument
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON GetDocument where
    toEncoding (GetDocument _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ ("depth" .=) <$> _0
        , ("pierce" .=) <$> _1
        ]
    toJSON (GetDocument _0 _1) = A.object $ P.catMaybes
        [ ("depth" .=) <$> _0
        , ("pierce" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetDocument where
    GetDocument _0 _1 <> GetDocument __0 __1 = GetDocument (_0 <|> __0) (_1 <|> __1)


------------------------------------------------------------------------------
instance P.Monoid GetDocument where
    mempty = GetDocument P.empty P.empty


------------------------------------------------------------------------------
-- | Returns the root DOM node (and optionally the subtree) to the caller.
data GetDocumentResult = GetDocumentResult
    { -- | Resulting node.
      root :: !Node
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetDocumentResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getDocumentResult" $ \_o -> GetDocumentResult
            <$> _o .: "root"
        ago = A.withArray "getDocumentResult" $ \_a -> GetDocumentResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetDocumentResult where
    toEncoding (GetDocumentResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "root" .= _0
        ]
    toJSON (GetDocumentResult _0) = A.object $ P.catMaybes
        [ P.pure $ "root" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetDocumentResult where
    GetDocumentResult _0 <> GetDocumentResult _ = GetDocumentResult _0


------------------------------------------------------------------------------
instance M.Method GetDocument where
    type Result GetDocument = GetDocumentResult
    name _ = "DOM.getDocument"


------------------------------------------------------------------------------
-- | Returns the root DOM node (and optionally the subtree) to the caller.
getDocument
    :: GetDocument
getDocument = GetDocument P.empty P.empty


------------------------------------------------------------------------------
-- | Returns the root DOM node (and optionally the subtree) to the caller.
data GetFlattenedDocument = GetFlattenedDocument
    { -- | The maximum depth at which children should be retrieved, defaults to 1. Use -1 for the
      -- entire subtree or provide an integer larger than 0.
      depth :: !(P.Maybe P.Int)
      -- | Whether or not iframes and shadow roots should be traversed when returning the subtree
      -- (default is false).
    , pierce :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetFlattenedDocument where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getFlattenedDocument" $ \_o -> GetFlattenedDocument
            <$> _o .:? "depth"
            <*> _o .:? "pierce"
        ago = A.withArray "getFlattenedDocument" $ \_a -> GetFlattenedDocument
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON GetFlattenedDocument where
    toEncoding (GetFlattenedDocument _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ ("depth" .=) <$> _0
        , ("pierce" .=) <$> _1
        ]
    toJSON (GetFlattenedDocument _0 _1) = A.object $ P.catMaybes
        [ ("depth" .=) <$> _0
        , ("pierce" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetFlattenedDocument where
    GetFlattenedDocument _0 _1 <> GetFlattenedDocument __0 __1 = GetFlattenedDocument (_0 <|> __0) (_1 <|> __1)


------------------------------------------------------------------------------
instance P.Monoid GetFlattenedDocument where
    mempty = GetFlattenedDocument P.empty P.empty


------------------------------------------------------------------------------
-- | Returns the root DOM node (and optionally the subtree) to the caller.
data GetFlattenedDocumentResult = GetFlattenedDocumentResult
    { -- | Resulting node.
      nodes :: ![Node]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetFlattenedDocumentResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getFlattenedDocumentResult" $ \_o -> GetFlattenedDocumentResult
            <$> _o .: "nodes"
        ago = A.withArray "getFlattenedDocumentResult" $ \_a -> GetFlattenedDocumentResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetFlattenedDocumentResult where
    toEncoding (GetFlattenedDocumentResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodes" .= _0
        ]
    toJSON (GetFlattenedDocumentResult _0) = A.object $ P.catMaybes
        [ P.pure $ "nodes" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetFlattenedDocumentResult where
    GetFlattenedDocumentResult _0 <> GetFlattenedDocumentResult _ = GetFlattenedDocumentResult _0


------------------------------------------------------------------------------
instance M.Method GetFlattenedDocument where
    type Result GetFlattenedDocument = GetFlattenedDocumentResult
    name _ = "DOM.getFlattenedDocument"


------------------------------------------------------------------------------
-- | Returns the root DOM node (and optionally the subtree) to the caller.
getFlattenedDocument
    :: GetFlattenedDocument
getFlattenedDocument = GetFlattenedDocument P.empty P.empty


------------------------------------------------------------------------------
-- | Returns node id at given location. Depending on whether DOM domain is enabled, nodeId is
-- either returned or not.
data GetNodeForLocation = GetNodeForLocation
    { -- | X coordinate.
      x :: !P.Int
      -- | Y coordinate.
    , y :: !P.Int
      -- | False to skip to the nearest non-UA shadow root ancestor (default: false).
    , includeUserAgentShadowDOM :: !(P.Maybe P.Bool)
      -- | Whether to ignore pointer-events: none on elements and hit test them.
    , ignorePointerEventsNone :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetNodeForLocation where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getNodeForLocation" $ \_o -> GetNodeForLocation
            <$> _o .: "x"
            <*> _o .: "y"
            <*> _o .:? "includeUserAgentShadowDOM"
            <*> _o .:? "ignorePointerEventsNone"
        ago = A.withArray "getNodeForLocation" $ \_a -> GetNodeForLocation
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON GetNodeForLocation where
    toEncoding (GetNodeForLocation _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "x" .= _0
        , P.pure $ "y" .= _1
        , ("includeUserAgentShadowDOM" .=) <$> _2
        , ("ignorePointerEventsNone" .=) <$> _3
        ]
    toJSON (GetNodeForLocation _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "x" .= _0
        , P.pure $ "y" .= _1
        , ("includeUserAgentShadowDOM" .=) <$> _2
        , ("ignorePointerEventsNone" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetNodeForLocation where
    GetNodeForLocation _0 _1 _2 _3 <> GetNodeForLocation _ _ __2 __3 = GetNodeForLocation _0 _1 (_2 <|> __2) (_3 <|> __3)


------------------------------------------------------------------------------
-- | Returns node id at given location. Depending on whether DOM domain is enabled, nodeId is
-- either returned or not.
data GetNodeForLocationResult = GetNodeForLocationResult
    { -- | Resulting node.
      backendNodeId :: !BackendNodeId
      -- | Frame this node belongs to.
    , frameId :: !Page.FrameId
      -- | Id of the node at given coordinates, only when enabled and requested document.
    , nodeId :: !(P.Maybe NodeId)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetNodeForLocationResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getNodeForLocationResult" $ \_o -> GetNodeForLocationResult
            <$> _o .: "backendNodeId"
            <*> _o .: "frameId"
            <*> _o .:? "nodeId"
        ago = A.withArray "getNodeForLocationResult" $ \_a -> GetNodeForLocationResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON GetNodeForLocationResult where
    toEncoding (GetNodeForLocationResult _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "backendNodeId" .= _0
        , P.pure $ "frameId" .= _1
        , ("nodeId" .=) <$> _2
        ]
    toJSON (GetNodeForLocationResult _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "backendNodeId" .= _0
        , P.pure $ "frameId" .= _1
        , ("nodeId" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetNodeForLocationResult where
    GetNodeForLocationResult _0 _1 _2 <> GetNodeForLocationResult _ _ __2 = GetNodeForLocationResult _0 _1 (_2 <|> __2)


------------------------------------------------------------------------------
instance M.Method GetNodeForLocation where
    type Result GetNodeForLocation = GetNodeForLocationResult
    name _ = "DOM.getNodeForLocation"


------------------------------------------------------------------------------
-- | Returns node id at given location. Depending on whether DOM domain is enabled, nodeId is
-- either returned or not.
getNodeForLocation
    :: P.Int
    -- ^ X coordinate.

    -> P.Int
    -- ^ Y coordinate.

    -> GetNodeForLocation
getNodeForLocation _0 _1 = GetNodeForLocation _0 _1 P.empty P.empty


------------------------------------------------------------------------------
-- | Returns node's HTML markup.
data GetOuterHTML = GetOuterHTML
    { -- | Identifier of the node.
      nodeId :: !(P.Maybe NodeId)
      -- | Identifier of the backend node.
    , backendNodeId :: !(P.Maybe BackendNodeId)
      -- | JavaScript object id of the node wrapper.
    , objectId :: !(P.Maybe Runtime.RemoteObjectId)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetOuterHTML where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getOuterHTML" $ \_o -> GetOuterHTML
            <$> _o .:? "nodeId"
            <*> _o .:? "backendNodeId"
            <*> _o .:? "objectId"
        ago = A.withArray "getOuterHTML" $ \_a -> GetOuterHTML
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON GetOuterHTML where
    toEncoding (GetOuterHTML _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ ("nodeId" .=) <$> _0
        , ("backendNodeId" .=) <$> _1
        , ("objectId" .=) <$> _2
        ]
    toJSON (GetOuterHTML _0 _1 _2) = A.object $ P.catMaybes
        [ ("nodeId" .=) <$> _0
        , ("backendNodeId" .=) <$> _1
        , ("objectId" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetOuterHTML where
    GetOuterHTML _0 _1 _2 <> GetOuterHTML __0 __1 __2 = GetOuterHTML (_0 <|> __0) (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
instance P.Monoid GetOuterHTML where
    mempty = GetOuterHTML P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Returns node's HTML markup.
data GetOuterHTMLResult = GetOuterHTMLResult
    { -- | Outer HTML markup.
      outerHTML :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetOuterHTMLResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getOuterHTMLResult" $ \_o -> GetOuterHTMLResult
            <$> _o .: "outerHTML"
        ago = A.withArray "getOuterHTMLResult" $ \_a -> GetOuterHTMLResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetOuterHTMLResult where
    toEncoding (GetOuterHTMLResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "outerHTML" .= _0
        ]
    toJSON (GetOuterHTMLResult _0) = A.object $ P.catMaybes
        [ P.pure $ "outerHTML" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetOuterHTMLResult where
    GetOuterHTMLResult _0 <> GetOuterHTMLResult _ = GetOuterHTMLResult _0


------------------------------------------------------------------------------
instance M.Method GetOuterHTML where
    type Result GetOuterHTML = GetOuterHTMLResult
    name _ = "DOM.getOuterHTML"


------------------------------------------------------------------------------
-- | Returns node's HTML markup.
getOuterHTML
    :: GetOuterHTML
getOuterHTML = GetOuterHTML P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Returns the id of the nearest ancestor that is a relayout boundary.
{-# WARNING GetRelayoutBoundary "This feature is marked as EXPERIMENTAL." #-}
data GetRelayoutBoundary = GetRelayoutBoundary
    { -- | Id of the node.
      nodeId :: !NodeId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetRelayoutBoundary where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getRelayoutBoundary" $ \_o -> GetRelayoutBoundary
            <$> _o .: "nodeId"
        ago = A.withArray "getRelayoutBoundary" $ \_a -> GetRelayoutBoundary
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetRelayoutBoundary where
    toEncoding (GetRelayoutBoundary _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]
    toJSON (GetRelayoutBoundary _0) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetRelayoutBoundary where
    GetRelayoutBoundary _0 <> GetRelayoutBoundary _ = GetRelayoutBoundary _0


------------------------------------------------------------------------------
-- | Returns the id of the nearest ancestor that is a relayout boundary.
{-# WARNING GetRelayoutBoundaryResult "This feature is marked as EXPERIMENTAL." #-}
data GetRelayoutBoundaryResult = GetRelayoutBoundaryResult
    { -- | Relayout boundary node id for the given node.
      nodeId :: !NodeId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetRelayoutBoundaryResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getRelayoutBoundaryResult" $ \_o -> GetRelayoutBoundaryResult
            <$> _o .: "nodeId"
        ago = A.withArray "getRelayoutBoundaryResult" $ \_a -> GetRelayoutBoundaryResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetRelayoutBoundaryResult where
    toEncoding (GetRelayoutBoundaryResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]
    toJSON (GetRelayoutBoundaryResult _0) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetRelayoutBoundaryResult where
    GetRelayoutBoundaryResult _0 <> GetRelayoutBoundaryResult _ = GetRelayoutBoundaryResult _0


------------------------------------------------------------------------------
instance M.Method GetRelayoutBoundary where
    type Result GetRelayoutBoundary = GetRelayoutBoundaryResult
    name _ = "DOM.getRelayoutBoundary"


------------------------------------------------------------------------------
-- | Returns the id of the nearest ancestor that is a relayout boundary.
{-# WARNING getRelayoutBoundary "This feature is marked as EXPERIMENTAL." #-}
getRelayoutBoundary
    :: NodeId
    -- ^ Id of the node.

    -> GetRelayoutBoundary
getRelayoutBoundary _0 = GetRelayoutBoundary _0


------------------------------------------------------------------------------
-- | Returns search results from given @fromIndex@ to given @toIndex@ from the search with the given
-- identifier.
{-# WARNING GetSearchResults "This feature is marked as EXPERIMENTAL." #-}
data GetSearchResults = GetSearchResults
    { -- | Unique search session identifier.
      searchId :: !T.Text
      -- | Start index of the search result to be returned.
    , fromIndex :: !P.Int
      -- | End index of the search result to be returned.
    , toIndex :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetSearchResults where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getSearchResults" $ \_o -> GetSearchResults
            <$> _o .: "searchId"
            <*> _o .: "fromIndex"
            <*> _o .: "toIndex"
        ago = A.withArray "getSearchResults" $ \_a -> GetSearchResults
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON GetSearchResults where
    toEncoding (GetSearchResults _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "searchId" .= _0
        , P.pure $ "fromIndex" .= _1
        , P.pure $ "toIndex" .= _2
        ]
    toJSON (GetSearchResults _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "searchId" .= _0
        , P.pure $ "fromIndex" .= _1
        , P.pure $ "toIndex" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetSearchResults where
    GetSearchResults _0 _1 _2 <> GetSearchResults _ _ _ = GetSearchResults _0 _1 _2


------------------------------------------------------------------------------
-- | Returns search results from given @fromIndex@ to given @toIndex@ from the search with the given
-- identifier.
{-# WARNING GetSearchResultsResult "This feature is marked as EXPERIMENTAL." #-}
data GetSearchResultsResult = GetSearchResultsResult
    { -- | Ids of the search result nodes.
      nodeIds :: ![NodeId]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetSearchResultsResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getSearchResultsResult" $ \_o -> GetSearchResultsResult
            <$> _o .: "nodeIds"
        ago = A.withArray "getSearchResultsResult" $ \_a -> GetSearchResultsResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetSearchResultsResult where
    toEncoding (GetSearchResultsResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeIds" .= _0
        ]
    toJSON (GetSearchResultsResult _0) = A.object $ P.catMaybes
        [ P.pure $ "nodeIds" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetSearchResultsResult where
    GetSearchResultsResult _0 <> GetSearchResultsResult _ = GetSearchResultsResult _0


------------------------------------------------------------------------------
instance M.Method GetSearchResults where
    type Result GetSearchResults = GetSearchResultsResult
    name _ = "DOM.getSearchResults"


------------------------------------------------------------------------------
-- | Returns search results from given @fromIndex@ to given @toIndex@ from the search with the given
-- identifier.
{-# WARNING getSearchResults "This feature is marked as EXPERIMENTAL." #-}
getSearchResults
    :: T.Text
    -- ^ Unique search session identifier.

    -> P.Int
    -- ^ Start index of the search result to be returned.

    -> P.Int
    -- ^ End index of the search result to be returned.

    -> GetSearchResults
getSearchResults _0 _1 _2 = GetSearchResults _0 _1 _2


------------------------------------------------------------------------------
-- | Hides any highlight.
data HideHighlight = HideHighlight
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON HideHighlight where
    parseJSON A.Null = P.pure HideHighlight
    parseJSON v = A.withArray "hideHighlight" go v
        <|> A.withObject "hideHighlight" go v
      where
        go _ = P.pure HideHighlight


------------------------------------------------------------------------------
instance A.ToJSON HideHighlight where
    toEncoding HideHighlight = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON HideHighlight = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup HideHighlight where
    HideHighlight <> HideHighlight = HideHighlight


------------------------------------------------------------------------------
instance P.Monoid HideHighlight where
    mempty = HideHighlight


------------------------------------------------------------------------------
instance M.Method HideHighlight where
    type Result HideHighlight = ()
    name _ = "DOM.hideHighlight"


------------------------------------------------------------------------------
-- | Hides any highlight.
hideHighlight
    :: HideHighlight
hideHighlight = HideHighlight


------------------------------------------------------------------------------
-- | Highlights DOM node.
data HighlightNode = HighlightNode
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON HighlightNode where
    parseJSON A.Null = P.pure HighlightNode
    parseJSON v = A.withArray "highlightNode" go v
        <|> A.withObject "highlightNode" go v
      where
        go _ = P.pure HighlightNode


------------------------------------------------------------------------------
instance A.ToJSON HighlightNode where
    toEncoding HighlightNode = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON HighlightNode = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup HighlightNode where
    HighlightNode <> HighlightNode = HighlightNode


------------------------------------------------------------------------------
instance P.Monoid HighlightNode where
    mempty = HighlightNode


------------------------------------------------------------------------------
instance M.Method HighlightNode where
    type Result HighlightNode = ()
    name _ = "DOM.highlightNode"


------------------------------------------------------------------------------
-- | Highlights DOM node.
highlightNode
    :: HighlightNode
highlightNode = HighlightNode


------------------------------------------------------------------------------
-- | Highlights given rectangle.
data HighlightRect = HighlightRect
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON HighlightRect where
    parseJSON A.Null = P.pure HighlightRect
    parseJSON v = A.withArray "highlightRect" go v
        <|> A.withObject "highlightRect" go v
      where
        go _ = P.pure HighlightRect


------------------------------------------------------------------------------
instance A.ToJSON HighlightRect where
    toEncoding HighlightRect = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON HighlightRect = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup HighlightRect where
    HighlightRect <> HighlightRect = HighlightRect


------------------------------------------------------------------------------
instance P.Monoid HighlightRect where
    mempty = HighlightRect


------------------------------------------------------------------------------
instance M.Method HighlightRect where
    type Result HighlightRect = ()
    name _ = "DOM.highlightRect"


------------------------------------------------------------------------------
-- | Highlights given rectangle.
highlightRect
    :: HighlightRect
highlightRect = HighlightRect


------------------------------------------------------------------------------
-- | Marks last undoable state.
{-# WARNING MarkUndoableState "This feature is marked as EXPERIMENTAL." #-}
data MarkUndoableState = MarkUndoableState
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON MarkUndoableState where
    parseJSON A.Null = P.pure MarkUndoableState
    parseJSON v = A.withArray "markUndoableState" go v
        <|> A.withObject "markUndoableState" go v
      where
        go _ = P.pure MarkUndoableState


------------------------------------------------------------------------------
instance A.ToJSON MarkUndoableState where
    toEncoding MarkUndoableState = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON MarkUndoableState = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup MarkUndoableState where
    MarkUndoableState <> MarkUndoableState = MarkUndoableState


------------------------------------------------------------------------------
instance P.Monoid MarkUndoableState where
    mempty = MarkUndoableState


------------------------------------------------------------------------------
instance M.Method MarkUndoableState where
    type Result MarkUndoableState = ()
    name _ = "DOM.markUndoableState"


------------------------------------------------------------------------------
-- | Marks last undoable state.
{-# WARNING markUndoableState "This feature is marked as EXPERIMENTAL." #-}
markUndoableState
    :: MarkUndoableState
markUndoableState = MarkUndoableState


------------------------------------------------------------------------------
-- | Moves node into the new container, places it before the given anchor.
data MoveTo = MoveTo
    { -- | Id of the node to move.
      nodeId :: !NodeId
      -- | Id of the element to drop the moved node into.
    , targetNodeId :: !NodeId
      -- | Drop node before this one (if absent, the moved node becomes the last child of
      -- @targetNodeId@).
    , insertBeforeNodeId :: !(P.Maybe NodeId)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON MoveTo where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "moveTo" $ \_o -> MoveTo
            <$> _o .: "nodeId"
            <*> _o .: "targetNodeId"
            <*> _o .:? "insertBeforeNodeId"
        ago = A.withArray "moveTo" $ \_a -> MoveTo
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON MoveTo where
    toEncoding (MoveTo _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "targetNodeId" .= _1
        , ("insertBeforeNodeId" .=) <$> _2
        ]
    toJSON (MoveTo _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "targetNodeId" .= _1
        , ("insertBeforeNodeId" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup MoveTo where
    MoveTo _0 _1 _2 <> MoveTo _ _ __2 = MoveTo _0 _1 (_2 <|> __2)


------------------------------------------------------------------------------
-- | Moves node into the new container, places it before the given anchor.
data MoveToResult = MoveToResult
    { -- | New id of the moved node.
      nodeId :: !NodeId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON MoveToResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "moveToResult" $ \_o -> MoveToResult
            <$> _o .: "nodeId"
        ago = A.withArray "moveToResult" $ \_a -> MoveToResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON MoveToResult where
    toEncoding (MoveToResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]
    toJSON (MoveToResult _0) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup MoveToResult where
    MoveToResult _0 <> MoveToResult _ = MoveToResult _0


------------------------------------------------------------------------------
instance M.Method MoveTo where
    type Result MoveTo = MoveToResult
    name _ = "DOM.moveTo"


------------------------------------------------------------------------------
-- | Moves node into the new container, places it before the given anchor.
moveTo
    :: NodeId
    -- ^ Id of the node to move.

    -> NodeId
    -- ^ Id of the element to drop the moved node into.

    -> MoveTo
moveTo _0 _1 = MoveTo _0 _1 P.empty


------------------------------------------------------------------------------
-- | Searches for a given string in the DOM tree. Use @getSearchResults@ to access search results or
-- @cancelSearch@ to end this search session.
{-# WARNING PerformSearch "This feature is marked as EXPERIMENTAL." #-}
data PerformSearch = PerformSearch
    { -- | Plain text or query selector or XPath search query.
      query :: !T.Text
      -- | True to search in user agent shadow DOM.
    , includeUserAgentShadowDOM :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PerformSearch where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "performSearch" $ \_o -> PerformSearch
            <$> _o .: "query"
            <*> _o .:? "includeUserAgentShadowDOM"
        ago = A.withArray "performSearch" $ \_a -> PerformSearch
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON PerformSearch where
    toEncoding (PerformSearch _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "query" .= _0
        , ("includeUserAgentShadowDOM" .=) <$> _1
        ]
    toJSON (PerformSearch _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "query" .= _0
        , ("includeUserAgentShadowDOM" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup PerformSearch where
    PerformSearch _0 _1 <> PerformSearch _ __1 = PerformSearch _0 (_1 <|> __1)


------------------------------------------------------------------------------
-- | Searches for a given string in the DOM tree. Use @getSearchResults@ to access search results or
-- @cancelSearch@ to end this search session.
{-# WARNING PerformSearchResult "This feature is marked as EXPERIMENTAL." #-}
data PerformSearchResult = PerformSearchResult
    { -- | Unique search session identifier.
      searchId :: !T.Text
      -- | Number of search results.
    , resultCount :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PerformSearchResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "performSearchResult" $ \_o -> PerformSearchResult
            <$> _o .: "searchId"
            <*> _o .: "resultCount"
        ago = A.withArray "performSearchResult" $ \_a -> PerformSearchResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON PerformSearchResult where
    toEncoding (PerformSearchResult _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "searchId" .= _0
        , P.pure $ "resultCount" .= _1
        ]
    toJSON (PerformSearchResult _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "searchId" .= _0
        , P.pure $ "resultCount" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup PerformSearchResult where
    PerformSearchResult _0 _1 <> PerformSearchResult _ _ = PerformSearchResult _0 _1


------------------------------------------------------------------------------
instance M.Method PerformSearch where
    type Result PerformSearch = PerformSearchResult
    name _ = "DOM.performSearch"


------------------------------------------------------------------------------
-- | Searches for a given string in the DOM tree. Use @getSearchResults@ to access search results or
-- @cancelSearch@ to end this search session.
{-# WARNING performSearch "This feature is marked as EXPERIMENTAL." #-}
performSearch
    :: T.Text
    -- ^ Plain text or query selector or XPath search query.

    -> PerformSearch
performSearch _0 = PerformSearch _0 P.empty


------------------------------------------------------------------------------
-- | Requests that the node is sent to the caller given its path. \/\/ FIXME, use XPath
{-# WARNING PushNodeByPathToFrontend "This feature is marked as EXPERIMENTAL." #-}
data PushNodeByPathToFrontend = PushNodeByPathToFrontend
    { -- | Path to node in the proprietary format.
      path :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PushNodeByPathToFrontend where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "pushNodeByPathToFrontend" $ \_o -> PushNodeByPathToFrontend
            <$> _o .: "path"
        ago = A.withArray "pushNodeByPathToFrontend" $ \_a -> PushNodeByPathToFrontend
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON PushNodeByPathToFrontend where
    toEncoding (PushNodeByPathToFrontend _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "path" .= _0
        ]
    toJSON (PushNodeByPathToFrontend _0) = A.object $ P.catMaybes
        [ P.pure $ "path" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup PushNodeByPathToFrontend where
    PushNodeByPathToFrontend _0 <> PushNodeByPathToFrontend _ = PushNodeByPathToFrontend _0


------------------------------------------------------------------------------
-- | Requests that the node is sent to the caller given its path. \/\/ FIXME, use XPath
{-# WARNING PushNodeByPathToFrontendResult "This feature is marked as EXPERIMENTAL." #-}
data PushNodeByPathToFrontendResult = PushNodeByPathToFrontendResult
    { -- | Id of the node for given path.
      nodeId :: !NodeId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PushNodeByPathToFrontendResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "pushNodeByPathToFrontendResult" $ \_o -> PushNodeByPathToFrontendResult
            <$> _o .: "nodeId"
        ago = A.withArray "pushNodeByPathToFrontendResult" $ \_a -> PushNodeByPathToFrontendResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON PushNodeByPathToFrontendResult where
    toEncoding (PushNodeByPathToFrontendResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]
    toJSON (PushNodeByPathToFrontendResult _0) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup PushNodeByPathToFrontendResult where
    PushNodeByPathToFrontendResult _0 <> PushNodeByPathToFrontendResult _ = PushNodeByPathToFrontendResult _0


------------------------------------------------------------------------------
instance M.Method PushNodeByPathToFrontend where
    type Result PushNodeByPathToFrontend = PushNodeByPathToFrontendResult
    name _ = "DOM.pushNodeByPathToFrontend"


------------------------------------------------------------------------------
-- | Requests that the node is sent to the caller given its path. \/\/ FIXME, use XPath
{-# WARNING pushNodeByPathToFrontend "This feature is marked as EXPERIMENTAL." #-}
pushNodeByPathToFrontend
    :: T.Text
    -- ^ Path to node in the proprietary format.

    -> PushNodeByPathToFrontend
pushNodeByPathToFrontend _0 = PushNodeByPathToFrontend _0


------------------------------------------------------------------------------
-- | Requests that a batch of nodes is sent to the caller given their backend node ids.
{-# WARNING PushNodesByBackendIdsToFrontend "This feature is marked as EXPERIMENTAL." #-}
data PushNodesByBackendIdsToFrontend = PushNodesByBackendIdsToFrontend
    { -- | The array of backend node ids.
      backendNodeIds :: ![BackendNodeId]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PushNodesByBackendIdsToFrontend where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "pushNodesByBackendIdsToFrontend" $ \_o -> PushNodesByBackendIdsToFrontend
            <$> _o .: "backendNodeIds"
        ago = A.withArray "pushNodesByBackendIdsToFrontend" $ \_a -> PushNodesByBackendIdsToFrontend
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON PushNodesByBackendIdsToFrontend where
    toEncoding (PushNodesByBackendIdsToFrontend _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "backendNodeIds" .= _0
        ]
    toJSON (PushNodesByBackendIdsToFrontend _0) = A.object $ P.catMaybes
        [ P.pure $ "backendNodeIds" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup PushNodesByBackendIdsToFrontend where
    PushNodesByBackendIdsToFrontend _0 <> PushNodesByBackendIdsToFrontend _ = PushNodesByBackendIdsToFrontend _0


------------------------------------------------------------------------------
-- | Requests that a batch of nodes is sent to the caller given their backend node ids.
{-# WARNING PushNodesByBackendIdsToFrontendResult "This feature is marked as EXPERIMENTAL." #-}
data PushNodesByBackendIdsToFrontendResult = PushNodesByBackendIdsToFrontendResult
    { -- | The array of ids of pushed nodes that correspond to the backend ids specified in
      -- backendNodeIds.
      nodeIds :: ![NodeId]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PushNodesByBackendIdsToFrontendResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "pushNodesByBackendIdsToFrontendResult" $ \_o -> PushNodesByBackendIdsToFrontendResult
            <$> _o .: "nodeIds"
        ago = A.withArray "pushNodesByBackendIdsToFrontendResult" $ \_a -> PushNodesByBackendIdsToFrontendResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON PushNodesByBackendIdsToFrontendResult where
    toEncoding (PushNodesByBackendIdsToFrontendResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeIds" .= _0
        ]
    toJSON (PushNodesByBackendIdsToFrontendResult _0) = A.object $ P.catMaybes
        [ P.pure $ "nodeIds" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup PushNodesByBackendIdsToFrontendResult where
    PushNodesByBackendIdsToFrontendResult _0 <> PushNodesByBackendIdsToFrontendResult _ = PushNodesByBackendIdsToFrontendResult _0


------------------------------------------------------------------------------
instance M.Method PushNodesByBackendIdsToFrontend where
    type Result PushNodesByBackendIdsToFrontend = PushNodesByBackendIdsToFrontendResult
    name _ = "DOM.pushNodesByBackendIdsToFrontend"


------------------------------------------------------------------------------
-- | Requests that a batch of nodes is sent to the caller given their backend node ids.
{-# WARNING pushNodesByBackendIdsToFrontend "This feature is marked as EXPERIMENTAL." #-}
pushNodesByBackendIdsToFrontend
    :: [BackendNodeId]
    -- ^ The array of backend node ids.

    -> PushNodesByBackendIdsToFrontend
pushNodesByBackendIdsToFrontend _0 = PushNodesByBackendIdsToFrontend _0


------------------------------------------------------------------------------
-- | Executes @querySelector@ on a given node.
data QuerySelector = QuerySelector
    { -- | Id of the node to query upon.
      nodeId :: !NodeId
      -- | Selector string.
    , selector :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON QuerySelector where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "querySelector" $ \_o -> QuerySelector
            <$> _o .: "nodeId"
            <*> _o .: "selector"
        ago = A.withArray "querySelector" $ \_a -> QuerySelector
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON QuerySelector where
    toEncoding (QuerySelector _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "selector" .= _1
        ]
    toJSON (QuerySelector _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "selector" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup QuerySelector where
    QuerySelector _0 _1 <> QuerySelector _ _ = QuerySelector _0 _1


------------------------------------------------------------------------------
-- | Executes @querySelector@ on a given node.
data QuerySelectorResult = QuerySelectorResult
    { -- | Query selector result.
      nodeId :: !NodeId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON QuerySelectorResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "querySelectorResult" $ \_o -> QuerySelectorResult
            <$> _o .: "nodeId"
        ago = A.withArray "querySelectorResult" $ \_a -> QuerySelectorResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON QuerySelectorResult where
    toEncoding (QuerySelectorResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]
    toJSON (QuerySelectorResult _0) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup QuerySelectorResult where
    QuerySelectorResult _0 <> QuerySelectorResult _ = QuerySelectorResult _0


------------------------------------------------------------------------------
instance M.Method QuerySelector where
    type Result QuerySelector = QuerySelectorResult
    name _ = "DOM.querySelector"


------------------------------------------------------------------------------
-- | Executes @querySelector@ on a given node.
querySelector
    :: NodeId
    -- ^ Id of the node to query upon.

    -> T.Text
    -- ^ Selector string.

    -> QuerySelector
querySelector _0 _1 = QuerySelector _0 _1


------------------------------------------------------------------------------
-- | Executes @querySelectorAll@ on a given node.
data QuerySelectorAll = QuerySelectorAll
    { -- | Id of the node to query upon.
      nodeId :: !NodeId
      -- | Selector string.
    , selector :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON QuerySelectorAll where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "querySelectorAll" $ \_o -> QuerySelectorAll
            <$> _o .: "nodeId"
            <*> _o .: "selector"
        ago = A.withArray "querySelectorAll" $ \_a -> QuerySelectorAll
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON QuerySelectorAll where
    toEncoding (QuerySelectorAll _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "selector" .= _1
        ]
    toJSON (QuerySelectorAll _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "selector" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup QuerySelectorAll where
    QuerySelectorAll _0 _1 <> QuerySelectorAll _ _ = QuerySelectorAll _0 _1


------------------------------------------------------------------------------
-- | Executes @querySelectorAll@ on a given node.
data QuerySelectorAllResult = QuerySelectorAllResult
    { -- | Query selector result.
      nodeIds :: ![NodeId]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON QuerySelectorAllResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "querySelectorAllResult" $ \_o -> QuerySelectorAllResult
            <$> _o .: "nodeIds"
        ago = A.withArray "querySelectorAllResult" $ \_a -> QuerySelectorAllResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON QuerySelectorAllResult where
    toEncoding (QuerySelectorAllResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeIds" .= _0
        ]
    toJSON (QuerySelectorAllResult _0) = A.object $ P.catMaybes
        [ P.pure $ "nodeIds" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup QuerySelectorAllResult where
    QuerySelectorAllResult _0 <> QuerySelectorAllResult _ = QuerySelectorAllResult _0


------------------------------------------------------------------------------
instance M.Method QuerySelectorAll where
    type Result QuerySelectorAll = QuerySelectorAllResult
    name _ = "DOM.querySelectorAll"


------------------------------------------------------------------------------
-- | Executes @querySelectorAll@ on a given node.
querySelectorAll
    :: NodeId
    -- ^ Id of the node to query upon.

    -> T.Text
    -- ^ Selector string.

    -> QuerySelectorAll
querySelectorAll _0 _1 = QuerySelectorAll _0 _1


------------------------------------------------------------------------------
-- | Re-does the last undone action.
{-# WARNING Redo "This feature is marked as EXPERIMENTAL." #-}
data Redo = Redo
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Redo where
    parseJSON A.Null = P.pure Redo
    parseJSON v = A.withArray "redo" go v
        <|> A.withObject "redo" go v
      where
        go _ = P.pure Redo


------------------------------------------------------------------------------
instance A.ToJSON Redo where
    toEncoding Redo = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON Redo = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup Redo where
    Redo <> Redo = Redo


------------------------------------------------------------------------------
instance P.Monoid Redo where
    mempty = Redo


------------------------------------------------------------------------------
instance M.Method Redo where
    type Result Redo = ()
    name _ = "DOM.redo"


------------------------------------------------------------------------------
-- | Re-does the last undone action.
{-# WARNING redo "This feature is marked as EXPERIMENTAL." #-}
redo
    :: Redo
redo = Redo


------------------------------------------------------------------------------
-- | Removes attribute with given name from an element with given id.
data RemoveAttribute = RemoveAttribute
    { -- | Id of the element to remove attribute from.
      nodeId :: !NodeId
      -- | Name of the attribute to remove.
    , name :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RemoveAttribute where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "removeAttribute" $ \_o -> RemoveAttribute
            <$> _o .: "nodeId"
            <*> _o .: "name"
        ago = A.withArray "removeAttribute" $ \_a -> RemoveAttribute
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON RemoveAttribute where
    toEncoding (RemoveAttribute _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "name" .= _1
        ]
    toJSON (RemoveAttribute _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "name" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup RemoveAttribute where
    RemoveAttribute _0 _1 <> RemoveAttribute _ _ = RemoveAttribute _0 _1


------------------------------------------------------------------------------
instance M.Method RemoveAttribute where
    type Result RemoveAttribute = ()
    name _ = "DOM.removeAttribute"


------------------------------------------------------------------------------
-- | Removes attribute with given name from an element with given id.
removeAttribute
    :: NodeId
    -- ^ Id of the element to remove attribute from.

    -> T.Text
    -- ^ Name of the attribute to remove.

    -> RemoveAttribute
removeAttribute _0 _1 = RemoveAttribute _0 _1


------------------------------------------------------------------------------
-- | Removes node with given id.
data RemoveNode = RemoveNode
    { -- | Id of the node to remove.
      nodeId :: !NodeId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RemoveNode where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "removeNode" $ \_o -> RemoveNode
            <$> _o .: "nodeId"
        ago = A.withArray "removeNode" $ \_a -> RemoveNode
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON RemoveNode where
    toEncoding (RemoveNode _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]
    toJSON (RemoveNode _0) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup RemoveNode where
    RemoveNode _0 <> RemoveNode _ = RemoveNode _0


------------------------------------------------------------------------------
instance M.Method RemoveNode where
    type Result RemoveNode = ()
    name _ = "DOM.removeNode"


------------------------------------------------------------------------------
-- | Removes node with given id.
removeNode
    :: NodeId
    -- ^ Id of the node to remove.

    -> RemoveNode
removeNode _0 = RemoveNode _0


------------------------------------------------------------------------------
-- | Requests that children of the node with given id are returned to the caller in form of
-- @setChildNodes@ events where not only immediate children are retrieved, but all children down to
-- the specified depth.
data RequestChildNodes = RequestChildNodes
    { -- | Id of the node to get children for.
      nodeId :: !NodeId
      -- | The maximum depth at which children should be retrieved, defaults to 1. Use -1 for the
      -- entire subtree or provide an integer larger than 0.
    , depth :: !(P.Maybe P.Int)
      -- | Whether or not iframes and shadow roots should be traversed when returning the sub-tree
      -- (default is false).
    , pierce :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RequestChildNodes where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "requestChildNodes" $ \_o -> RequestChildNodes
            <$> _o .: "nodeId"
            <*> _o .:? "depth"
            <*> _o .:? "pierce"
        ago = A.withArray "requestChildNodes" $ \_a -> RequestChildNodes
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON RequestChildNodes where
    toEncoding (RequestChildNodes _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , ("depth" .=) <$> _1
        , ("pierce" .=) <$> _2
        ]
    toJSON (RequestChildNodes _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , ("depth" .=) <$> _1
        , ("pierce" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup RequestChildNodes where
    RequestChildNodes _0 _1 _2 <> RequestChildNodes _ __1 __2 = RequestChildNodes _0 (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
instance M.Method RequestChildNodes where
    type Result RequestChildNodes = ()
    name _ = "DOM.requestChildNodes"


------------------------------------------------------------------------------
-- | Requests that children of the node with given id are returned to the caller in form of
-- @setChildNodes@ events where not only immediate children are retrieved, but all children down to
-- the specified depth.
requestChildNodes
    :: NodeId
    -- ^ Id of the node to get children for.

    -> RequestChildNodes
requestChildNodes _0 = RequestChildNodes _0 P.empty P.empty


------------------------------------------------------------------------------
-- | Requests that the node is sent to the caller given the JavaScript node object reference. All
-- nodes that form the path from the node to the root are also sent to the client as a series of
-- @setChildNodes@ notifications.
data RequestNode = RequestNode
    { -- | JavaScript object id to convert into node.
      objectId :: !Runtime.RemoteObjectId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RequestNode where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "requestNode" $ \_o -> RequestNode
            <$> _o .: "objectId"
        ago = A.withArray "requestNode" $ \_a -> RequestNode
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON RequestNode where
    toEncoding (RequestNode _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "objectId" .= _0
        ]
    toJSON (RequestNode _0) = A.object $ P.catMaybes
        [ P.pure $ "objectId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup RequestNode where
    RequestNode _0 <> RequestNode _ = RequestNode _0


------------------------------------------------------------------------------
-- | Requests that the node is sent to the caller given the JavaScript node object reference. All
-- nodes that form the path from the node to the root are also sent to the client as a series of
-- @setChildNodes@ notifications.
data RequestNodeResult = RequestNodeResult
    { -- | Node id for given object.
      nodeId :: !NodeId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RequestNodeResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "requestNodeResult" $ \_o -> RequestNodeResult
            <$> _o .: "nodeId"
        ago = A.withArray "requestNodeResult" $ \_a -> RequestNodeResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON RequestNodeResult where
    toEncoding (RequestNodeResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]
    toJSON (RequestNodeResult _0) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup RequestNodeResult where
    RequestNodeResult _0 <> RequestNodeResult _ = RequestNodeResult _0


------------------------------------------------------------------------------
instance M.Method RequestNode where
    type Result RequestNode = RequestNodeResult
    name _ = "DOM.requestNode"


------------------------------------------------------------------------------
-- | Requests that the node is sent to the caller given the JavaScript node object reference. All
-- nodes that form the path from the node to the root are also sent to the client as a series of
-- @setChildNodes@ notifications.
requestNode
    :: Runtime.RemoteObjectId
    -- ^ JavaScript object id to convert into node.

    -> RequestNode
requestNode _0 = RequestNode _0


------------------------------------------------------------------------------
-- | Resolves the JavaScript node object for a given NodeId or BackendNodeId.
data ResolveNode = ResolveNode
    { -- | Id of the node to resolve.
      nodeId :: !(P.Maybe NodeId)
      -- | Backend identifier of the node to resolve.
    , backendNodeId :: !(P.Maybe DOM.BackendNodeId)
      -- | Symbolic group name that can be used to release multiple objects.
    , objectGroup :: !(P.Maybe T.Text)
      -- | Execution context in which to resolve the node.
    , executionContextId :: !(P.Maybe Runtime.ExecutionContextId)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ResolveNode where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "resolveNode" $ \_o -> ResolveNode
            <$> _o .:? "nodeId"
            <*> _o .:? "backendNodeId"
            <*> _o .:? "objectGroup"
            <*> _o .:? "executionContextId"
        ago = A.withArray "resolveNode" $ \_a -> ResolveNode
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON ResolveNode where
    toEncoding (ResolveNode _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ ("nodeId" .=) <$> _0
        , ("backendNodeId" .=) <$> _1
        , ("objectGroup" .=) <$> _2
        , ("executionContextId" .=) <$> _3
        ]
    toJSON (ResolveNode _0 _1 _2 _3) = A.object $ P.catMaybes
        [ ("nodeId" .=) <$> _0
        , ("backendNodeId" .=) <$> _1
        , ("objectGroup" .=) <$> _2
        , ("executionContextId" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup ResolveNode where
    ResolveNode _0 _1 _2 _3 <> ResolveNode __0 __1 __2 __3 = ResolveNode (_0 <|> __0) (_1 <|> __1) (_2 <|> __2) (_3 <|> __3)


------------------------------------------------------------------------------
instance P.Monoid ResolveNode where
    mempty = ResolveNode P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Resolves the JavaScript node object for a given NodeId or BackendNodeId.
data ResolveNodeResult = ResolveNodeResult
    { -- | JavaScript object wrapper for given node.
      object :: !Runtime.RemoteObject
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ResolveNodeResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "resolveNodeResult" $ \_o -> ResolveNodeResult
            <$> _o .: "object"
        ago = A.withArray "resolveNodeResult" $ \_a -> ResolveNodeResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON ResolveNodeResult where
    toEncoding (ResolveNodeResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "object" .= _0
        ]
    toJSON (ResolveNodeResult _0) = A.object $ P.catMaybes
        [ P.pure $ "object" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup ResolveNodeResult where
    ResolveNodeResult _0 <> ResolveNodeResult _ = ResolveNodeResult _0


------------------------------------------------------------------------------
instance M.Method ResolveNode where
    type Result ResolveNode = ResolveNodeResult
    name _ = "DOM.resolveNode"


------------------------------------------------------------------------------
-- | Resolves the JavaScript node object for a given NodeId or BackendNodeId.
resolveNode
    :: ResolveNode
resolveNode = ResolveNode P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Sets attribute for an element with given id.
data SetAttributeValue = SetAttributeValue
    { -- | Id of the element to set attribute for.
      nodeId :: !NodeId
      -- | Attribute name.
    , name :: !T.Text
      -- | Attribute value.
    , value :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetAttributeValue where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setAttributeValue" $ \_o -> SetAttributeValue
            <$> _o .: "nodeId"
            <*> _o .: "name"
            <*> _o .: "value"
        ago = A.withArray "setAttributeValue" $ \_a -> SetAttributeValue
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON SetAttributeValue where
    toEncoding (SetAttributeValue _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "name" .= _1
        , P.pure $ "value" .= _2
        ]
    toJSON (SetAttributeValue _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "name" .= _1
        , P.pure $ "value" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetAttributeValue where
    SetAttributeValue _0 _1 _2 <> SetAttributeValue _ _ _ = SetAttributeValue _0 _1 _2


------------------------------------------------------------------------------
instance M.Method SetAttributeValue where
    type Result SetAttributeValue = ()
    name _ = "DOM.setAttributeValue"


------------------------------------------------------------------------------
-- | Sets attribute for an element with given id.
setAttributeValue
    :: NodeId
    -- ^ Id of the element to set attribute for.

    -> T.Text
    -- ^ Attribute name.

    -> T.Text
    -- ^ Attribute value.

    -> SetAttributeValue
setAttributeValue _0 _1 _2 = SetAttributeValue _0 _1 _2


------------------------------------------------------------------------------
-- | Sets attributes on element with given id. This method is useful when user edits some existing
-- attribute value and types in several attribute name\/value pairs.
data SetAttributesAsText = SetAttributesAsText
    { -- | Id of the element to set attributes for.
      nodeId :: !NodeId
      -- | Text with a number of attributes. Will parse this text using HTML parser.
    , text :: !T.Text
      -- | Attribute name to replace with new attributes derived from text in case text parsed
      -- successfully.
    , name :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetAttributesAsText where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setAttributesAsText" $ \_o -> SetAttributesAsText
            <$> _o .: "nodeId"
            <*> _o .: "text"
            <*> _o .:? "name"
        ago = A.withArray "setAttributesAsText" $ \_a -> SetAttributesAsText
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON SetAttributesAsText where
    toEncoding (SetAttributesAsText _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "text" .= _1
        , ("name" .=) <$> _2
        ]
    toJSON (SetAttributesAsText _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "text" .= _1
        , ("name" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetAttributesAsText where
    SetAttributesAsText _0 _1 _2 <> SetAttributesAsText _ _ __2 = SetAttributesAsText _0 _1 (_2 <|> __2)


------------------------------------------------------------------------------
instance M.Method SetAttributesAsText where
    type Result SetAttributesAsText = ()
    name _ = "DOM.setAttributesAsText"


------------------------------------------------------------------------------
-- | Sets attributes on element with given id. This method is useful when user edits some existing
-- attribute value and types in several attribute name\/value pairs.
setAttributesAsText
    :: NodeId
    -- ^ Id of the element to set attributes for.

    -> T.Text
    -- ^ Text with a number of attributes. Will parse this text using HTML parser.

    -> SetAttributesAsText
setAttributesAsText _0 _1 = SetAttributesAsText _0 _1 P.empty


------------------------------------------------------------------------------
-- | Sets files for the given file input element.
data SetFileInputFiles = SetFileInputFiles
    { -- | Array of file paths to set.
      files :: ![T.Text]
      -- | Identifier of the node.
    , nodeId :: !(P.Maybe NodeId)
      -- | Identifier of the backend node.
    , backendNodeId :: !(P.Maybe BackendNodeId)
      -- | JavaScript object id of the node wrapper.
    , objectId :: !(P.Maybe Runtime.RemoteObjectId)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetFileInputFiles where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setFileInputFiles" $ \_o -> SetFileInputFiles
            <$> _o .: "files"
            <*> _o .:? "nodeId"
            <*> _o .:? "backendNodeId"
            <*> _o .:? "objectId"
        ago = A.withArray "setFileInputFiles" $ \_a -> SetFileInputFiles
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON SetFileInputFiles where
    toEncoding (SetFileInputFiles _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "files" .= _0
        , ("nodeId" .=) <$> _1
        , ("backendNodeId" .=) <$> _2
        , ("objectId" .=) <$> _3
        ]
    toJSON (SetFileInputFiles _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "files" .= _0
        , ("nodeId" .=) <$> _1
        , ("backendNodeId" .=) <$> _2
        , ("objectId" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetFileInputFiles where
    SetFileInputFiles _0 _1 _2 _3 <> SetFileInputFiles _ __1 __2 __3 = SetFileInputFiles _0 (_1 <|> __1) (_2 <|> __2) (_3 <|> __3)


------------------------------------------------------------------------------
instance M.Method SetFileInputFiles where
    type Result SetFileInputFiles = ()
    name _ = "DOM.setFileInputFiles"


------------------------------------------------------------------------------
-- | Sets files for the given file input element.
setFileInputFiles
    :: [T.Text]
    -- ^ Array of file paths to set.

    -> SetFileInputFiles
setFileInputFiles _0 = SetFileInputFiles _0 P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Sets if stack traces should be captured for Nodes. See @Node.getNodeStackTraces@. Default is disabled.
{-# WARNING SetNodeStackTracesEnabled "This feature is marked as EXPERIMENTAL." #-}
data SetNodeStackTracesEnabled = SetNodeStackTracesEnabled
    { -- | Enable or disable.
      enable_ :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetNodeStackTracesEnabled where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setNodeStackTracesEnabled" $ \_o -> SetNodeStackTracesEnabled
            <$> _o .: "enable"
        ago = A.withArray "setNodeStackTracesEnabled" $ \_a -> SetNodeStackTracesEnabled
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetNodeStackTracesEnabled where
    toEncoding (SetNodeStackTracesEnabled _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "enable" .= _0
        ]
    toJSON (SetNodeStackTracesEnabled _0) = A.object $ P.catMaybes
        [ P.pure $ "enable" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetNodeStackTracesEnabled where
    SetNodeStackTracesEnabled _0 <> SetNodeStackTracesEnabled _ = SetNodeStackTracesEnabled _0


------------------------------------------------------------------------------
instance M.Method SetNodeStackTracesEnabled where
    type Result SetNodeStackTracesEnabled = ()
    name _ = "DOM.setNodeStackTracesEnabled"


------------------------------------------------------------------------------
-- | Sets if stack traces should be captured for Nodes. See @Node.getNodeStackTraces@. Default is disabled.
{-# WARNING setNodeStackTracesEnabled "This feature is marked as EXPERIMENTAL." #-}
setNodeStackTracesEnabled
    :: P.Bool
    -- ^ Enable or disable.

    -> SetNodeStackTracesEnabled
setNodeStackTracesEnabled _0 = SetNodeStackTracesEnabled _0


------------------------------------------------------------------------------
-- | Gets stack traces associated with a Node. As of now, only provides stack trace for Node creation.
{-# WARNING GetNodeStackTraces "This feature is marked as EXPERIMENTAL." #-}
data GetNodeStackTraces = GetNodeStackTraces
    { -- | Id of the node to get stack traces for.
      nodeId :: !NodeId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetNodeStackTraces where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getNodeStackTraces" $ \_o -> GetNodeStackTraces
            <$> _o .: "nodeId"
        ago = A.withArray "getNodeStackTraces" $ \_a -> GetNodeStackTraces
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetNodeStackTraces where
    toEncoding (GetNodeStackTraces _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]
    toJSON (GetNodeStackTraces _0) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetNodeStackTraces where
    GetNodeStackTraces _0 <> GetNodeStackTraces _ = GetNodeStackTraces _0


------------------------------------------------------------------------------
-- | Gets stack traces associated with a Node. As of now, only provides stack trace for Node creation.
{-# WARNING GetNodeStackTracesResult "This feature is marked as EXPERIMENTAL." #-}
data GetNodeStackTracesResult = GetNodeStackTracesResult
    { -- | Creation stack trace, if available.
      creation :: !(P.Maybe Runtime.StackTrace)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetNodeStackTracesResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getNodeStackTracesResult" $ \_o -> GetNodeStackTracesResult
            <$> _o .:? "creation"
        ago = A.withArray "getNodeStackTracesResult" $ \_a -> GetNodeStackTracesResult
            <$> P.traverse A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetNodeStackTracesResult where
    toEncoding (GetNodeStackTracesResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ ("creation" .=) <$> _0
        ]
    toJSON (GetNodeStackTracesResult _0) = A.object $ P.catMaybes
        [ ("creation" .=) <$> _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetNodeStackTracesResult where
    GetNodeStackTracesResult _0 <> GetNodeStackTracesResult __0 = GetNodeStackTracesResult (_0 <|> __0)


------------------------------------------------------------------------------
instance P.Monoid GetNodeStackTracesResult where
    mempty = GetNodeStackTracesResult P.empty


------------------------------------------------------------------------------
instance M.Method GetNodeStackTraces where
    type Result GetNodeStackTraces = GetNodeStackTracesResult
    name _ = "DOM.getNodeStackTraces"


------------------------------------------------------------------------------
-- | Gets stack traces associated with a Node. As of now, only provides stack trace for Node creation.
{-# WARNING getNodeStackTraces "This feature is marked as EXPERIMENTAL." #-}
getNodeStackTraces
    :: NodeId
    -- ^ Id of the node to get stack traces for.

    -> GetNodeStackTraces
getNodeStackTraces _0 = GetNodeStackTraces _0


------------------------------------------------------------------------------
-- | Returns file information for the given
-- File wrapper.
{-# WARNING GetFileInfo "This feature is marked as EXPERIMENTAL." #-}
data GetFileInfo = GetFileInfo
    { -- | JavaScript object id of the node wrapper.
      objectId :: !Runtime.RemoteObjectId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetFileInfo where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getFileInfo" $ \_o -> GetFileInfo
            <$> _o .: "objectId"
        ago = A.withArray "getFileInfo" $ \_a -> GetFileInfo
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetFileInfo where
    toEncoding (GetFileInfo _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "objectId" .= _0
        ]
    toJSON (GetFileInfo _0) = A.object $ P.catMaybes
        [ P.pure $ "objectId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetFileInfo where
    GetFileInfo _0 <> GetFileInfo _ = GetFileInfo _0


------------------------------------------------------------------------------
-- | Returns file information for the given
-- File wrapper.
{-# WARNING GetFileInfoResult "This feature is marked as EXPERIMENTAL." #-}
data GetFileInfoResult = GetFileInfoResult
    { path :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetFileInfoResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getFileInfoResult" $ \_o -> GetFileInfoResult
            <$> _o .: "path"
        ago = A.withArray "getFileInfoResult" $ \_a -> GetFileInfoResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetFileInfoResult where
    toEncoding (GetFileInfoResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "path" .= _0
        ]
    toJSON (GetFileInfoResult _0) = A.object $ P.catMaybes
        [ P.pure $ "path" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetFileInfoResult where
    GetFileInfoResult _0 <> GetFileInfoResult _ = GetFileInfoResult _0


------------------------------------------------------------------------------
instance M.Method GetFileInfo where
    type Result GetFileInfo = GetFileInfoResult
    name _ = "DOM.getFileInfo"


------------------------------------------------------------------------------
-- | Returns file information for the given
-- File wrapper.
{-# WARNING getFileInfo "This feature is marked as EXPERIMENTAL." #-}
getFileInfo
    :: Runtime.RemoteObjectId
    -- ^ JavaScript object id of the node wrapper.

    -> GetFileInfo
getFileInfo _0 = GetFileInfo _0


------------------------------------------------------------------------------
-- | Enables console to refer to the node with given id via $x (see Command Line API for more details
-- $x functions).
{-# WARNING SetInspectedNode "This feature is marked as EXPERIMENTAL." #-}
data SetInspectedNode = SetInspectedNode
    { -- | DOM node id to be accessible by means of $x command line API.
      nodeId :: !NodeId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetInspectedNode where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setInspectedNode" $ \_o -> SetInspectedNode
            <$> _o .: "nodeId"
        ago = A.withArray "setInspectedNode" $ \_a -> SetInspectedNode
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetInspectedNode where
    toEncoding (SetInspectedNode _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]
    toJSON (SetInspectedNode _0) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetInspectedNode where
    SetInspectedNode _0 <> SetInspectedNode _ = SetInspectedNode _0


------------------------------------------------------------------------------
instance M.Method SetInspectedNode where
    type Result SetInspectedNode = ()
    name _ = "DOM.setInspectedNode"


------------------------------------------------------------------------------
-- | Enables console to refer to the node with given id via $x (see Command Line API for more details
-- $x functions).
{-# WARNING setInspectedNode "This feature is marked as EXPERIMENTAL." #-}
setInspectedNode
    :: NodeId
    -- ^ DOM node id to be accessible by means of $x command line API.

    -> SetInspectedNode
setInspectedNode _0 = SetInspectedNode _0


------------------------------------------------------------------------------
-- | Sets node name for a node with given id.
data SetNodeName = SetNodeName
    { -- | Id of the node to set name for.
      nodeId :: !NodeId
      -- | New node's name.
    , name :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetNodeName where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setNodeName" $ \_o -> SetNodeName
            <$> _o .: "nodeId"
            <*> _o .: "name"
        ago = A.withArray "setNodeName" $ \_a -> SetNodeName
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SetNodeName where
    toEncoding (SetNodeName _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "name" .= _1
        ]
    toJSON (SetNodeName _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "name" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetNodeName where
    SetNodeName _0 _1 <> SetNodeName _ _ = SetNodeName _0 _1


------------------------------------------------------------------------------
-- | Sets node name for a node with given id.
data SetNodeNameResult = SetNodeNameResult
    { -- | New node's id.
      nodeId :: !NodeId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetNodeNameResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setNodeNameResult" $ \_o -> SetNodeNameResult
            <$> _o .: "nodeId"
        ago = A.withArray "setNodeNameResult" $ \_a -> SetNodeNameResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetNodeNameResult where
    toEncoding (SetNodeNameResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]
    toJSON (SetNodeNameResult _0) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetNodeNameResult where
    SetNodeNameResult _0 <> SetNodeNameResult _ = SetNodeNameResult _0


------------------------------------------------------------------------------
instance M.Method SetNodeName where
    type Result SetNodeName = SetNodeNameResult
    name _ = "DOM.setNodeName"


------------------------------------------------------------------------------
-- | Sets node name for a node with given id.
setNodeName
    :: NodeId
    -- ^ Id of the node to set name for.

    -> T.Text
    -- ^ New node's name.

    -> SetNodeName
setNodeName _0 _1 = SetNodeName _0 _1


------------------------------------------------------------------------------
-- | Sets node value for a node with given id.
data SetNodeValue = SetNodeValue
    { -- | Id of the node to set value for.
      nodeId :: !NodeId
      -- | New node's value.
    , value :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetNodeValue where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setNodeValue" $ \_o -> SetNodeValue
            <$> _o .: "nodeId"
            <*> _o .: "value"
        ago = A.withArray "setNodeValue" $ \_a -> SetNodeValue
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SetNodeValue where
    toEncoding (SetNodeValue _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "value" .= _1
        ]
    toJSON (SetNodeValue _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "value" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetNodeValue where
    SetNodeValue _0 _1 <> SetNodeValue _ _ = SetNodeValue _0 _1


------------------------------------------------------------------------------
instance M.Method SetNodeValue where
    type Result SetNodeValue = ()
    name _ = "DOM.setNodeValue"


------------------------------------------------------------------------------
-- | Sets node value for a node with given id.
setNodeValue
    :: NodeId
    -- ^ Id of the node to set value for.

    -> T.Text
    -- ^ New node's value.

    -> SetNodeValue
setNodeValue _0 _1 = SetNodeValue _0 _1


------------------------------------------------------------------------------
-- | Sets node HTML markup, returns new node id.
data SetOuterHTML = SetOuterHTML
    { -- | Id of the node to set markup for.
      nodeId :: !NodeId
      -- | Outer HTML markup to set.
    , outerHTML :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetOuterHTML where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setOuterHTML" $ \_o -> SetOuterHTML
            <$> _o .: "nodeId"
            <*> _o .: "outerHTML"
        ago = A.withArray "setOuterHTML" $ \_a -> SetOuterHTML
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SetOuterHTML where
    toEncoding (SetOuterHTML _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "outerHTML" .= _1
        ]
    toJSON (SetOuterHTML _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "outerHTML" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetOuterHTML where
    SetOuterHTML _0 _1 <> SetOuterHTML _ _ = SetOuterHTML _0 _1


------------------------------------------------------------------------------
instance M.Method SetOuterHTML where
    type Result SetOuterHTML = ()
    name _ = "DOM.setOuterHTML"


------------------------------------------------------------------------------
-- | Sets node HTML markup, returns new node id.
setOuterHTML
    :: NodeId
    -- ^ Id of the node to set markup for.

    -> T.Text
    -- ^ Outer HTML markup to set.

    -> SetOuterHTML
setOuterHTML _0 _1 = SetOuterHTML _0 _1


------------------------------------------------------------------------------
-- | Undoes the last performed action.
{-# WARNING Undo "This feature is marked as EXPERIMENTAL." #-}
data Undo = Undo
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Undo where
    parseJSON A.Null = P.pure Undo
    parseJSON v = A.withArray "undo" go v
        <|> A.withObject "undo" go v
      where
        go _ = P.pure Undo


------------------------------------------------------------------------------
instance A.ToJSON Undo where
    toEncoding Undo = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON Undo = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup Undo where
    Undo <> Undo = Undo


------------------------------------------------------------------------------
instance P.Monoid Undo where
    mempty = Undo


------------------------------------------------------------------------------
instance M.Method Undo where
    type Result Undo = ()
    name _ = "DOM.undo"


------------------------------------------------------------------------------
-- | Undoes the last performed action.
{-# WARNING undo "This feature is marked as EXPERIMENTAL." #-}
undo
    :: Undo
undo = Undo


------------------------------------------------------------------------------
-- | Returns iframe node that owns iframe with the given domain.
{-# WARNING GetFrameOwner "This feature is marked as EXPERIMENTAL." #-}
data GetFrameOwner = GetFrameOwner
    { frameId :: !Page.FrameId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetFrameOwner where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getFrameOwner" $ \_o -> GetFrameOwner
            <$> _o .: "frameId"
        ago = A.withArray "getFrameOwner" $ \_a -> GetFrameOwner
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetFrameOwner where
    toEncoding (GetFrameOwner _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        ]
    toJSON (GetFrameOwner _0) = A.object $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetFrameOwner where
    GetFrameOwner _0 <> GetFrameOwner _ = GetFrameOwner _0


------------------------------------------------------------------------------
-- | Returns iframe node that owns iframe with the given domain.
{-# WARNING GetFrameOwnerResult "This feature is marked as EXPERIMENTAL." #-}
data GetFrameOwnerResult = GetFrameOwnerResult
    { -- | Resulting node.
      backendNodeId :: !BackendNodeId
      -- | Id of the node at given coordinates, only when enabled and requested document.
    , nodeId :: !(P.Maybe NodeId)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetFrameOwnerResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getFrameOwnerResult" $ \_o -> GetFrameOwnerResult
            <$> _o .: "backendNodeId"
            <*> _o .:? "nodeId"
        ago = A.withArray "getFrameOwnerResult" $ \_a -> GetFrameOwnerResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON GetFrameOwnerResult where
    toEncoding (GetFrameOwnerResult _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "backendNodeId" .= _0
        , ("nodeId" .=) <$> _1
        ]
    toJSON (GetFrameOwnerResult _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "backendNodeId" .= _0
        , ("nodeId" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetFrameOwnerResult where
    GetFrameOwnerResult _0 _1 <> GetFrameOwnerResult _ __1 = GetFrameOwnerResult _0 (_1 <|> __1)


------------------------------------------------------------------------------
instance M.Method GetFrameOwner where
    type Result GetFrameOwner = GetFrameOwnerResult
    name _ = "DOM.getFrameOwner"


------------------------------------------------------------------------------
-- | Returns iframe node that owns iframe with the given domain.
{-# WARNING getFrameOwner "This feature is marked as EXPERIMENTAL." #-}
getFrameOwner
    :: Page.FrameId
    -> GetFrameOwner
getFrameOwner _0 = GetFrameOwner _0


------------------------------------------------------------------------------
-- | Fired when @Element@'s attribute is modified.
data AttributeModified = AttributeModified
    { -- | Id of the node that has changed.
      nodeId :: !NodeId
      -- | Attribute name.
    , name :: !T.Text
      -- | Attribute value.
    , value :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AttributeModified where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "attributeModified" $ \_o -> AttributeModified
            <$> _o .: "nodeId"
            <*> _o .: "name"
            <*> _o .: "value"
        ago = A.withArray "attributeModified" $ \_a -> AttributeModified
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON AttributeModified where
    toEncoding (AttributeModified _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "name" .= _1
        , P.pure $ "value" .= _2
        ]
    toJSON (AttributeModified _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "name" .= _1
        , P.pure $ "value" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup AttributeModified where
    AttributeModified _0 _1 _2 <> AttributeModified _ _ _ = AttributeModified _0 _1 _2


------------------------------------------------------------------------------
instance E.Event AttributeModified where
    type Result AttributeModified = AttributeModified
    name _ = "DOM.attributeModified"


------------------------------------------------------------------------------
-- | Fired when @Element@'s attribute is modified.
attributeModified :: P.Proxy AttributeModified
attributeModified = P.Proxy


------------------------------------------------------------------------------
-- | Fired when @Element@'s attribute is removed.
data AttributeRemoved = AttributeRemoved
    { -- | Id of the node that has changed.
      nodeId :: !NodeId
      -- | A ttribute name.
    , name :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AttributeRemoved where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "attributeRemoved" $ \_o -> AttributeRemoved
            <$> _o .: "nodeId"
            <*> _o .: "name"
        ago = A.withArray "attributeRemoved" $ \_a -> AttributeRemoved
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON AttributeRemoved where
    toEncoding (AttributeRemoved _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "name" .= _1
        ]
    toJSON (AttributeRemoved _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "name" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup AttributeRemoved where
    AttributeRemoved _0 _1 <> AttributeRemoved _ _ = AttributeRemoved _0 _1


------------------------------------------------------------------------------
instance E.Event AttributeRemoved where
    type Result AttributeRemoved = AttributeRemoved
    name _ = "DOM.attributeRemoved"


------------------------------------------------------------------------------
-- | Fired when @Element@'s attribute is removed.
attributeRemoved :: P.Proxy AttributeRemoved
attributeRemoved = P.Proxy


------------------------------------------------------------------------------
-- | Mirrors @DOMCharacterDataModified@ event.
data CharacterDataModified = CharacterDataModified
    { -- | Id of the node that has changed.
      nodeId :: !NodeId
      -- | New text value.
    , characterData :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CharacterDataModified where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "characterDataModified" $ \_o -> CharacterDataModified
            <$> _o .: "nodeId"
            <*> _o .: "characterData"
        ago = A.withArray "characterDataModified" $ \_a -> CharacterDataModified
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON CharacterDataModified where
    toEncoding (CharacterDataModified _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "characterData" .= _1
        ]
    toJSON (CharacterDataModified _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "characterData" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup CharacterDataModified where
    CharacterDataModified _0 _1 <> CharacterDataModified _ _ = CharacterDataModified _0 _1


------------------------------------------------------------------------------
instance E.Event CharacterDataModified where
    type Result CharacterDataModified = CharacterDataModified
    name _ = "DOM.characterDataModified"


------------------------------------------------------------------------------
-- | Mirrors @DOMCharacterDataModified@ event.
characterDataModified :: P.Proxy CharacterDataModified
characterDataModified = P.Proxy


------------------------------------------------------------------------------
-- | Fired when @Container@'s child node count has changed.
data ChildNodeCountUpdated = ChildNodeCountUpdated
    { -- | Id of the node that has changed.
      nodeId :: !NodeId
      -- | New node count.
    , childNodeCount :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ChildNodeCountUpdated where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "childNodeCountUpdated" $ \_o -> ChildNodeCountUpdated
            <$> _o .: "nodeId"
            <*> _o .: "childNodeCount"
        ago = A.withArray "childNodeCountUpdated" $ \_a -> ChildNodeCountUpdated
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON ChildNodeCountUpdated where
    toEncoding (ChildNodeCountUpdated _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "childNodeCount" .= _1
        ]
    toJSON (ChildNodeCountUpdated _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "nodeId" .= _0
        , P.pure $ "childNodeCount" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup ChildNodeCountUpdated where
    ChildNodeCountUpdated _0 _1 <> ChildNodeCountUpdated _ _ = ChildNodeCountUpdated _0 _1


------------------------------------------------------------------------------
instance E.Event ChildNodeCountUpdated where
    type Result ChildNodeCountUpdated = ChildNodeCountUpdated
    name _ = "DOM.childNodeCountUpdated"


------------------------------------------------------------------------------
-- | Fired when @Container@'s child node count has changed.
childNodeCountUpdated :: P.Proxy ChildNodeCountUpdated
childNodeCountUpdated = P.Proxy


------------------------------------------------------------------------------
-- | Mirrors @DOMNodeInserted@ event.
data ChildNodeInserted = ChildNodeInserted
    { -- | Id of the node that has changed.
      parentNodeId :: !NodeId
      -- | If of the previous siblint.
    , previousNodeId :: !NodeId
      -- | Inserted node data.
    , node :: !Node
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ChildNodeInserted where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "childNodeInserted" $ \_o -> ChildNodeInserted
            <$> _o .: "parentNodeId"
            <*> _o .: "previousNodeId"
            <*> _o .: "node"
        ago = A.withArray "childNodeInserted" $ \_a -> ChildNodeInserted
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON ChildNodeInserted where
    toEncoding (ChildNodeInserted _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "parentNodeId" .= _0
        , P.pure $ "previousNodeId" .= _1
        , P.pure $ "node" .= _2
        ]
    toJSON (ChildNodeInserted _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "parentNodeId" .= _0
        , P.pure $ "previousNodeId" .= _1
        , P.pure $ "node" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup ChildNodeInserted where
    ChildNodeInserted _0 _1 _2 <> ChildNodeInserted _ _ _ = ChildNodeInserted _0 _1 _2


------------------------------------------------------------------------------
instance E.Event ChildNodeInserted where
    type Result ChildNodeInserted = ChildNodeInserted
    name _ = "DOM.childNodeInserted"


------------------------------------------------------------------------------
-- | Mirrors @DOMNodeInserted@ event.
childNodeInserted :: P.Proxy ChildNodeInserted
childNodeInserted = P.Proxy


------------------------------------------------------------------------------
-- | Mirrors @DOMNodeRemoved@ event.
data ChildNodeRemoved = ChildNodeRemoved
    { -- | Parent id.
      parentNodeId :: !NodeId
      -- | Id of the node that has been removed.
    , nodeId :: !NodeId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ChildNodeRemoved where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "childNodeRemoved" $ \_o -> ChildNodeRemoved
            <$> _o .: "parentNodeId"
            <*> _o .: "nodeId"
        ago = A.withArray "childNodeRemoved" $ \_a -> ChildNodeRemoved
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON ChildNodeRemoved where
    toEncoding (ChildNodeRemoved _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "parentNodeId" .= _0
        , P.pure $ "nodeId" .= _1
        ]
    toJSON (ChildNodeRemoved _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "parentNodeId" .= _0
        , P.pure $ "nodeId" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup ChildNodeRemoved where
    ChildNodeRemoved _0 _1 <> ChildNodeRemoved _ _ = ChildNodeRemoved _0 _1


------------------------------------------------------------------------------
instance E.Event ChildNodeRemoved where
    type Result ChildNodeRemoved = ChildNodeRemoved
    name _ = "DOM.childNodeRemoved"


------------------------------------------------------------------------------
-- | Mirrors @DOMNodeRemoved@ event.
childNodeRemoved :: P.Proxy ChildNodeRemoved
childNodeRemoved = P.Proxy


------------------------------------------------------------------------------
-- | Called when distrubution is changed.
{-# WARNING DistributedNodesUpdated "This feature is marked as EXPERIMENTAL." #-}
data DistributedNodesUpdated = DistributedNodesUpdated
    { -- | Insertion point where distrubuted nodes were updated.
      insertionPointId :: !NodeId
      -- | Distributed nodes for given insertion point.
    , distributedNodes :: ![BackendNode]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DistributedNodesUpdated where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "distributedNodesUpdated" $ \_o -> DistributedNodesUpdated
            <$> _o .: "insertionPointId"
            <*> _o .: "distributedNodes"
        ago = A.withArray "distributedNodesUpdated" $ \_a -> DistributedNodesUpdated
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON DistributedNodesUpdated where
    toEncoding (DistributedNodesUpdated _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "insertionPointId" .= _0
        , P.pure $ "distributedNodes" .= _1
        ]
    toJSON (DistributedNodesUpdated _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "insertionPointId" .= _0
        , P.pure $ "distributedNodes" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup DistributedNodesUpdated where
    DistributedNodesUpdated _0 _1 <> DistributedNodesUpdated _ _ = DistributedNodesUpdated _0 _1


------------------------------------------------------------------------------
instance E.Event DistributedNodesUpdated where
    type Result DistributedNodesUpdated = DistributedNodesUpdated
    name _ = "DOM.distributedNodesUpdated"


------------------------------------------------------------------------------
-- | Called when distrubution is changed.
{-# WARNING distributedNodesUpdated "This feature is marked as EXPERIMENTAL." #-}
distributedNodesUpdated :: P.Proxy DistributedNodesUpdated
distributedNodesUpdated = P.Proxy


------------------------------------------------------------------------------
-- | Fired when @Document@ has been totally updated. Node ids are no longer valid.
data DocumentUpdated = DocumentUpdated
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DocumentUpdated where
    parseJSON A.Null = P.pure DocumentUpdated
    parseJSON v = A.withArray "documentUpdated" go v
        <|> A.withObject "documentUpdated" go v
      where
        go _ = P.pure DocumentUpdated


------------------------------------------------------------------------------
instance A.ToJSON DocumentUpdated where
    toEncoding DocumentUpdated = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON DocumentUpdated = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup DocumentUpdated where
    DocumentUpdated <> DocumentUpdated = DocumentUpdated


------------------------------------------------------------------------------
instance P.Monoid DocumentUpdated where
    mempty = DocumentUpdated


------------------------------------------------------------------------------
instance E.Event DocumentUpdated where
    type Result DocumentUpdated = ()
    name _ = "DOM.documentUpdated"


------------------------------------------------------------------------------
-- | Fired when @Document@ has been totally updated. Node ids are no longer valid.
documentUpdated :: P.Proxy DocumentUpdated
documentUpdated = P.Proxy


------------------------------------------------------------------------------
-- | Fired when @Element@'s inline style is modified via a CSS property modification.
{-# WARNING InlineStyleInvalidated "This feature is marked as EXPERIMENTAL." #-}
data InlineStyleInvalidated = InlineStyleInvalidated
    { -- | Ids of the nodes for which the inline styles have been invalidated.
      nodeIds :: ![NodeId]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON InlineStyleInvalidated where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "inlineStyleInvalidated" $ \_o -> InlineStyleInvalidated
            <$> _o .: "nodeIds"
        ago = A.withArray "inlineStyleInvalidated" $ \_a -> InlineStyleInvalidated
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON InlineStyleInvalidated where
    toEncoding (InlineStyleInvalidated _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "nodeIds" .= _0
        ]
    toJSON (InlineStyleInvalidated _0) = A.object $ P.catMaybes
        [ P.pure $ "nodeIds" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup InlineStyleInvalidated where
    InlineStyleInvalidated _0 <> InlineStyleInvalidated _ = InlineStyleInvalidated _0


------------------------------------------------------------------------------
instance E.Event InlineStyleInvalidated where
    type Result InlineStyleInvalidated = InlineStyleInvalidated
    name _ = "DOM.inlineStyleInvalidated"


------------------------------------------------------------------------------
-- | Fired when @Element@'s inline style is modified via a CSS property modification.
{-# WARNING inlineStyleInvalidated "This feature is marked as EXPERIMENTAL." #-}
inlineStyleInvalidated :: P.Proxy InlineStyleInvalidated
inlineStyleInvalidated = P.Proxy


------------------------------------------------------------------------------
-- | Called when a pseudo element is added to an element.
{-# WARNING PseudoElementAdded "This feature is marked as EXPERIMENTAL." #-}
data PseudoElementAdded = PseudoElementAdded
    { -- | Pseudo element's parent element id.
      parentId :: !NodeId
      -- | The added pseudo element.
    , pseudoElement :: !Node
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PseudoElementAdded where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "pseudoElementAdded" $ \_o -> PseudoElementAdded
            <$> _o .: "parentId"
            <*> _o .: "pseudoElement"
        ago = A.withArray "pseudoElementAdded" $ \_a -> PseudoElementAdded
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON PseudoElementAdded where
    toEncoding (PseudoElementAdded _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "parentId" .= _0
        , P.pure $ "pseudoElement" .= _1
        ]
    toJSON (PseudoElementAdded _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "parentId" .= _0
        , P.pure $ "pseudoElement" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup PseudoElementAdded where
    PseudoElementAdded _0 _1 <> PseudoElementAdded _ _ = PseudoElementAdded _0 _1


------------------------------------------------------------------------------
instance E.Event PseudoElementAdded where
    type Result PseudoElementAdded = PseudoElementAdded
    name _ = "DOM.pseudoElementAdded"


------------------------------------------------------------------------------
-- | Called when a pseudo element is added to an element.
{-# WARNING pseudoElementAdded "This feature is marked as EXPERIMENTAL." #-}
pseudoElementAdded :: P.Proxy PseudoElementAdded
pseudoElementAdded = P.Proxy


------------------------------------------------------------------------------
-- | Called when a pseudo element is removed from an element.
{-# WARNING PseudoElementRemoved "This feature is marked as EXPERIMENTAL." #-}
data PseudoElementRemoved = PseudoElementRemoved
    { -- | Pseudo element's parent element id.
      parentId :: !NodeId
      -- | The removed pseudo element id.
    , pseudoElementId :: !NodeId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PseudoElementRemoved where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "pseudoElementRemoved" $ \_o -> PseudoElementRemoved
            <$> _o .: "parentId"
            <*> _o .: "pseudoElementId"
        ago = A.withArray "pseudoElementRemoved" $ \_a -> PseudoElementRemoved
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON PseudoElementRemoved where
    toEncoding (PseudoElementRemoved _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "parentId" .= _0
        , P.pure $ "pseudoElementId" .= _1
        ]
    toJSON (PseudoElementRemoved _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "parentId" .= _0
        , P.pure $ "pseudoElementId" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup PseudoElementRemoved where
    PseudoElementRemoved _0 _1 <> PseudoElementRemoved _ _ = PseudoElementRemoved _0 _1


------------------------------------------------------------------------------
instance E.Event PseudoElementRemoved where
    type Result PseudoElementRemoved = PseudoElementRemoved
    name _ = "DOM.pseudoElementRemoved"


------------------------------------------------------------------------------
-- | Called when a pseudo element is removed from an element.
{-# WARNING pseudoElementRemoved "This feature is marked as EXPERIMENTAL." #-}
pseudoElementRemoved :: P.Proxy PseudoElementRemoved
pseudoElementRemoved = P.Proxy


------------------------------------------------------------------------------
-- | Fired when backend wants to provide client with the missing DOM structure. This happens upon
-- most of the calls requesting node ids.
data SetChildNodes = SetChildNodes
    { -- | Parent node id to populate with children.
      parentId :: !NodeId
      -- | Child nodes array.
    , nodes :: ![Node]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetChildNodes where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setChildNodes" $ \_o -> SetChildNodes
            <$> _o .: "parentId"
            <*> _o .: "nodes"
        ago = A.withArray "setChildNodes" $ \_a -> SetChildNodes
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SetChildNodes where
    toEncoding (SetChildNodes _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "parentId" .= _0
        , P.pure $ "nodes" .= _1
        ]
    toJSON (SetChildNodes _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "parentId" .= _0
        , P.pure $ "nodes" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetChildNodes where
    SetChildNodes _0 _1 <> SetChildNodes _ _ = SetChildNodes _0 _1


------------------------------------------------------------------------------
instance E.Event SetChildNodes where
    type Result SetChildNodes = SetChildNodes
    name _ = "DOM.setChildNodes"


------------------------------------------------------------------------------
-- | Fired when backend wants to provide client with the missing DOM structure. This happens upon
-- most of the calls requesting node ids.
setChildNodes :: P.Proxy SetChildNodes
setChildNodes = P.Proxy


------------------------------------------------------------------------------
-- | Called when shadow root is popped from the element.
{-# WARNING ShadowRootPopped "This feature is marked as EXPERIMENTAL." #-}
data ShadowRootPopped = ShadowRootPopped
    { -- | Host element id.
      hostId :: !NodeId
      -- | Shadow root id.
    , rootId :: !NodeId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ShadowRootPopped where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "shadowRootPopped" $ \_o -> ShadowRootPopped
            <$> _o .: "hostId"
            <*> _o .: "rootId"
        ago = A.withArray "shadowRootPopped" $ \_a -> ShadowRootPopped
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON ShadowRootPopped where
    toEncoding (ShadowRootPopped _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "hostId" .= _0
        , P.pure $ "rootId" .= _1
        ]
    toJSON (ShadowRootPopped _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "hostId" .= _0
        , P.pure $ "rootId" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup ShadowRootPopped where
    ShadowRootPopped _0 _1 <> ShadowRootPopped _ _ = ShadowRootPopped _0 _1


------------------------------------------------------------------------------
instance E.Event ShadowRootPopped where
    type Result ShadowRootPopped = ShadowRootPopped
    name _ = "DOM.shadowRootPopped"


------------------------------------------------------------------------------
-- | Called when shadow root is popped from the element.
{-# WARNING shadowRootPopped "This feature is marked as EXPERIMENTAL." #-}
shadowRootPopped :: P.Proxy ShadowRootPopped
shadowRootPopped = P.Proxy


------------------------------------------------------------------------------
-- | Called when shadow root is pushed into the element.
{-# WARNING ShadowRootPushed "This feature is marked as EXPERIMENTAL." #-}
data ShadowRootPushed = ShadowRootPushed
    { -- | Host element id.
      hostId :: !NodeId
      -- | Shadow root.
    , root :: !Node
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ShadowRootPushed where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "shadowRootPushed" $ \_o -> ShadowRootPushed
            <$> _o .: "hostId"
            <*> _o .: "root"
        ago = A.withArray "shadowRootPushed" $ \_a -> ShadowRootPushed
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON ShadowRootPushed where
    toEncoding (ShadowRootPushed _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "hostId" .= _0
        , P.pure $ "root" .= _1
        ]
    toJSON (ShadowRootPushed _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "hostId" .= _0
        , P.pure $ "root" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup ShadowRootPushed where
    ShadowRootPushed _0 _1 <> ShadowRootPushed _ _ = ShadowRootPushed _0 _1


------------------------------------------------------------------------------
instance E.Event ShadowRootPushed where
    type Result ShadowRootPushed = ShadowRootPushed
    name _ = "DOM.shadowRootPushed"


------------------------------------------------------------------------------
-- | Called when shadow root is pushed into the element.
{-# WARNING shadowRootPushed "This feature is marked as EXPERIMENTAL." #-}
shadowRootPushed :: P.Proxy ShadowRootPushed
shadowRootPushed = P.Proxy

