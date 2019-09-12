{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | This domain facilitates obtaining document snapshots with DOM, layout, and style information.
module DevTools.API.DOMSnapshot{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

    ( module DevTools.API.DOMSnapshot.Types
    , module DevTools.API.DOMSnapshot
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
import           DevTools.API.DOMSnapshot.Types


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Disables DOM snapshot agent for the given page.
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
    name _ = "DOMSnapshot.disable"


------------------------------------------------------------------------------
-- | Disables DOM snapshot agent for the given page.
disable
    :: Disable
disable = Disable


------------------------------------------------------------------------------
-- | Enables DOM snapshot agent for the given page.
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
    name _ = "DOMSnapshot.enable"


------------------------------------------------------------------------------
-- | Enables DOM snapshot agent for the given page.
enable
    :: Enable
enable = Enable


------------------------------------------------------------------------------
-- | Returns a document snapshot, including the full DOM tree of the root node (including iframes,
-- template contents, and imported documents) in a flattened array, as well as layout and
-- white-listed computed style information for the nodes. Shadow DOM in the returned DOM tree is
-- flattened.
{-# DEPRECATED GetSnapshot "This may be removed in a future release." #-}
data GetSnapshot = GetSnapshot
    { -- | Whitelist of computed styles to return.
      computedStyleWhitelist :: ![T.Text]
      -- | Whether or not to retrieve details of DOM listeners (default false).
    , includeEventListeners :: !(P.Maybe P.Bool)
      -- | Whether to determine and include the paint order index of LayoutTreeNodes (default false).
    , includePaintOrder :: !(P.Maybe P.Bool)
      -- | Whether to include UA shadow tree in the snapshot (default false).
    , includeUserAgentShadowTree :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetSnapshot where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getSnapshot" $ \_o -> GetSnapshot
            <$> _o .: "computedStyleWhitelist"
            <*> _o .:? "includeEventListeners"
            <*> _o .:? "includePaintOrder"
            <*> _o .:? "includeUserAgentShadowTree"
        ago = A.withArray "getSnapshot" $ \_a -> GetSnapshot
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON GetSnapshot where
    toEncoding (GetSnapshot _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "computedStyleWhitelist" .= _0
        , ("includeEventListeners" .=) <$> _1
        , ("includePaintOrder" .=) <$> _2
        , ("includeUserAgentShadowTree" .=) <$> _3
        ]
    toJSON (GetSnapshot _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "computedStyleWhitelist" .= _0
        , ("includeEventListeners" .=) <$> _1
        , ("includePaintOrder" .=) <$> _2
        , ("includeUserAgentShadowTree" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetSnapshot where
    GetSnapshot _0 _1 _2 _3 <> GetSnapshot _ __1 __2 __3 = GetSnapshot _0 (_1 <|> __1) (_2 <|> __2) (_3 <|> __3)


------------------------------------------------------------------------------
-- | Returns a document snapshot, including the full DOM tree of the root node (including iframes,
-- template contents, and imported documents) in a flattened array, as well as layout and
-- white-listed computed style information for the nodes. Shadow DOM in the returned DOM tree is
-- flattened.
{-# DEPRECATED GetSnapshotResult "This may be removed in a future release." #-}
data GetSnapshotResult = GetSnapshotResult
    { -- | The nodes in the DOM tree. The DOMNode at index 0 corresponds to the root document.
      domNodes :: ![DOMNode]
      -- | The nodes in the layout tree.
    , layoutTreeNodes :: ![LayoutTreeNode]
      -- | Whitelisted ComputedStyle properties for each node in the layout tree.
    , computedStyles :: ![ComputedStyle]
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetSnapshotResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getSnapshotResult" $ \_o -> GetSnapshotResult
            <$> _o .: "domNodes"
            <*> _o .: "layoutTreeNodes"
            <*> _o .: "computedStyles"
        ago = A.withArray "getSnapshotResult" $ \_a -> GetSnapshotResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON GetSnapshotResult where
    toEncoding (GetSnapshotResult _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "domNodes" .= _0
        , P.pure $ "layoutTreeNodes" .= _1
        , P.pure $ "computedStyles" .= _2
        ]
    toJSON (GetSnapshotResult _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "domNodes" .= _0
        , P.pure $ "layoutTreeNodes" .= _1
        , P.pure $ "computedStyles" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetSnapshotResult where
    GetSnapshotResult _0 _1 _2 <> GetSnapshotResult _ _ _ = GetSnapshotResult _0 _1 _2


------------------------------------------------------------------------------
instance M.Method GetSnapshot where
    type Result GetSnapshot = GetSnapshotResult
    name _ = "DOMSnapshot.getSnapshot"


------------------------------------------------------------------------------
-- | Returns a document snapshot, including the full DOM tree of the root node (including iframes,
-- template contents, and imported documents) in a flattened array, as well as layout and
-- white-listed computed style information for the nodes. Shadow DOM in the returned DOM tree is
-- flattened.
{-# DEPRECATED getSnapshot "This may be removed in a future release." #-}
getSnapshot
    :: [T.Text]
    -- ^ Whitelist of computed styles to return.

    -> GetSnapshot
getSnapshot _0 = GetSnapshot _0 P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Returns a document snapshot, including the full DOM tree of the root node (including iframes,
-- template contents, and imported documents) in a flattened array, as well as layout and
-- white-listed computed style information for the nodes. Shadow DOM in the returned DOM tree is
-- flattened.
data CaptureSnapshot = CaptureSnapshot
    { -- | Whitelist of computed styles to return.
      computedStyles :: ![T.Text]
      -- | Whether to include layout object paint orders into the snapshot.
    , includePaintOrder :: !(P.Maybe P.Bool)
      -- | Whether to include DOM rectangles (offsetRects, clientRects, scrollRects) into the snapshot
    , includeDOMRects :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CaptureSnapshot where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "captureSnapshot" $ \_o -> CaptureSnapshot
            <$> _o .: "computedStyles"
            <*> _o .:? "includePaintOrder"
            <*> _o .:? "includeDOMRects"
        ago = A.withArray "captureSnapshot" $ \_a -> CaptureSnapshot
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON CaptureSnapshot where
    toEncoding (CaptureSnapshot _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "computedStyles" .= _0
        , ("includePaintOrder" .=) <$> _1
        , ("includeDOMRects" .=) <$> _2
        ]
    toJSON (CaptureSnapshot _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "computedStyles" .= _0
        , ("includePaintOrder" .=) <$> _1
        , ("includeDOMRects" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup CaptureSnapshot where
    CaptureSnapshot _0 _1 _2 <> CaptureSnapshot _ __1 __2 = CaptureSnapshot _0 (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
-- | Returns a document snapshot, including the full DOM tree of the root node (including iframes,
-- template contents, and imported documents) in a flattened array, as well as layout and
-- white-listed computed style information for the nodes. Shadow DOM in the returned DOM tree is
-- flattened.
data CaptureSnapshotResult = CaptureSnapshotResult
    { -- | The nodes in the DOM tree. The DOMNode at index 0 corresponds to the root document.
      documents :: ![DocumentSnapshot]
      -- | Shared string table that all string properties refer to with indexes.
    , strings :: ![T.Text]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CaptureSnapshotResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "captureSnapshotResult" $ \_o -> CaptureSnapshotResult
            <$> _o .: "documents"
            <*> _o .: "strings"
        ago = A.withArray "captureSnapshotResult" $ \_a -> CaptureSnapshotResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON CaptureSnapshotResult where
    toEncoding (CaptureSnapshotResult _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "documents" .= _0
        , P.pure $ "strings" .= _1
        ]
    toJSON (CaptureSnapshotResult _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "documents" .= _0
        , P.pure $ "strings" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup CaptureSnapshotResult where
    CaptureSnapshotResult _0 _1 <> CaptureSnapshotResult _ _ = CaptureSnapshotResult _0 _1


------------------------------------------------------------------------------
instance M.Method CaptureSnapshot where
    type Result CaptureSnapshot = CaptureSnapshotResult
    name _ = "DOMSnapshot.captureSnapshot"


------------------------------------------------------------------------------
-- | Returns a document snapshot, including the full DOM tree of the root node (including iframes,
-- template contents, and imported documents) in a flattened array, as well as layout and
-- white-listed computed style information for the nodes. Shadow DOM in the returned DOM tree is
-- flattened.
captureSnapshot
    :: [T.Text]
    -- ^ Whitelist of computed styles to return.

    -> CaptureSnapshot
captureSnapshot _0 = CaptureSnapshot _0 P.empty P.empty

