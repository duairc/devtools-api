{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Actions and events related to the inspected page belong to the page domain.
module DevTools.API.Page.Types
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
import qualified DevTools.API.Network.Types as Network


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Unique frame identifier.
type FrameId = T.Text


------------------------------------------------------------------------------
-- | Information about the Frame on the page.
{-# WARNING unreachableUrl "This feature is marked as EXPERIMENTAL." #-}
data Frame = Frame
    { -- | Frame unique identifier.
      id :: !T.Text
      -- | Parent frame identifier.
    , parentId :: !(P.Maybe T.Text)
      -- | Identifier of the loader associated with this frame.
    , loaderId :: !Network.LoaderId
      -- | Frame's name as specified in the tag.
    , name :: !(P.Maybe T.Text)
      -- | Frame document's URL.
    , url :: !T.Text
      -- | Frame document's security origin.
    , securityOrigin :: !T.Text
      -- | Frame document's mimeType as determined by the browser.
    , mimeType :: !T.Text
      -- | If the frame failed to load, this contains the URL that could not be loaded.
    , unreachableUrl :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Frame where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "Frame" $ \_o -> Frame
            <$> _o .: "id"
            <*> _o .:? "parentId"
            <*> _o .: "loaderId"
            <*> _o .:? "name"
            <*> _o .: "url"
            <*> _o .: "securityOrigin"
            <*> _o .: "mimeType"
            <*> _o .:? "unreachableUrl"
        ago = A.withArray "Frame" $ \_a -> Frame
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.maybe P.empty A.parseJSON (_a !? 6)
            <*> P.traverse A.parseJSON (_a !? 7)


------------------------------------------------------------------------------
instance A.ToJSON Frame where
    toEncoding (Frame _0 _1 _2 _3 _4 _5 _6 _7) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "id" .= _0
        , ("parentId" .=) <$> _1
        , P.pure $ "loaderId" .= _2
        , ("name" .=) <$> _3
        , P.pure $ "url" .= _4
        , P.pure $ "securityOrigin" .= _5
        , P.pure $ "mimeType" .= _6
        , ("unreachableUrl" .=) <$> _7
        ]
    toJSON (Frame _0 _1 _2 _3 _4 _5 _6 _7) = A.object $ P.catMaybes
        [ P.pure $ "id" .= _0
        , ("parentId" .=) <$> _1
        , P.pure $ "loaderId" .= _2
        , ("name" .=) <$> _3
        , P.pure $ "url" .= _4
        , P.pure $ "securityOrigin" .= _5
        , P.pure $ "mimeType" .= _6
        , ("unreachableUrl" .=) <$> _7
        ]


------------------------------------------------------------------------------
instance P.Semigroup Frame where
    Frame _0 _1 _2 _3 _4 _5 _6 _7 <> Frame _ __1 _ __3 _ _ _ __7 = Frame _0 (_1 <|> __1) _2 (_3 <|> __3) _4 _5 _6 (_7 <|> __7)


------------------------------------------------------------------------------
-- | Information about the Resource on the page.
{-# WARNING FrameResource "This feature is marked as EXPERIMENTAL." #-}
data FrameResource = FrameResource
    { -- | Resource URL.
      url :: !T.Text
      -- | Type of this resource.
    , type_ :: !Network.ResourceType
      -- | Resource mimeType as determined by the browser.
    , mimeType :: !T.Text
      -- | last-modified timestamp as reported by server.
    , lastModified :: !(P.Maybe Network.TimeSinceEpoch)
      -- | Resource content size.
    , contentSize :: !(P.Maybe P.Double)
      -- | True if the resource failed to load.
    , failed :: !(P.Maybe P.Bool)
      -- | True if the resource was canceled during loading.
    , canceled :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON FrameResource where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "FrameResource" $ \_o -> FrameResource
            <$> _o .: "url"
            <*> _o .: "type"
            <*> _o .: "mimeType"
            <*> _o .:? "lastModified"
            <*> _o .:? "contentSize"
            <*> _o .:? "failed"
            <*> _o .:? "canceled"
        ago = A.withArray "FrameResource" $ \_a -> FrameResource
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)


------------------------------------------------------------------------------
instance A.ToJSON FrameResource where
    toEncoding (FrameResource _0 _1 _2 _3 _4 _5 _6) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "url" .= _0
        , P.pure $ "type" .= _1
        , P.pure $ "mimeType" .= _2
        , ("lastModified" .=) <$> _3
        , ("contentSize" .=) <$> _4
        , ("failed" .=) <$> _5
        , ("canceled" .=) <$> _6
        ]
    toJSON (FrameResource _0 _1 _2 _3 _4 _5 _6) = A.object $ P.catMaybes
        [ P.pure $ "url" .= _0
        , P.pure $ "type" .= _1
        , P.pure $ "mimeType" .= _2
        , ("lastModified" .=) <$> _3
        , ("contentSize" .=) <$> _4
        , ("failed" .=) <$> _5
        , ("canceled" .=) <$> _6
        ]


------------------------------------------------------------------------------
instance P.Semigroup FrameResource where
    FrameResource _0 _1 _2 _3 _4 _5 _6 <> FrameResource _ _ _ __3 __4 __5 __6 = FrameResource _0 _1 _2 (_3 <|> __3) (_4 <|> __4) (_5 <|> __5) (_6 <|> __6)


------------------------------------------------------------------------------
-- | Information about the Frame hierarchy along with their cached resources.
{-# WARNING FrameResourceTree "This feature is marked as EXPERIMENTAL." #-}
data FrameResourceTree = FrameResourceTree
    { -- | Frame information for this tree item.
      frame :: !Frame
      -- | Child frames.
    , childFrames :: !(P.Maybe [FrameResourceTree])
      -- | Information about frame resources.
    , resources :: ![FrameResource]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON FrameResourceTree where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "FrameResourceTree" $ \_o -> FrameResourceTree
            <$> _o .: "frame"
            <*> _o .:? "childFrames"
            <*> _o .: "resources"
        ago = A.withArray "FrameResourceTree" $ \_a -> FrameResourceTree
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON FrameResourceTree where
    toEncoding (FrameResourceTree _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "frame" .= _0
        , ("childFrames" .=) <$> _1
        , P.pure $ "resources" .= _2
        ]
    toJSON (FrameResourceTree _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "frame" .= _0
        , ("childFrames" .=) <$> _1
        , P.pure $ "resources" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup FrameResourceTree where
    FrameResourceTree _0 _1 _2 <> FrameResourceTree _ __1 _ = FrameResourceTree _0 (_1 <|> __1) _2


------------------------------------------------------------------------------
-- | Information about the Frame hierarchy.
data FrameTree = FrameTree
    { -- | Frame information for this tree item.
      frame :: !Frame
      -- | Child frames.
    , childFrames :: !(P.Maybe [FrameTree])
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON FrameTree where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "FrameTree" $ \_o -> FrameTree
            <$> _o .: "frame"
            <*> _o .:? "childFrames"
        ago = A.withArray "FrameTree" $ \_a -> FrameTree
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON FrameTree where
    toEncoding (FrameTree _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "frame" .= _0
        , ("childFrames" .=) <$> _1
        ]
    toJSON (FrameTree _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "frame" .= _0
        , ("childFrames" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup FrameTree where
    FrameTree _0 _1 <> FrameTree _ __1 = FrameTree _0 (_1 <|> __1)


------------------------------------------------------------------------------
-- | Unique script identifier.
type ScriptIdentifier = T.Text


------------------------------------------------------------------------------
-- | Transition type.
data TransitionType
    = Link
    | Typed
    | AddressBar
    | AutoBookmark
    | AutoSubframe
    | ManualSubframe
    | Generated
    | AutoToplevel
    | FormSubmit
    | Reload
    | Keyword
    | KeywordGenerated
    | Other
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TransitionType where
    parseJSON = A.withText "TransitionType" $ \t -> case t of
        "link" -> P.pure Link
        "typed" -> P.pure Typed
        "address_bar" -> P.pure AddressBar
        "auto_bookmark" -> P.pure AutoBookmark
        "auto_subframe" -> P.pure AutoSubframe
        "manual_subframe" -> P.pure ManualSubframe
        "generated" -> P.pure Generated
        "auto_toplevel" -> P.pure AutoToplevel
        "form_submit" -> P.pure FormSubmit
        "reload" -> P.pure Reload
        "keyword" -> P.pure Keyword
        "keyword_generated" -> P.pure KeywordGenerated
        "other" -> P.pure Other
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON TransitionType where
    toJSON Link = "link"
    toJSON Typed = "typed"
    toJSON AddressBar = "address_bar"
    toJSON AutoBookmark = "auto_bookmark"
    toJSON AutoSubframe = "auto_subframe"
    toJSON ManualSubframe = "manual_subframe"
    toJSON Generated = "generated"
    toJSON AutoToplevel = "auto_toplevel"
    toJSON FormSubmit = "form_submit"
    toJSON Reload = "reload"
    toJSON Keyword = "keyword"
    toJSON KeywordGenerated = "keyword_generated"
    toJSON Other = "other"


------------------------------------------------------------------------------
-- | Navigation history entry.
data NavigationEntry = NavigationEntry
    { -- | Unique id of the navigation history entry.
      id :: !P.Int
      -- | URL of the navigation history entry.
    , url :: !T.Text
      -- | URL that the user typed in the url bar.
    , userTypedURL :: !T.Text
      -- | Title of the navigation history entry.
    , title :: !T.Text
      -- | Transition type.
    , transitionType :: !TransitionType
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON NavigationEntry where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "NavigationEntry" $ \_o -> NavigationEntry
            <$> _o .: "id"
            <*> _o .: "url"
            <*> _o .: "userTypedURL"
            <*> _o .: "title"
            <*> _o .: "transitionType"
        ago = A.withArray "NavigationEntry" $ \_a -> NavigationEntry
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON NavigationEntry where
    toEncoding (NavigationEntry _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "id" .= _0
        , P.pure $ "url" .= _1
        , P.pure $ "userTypedURL" .= _2
        , P.pure $ "title" .= _3
        , P.pure $ "transitionType" .= _4
        ]
    toJSON (NavigationEntry _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ P.pure $ "id" .= _0
        , P.pure $ "url" .= _1
        , P.pure $ "userTypedURL" .= _2
        , P.pure $ "title" .= _3
        , P.pure $ "transitionType" .= _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup NavigationEntry where
    NavigationEntry _0 _1 _2 _3 _4 <> NavigationEntry _ _ _ _ _ = NavigationEntry _0 _1 _2 _3 _4


------------------------------------------------------------------------------
-- | Screencast frame metadata.
{-# WARNING ScreencastFrameMetadata "This feature is marked as EXPERIMENTAL." #-}
data ScreencastFrameMetadata = ScreencastFrameMetadata
    { -- | Top offset in DIP.
      offsetTop :: !P.Double
      -- | Page scale factor.
    , pageScaleFactor :: !P.Double
      -- | Device screen width in DIP.
    , deviceWidth :: !P.Double
      -- | Device screen height in DIP.
    , deviceHeight :: !P.Double
      -- | Position of horizontal scroll in CSS pixels.
    , scrollOffsetX :: !P.Double
      -- | Position of vertical scroll in CSS pixels.
    , scrollOffsetY :: !P.Double
      -- | Frame swap timestamp.
    , timestamp :: !(P.Maybe Network.TimeSinceEpoch)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ScreencastFrameMetadata where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "ScreencastFrameMetadata" $ \_o -> ScreencastFrameMetadata
            <$> _o .: "offsetTop"
            <*> _o .: "pageScaleFactor"
            <*> _o .: "deviceWidth"
            <*> _o .: "deviceHeight"
            <*> _o .: "scrollOffsetX"
            <*> _o .: "scrollOffsetY"
            <*> _o .:? "timestamp"
        ago = A.withArray "ScreencastFrameMetadata" $ \_a -> ScreencastFrameMetadata
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)


------------------------------------------------------------------------------
instance A.ToJSON ScreencastFrameMetadata where
    toEncoding (ScreencastFrameMetadata _0 _1 _2 _3 _4 _5 _6) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "offsetTop" .= _0
        , P.pure $ "pageScaleFactor" .= _1
        , P.pure $ "deviceWidth" .= _2
        , P.pure $ "deviceHeight" .= _3
        , P.pure $ "scrollOffsetX" .= _4
        , P.pure $ "scrollOffsetY" .= _5
        , ("timestamp" .=) <$> _6
        ]
    toJSON (ScreencastFrameMetadata _0 _1 _2 _3 _4 _5 _6) = A.object $ P.catMaybes
        [ P.pure $ "offsetTop" .= _0
        , P.pure $ "pageScaleFactor" .= _1
        , P.pure $ "deviceWidth" .= _2
        , P.pure $ "deviceHeight" .= _3
        , P.pure $ "scrollOffsetX" .= _4
        , P.pure $ "scrollOffsetY" .= _5
        , ("timestamp" .=) <$> _6
        ]


------------------------------------------------------------------------------
instance P.Semigroup ScreencastFrameMetadata where
    ScreencastFrameMetadata _0 _1 _2 _3 _4 _5 _6 <> ScreencastFrameMetadata _ _ _ _ _ _ __6 = ScreencastFrameMetadata _0 _1 _2 _3 _4 _5 (_6 <|> __6)


------------------------------------------------------------------------------
-- | Javascript dialog type.
data DialogType
    = Alert
    | Confirm
    | Prompt
    | Beforeunload
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DialogType where
    parseJSON = A.withText "DialogType" $ \t -> case t of
        "alert" -> P.pure Alert
        "confirm" -> P.pure Confirm
        "prompt" -> P.pure Prompt
        "beforeunload" -> P.pure Beforeunload
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON DialogType where
    toJSON Alert = "alert"
    toJSON Confirm = "confirm"
    toJSON Prompt = "prompt"
    toJSON Beforeunload = "beforeunload"


------------------------------------------------------------------------------
-- | Error while paring app manifest.
data AppManifestError = AppManifestError
    { -- | Error message.
      message :: !T.Text
      -- | If criticial, this is a non-recoverable parse error.
    , critical :: !P.Int
      -- | Error line.
    , line :: !P.Int
      -- | Error column.
    , column :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AppManifestError where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "AppManifestError" $ \_o -> AppManifestError
            <$> _o .: "message"
            <*> _o .: "critical"
            <*> _o .: "line"
            <*> _o .: "column"
        ago = A.withArray "AppManifestError" $ \_a -> AppManifestError
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON AppManifestError where
    toEncoding (AppManifestError _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "message" .= _0
        , P.pure $ "critical" .= _1
        , P.pure $ "line" .= _2
        , P.pure $ "column" .= _3
        ]
    toJSON (AppManifestError _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "message" .= _0
        , P.pure $ "critical" .= _1
        , P.pure $ "line" .= _2
        , P.pure $ "column" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup AppManifestError where
    AppManifestError _0 _1 _2 _3 <> AppManifestError _ _ _ _ = AppManifestError _0 _1 _2 _3


------------------------------------------------------------------------------
-- | Layout viewport position and dimensions.
data LayoutViewport = LayoutViewport
    { -- | Horizontal offset relative to the document (CSS pixels).
      pageX :: !P.Int
      -- | Vertical offset relative to the document (CSS pixels).
    , pageY :: !P.Int
      -- | Width (CSS pixels), excludes scrollbar if present.
    , clientWidth :: !P.Int
      -- | Height (CSS pixels), excludes scrollbar if present.
    , clientHeight :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON LayoutViewport where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "LayoutViewport" $ \_o -> LayoutViewport
            <$> _o .: "pageX"
            <*> _o .: "pageY"
            <*> _o .: "clientWidth"
            <*> _o .: "clientHeight"
        ago = A.withArray "LayoutViewport" $ \_a -> LayoutViewport
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON LayoutViewport where
    toEncoding (LayoutViewport _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "pageX" .= _0
        , P.pure $ "pageY" .= _1
        , P.pure $ "clientWidth" .= _2
        , P.pure $ "clientHeight" .= _3
        ]
    toJSON (LayoutViewport _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "pageX" .= _0
        , P.pure $ "pageY" .= _1
        , P.pure $ "clientWidth" .= _2
        , P.pure $ "clientHeight" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup LayoutViewport where
    LayoutViewport _0 _1 _2 _3 <> LayoutViewport _ _ _ _ = LayoutViewport _0 _1 _2 _3


------------------------------------------------------------------------------
-- | Visual viewport position, dimensions, and scale.
data VisualViewport = VisualViewport
    { -- | Horizontal offset relative to the layout viewport (CSS pixels).
      offsetX :: !P.Double
      -- | Vertical offset relative to the layout viewport (CSS pixels).
    , offsetY :: !P.Double
      -- | Horizontal offset relative to the document (CSS pixels).
    , pageX :: !P.Double
      -- | Vertical offset relative to the document (CSS pixels).
    , pageY :: !P.Double
      -- | Width (CSS pixels), excludes scrollbar if present.
    , clientWidth :: !P.Double
      -- | Height (CSS pixels), excludes scrollbar if present.
    , clientHeight :: !P.Double
      -- | Scale relative to the ideal viewport (size at width=device-width).
    , scale :: !P.Double
      -- | Page zoom factor (CSS to device independent pixels ratio).
    , zoom :: !(P.Maybe P.Double)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON VisualViewport where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "VisualViewport" $ \_o -> VisualViewport
            <$> _o .: "offsetX"
            <*> _o .: "offsetY"
            <*> _o .: "pageX"
            <*> _o .: "pageY"
            <*> _o .: "clientWidth"
            <*> _o .: "clientHeight"
            <*> _o .: "scale"
            <*> _o .:? "zoom"
        ago = A.withArray "VisualViewport" $ \_a -> VisualViewport
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)
            <*> P.maybe P.empty A.parseJSON (_a !? 5)
            <*> P.maybe P.empty A.parseJSON (_a !? 6)
            <*> P.traverse A.parseJSON (_a !? 7)


------------------------------------------------------------------------------
instance A.ToJSON VisualViewport where
    toEncoding (VisualViewport _0 _1 _2 _3 _4 _5 _6 _7) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "offsetX" .= _0
        , P.pure $ "offsetY" .= _1
        , P.pure $ "pageX" .= _2
        , P.pure $ "pageY" .= _3
        , P.pure $ "clientWidth" .= _4
        , P.pure $ "clientHeight" .= _5
        , P.pure $ "scale" .= _6
        , ("zoom" .=) <$> _7
        ]
    toJSON (VisualViewport _0 _1 _2 _3 _4 _5 _6 _7) = A.object $ P.catMaybes
        [ P.pure $ "offsetX" .= _0
        , P.pure $ "offsetY" .= _1
        , P.pure $ "pageX" .= _2
        , P.pure $ "pageY" .= _3
        , P.pure $ "clientWidth" .= _4
        , P.pure $ "clientHeight" .= _5
        , P.pure $ "scale" .= _6
        , ("zoom" .=) <$> _7
        ]


------------------------------------------------------------------------------
instance P.Semigroup VisualViewport where
    VisualViewport _0 _1 _2 _3 _4 _5 _6 _7 <> VisualViewport _ _ _ _ _ _ _ __7 = VisualViewport _0 _1 _2 _3 _4 _5 _6 (_7 <|> __7)


------------------------------------------------------------------------------
-- | Viewport for capturing screenshot.
data Viewport = Viewport
    { -- | X offset in device independent pixels (dip).
      x :: !P.Double
      -- | Y offset in device independent pixels (dip).
    , y :: !P.Double
      -- | Rectangle width in device independent pixels (dip).
    , width :: !P.Double
      -- | Rectangle height in device independent pixels (dip).
    , height :: !P.Double
      -- | Page scale factor.
    , scale :: !P.Double
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Viewport where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "Viewport" $ \_o -> Viewport
            <$> _o .: "x"
            <*> _o .: "y"
            <*> _o .: "width"
            <*> _o .: "height"
            <*> _o .: "scale"
        ago = A.withArray "Viewport" $ \_a -> Viewport
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON Viewport where
    toEncoding (Viewport _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "x" .= _0
        , P.pure $ "y" .= _1
        , P.pure $ "width" .= _2
        , P.pure $ "height" .= _3
        , P.pure $ "scale" .= _4
        ]
    toJSON (Viewport _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ P.pure $ "x" .= _0
        , P.pure $ "y" .= _1
        , P.pure $ "width" .= _2
        , P.pure $ "height" .= _3
        , P.pure $ "scale" .= _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup Viewport where
    Viewport _0 _1 _2 _3 _4 <> Viewport _ _ _ _ _ = Viewport _0 _1 _2 _3 _4


------------------------------------------------------------------------------
-- | Generic font families collection.
{-# WARNING FontFamilies "This feature is marked as EXPERIMENTAL." #-}
data FontFamilies = FontFamilies
    { -- | The standard font-family.
      standard :: !(P.Maybe T.Text)
      -- | The fixed font-family.
    , fixed :: !(P.Maybe T.Text)
      -- | The serif font-family.
    , serif :: !(P.Maybe T.Text)
      -- | The sansSerif font-family.
    , sansSerif :: !(P.Maybe T.Text)
      -- | The cursive font-family.
    , cursive :: !(P.Maybe T.Text)
      -- | The fantasy font-family.
    , fantasy :: !(P.Maybe T.Text)
      -- | The pictograph font-family.
    , pictograph :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON FontFamilies where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "FontFamilies" $ \_o -> FontFamilies
            <$> _o .:? "standard"
            <*> _o .:? "fixed"
            <*> _o .:? "serif"
            <*> _o .:? "sansSerif"
            <*> _o .:? "cursive"
            <*> _o .:? "fantasy"
            <*> _o .:? "pictograph"
        ago = A.withArray "FontFamilies" $ \_a -> FontFamilies
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)


------------------------------------------------------------------------------
instance A.ToJSON FontFamilies where
    toEncoding (FontFamilies _0 _1 _2 _3 _4 _5 _6) = A.pairs $ P.fold $ P.catMaybes
        [ ("standard" .=) <$> _0
        , ("fixed" .=) <$> _1
        , ("serif" .=) <$> _2
        , ("sansSerif" .=) <$> _3
        , ("cursive" .=) <$> _4
        , ("fantasy" .=) <$> _5
        , ("pictograph" .=) <$> _6
        ]
    toJSON (FontFamilies _0 _1 _2 _3 _4 _5 _6) = A.object $ P.catMaybes
        [ ("standard" .=) <$> _0
        , ("fixed" .=) <$> _1
        , ("serif" .=) <$> _2
        , ("sansSerif" .=) <$> _3
        , ("cursive" .=) <$> _4
        , ("fantasy" .=) <$> _5
        , ("pictograph" .=) <$> _6
        ]


------------------------------------------------------------------------------
instance P.Semigroup FontFamilies where
    FontFamilies _0 _1 _2 _3 _4 _5 _6 <> FontFamilies __0 __1 __2 __3 __4 __5 __6 = FontFamilies (_0 <|> __0) (_1 <|> __1) (_2 <|> __2) (_3 <|> __3) (_4 <|> __4) (_5 <|> __5) (_6 <|> __6)


------------------------------------------------------------------------------
instance P.Monoid FontFamilies where
    mempty = FontFamilies P.empty P.empty P.empty P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Default font sizes.
{-# WARNING FontSizes "This feature is marked as EXPERIMENTAL." #-}
data FontSizes = FontSizes
    { -- | Default standard font size.
      standard :: !(P.Maybe P.Int)
      -- | Default fixed font size.
    , fixed :: !(P.Maybe P.Int)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON FontSizes where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "FontSizes" $ \_o -> FontSizes
            <$> _o .:? "standard"
            <*> _o .:? "fixed"
        ago = A.withArray "FontSizes" $ \_a -> FontSizes
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON FontSizes where
    toEncoding (FontSizes _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ ("standard" .=) <$> _0
        , ("fixed" .=) <$> _1
        ]
    toJSON (FontSizes _0 _1) = A.object $ P.catMaybes
        [ ("standard" .=) <$> _0
        , ("fixed" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup FontSizes where
    FontSizes _0 _1 <> FontSizes __0 __1 = FontSizes (_0 <|> __0) (_1 <|> __1)


------------------------------------------------------------------------------
instance P.Monoid FontSizes where
    mempty = FontSizes P.empty P.empty


------------------------------------------------------------------------------
{-# WARNING ClientNavigationReason "This feature is marked as EXPERIMENTAL." #-}
data ClientNavigationReason
    = FormSubmissionGet
    | FormSubmissionPost
    | HttpHeaderRefresh
    | ScriptInitiated
    | MetaTagRefresh
    | PageBlockInterstitial
    | Reload_
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ClientNavigationReason where
    parseJSON = A.withText "ClientNavigationReason" $ \t -> case t of
        "formSubmissionGet" -> P.pure FormSubmissionGet
        "formSubmissionPost" -> P.pure FormSubmissionPost
        "httpHeaderRefresh" -> P.pure HttpHeaderRefresh
        "scriptInitiated" -> P.pure ScriptInitiated
        "metaTagRefresh" -> P.pure MetaTagRefresh
        "pageBlockInterstitial" -> P.pure PageBlockInterstitial
        "reload" -> P.pure Reload_
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON ClientNavigationReason where
    toJSON FormSubmissionGet = "formSubmissionGet"
    toJSON FormSubmissionPost = "formSubmissionPost"
    toJSON HttpHeaderRefresh = "httpHeaderRefresh"
    toJSON ScriptInitiated = "scriptInitiated"
    toJSON MetaTagRefresh = "metaTagRefresh"
    toJSON PageBlockInterstitial = "pageBlockInterstitial"
    toJSON Reload_ = "reload"

