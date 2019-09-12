{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Actions and events related to the inspected page belong to the page domain.
module DevTools.API.Page
    ( module DevTools.API.Page.Types
    , module DevTools.API.Page
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
import qualified DevTools.API.Debugger.Types as Debugger
import qualified DevTools.API.Emulation.Types as Emulation
import qualified DevTools.API.IO.Types as IO
import qualified DevTools.API.Network.Types as Network
import           DevTools.API.Page.Types
import qualified DevTools.API.Runtime.Types as Runtime


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Deprecated, please use addScriptToEvaluateOnNewDocument instead.
{-# DEPRECATED AddScriptToEvaluateOnLoad "This may be removed in a future release." #-}
{-{-# WARNING AddScriptToEvaluateOnLoad "This feature is marked as EXPERIMENTAL." #-}-}
data AddScriptToEvaluateOnLoad = AddScriptToEvaluateOnLoad
    { scriptSource :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AddScriptToEvaluateOnLoad where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "addScriptToEvaluateOnLoad" $ \_o -> AddScriptToEvaluateOnLoad
            <$> _o .: "scriptSource"
        ago = A.withArray "addScriptToEvaluateOnLoad" $ \_a -> AddScriptToEvaluateOnLoad
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON AddScriptToEvaluateOnLoad where
    toEncoding (AddScriptToEvaluateOnLoad _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "scriptSource" .= _0
        ]
    toJSON (AddScriptToEvaluateOnLoad _0) = A.object $ P.catMaybes
        [ P.pure $ "scriptSource" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup AddScriptToEvaluateOnLoad where
    AddScriptToEvaluateOnLoad _0 <> AddScriptToEvaluateOnLoad _ = AddScriptToEvaluateOnLoad _0


------------------------------------------------------------------------------
-- | Deprecated, please use addScriptToEvaluateOnNewDocument instead.
{-# DEPRECATED AddScriptToEvaluateOnLoadResult "This may be removed in a future release." #-}
{-{-# WARNING AddScriptToEvaluateOnLoadResult "This feature is marked as EXPERIMENTAL." #-}-}
data AddScriptToEvaluateOnLoadResult = AddScriptToEvaluateOnLoadResult
    { -- | Identifier of the added script.
      identifier :: !ScriptIdentifier
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AddScriptToEvaluateOnLoadResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "addScriptToEvaluateOnLoadResult" $ \_o -> AddScriptToEvaluateOnLoadResult
            <$> _o .: "identifier"
        ago = A.withArray "addScriptToEvaluateOnLoadResult" $ \_a -> AddScriptToEvaluateOnLoadResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON AddScriptToEvaluateOnLoadResult where
    toEncoding (AddScriptToEvaluateOnLoadResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "identifier" .= _0
        ]
    toJSON (AddScriptToEvaluateOnLoadResult _0) = A.object $ P.catMaybes
        [ P.pure $ "identifier" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup AddScriptToEvaluateOnLoadResult where
    AddScriptToEvaluateOnLoadResult _0 <> AddScriptToEvaluateOnLoadResult _ = AddScriptToEvaluateOnLoadResult _0


------------------------------------------------------------------------------
instance M.Method AddScriptToEvaluateOnLoad where
    type Result AddScriptToEvaluateOnLoad = AddScriptToEvaluateOnLoadResult
    name _ = "Page.addScriptToEvaluateOnLoad"


------------------------------------------------------------------------------
-- | Deprecated, please use addScriptToEvaluateOnNewDocument instead.
{-# DEPRECATED addScriptToEvaluateOnLoad "This may be removed in a future release." #-}
{-{-# WARNING addScriptToEvaluateOnLoad "This feature is marked as EXPERIMENTAL." #-}-}
addScriptToEvaluateOnLoad
    :: T.Text
    -> AddScriptToEvaluateOnLoad
addScriptToEvaluateOnLoad _0 = AddScriptToEvaluateOnLoad _0


------------------------------------------------------------------------------
-- | Evaluates given script in every frame upon creation (before loading frame's scripts).
{-# WARNING worldName "This feature is marked as EXPERIMENTAL." #-}
data AddScriptToEvaluateOnNewDocument = AddScriptToEvaluateOnNewDocument
    { source :: !T.Text
      -- | If specified, creates an isolated world with the given name and evaluates given script in it.
      -- This world name will be used as the ExecutionContextDescription::name when the corresponding
      -- event is emitted.
    , worldName :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AddScriptToEvaluateOnNewDocument where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "addScriptToEvaluateOnNewDocument" $ \_o -> AddScriptToEvaluateOnNewDocument
            <$> _o .: "source"
            <*> _o .:? "worldName"
        ago = A.withArray "addScriptToEvaluateOnNewDocument" $ \_a -> AddScriptToEvaluateOnNewDocument
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON AddScriptToEvaluateOnNewDocument where
    toEncoding (AddScriptToEvaluateOnNewDocument _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "source" .= _0
        , ("worldName" .=) <$> _1
        ]
    toJSON (AddScriptToEvaluateOnNewDocument _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "source" .= _0
        , ("worldName" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup AddScriptToEvaluateOnNewDocument where
    AddScriptToEvaluateOnNewDocument _0 _1 <> AddScriptToEvaluateOnNewDocument _ __1 = AddScriptToEvaluateOnNewDocument _0 (_1 <|> __1)


------------------------------------------------------------------------------
-- | Evaluates given script in every frame upon creation (before loading frame's scripts).
data AddScriptToEvaluateOnNewDocumentResult = AddScriptToEvaluateOnNewDocumentResult
    { -- | Identifier of the added script.
      identifier :: !ScriptIdentifier
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AddScriptToEvaluateOnNewDocumentResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "addScriptToEvaluateOnNewDocumentResult" $ \_o -> AddScriptToEvaluateOnNewDocumentResult
            <$> _o .: "identifier"
        ago = A.withArray "addScriptToEvaluateOnNewDocumentResult" $ \_a -> AddScriptToEvaluateOnNewDocumentResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON AddScriptToEvaluateOnNewDocumentResult where
    toEncoding (AddScriptToEvaluateOnNewDocumentResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "identifier" .= _0
        ]
    toJSON (AddScriptToEvaluateOnNewDocumentResult _0) = A.object $ P.catMaybes
        [ P.pure $ "identifier" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup AddScriptToEvaluateOnNewDocumentResult where
    AddScriptToEvaluateOnNewDocumentResult _0 <> AddScriptToEvaluateOnNewDocumentResult _ = AddScriptToEvaluateOnNewDocumentResult _0


------------------------------------------------------------------------------
instance M.Method AddScriptToEvaluateOnNewDocument where
    type Result AddScriptToEvaluateOnNewDocument = AddScriptToEvaluateOnNewDocumentResult
    name _ = "Page.addScriptToEvaluateOnNewDocument"


------------------------------------------------------------------------------
-- | Evaluates given script in every frame upon creation (before loading frame's scripts).
addScriptToEvaluateOnNewDocument
    :: T.Text
    -> AddScriptToEvaluateOnNewDocument
addScriptToEvaluateOnNewDocument _0 = AddScriptToEvaluateOnNewDocument _0 P.empty


------------------------------------------------------------------------------
-- | Brings page to front (activates tab).
data BringToFront = BringToFront
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON BringToFront where
    parseJSON A.Null = P.pure BringToFront
    parseJSON v = A.withArray "bringToFront" go v
        <|> A.withObject "bringToFront" go v
      where
        go _ = P.pure BringToFront


------------------------------------------------------------------------------
instance A.ToJSON BringToFront where
    toEncoding BringToFront = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON BringToFront = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup BringToFront where
    BringToFront <> BringToFront = BringToFront


------------------------------------------------------------------------------
instance P.Monoid BringToFront where
    mempty = BringToFront


------------------------------------------------------------------------------
instance M.Method BringToFront where
    type Result BringToFront = ()
    name _ = "Page.bringToFront"


------------------------------------------------------------------------------
-- | Brings page to front (activates tab).
bringToFront
    :: BringToFront
bringToFront = BringToFront


------------------------------------------------------------------------------
-- | Capture page screenshot.
{-# WARNING fromSurface "This feature is marked as EXPERIMENTAL." #-}
data CaptureScreenshot = CaptureScreenshot
    { -- | Image compression format (defaults to png).
      format :: !(P.Maybe Format)
      -- | Compression quality from range [0..100] (jpeg only).
    , quality :: !(P.Maybe P.Int)
      -- | Capture the screenshot of a given region only.
    , clip :: !(P.Maybe Viewport)
      -- | Capture the screenshot from the surface, rather than the view. Defaults to true.
    , fromSurface :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CaptureScreenshot where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "captureScreenshot" $ \_o -> CaptureScreenshot
            <$> _o .:? "format"
            <*> _o .:? "quality"
            <*> _o .:? "clip"
            <*> _o .:? "fromSurface"
        ago = A.withArray "captureScreenshot" $ \_a -> CaptureScreenshot
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON CaptureScreenshot where
    toEncoding (CaptureScreenshot _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ ("format" .=) <$> _0
        , ("quality" .=) <$> _1
        , ("clip" .=) <$> _2
        , ("fromSurface" .=) <$> _3
        ]
    toJSON (CaptureScreenshot _0 _1 _2 _3) = A.object $ P.catMaybes
        [ ("format" .=) <$> _0
        , ("quality" .=) <$> _1
        , ("clip" .=) <$> _2
        , ("fromSurface" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup CaptureScreenshot where
    CaptureScreenshot _0 _1 _2 _3 <> CaptureScreenshot __0 __1 __2 __3 = CaptureScreenshot (_0 <|> __0) (_1 <|> __1) (_2 <|> __2) (_3 <|> __3)


------------------------------------------------------------------------------
instance P.Monoid CaptureScreenshot where
    mempty = CaptureScreenshot P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
data Format
    = Jpeg
    | Png
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Format where
    parseJSON = A.withText "Format" $ \t -> case t of
        "jpeg" -> P.pure Jpeg
        "png" -> P.pure Png
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Format where
    toJSON Jpeg = "jpeg"
    toJSON Png = "png"


------------------------------------------------------------------------------
-- | Capture page screenshot.
data CaptureScreenshotResult = CaptureScreenshotResult
    { -- | Base64-encoded image data.
      data_ :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CaptureScreenshotResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "captureScreenshotResult" $ \_o -> CaptureScreenshotResult
            <$> _o .: "data"
        ago = A.withArray "captureScreenshotResult" $ \_a -> CaptureScreenshotResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON CaptureScreenshotResult where
    toEncoding (CaptureScreenshotResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "data" .= _0
        ]
    toJSON (CaptureScreenshotResult _0) = A.object $ P.catMaybes
        [ P.pure $ "data" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup CaptureScreenshotResult where
    CaptureScreenshotResult _0 <> CaptureScreenshotResult _ = CaptureScreenshotResult _0


------------------------------------------------------------------------------
instance M.Method CaptureScreenshot where
    type Result CaptureScreenshot = CaptureScreenshotResult
    name _ = "Page.captureScreenshot"


------------------------------------------------------------------------------
-- | Capture page screenshot.
captureScreenshot
    :: CaptureScreenshot
captureScreenshot = CaptureScreenshot P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Returns a snapshot of the page as a string. For MHTML format, the serialization includes
-- iframes, shadow DOM, external resources, and element-inline styles.
{-# WARNING CaptureSnapshot "This feature is marked as EXPERIMENTAL." #-}
data CaptureSnapshot = CaptureSnapshot
    { -- | Format (defaults to mhtml).
      format :: !(P.Maybe Format_)
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
            <$> _o .:? "format"
        ago = A.withArray "captureSnapshot" $ \_a -> CaptureSnapshot
            <$> P.traverse A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON CaptureSnapshot where
    toEncoding (CaptureSnapshot _0) = A.pairs $ P.fold $ P.catMaybes
        [ ("format" .=) <$> _0
        ]
    toJSON (CaptureSnapshot _0) = A.object $ P.catMaybes
        [ ("format" .=) <$> _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup CaptureSnapshot where
    CaptureSnapshot _0 <> CaptureSnapshot __0 = CaptureSnapshot (_0 <|> __0)


------------------------------------------------------------------------------
instance P.Monoid CaptureSnapshot where
    mempty = CaptureSnapshot P.empty


------------------------------------------------------------------------------
data Format_
    = Mhtml
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Format_ where
    parseJSON = A.withText "Format" $ \t -> case t of
        "mhtml" -> P.pure Mhtml
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Format_ where
    toJSON Mhtml = "mhtml"


------------------------------------------------------------------------------
-- | Returns a snapshot of the page as a string. For MHTML format, the serialization includes
-- iframes, shadow DOM, external resources, and element-inline styles.
{-# WARNING CaptureSnapshotResult "This feature is marked as EXPERIMENTAL." #-}
data CaptureSnapshotResult = CaptureSnapshotResult
    { -- | Serialized page data.
      data_ :: !T.Text
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
            <$> _o .: "data"
        ago = A.withArray "captureSnapshotResult" $ \_a -> CaptureSnapshotResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON CaptureSnapshotResult where
    toEncoding (CaptureSnapshotResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "data" .= _0
        ]
    toJSON (CaptureSnapshotResult _0) = A.object $ P.catMaybes
        [ P.pure $ "data" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup CaptureSnapshotResult where
    CaptureSnapshotResult _0 <> CaptureSnapshotResult _ = CaptureSnapshotResult _0


------------------------------------------------------------------------------
instance M.Method CaptureSnapshot where
    type Result CaptureSnapshot = CaptureSnapshotResult
    name _ = "Page.captureSnapshot"


------------------------------------------------------------------------------
-- | Returns a snapshot of the page as a string. For MHTML format, the serialization includes
-- iframes, shadow DOM, external resources, and element-inline styles.
{-# WARNING captureSnapshot "This feature is marked as EXPERIMENTAL." #-}
captureSnapshot
    :: CaptureSnapshot
captureSnapshot = CaptureSnapshot P.empty


------------------------------------------------------------------------------
-- | Clears the overriden device metrics.
{-# DEPRECATED ClearDeviceMetricsOverride "This may be removed in a future release." #-}
{-{-# WARNING ClearDeviceMetricsOverride "This feature is marked as EXPERIMENTAL." #-}-}
data ClearDeviceMetricsOverride = ClearDeviceMetricsOverride
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ClearDeviceMetricsOverride where
    parseJSON A.Null = P.pure ClearDeviceMetricsOverride
    parseJSON v = A.withArray "clearDeviceMetricsOverride" go v
        <|> A.withObject "clearDeviceMetricsOverride" go v
      where
        go _ = P.pure ClearDeviceMetricsOverride


------------------------------------------------------------------------------
instance A.ToJSON ClearDeviceMetricsOverride where
    toEncoding ClearDeviceMetricsOverride = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON ClearDeviceMetricsOverride = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup ClearDeviceMetricsOverride where
    ClearDeviceMetricsOverride <> ClearDeviceMetricsOverride = ClearDeviceMetricsOverride


------------------------------------------------------------------------------
instance P.Monoid ClearDeviceMetricsOverride where
    mempty = ClearDeviceMetricsOverride


------------------------------------------------------------------------------
instance M.Method ClearDeviceMetricsOverride where
    type Result ClearDeviceMetricsOverride = ()
    name _ = "Page.clearDeviceMetricsOverride"


------------------------------------------------------------------------------
-- | Clears the overriden device metrics.
{-# DEPRECATED clearDeviceMetricsOverride "This may be removed in a future release." #-}
{-{-# WARNING clearDeviceMetricsOverride "This feature is marked as EXPERIMENTAL." #-}-}
clearDeviceMetricsOverride
    :: ClearDeviceMetricsOverride
clearDeviceMetricsOverride = ClearDeviceMetricsOverride


------------------------------------------------------------------------------
-- | Clears the overridden Device Orientation.
{-# DEPRECATED ClearDeviceOrientationOverride "This may be removed in a future release." #-}
{-{-# WARNING ClearDeviceOrientationOverride "This feature is marked as EXPERIMENTAL." #-}-}
data ClearDeviceOrientationOverride = ClearDeviceOrientationOverride
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ClearDeviceOrientationOverride where
    parseJSON A.Null = P.pure ClearDeviceOrientationOverride
    parseJSON v = A.withArray "clearDeviceOrientationOverride" go v
        <|> A.withObject "clearDeviceOrientationOverride" go v
      where
        go _ = P.pure ClearDeviceOrientationOverride


------------------------------------------------------------------------------
instance A.ToJSON ClearDeviceOrientationOverride where
    toEncoding ClearDeviceOrientationOverride = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON ClearDeviceOrientationOverride = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup ClearDeviceOrientationOverride where
    ClearDeviceOrientationOverride <> ClearDeviceOrientationOverride = ClearDeviceOrientationOverride


------------------------------------------------------------------------------
instance P.Monoid ClearDeviceOrientationOverride where
    mempty = ClearDeviceOrientationOverride


------------------------------------------------------------------------------
instance M.Method ClearDeviceOrientationOverride where
    type Result ClearDeviceOrientationOverride = ()
    name _ = "Page.clearDeviceOrientationOverride"


------------------------------------------------------------------------------
-- | Clears the overridden Device Orientation.
{-# DEPRECATED clearDeviceOrientationOverride "This may be removed in a future release." #-}
{-{-# WARNING clearDeviceOrientationOverride "This feature is marked as EXPERIMENTAL." #-}-}
clearDeviceOrientationOverride
    :: ClearDeviceOrientationOverride
clearDeviceOrientationOverride = ClearDeviceOrientationOverride


------------------------------------------------------------------------------
-- | Clears the overriden Geolocation Position and Error.
{-# DEPRECATED ClearGeolocationOverride "This may be removed in a future release." #-}
data ClearGeolocationOverride = ClearGeolocationOverride
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ClearGeolocationOverride where
    parseJSON A.Null = P.pure ClearGeolocationOverride
    parseJSON v = A.withArray "clearGeolocationOverride" go v
        <|> A.withObject "clearGeolocationOverride" go v
      where
        go _ = P.pure ClearGeolocationOverride


------------------------------------------------------------------------------
instance A.ToJSON ClearGeolocationOverride where
    toEncoding ClearGeolocationOverride = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON ClearGeolocationOverride = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup ClearGeolocationOverride where
    ClearGeolocationOverride <> ClearGeolocationOverride = ClearGeolocationOverride


------------------------------------------------------------------------------
instance P.Monoid ClearGeolocationOverride where
    mempty = ClearGeolocationOverride


------------------------------------------------------------------------------
instance M.Method ClearGeolocationOverride where
    type Result ClearGeolocationOverride = ()
    name _ = "Page.clearGeolocationOverride"


------------------------------------------------------------------------------
-- | Clears the overriden Geolocation Position and Error.
{-# DEPRECATED clearGeolocationOverride "This may be removed in a future release." #-}
clearGeolocationOverride
    :: ClearGeolocationOverride
clearGeolocationOverride = ClearGeolocationOverride


------------------------------------------------------------------------------
-- | Creates an isolated world for the given frame.
data CreateIsolatedWorld = CreateIsolatedWorld
    { -- | Id of the frame in which the isolated world should be created.
      frameId :: !FrameId
      -- | An optional name which is reported in the Execution Context.
    , worldName :: !(P.Maybe T.Text)
      -- | Whether or not universal access should be granted to the isolated world. This is a powerful
      -- option, use with caution.
    , grantUniveralAccess :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CreateIsolatedWorld where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "createIsolatedWorld" $ \_o -> CreateIsolatedWorld
            <$> _o .: "frameId"
            <*> _o .:? "worldName"
            <*> _o .:? "grantUniveralAccess"
        ago = A.withArray "createIsolatedWorld" $ \_a -> CreateIsolatedWorld
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON CreateIsolatedWorld where
    toEncoding (CreateIsolatedWorld _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        , ("worldName" .=) <$> _1
        , ("grantUniveralAccess" .=) <$> _2
        ]
    toJSON (CreateIsolatedWorld _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        , ("worldName" .=) <$> _1
        , ("grantUniveralAccess" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup CreateIsolatedWorld where
    CreateIsolatedWorld _0 _1 _2 <> CreateIsolatedWorld _ __1 __2 = CreateIsolatedWorld _0 (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
-- | Creates an isolated world for the given frame.
data CreateIsolatedWorldResult = CreateIsolatedWorldResult
    { -- | Execution context of the isolated world.
      executionContextId :: !Runtime.ExecutionContextId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CreateIsolatedWorldResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "createIsolatedWorldResult" $ \_o -> CreateIsolatedWorldResult
            <$> _o .: "executionContextId"
        ago = A.withArray "createIsolatedWorldResult" $ \_a -> CreateIsolatedWorldResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON CreateIsolatedWorldResult where
    toEncoding (CreateIsolatedWorldResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "executionContextId" .= _0
        ]
    toJSON (CreateIsolatedWorldResult _0) = A.object $ P.catMaybes
        [ P.pure $ "executionContextId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup CreateIsolatedWorldResult where
    CreateIsolatedWorldResult _0 <> CreateIsolatedWorldResult _ = CreateIsolatedWorldResult _0


------------------------------------------------------------------------------
instance M.Method CreateIsolatedWorld where
    type Result CreateIsolatedWorld = CreateIsolatedWorldResult
    name _ = "Page.createIsolatedWorld"


------------------------------------------------------------------------------
-- | Creates an isolated world for the given frame.
createIsolatedWorld
    :: FrameId
    -- ^ Id of the frame in which the isolated world should be created.

    -> CreateIsolatedWorld
createIsolatedWorld _0 = CreateIsolatedWorld _0 P.empty P.empty


------------------------------------------------------------------------------
-- | Deletes browser cookie with given name, domain and path.
{-# DEPRECATED DeleteCookie "This may be removed in a future release." #-}
{-{-# WARNING DeleteCookie "This feature is marked as EXPERIMENTAL." #-}-}
data DeleteCookie = DeleteCookie
    { -- | Name of the cookie to remove.
      cookieName :: !T.Text
      -- | URL to match cooke domain and path.
    , url :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DeleteCookie where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "deleteCookie" $ \_o -> DeleteCookie
            <$> _o .: "cookieName"
            <*> _o .: "url"
        ago = A.withArray "deleteCookie" $ \_a -> DeleteCookie
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON DeleteCookie where
    toEncoding (DeleteCookie _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "cookieName" .= _0
        , P.pure $ "url" .= _1
        ]
    toJSON (DeleteCookie _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "cookieName" .= _0
        , P.pure $ "url" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup DeleteCookie where
    DeleteCookie _0 _1 <> DeleteCookie _ _ = DeleteCookie _0 _1


------------------------------------------------------------------------------
instance M.Method DeleteCookie where
    type Result DeleteCookie = ()
    name _ = "Page.deleteCookie"


------------------------------------------------------------------------------
-- | Deletes browser cookie with given name, domain and path.
{-# DEPRECATED deleteCookie "This may be removed in a future release." #-}
{-{-# WARNING deleteCookie "This feature is marked as EXPERIMENTAL." #-}-}
deleteCookie
    :: T.Text
    -- ^ Name of the cookie to remove.

    -> T.Text
    -- ^ URL to match cooke domain and path.

    -> DeleteCookie
deleteCookie _0 _1 = DeleteCookie _0 _1


------------------------------------------------------------------------------
-- | Disables page domain notifications.
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
    name _ = "Page.disable"


------------------------------------------------------------------------------
-- | Disables page domain notifications.
disable
    :: Disable
disable = Disable


------------------------------------------------------------------------------
-- | Enables page domain notifications.
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
    name _ = "Page.enable"


------------------------------------------------------------------------------
-- | Enables page domain notifications.
enable
    :: Enable
enable = Enable


------------------------------------------------------------------------------
data GetAppManifest = GetAppManifest
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetAppManifest where
    parseJSON A.Null = P.pure GetAppManifest
    parseJSON v = A.withArray "getAppManifest" go v
        <|> A.withObject "getAppManifest" go v
      where
        go _ = P.pure GetAppManifest


------------------------------------------------------------------------------
instance A.ToJSON GetAppManifest where
    toEncoding GetAppManifest = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetAppManifest = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetAppManifest where
    GetAppManifest <> GetAppManifest = GetAppManifest


------------------------------------------------------------------------------
instance P.Monoid GetAppManifest where
    mempty = GetAppManifest


------------------------------------------------------------------------------
data GetAppManifestResult = GetAppManifestResult
    { -- | Manifest location.
      url :: !T.Text
    , errors :: ![AppManifestError]
      -- | Manifest content.
    , data_ :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetAppManifestResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getAppManifestResult" $ \_o -> GetAppManifestResult
            <$> _o .: "url"
            <*> _o .: "errors"
            <*> _o .:? "data"
        ago = A.withArray "getAppManifestResult" $ \_a -> GetAppManifestResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON GetAppManifestResult where
    toEncoding (GetAppManifestResult _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "url" .= _0
        , P.pure $ "errors" .= _1
        , ("data" .=) <$> _2
        ]
    toJSON (GetAppManifestResult _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "url" .= _0
        , P.pure $ "errors" .= _1
        , ("data" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetAppManifestResult where
    GetAppManifestResult _0 _1 _2 <> GetAppManifestResult _ _ __2 = GetAppManifestResult _0 _1 (_2 <|> __2)


------------------------------------------------------------------------------
instance M.Method GetAppManifest where
    type Result GetAppManifest = GetAppManifestResult
    name _ = "Page.getAppManifest"


------------------------------------------------------------------------------
getAppManifest
    :: GetAppManifest
getAppManifest = GetAppManifest


------------------------------------------------------------------------------
{-# WARNING GetInstallabilityErrors "This feature is marked as EXPERIMENTAL." #-}
data GetInstallabilityErrors = GetInstallabilityErrors
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetInstallabilityErrors where
    parseJSON A.Null = P.pure GetInstallabilityErrors
    parseJSON v = A.withArray "getInstallabilityErrors" go v
        <|> A.withObject "getInstallabilityErrors" go v
      where
        go _ = P.pure GetInstallabilityErrors


------------------------------------------------------------------------------
instance A.ToJSON GetInstallabilityErrors where
    toEncoding GetInstallabilityErrors = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetInstallabilityErrors = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetInstallabilityErrors where
    GetInstallabilityErrors <> GetInstallabilityErrors = GetInstallabilityErrors


------------------------------------------------------------------------------
instance P.Monoid GetInstallabilityErrors where
    mempty = GetInstallabilityErrors


------------------------------------------------------------------------------
{-# WARNING GetInstallabilityErrorsResult "This feature is marked as EXPERIMENTAL." #-}
data GetInstallabilityErrorsResult = GetInstallabilityErrorsResult
    { errors :: ![T.Text]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetInstallabilityErrorsResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getInstallabilityErrorsResult" $ \_o -> GetInstallabilityErrorsResult
            <$> _o .: "errors"
        ago = A.withArray "getInstallabilityErrorsResult" $ \_a -> GetInstallabilityErrorsResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetInstallabilityErrorsResult where
    toEncoding (GetInstallabilityErrorsResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "errors" .= _0
        ]
    toJSON (GetInstallabilityErrorsResult _0) = A.object $ P.catMaybes
        [ P.pure $ "errors" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetInstallabilityErrorsResult where
    GetInstallabilityErrorsResult _0 <> GetInstallabilityErrorsResult _ = GetInstallabilityErrorsResult _0


------------------------------------------------------------------------------
instance M.Method GetInstallabilityErrors where
    type Result GetInstallabilityErrors = GetInstallabilityErrorsResult
    name _ = "Page.getInstallabilityErrors"


------------------------------------------------------------------------------
{-# WARNING getInstallabilityErrors "This feature is marked as EXPERIMENTAL." #-}
getInstallabilityErrors
    :: GetInstallabilityErrors
getInstallabilityErrors = GetInstallabilityErrors


------------------------------------------------------------------------------
-- | Returns all browser cookies. Depending on the backend support, will return detailed cookie
-- information in the @cookies@ field.
{-# DEPRECATED GetCookies "This may be removed in a future release." #-}
{-{-# WARNING GetCookies "This feature is marked as EXPERIMENTAL." #-}-}
data GetCookies = GetCookies
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetCookies where
    parseJSON A.Null = P.pure GetCookies
    parseJSON v = A.withArray "getCookies" go v
        <|> A.withObject "getCookies" go v
      where
        go _ = P.pure GetCookies


------------------------------------------------------------------------------
instance A.ToJSON GetCookies where
    toEncoding GetCookies = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetCookies = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetCookies where
    GetCookies <> GetCookies = GetCookies


------------------------------------------------------------------------------
instance P.Monoid GetCookies where
    mempty = GetCookies


------------------------------------------------------------------------------
-- | Returns all browser cookies. Depending on the backend support, will return detailed cookie
-- information in the @cookies@ field.
{-# DEPRECATED GetCookiesResult "This may be removed in a future release." #-}
{-{-# WARNING GetCookiesResult "This feature is marked as EXPERIMENTAL." #-}-}
data GetCookiesResult = GetCookiesResult
    { -- | Array of cookie objects.
      cookies :: ![Network.Cookie]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetCookiesResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getCookiesResult" $ \_o -> GetCookiesResult
            <$> _o .: "cookies"
        ago = A.withArray "getCookiesResult" $ \_a -> GetCookiesResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetCookiesResult where
    toEncoding (GetCookiesResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "cookies" .= _0
        ]
    toJSON (GetCookiesResult _0) = A.object $ P.catMaybes
        [ P.pure $ "cookies" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetCookiesResult where
    GetCookiesResult _0 <> GetCookiesResult _ = GetCookiesResult _0


------------------------------------------------------------------------------
instance M.Method GetCookies where
    type Result GetCookies = GetCookiesResult
    name _ = "Page.getCookies"


------------------------------------------------------------------------------
-- | Returns all browser cookies. Depending on the backend support, will return detailed cookie
-- information in the @cookies@ field.
{-# DEPRECATED getCookies "This may be removed in a future release." #-}
{-{-# WARNING getCookies "This feature is marked as EXPERIMENTAL." #-}-}
getCookies
    :: GetCookies
getCookies = GetCookies


------------------------------------------------------------------------------
-- | Returns present frame tree structure.
data GetFrameTree = GetFrameTree
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetFrameTree where
    parseJSON A.Null = P.pure GetFrameTree
    parseJSON v = A.withArray "getFrameTree" go v
        <|> A.withObject "getFrameTree" go v
      where
        go _ = P.pure GetFrameTree


------------------------------------------------------------------------------
instance A.ToJSON GetFrameTree where
    toEncoding GetFrameTree = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetFrameTree = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetFrameTree where
    GetFrameTree <> GetFrameTree = GetFrameTree


------------------------------------------------------------------------------
instance P.Monoid GetFrameTree where
    mempty = GetFrameTree


------------------------------------------------------------------------------
-- | Returns present frame tree structure.
data GetFrameTreeResult = GetFrameTreeResult
    { -- | Present frame tree structure.
      frameTree :: !FrameTree
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetFrameTreeResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getFrameTreeResult" $ \_o -> GetFrameTreeResult
            <$> _o .: "frameTree"
        ago = A.withArray "getFrameTreeResult" $ \_a -> GetFrameTreeResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetFrameTreeResult where
    toEncoding (GetFrameTreeResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "frameTree" .= _0
        ]
    toJSON (GetFrameTreeResult _0) = A.object $ P.catMaybes
        [ P.pure $ "frameTree" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetFrameTreeResult where
    GetFrameTreeResult _0 <> GetFrameTreeResult _ = GetFrameTreeResult _0


------------------------------------------------------------------------------
instance M.Method GetFrameTree where
    type Result GetFrameTree = GetFrameTreeResult
    name _ = "Page.getFrameTree"


------------------------------------------------------------------------------
-- | Returns present frame tree structure.
getFrameTree
    :: GetFrameTree
getFrameTree = GetFrameTree


------------------------------------------------------------------------------
-- | Returns metrics relating to the layouting of the page, such as viewport bounds\/scale.
data GetLayoutMetrics = GetLayoutMetrics
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetLayoutMetrics where
    parseJSON A.Null = P.pure GetLayoutMetrics
    parseJSON v = A.withArray "getLayoutMetrics" go v
        <|> A.withObject "getLayoutMetrics" go v
      where
        go _ = P.pure GetLayoutMetrics


------------------------------------------------------------------------------
instance A.ToJSON GetLayoutMetrics where
    toEncoding GetLayoutMetrics = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetLayoutMetrics = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetLayoutMetrics where
    GetLayoutMetrics <> GetLayoutMetrics = GetLayoutMetrics


------------------------------------------------------------------------------
instance P.Monoid GetLayoutMetrics where
    mempty = GetLayoutMetrics


------------------------------------------------------------------------------
-- | Returns metrics relating to the layouting of the page, such as viewport bounds\/scale.
data GetLayoutMetricsResult = GetLayoutMetricsResult
    { -- | Metrics relating to the layout viewport.
      layoutViewport :: !LayoutViewport
      -- | Metrics relating to the visual viewport.
    , visualViewport :: !VisualViewport
      -- | Size of scrollable area.
    , contentSize :: !DOM.Rect
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetLayoutMetricsResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getLayoutMetricsResult" $ \_o -> GetLayoutMetricsResult
            <$> _o .: "layoutViewport"
            <*> _o .: "visualViewport"
            <*> _o .: "contentSize"
        ago = A.withArray "getLayoutMetricsResult" $ \_a -> GetLayoutMetricsResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON GetLayoutMetricsResult where
    toEncoding (GetLayoutMetricsResult _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "layoutViewport" .= _0
        , P.pure $ "visualViewport" .= _1
        , P.pure $ "contentSize" .= _2
        ]
    toJSON (GetLayoutMetricsResult _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "layoutViewport" .= _0
        , P.pure $ "visualViewport" .= _1
        , P.pure $ "contentSize" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetLayoutMetricsResult where
    GetLayoutMetricsResult _0 _1 _2 <> GetLayoutMetricsResult _ _ _ = GetLayoutMetricsResult _0 _1 _2


------------------------------------------------------------------------------
instance M.Method GetLayoutMetrics where
    type Result GetLayoutMetrics = GetLayoutMetricsResult
    name _ = "Page.getLayoutMetrics"


------------------------------------------------------------------------------
-- | Returns metrics relating to the layouting of the page, such as viewport bounds\/scale.
getLayoutMetrics
    :: GetLayoutMetrics
getLayoutMetrics = GetLayoutMetrics


------------------------------------------------------------------------------
-- | Returns navigation history for the current page.
data GetNavigationHistory = GetNavigationHistory
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetNavigationHistory where
    parseJSON A.Null = P.pure GetNavigationHistory
    parseJSON v = A.withArray "getNavigationHistory" go v
        <|> A.withObject "getNavigationHistory" go v
      where
        go _ = P.pure GetNavigationHistory


------------------------------------------------------------------------------
instance A.ToJSON GetNavigationHistory where
    toEncoding GetNavigationHistory = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetNavigationHistory = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetNavigationHistory where
    GetNavigationHistory <> GetNavigationHistory = GetNavigationHistory


------------------------------------------------------------------------------
instance P.Monoid GetNavigationHistory where
    mempty = GetNavigationHistory


------------------------------------------------------------------------------
-- | Returns navigation history for the current page.
data GetNavigationHistoryResult = GetNavigationHistoryResult
    { -- | Index of the current navigation history entry.
      currentIndex :: !P.Int
      -- | Array of navigation history entries.
    , entries :: ![NavigationEntry]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetNavigationHistoryResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getNavigationHistoryResult" $ \_o -> GetNavigationHistoryResult
            <$> _o .: "currentIndex"
            <*> _o .: "entries"
        ago = A.withArray "getNavigationHistoryResult" $ \_a -> GetNavigationHistoryResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON GetNavigationHistoryResult where
    toEncoding (GetNavigationHistoryResult _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "currentIndex" .= _0
        , P.pure $ "entries" .= _1
        ]
    toJSON (GetNavigationHistoryResult _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "currentIndex" .= _0
        , P.pure $ "entries" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetNavigationHistoryResult where
    GetNavigationHistoryResult _0 _1 <> GetNavigationHistoryResult _ _ = GetNavigationHistoryResult _0 _1


------------------------------------------------------------------------------
instance M.Method GetNavigationHistory where
    type Result GetNavigationHistory = GetNavigationHistoryResult
    name _ = "Page.getNavigationHistory"


------------------------------------------------------------------------------
-- | Returns navigation history for the current page.
getNavigationHistory
    :: GetNavigationHistory
getNavigationHistory = GetNavigationHistory


------------------------------------------------------------------------------
-- | Resets navigation history for the current page.
data ResetNavigationHistory = ResetNavigationHistory
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ResetNavigationHistory where
    parseJSON A.Null = P.pure ResetNavigationHistory
    parseJSON v = A.withArray "resetNavigationHistory" go v
        <|> A.withObject "resetNavigationHistory" go v
      where
        go _ = P.pure ResetNavigationHistory


------------------------------------------------------------------------------
instance A.ToJSON ResetNavigationHistory where
    toEncoding ResetNavigationHistory = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON ResetNavigationHistory = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup ResetNavigationHistory where
    ResetNavigationHistory <> ResetNavigationHistory = ResetNavigationHistory


------------------------------------------------------------------------------
instance P.Monoid ResetNavigationHistory where
    mempty = ResetNavigationHistory


------------------------------------------------------------------------------
instance M.Method ResetNavigationHistory where
    type Result ResetNavigationHistory = ()
    name _ = "Page.resetNavigationHistory"


------------------------------------------------------------------------------
-- | Resets navigation history for the current page.
resetNavigationHistory
    :: ResetNavigationHistory
resetNavigationHistory = ResetNavigationHistory


------------------------------------------------------------------------------
-- | Returns content of the given resource.
{-# WARNING GetResourceContent "This feature is marked as EXPERIMENTAL." #-}
data GetResourceContent = GetResourceContent
    { -- | Frame id to get resource for.
      frameId :: !FrameId
      -- | URL of the resource to get content for.
    , url :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetResourceContent where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getResourceContent" $ \_o -> GetResourceContent
            <$> _o .: "frameId"
            <*> _o .: "url"
        ago = A.withArray "getResourceContent" $ \_a -> GetResourceContent
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON GetResourceContent where
    toEncoding (GetResourceContent _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        , P.pure $ "url" .= _1
        ]
    toJSON (GetResourceContent _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        , P.pure $ "url" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetResourceContent where
    GetResourceContent _0 _1 <> GetResourceContent _ _ = GetResourceContent _0 _1


------------------------------------------------------------------------------
-- | Returns content of the given resource.
{-# WARNING GetResourceContentResult "This feature is marked as EXPERIMENTAL." #-}
data GetResourceContentResult = GetResourceContentResult
    { -- | Resource content.
      content :: !T.Text
      -- | True, if content was served as base64.
    , base64Encoded :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetResourceContentResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getResourceContentResult" $ \_o -> GetResourceContentResult
            <$> _o .: "content"
            <*> _o .: "base64Encoded"
        ago = A.withArray "getResourceContentResult" $ \_a -> GetResourceContentResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON GetResourceContentResult where
    toEncoding (GetResourceContentResult _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "content" .= _0
        , P.pure $ "base64Encoded" .= _1
        ]
    toJSON (GetResourceContentResult _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "content" .= _0
        , P.pure $ "base64Encoded" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetResourceContentResult where
    GetResourceContentResult _0 _1 <> GetResourceContentResult _ _ = GetResourceContentResult _0 _1


------------------------------------------------------------------------------
instance M.Method GetResourceContent where
    type Result GetResourceContent = GetResourceContentResult
    name _ = "Page.getResourceContent"


------------------------------------------------------------------------------
-- | Returns content of the given resource.
{-# WARNING getResourceContent "This feature is marked as EXPERIMENTAL." #-}
getResourceContent
    :: FrameId
    -- ^ Frame id to get resource for.

    -> T.Text
    -- ^ URL of the resource to get content for.

    -> GetResourceContent
getResourceContent _0 _1 = GetResourceContent _0 _1


------------------------------------------------------------------------------
-- | Returns present frame \/ resource tree structure.
{-# WARNING GetResourceTree "This feature is marked as EXPERIMENTAL." #-}
data GetResourceTree = GetResourceTree
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetResourceTree where
    parseJSON A.Null = P.pure GetResourceTree
    parseJSON v = A.withArray "getResourceTree" go v
        <|> A.withObject "getResourceTree" go v
      where
        go _ = P.pure GetResourceTree


------------------------------------------------------------------------------
instance A.ToJSON GetResourceTree where
    toEncoding GetResourceTree = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetResourceTree = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetResourceTree where
    GetResourceTree <> GetResourceTree = GetResourceTree


------------------------------------------------------------------------------
instance P.Monoid GetResourceTree where
    mempty = GetResourceTree


------------------------------------------------------------------------------
-- | Returns present frame \/ resource tree structure.
{-# WARNING GetResourceTreeResult "This feature is marked as EXPERIMENTAL." #-}
data GetResourceTreeResult = GetResourceTreeResult
    { -- | Present frame \/ resource tree structure.
      frameTree :: !FrameResourceTree
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetResourceTreeResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getResourceTreeResult" $ \_o -> GetResourceTreeResult
            <$> _o .: "frameTree"
        ago = A.withArray "getResourceTreeResult" $ \_a -> GetResourceTreeResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetResourceTreeResult where
    toEncoding (GetResourceTreeResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "frameTree" .= _0
        ]
    toJSON (GetResourceTreeResult _0) = A.object $ P.catMaybes
        [ P.pure $ "frameTree" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetResourceTreeResult where
    GetResourceTreeResult _0 <> GetResourceTreeResult _ = GetResourceTreeResult _0


------------------------------------------------------------------------------
instance M.Method GetResourceTree where
    type Result GetResourceTree = GetResourceTreeResult
    name _ = "Page.getResourceTree"


------------------------------------------------------------------------------
-- | Returns present frame \/ resource tree structure.
{-# WARNING getResourceTree "This feature is marked as EXPERIMENTAL." #-}
getResourceTree
    :: GetResourceTree
getResourceTree = GetResourceTree


------------------------------------------------------------------------------
-- | Accepts or dismisses a JavaScript initiated dialog (alert, confirm, prompt, or onbeforeunload).
data HandleJavaScriptDialog = HandleJavaScriptDialog
    { -- | Whether to accept or dismiss the dialog.
      accept :: !P.Bool
      -- | The text to enter into the dialog prompt before accepting. Used only if this is a prompt
      -- dialog.
    , promptText :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON HandleJavaScriptDialog where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "handleJavaScriptDialog" $ \_o -> HandleJavaScriptDialog
            <$> _o .: "accept"
            <*> _o .:? "promptText"
        ago = A.withArray "handleJavaScriptDialog" $ \_a -> HandleJavaScriptDialog
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON HandleJavaScriptDialog where
    toEncoding (HandleJavaScriptDialog _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "accept" .= _0
        , ("promptText" .=) <$> _1
        ]
    toJSON (HandleJavaScriptDialog _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "accept" .= _0
        , ("promptText" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup HandleJavaScriptDialog where
    HandleJavaScriptDialog _0 _1 <> HandleJavaScriptDialog _ __1 = HandleJavaScriptDialog _0 (_1 <|> __1)


------------------------------------------------------------------------------
instance M.Method HandleJavaScriptDialog where
    type Result HandleJavaScriptDialog = ()
    name _ = "Page.handleJavaScriptDialog"


------------------------------------------------------------------------------
-- | Accepts or dismisses a JavaScript initiated dialog (alert, confirm, prompt, or onbeforeunload).
handleJavaScriptDialog
    :: P.Bool
    -- ^ Whether to accept or dismiss the dialog.

    -> HandleJavaScriptDialog
handleJavaScriptDialog _0 = HandleJavaScriptDialog _0 P.empty


------------------------------------------------------------------------------
-- | Navigates current page to the given URL.
data Navigate = Navigate
    { -- | URL to navigate the page to.
      url :: !T.Text
      -- | Referrer URL.
    , referrer :: !(P.Maybe T.Text)
      -- | Intended transition type.
    , transitionType :: !(P.Maybe TransitionType)
      -- | Frame id to navigate, if not specified navigates the top frame.
    , frameId :: !(P.Maybe FrameId)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Navigate where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "navigate" $ \_o -> Navigate
            <$> _o .: "url"
            <*> _o .:? "referrer"
            <*> _o .:? "transitionType"
            <*> _o .:? "frameId"
        ago = A.withArray "navigate" $ \_a -> Navigate
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON Navigate where
    toEncoding (Navigate _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "url" .= _0
        , ("referrer" .=) <$> _1
        , ("transitionType" .=) <$> _2
        , ("frameId" .=) <$> _3
        ]
    toJSON (Navigate _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "url" .= _0
        , ("referrer" .=) <$> _1
        , ("transitionType" .=) <$> _2
        , ("frameId" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup Navigate where
    Navigate _0 _1 _2 _3 <> Navigate _ __1 __2 __3 = Navigate _0 (_1 <|> __1) (_2 <|> __2) (_3 <|> __3)


------------------------------------------------------------------------------
-- | Navigates current page to the given URL.
data NavigateResult = NavigateResult
    { -- | Frame id that has navigated (or failed to navigate)
      frameId :: !FrameId
      -- | Loader identifier.
    , loaderId :: !(P.Maybe Network.LoaderId)
      -- | User friendly error message, present if and only if navigation has failed.
    , errorText :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON NavigateResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "navigateResult" $ \_o -> NavigateResult
            <$> _o .: "frameId"
            <*> _o .:? "loaderId"
            <*> _o .:? "errorText"
        ago = A.withArray "navigateResult" $ \_a -> NavigateResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON NavigateResult where
    toEncoding (NavigateResult _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        , ("loaderId" .=) <$> _1
        , ("errorText" .=) <$> _2
        ]
    toJSON (NavigateResult _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        , ("loaderId" .=) <$> _1
        , ("errorText" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup NavigateResult where
    NavigateResult _0 _1 _2 <> NavigateResult _ __1 __2 = NavigateResult _0 (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
instance M.Method Navigate where
    type Result Navigate = NavigateResult
    name _ = "Page.navigate"


------------------------------------------------------------------------------
-- | Navigates current page to the given URL.
navigate
    :: T.Text
    -- ^ URL to navigate the page to.

    -> Navigate
navigate _0 = Navigate _0 P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Navigates current page to the given history entry.
data NavigateToHistoryEntry = NavigateToHistoryEntry
    { -- | Unique id of the entry to navigate to.
      entryId :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON NavigateToHistoryEntry where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "navigateToHistoryEntry" $ \_o -> NavigateToHistoryEntry
            <$> _o .: "entryId"
        ago = A.withArray "navigateToHistoryEntry" $ \_a -> NavigateToHistoryEntry
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON NavigateToHistoryEntry where
    toEncoding (NavigateToHistoryEntry _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "entryId" .= _0
        ]
    toJSON (NavigateToHistoryEntry _0) = A.object $ P.catMaybes
        [ P.pure $ "entryId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup NavigateToHistoryEntry where
    NavigateToHistoryEntry _0 <> NavigateToHistoryEntry _ = NavigateToHistoryEntry _0


------------------------------------------------------------------------------
instance M.Method NavigateToHistoryEntry where
    type Result NavigateToHistoryEntry = ()
    name _ = "Page.navigateToHistoryEntry"


------------------------------------------------------------------------------
-- | Navigates current page to the given history entry.
navigateToHistoryEntry
    :: P.Int
    -- ^ Unique id of the entry to navigate to.

    -> NavigateToHistoryEntry
navigateToHistoryEntry _0 = NavigateToHistoryEntry _0


------------------------------------------------------------------------------
-- | Print page as PDF.
{-# WARNING transferMode "This feature is marked as EXPERIMENTAL." #-}
data PrintToPDF = PrintToPDF
    { -- | Paper orientation. Defaults to false.
      landscape :: !(P.Maybe P.Bool)
      -- | Display header and footer. Defaults to false.
    , displayHeaderFooter :: !(P.Maybe P.Bool)
      -- | Print background graphics. Defaults to false.
    , printBackground :: !(P.Maybe P.Bool)
      -- | Scale of the webpage rendering. Defaults to 1.
    , scale :: !(P.Maybe P.Double)
      -- | Paper width in inches. Defaults to 8.5 inches.
    , paperWidth :: !(P.Maybe P.Double)
      -- | Paper height in inches. Defaults to 11 inches.
    , paperHeight :: !(P.Maybe P.Double)
      -- | Top margin in inches. Defaults to 1cm (~0.4 inches).
    , marginTop :: !(P.Maybe P.Double)
      -- | Bottom margin in inches. Defaults to 1cm (~0.4 inches).
    , marginBottom :: !(P.Maybe P.Double)
      -- | Left margin in inches. Defaults to 1cm (~0.4 inches).
    , marginLeft :: !(P.Maybe P.Double)
      -- | Right margin in inches. Defaults to 1cm (~0.4 inches).
    , marginRight :: !(P.Maybe P.Double)
      -- | Paper ranges to print, e.g., '1-5, 8, 11-13'. Defaults to the empty string, which means
      -- print all pages.
    , pageRanges :: !(P.Maybe T.Text)
      -- | Whether to silently ignore invalid but successfully parsed page ranges, such as '3-2'.
      -- Defaults to false.
    , ignoreInvalidPageRanges :: !(P.Maybe P.Bool)
      -- | HTML template for the print header. Should be valid HTML markup with following
      -- classes used to inject printing values into them:
      -- - @date@: formatted print date
      -- - @title@: document title
      -- - @url@: document location
      -- - @pageNumber@: current page number
      -- - @totalPages@: total pages in the document
      -- 
      -- For example, @<span class=title><\/span>@ would generate span containing the title.
    , headerTemplate :: !(P.Maybe T.Text)
      -- | HTML template for the print footer. Should use the same format as the @headerTemplate@.
    , footerTemplate :: !(P.Maybe T.Text)
      -- | Whether or not to prefer page size as defined by css. Defaults to false,
      -- in which case the content will be scaled to fit the paper size.
    , preferCSSPageSize :: !(P.Maybe P.Bool)
      -- | return as stream
    , transferMode :: !(P.Maybe TransferMode)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PrintToPDF where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "printToPDF" $ \_o -> PrintToPDF
            <$> _o .:? "landscape"
            <*> _o .:? "displayHeaderFooter"
            <*> _o .:? "printBackground"
            <*> _o .:? "scale"
            <*> _o .:? "paperWidth"
            <*> _o .:? "paperHeight"
            <*> _o .:? "marginTop"
            <*> _o .:? "marginBottom"
            <*> _o .:? "marginLeft"
            <*> _o .:? "marginRight"
            <*> _o .:? "pageRanges"
            <*> _o .:? "ignoreInvalidPageRanges"
            <*> _o .:? "headerTemplate"
            <*> _o .:? "footerTemplate"
            <*> _o .:? "preferCSSPageSize"
            <*> _o .:? "transferMode"
        ago = A.withArray "printToPDF" $ \_a -> PrintToPDF
            <$> P.traverse A.parseJSON (_a !? 0)
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
            <*> P.traverse A.parseJSON (_a !? 11)
            <*> P.traverse A.parseJSON (_a !? 12)
            <*> P.traverse A.parseJSON (_a !? 13)
            <*> P.traverse A.parseJSON (_a !? 14)
            <*> P.traverse A.parseJSON (_a !? 15)


------------------------------------------------------------------------------
instance A.ToJSON PrintToPDF where
    toEncoding (PrintToPDF _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 _15) = A.pairs $ P.fold $ P.catMaybes
        [ ("landscape" .=) <$> _0
        , ("displayHeaderFooter" .=) <$> _1
        , ("printBackground" .=) <$> _2
        , ("scale" .=) <$> _3
        , ("paperWidth" .=) <$> _4
        , ("paperHeight" .=) <$> _5
        , ("marginTop" .=) <$> _6
        , ("marginBottom" .=) <$> _7
        , ("marginLeft" .=) <$> _8
        , ("marginRight" .=) <$> _9
        , ("pageRanges" .=) <$> _10
        , ("ignoreInvalidPageRanges" .=) <$> _11
        , ("headerTemplate" .=) <$> _12
        , ("footerTemplate" .=) <$> _13
        , ("preferCSSPageSize" .=) <$> _14
        , ("transferMode" .=) <$> _15
        ]
    toJSON (PrintToPDF _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 _15) = A.object $ P.catMaybes
        [ ("landscape" .=) <$> _0
        , ("displayHeaderFooter" .=) <$> _1
        , ("printBackground" .=) <$> _2
        , ("scale" .=) <$> _3
        , ("paperWidth" .=) <$> _4
        , ("paperHeight" .=) <$> _5
        , ("marginTop" .=) <$> _6
        , ("marginBottom" .=) <$> _7
        , ("marginLeft" .=) <$> _8
        , ("marginRight" .=) <$> _9
        , ("pageRanges" .=) <$> _10
        , ("ignoreInvalidPageRanges" .=) <$> _11
        , ("headerTemplate" .=) <$> _12
        , ("footerTemplate" .=) <$> _13
        , ("preferCSSPageSize" .=) <$> _14
        , ("transferMode" .=) <$> _15
        ]


------------------------------------------------------------------------------
instance P.Semigroup PrintToPDF where
    PrintToPDF _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 _15 <> PrintToPDF __0 __1 __2 __3 __4 __5 __6 __7 __8 __9 __10 __11 __12 __13 __14 __15 = PrintToPDF (_0 <|> __0) (_1 <|> __1) (_2 <|> __2) (_3 <|> __3) (_4 <|> __4) (_5 <|> __5) (_6 <|> __6) (_7 <|> __7) (_8 <|> __8) (_9 <|> __9) (_10 <|> __10) (_11 <|> __11) (_12 <|> __12) (_13 <|> __13) (_14 <|> __14) (_15 <|> __15)


------------------------------------------------------------------------------
instance P.Monoid PrintToPDF where
    mempty = PrintToPDF P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
data TransferMode
    = ReturnAsBase64
    | ReturnAsStream
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TransferMode where
    parseJSON = A.withText "TransferMode" $ \t -> case t of
        "ReturnAsBase64" -> P.pure ReturnAsBase64
        "ReturnAsStream" -> P.pure ReturnAsStream
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON TransferMode where
    toJSON ReturnAsBase64 = "ReturnAsBase64"
    toJSON ReturnAsStream = "ReturnAsStream"


------------------------------------------------------------------------------
-- | Print page as PDF.
{-# WARNING stream "This feature is marked as EXPERIMENTAL." #-}
data PrintToPDFResult = PrintToPDFResult
    { -- | Base64-encoded pdf data. Empty if |returnAsStream| is specified.
      data_ :: !T.Text
      -- | A handle of the stream that holds resulting PDF data.
    , stream :: !(P.Maybe IO.StreamHandle)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PrintToPDFResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "printToPDFResult" $ \_o -> PrintToPDFResult
            <$> _o .: "data"
            <*> _o .:? "stream"
        ago = A.withArray "printToPDFResult" $ \_a -> PrintToPDFResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON PrintToPDFResult where
    toEncoding (PrintToPDFResult _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "data" .= _0
        , ("stream" .=) <$> _1
        ]
    toJSON (PrintToPDFResult _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "data" .= _0
        , ("stream" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup PrintToPDFResult where
    PrintToPDFResult _0 _1 <> PrintToPDFResult _ __1 = PrintToPDFResult _0 (_1 <|> __1)


------------------------------------------------------------------------------
instance M.Method PrintToPDF where
    type Result PrintToPDF = PrintToPDFResult
    name _ = "Page.printToPDF"


------------------------------------------------------------------------------
-- | Print page as PDF.
printToPDF
    :: PrintToPDF
printToPDF = PrintToPDF P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Reloads given page optionally ignoring the cache.
data Reload = Reload__
    { -- | If true, browser cache is ignored (as if the user pressed Shift+refresh).
      ignoreCache :: !(P.Maybe P.Bool)
      -- | If set, the script will be injected into all frames of the inspected page after reload.
      -- Argument will be ignored if reloading dataURL origin.
    , scriptToEvaluateOnLoad :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Reload where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "reload" $ \_o -> Reload__
            <$> _o .:? "ignoreCache"
            <*> _o .:? "scriptToEvaluateOnLoad"
        ago = A.withArray "reload" $ \_a -> Reload__
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON Reload where
    toEncoding (Reload__ _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ ("ignoreCache" .=) <$> _0
        , ("scriptToEvaluateOnLoad" .=) <$> _1
        ]
    toJSON (Reload__ _0 _1) = A.object $ P.catMaybes
        [ ("ignoreCache" .=) <$> _0
        , ("scriptToEvaluateOnLoad" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup Reload where
    Reload__ _0 _1 <> Reload__ __0 __1 = Reload__ (_0 <|> __0) (_1 <|> __1)


------------------------------------------------------------------------------
instance P.Monoid Reload where
    mempty = Reload__ P.empty P.empty


------------------------------------------------------------------------------
instance M.Method Reload where
    type Result Reload = ()
    name _ = "Page.reload"


------------------------------------------------------------------------------
-- | Reloads given page optionally ignoring the cache.
reload
    :: Reload
reload = Reload__ P.empty P.empty


------------------------------------------------------------------------------
-- | Deprecated, please use removeScriptToEvaluateOnNewDocument instead.
{-# DEPRECATED RemoveScriptToEvaluateOnLoad "This may be removed in a future release." #-}
{-{-# WARNING RemoveScriptToEvaluateOnLoad "This feature is marked as EXPERIMENTAL." #-}-}
data RemoveScriptToEvaluateOnLoad = RemoveScriptToEvaluateOnLoad
    { identifier :: !ScriptIdentifier
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RemoveScriptToEvaluateOnLoad where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "removeScriptToEvaluateOnLoad" $ \_o -> RemoveScriptToEvaluateOnLoad
            <$> _o .: "identifier"
        ago = A.withArray "removeScriptToEvaluateOnLoad" $ \_a -> RemoveScriptToEvaluateOnLoad
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON RemoveScriptToEvaluateOnLoad where
    toEncoding (RemoveScriptToEvaluateOnLoad _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "identifier" .= _0
        ]
    toJSON (RemoveScriptToEvaluateOnLoad _0) = A.object $ P.catMaybes
        [ P.pure $ "identifier" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup RemoveScriptToEvaluateOnLoad where
    RemoveScriptToEvaluateOnLoad _0 <> RemoveScriptToEvaluateOnLoad _ = RemoveScriptToEvaluateOnLoad _0


------------------------------------------------------------------------------
instance M.Method RemoveScriptToEvaluateOnLoad where
    type Result RemoveScriptToEvaluateOnLoad = ()
    name _ = "Page.removeScriptToEvaluateOnLoad"


------------------------------------------------------------------------------
-- | Deprecated, please use removeScriptToEvaluateOnNewDocument instead.
{-# DEPRECATED removeScriptToEvaluateOnLoad "This may be removed in a future release." #-}
{-{-# WARNING removeScriptToEvaluateOnLoad "This feature is marked as EXPERIMENTAL." #-}-}
removeScriptToEvaluateOnLoad
    :: ScriptIdentifier
    -> RemoveScriptToEvaluateOnLoad
removeScriptToEvaluateOnLoad _0 = RemoveScriptToEvaluateOnLoad _0


------------------------------------------------------------------------------
-- | Removes given script from the list.
data RemoveScriptToEvaluateOnNewDocument = RemoveScriptToEvaluateOnNewDocument
    { identifier :: !ScriptIdentifier
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON RemoveScriptToEvaluateOnNewDocument where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "removeScriptToEvaluateOnNewDocument" $ \_o -> RemoveScriptToEvaluateOnNewDocument
            <$> _o .: "identifier"
        ago = A.withArray "removeScriptToEvaluateOnNewDocument" $ \_a -> RemoveScriptToEvaluateOnNewDocument
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON RemoveScriptToEvaluateOnNewDocument where
    toEncoding (RemoveScriptToEvaluateOnNewDocument _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "identifier" .= _0
        ]
    toJSON (RemoveScriptToEvaluateOnNewDocument _0) = A.object $ P.catMaybes
        [ P.pure $ "identifier" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup RemoveScriptToEvaluateOnNewDocument where
    RemoveScriptToEvaluateOnNewDocument _0 <> RemoveScriptToEvaluateOnNewDocument _ = RemoveScriptToEvaluateOnNewDocument _0


------------------------------------------------------------------------------
instance M.Method RemoveScriptToEvaluateOnNewDocument where
    type Result RemoveScriptToEvaluateOnNewDocument = ()
    name _ = "Page.removeScriptToEvaluateOnNewDocument"


------------------------------------------------------------------------------
-- | Removes given script from the list.
removeScriptToEvaluateOnNewDocument
    :: ScriptIdentifier
    -> RemoveScriptToEvaluateOnNewDocument
removeScriptToEvaluateOnNewDocument _0 = RemoveScriptToEvaluateOnNewDocument _0


------------------------------------------------------------------------------
-- | Acknowledges that a screencast frame has been received by the frontend.
{-# WARNING ScreencastFrameAck "This feature is marked as EXPERIMENTAL." #-}
data ScreencastFrameAck = ScreencastFrameAck
    { -- | Frame number.
      sessionId :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ScreencastFrameAck where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "screencastFrameAck" $ \_o -> ScreencastFrameAck
            <$> _o .: "sessionId"
        ago = A.withArray "screencastFrameAck" $ \_a -> ScreencastFrameAck
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON ScreencastFrameAck where
    toEncoding (ScreencastFrameAck _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "sessionId" .= _0
        ]
    toJSON (ScreencastFrameAck _0) = A.object $ P.catMaybes
        [ P.pure $ "sessionId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup ScreencastFrameAck where
    ScreencastFrameAck _0 <> ScreencastFrameAck _ = ScreencastFrameAck _0


------------------------------------------------------------------------------
instance M.Method ScreencastFrameAck where
    type Result ScreencastFrameAck = ()
    name _ = "Page.screencastFrameAck"


------------------------------------------------------------------------------
-- | Acknowledges that a screencast frame has been received by the frontend.
{-# WARNING screencastFrameAck "This feature is marked as EXPERIMENTAL." #-}
screencastFrameAck
    :: P.Int
    -- ^ Frame number.

    -> ScreencastFrameAck
screencastFrameAck _0 = ScreencastFrameAck _0


------------------------------------------------------------------------------
-- | Searches for given string in resource content.
{-# WARNING SearchInResource "This feature is marked as EXPERIMENTAL." #-}
data SearchInResource = SearchInResource
    { -- | Frame id for resource to search in.
      frameId :: !FrameId
      -- | URL of the resource to search in.
    , url :: !T.Text
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
instance A.FromJSON SearchInResource where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "searchInResource" $ \_o -> SearchInResource
            <$> _o .: "frameId"
            <*> _o .: "url"
            <*> _o .: "query"
            <*> _o .:? "caseSensitive"
            <*> _o .:? "isRegex"
        ago = A.withArray "searchInResource" $ \_a -> SearchInResource
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON SearchInResource where
    toEncoding (SearchInResource _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        , P.pure $ "url" .= _1
        , P.pure $ "query" .= _2
        , ("caseSensitive" .=) <$> _3
        , ("isRegex" .=) <$> _4
        ]
    toJSON (SearchInResource _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        , P.pure $ "url" .= _1
        , P.pure $ "query" .= _2
        , ("caseSensitive" .=) <$> _3
        , ("isRegex" .=) <$> _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup SearchInResource where
    SearchInResource _0 _1 _2 _3 _4 <> SearchInResource _ _ _ __3 __4 = SearchInResource _0 _1 _2 (_3 <|> __3) (_4 <|> __4)


------------------------------------------------------------------------------
-- | Searches for given string in resource content.
{-# WARNING SearchInResourceResult "This feature is marked as EXPERIMENTAL." #-}
data SearchInResourceResult = SearchInResourceResult
    { -- | List of search matches.
      result :: ![Debugger.SearchMatch]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SearchInResourceResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "searchInResourceResult" $ \_o -> SearchInResourceResult
            <$> _o .: "result"
        ago = A.withArray "searchInResourceResult" $ \_a -> SearchInResourceResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SearchInResourceResult where
    toEncoding (SearchInResourceResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "result" .= _0
        ]
    toJSON (SearchInResourceResult _0) = A.object $ P.catMaybes
        [ P.pure $ "result" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SearchInResourceResult where
    SearchInResourceResult _0 <> SearchInResourceResult _ = SearchInResourceResult _0


------------------------------------------------------------------------------
instance M.Method SearchInResource where
    type Result SearchInResource = SearchInResourceResult
    name _ = "Page.searchInResource"


------------------------------------------------------------------------------
-- | Searches for given string in resource content.
{-# WARNING searchInResource "This feature is marked as EXPERIMENTAL." #-}
searchInResource
    :: FrameId
    -- ^ Frame id for resource to search in.

    -> T.Text
    -- ^ URL of the resource to search in.

    -> T.Text
    -- ^ String to search for.

    -> SearchInResource
searchInResource _0 _1 _2 = SearchInResource _0 _1 _2 P.empty P.empty


------------------------------------------------------------------------------
-- | Enable Chrome's experimental ad filter on all sites.
{-# WARNING SetAdBlockingEnabled "This feature is marked as EXPERIMENTAL." #-}
data SetAdBlockingEnabled = SetAdBlockingEnabled
    { -- | Whether to block ads.
      enabled :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetAdBlockingEnabled where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setAdBlockingEnabled" $ \_o -> SetAdBlockingEnabled
            <$> _o .: "enabled"
        ago = A.withArray "setAdBlockingEnabled" $ \_a -> SetAdBlockingEnabled
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetAdBlockingEnabled where
    toEncoding (SetAdBlockingEnabled _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "enabled" .= _0
        ]
    toJSON (SetAdBlockingEnabled _0) = A.object $ P.catMaybes
        [ P.pure $ "enabled" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetAdBlockingEnabled where
    SetAdBlockingEnabled _0 <> SetAdBlockingEnabled _ = SetAdBlockingEnabled _0


------------------------------------------------------------------------------
instance M.Method SetAdBlockingEnabled where
    type Result SetAdBlockingEnabled = ()
    name _ = "Page.setAdBlockingEnabled"


------------------------------------------------------------------------------
-- | Enable Chrome's experimental ad filter on all sites.
{-# WARNING setAdBlockingEnabled "This feature is marked as EXPERIMENTAL." #-}
setAdBlockingEnabled
    :: P.Bool
    -- ^ Whether to block ads.

    -> SetAdBlockingEnabled
setAdBlockingEnabled _0 = SetAdBlockingEnabled _0


------------------------------------------------------------------------------
-- | Enable page Content Security Policy by-passing.
{-# WARNING SetBypassCSP "This feature is marked as EXPERIMENTAL." #-}
data SetBypassCSP = SetBypassCSP
    { -- | Whether to bypass page CSP.
      enabled :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetBypassCSP where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setBypassCSP" $ \_o -> SetBypassCSP
            <$> _o .: "enabled"
        ago = A.withArray "setBypassCSP" $ \_a -> SetBypassCSP
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetBypassCSP where
    toEncoding (SetBypassCSP _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "enabled" .= _0
        ]
    toJSON (SetBypassCSP _0) = A.object $ P.catMaybes
        [ P.pure $ "enabled" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetBypassCSP where
    SetBypassCSP _0 <> SetBypassCSP _ = SetBypassCSP _0


------------------------------------------------------------------------------
instance M.Method SetBypassCSP where
    type Result SetBypassCSP = ()
    name _ = "Page.setBypassCSP"


------------------------------------------------------------------------------
-- | Enable page Content Security Policy by-passing.
{-# WARNING setBypassCSP "This feature is marked as EXPERIMENTAL." #-}
setBypassCSP
    :: P.Bool
    -- ^ Whether to bypass page CSP.

    -> SetBypassCSP
setBypassCSP _0 = SetBypassCSP _0


------------------------------------------------------------------------------
-- | Overrides the values of device screen dimensions (window.screen.width, window.screen.height,
-- window.innerWidth, window.innerHeight, and "device-width"\/"device-height"-related CSS media
-- query results).
{-# DEPRECATED SetDeviceMetricsOverride "This may be removed in a future release." #-}
{-{-# WARNING SetDeviceMetricsOverride "This feature is marked as EXPERIMENTAL." #-}-}
data SetDeviceMetricsOverride = SetDeviceMetricsOverride
    { -- | Overriding width value in pixels (minimum 0, maximum 10000000). 0 disables the override.
      width :: !P.Int
      -- | Overriding height value in pixels (minimum 0, maximum 10000000). 0 disables the override.
    , height :: !P.Int
      -- | Overriding device scale factor value. 0 disables the override.
    , deviceScaleFactor :: !P.Double
      -- | Whether to emulate mobile device. This includes viewport meta tag, overlay scrollbars, text
      -- autosizing and more.
    , mobile :: !P.Bool
      -- | Scale to apply to resulting view image.
    , scale :: !(P.Maybe P.Double)
      -- | Overriding screen width value in pixels (minimum 0, maximum 10000000).
    , screenWidth :: !(P.Maybe P.Int)
      -- | Overriding screen height value in pixels (minimum 0, maximum 10000000).
    , screenHeight :: !(P.Maybe P.Int)
      -- | Overriding view X position on screen in pixels (minimum 0, maximum 10000000).
    , positionX :: !(P.Maybe P.Int)
      -- | Overriding view Y position on screen in pixels (minimum 0, maximum 10000000).
    , positionY :: !(P.Maybe P.Int)
      -- | Do not set visible view size, rely upon explicit setVisibleSize call.
    , dontSetVisibleSize :: !(P.Maybe P.Bool)
      -- | Screen orientation override.
    , screenOrientation :: !(P.Maybe Emulation.ScreenOrientation)
      -- | The viewport dimensions and scale. If not set, the override is cleared.
    , viewport :: !(P.Maybe Viewport)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetDeviceMetricsOverride where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setDeviceMetricsOverride" $ \_o -> SetDeviceMetricsOverride
            <$> _o .: "width"
            <*> _o .: "height"
            <*> _o .: "deviceScaleFactor"
            <*> _o .: "mobile"
            <*> _o .:? "scale"
            <*> _o .:? "screenWidth"
            <*> _o .:? "screenHeight"
            <*> _o .:? "positionX"
            <*> _o .:? "positionY"
            <*> _o .:? "dontSetVisibleSize"
            <*> _o .:? "screenOrientation"
            <*> _o .:? "viewport"
        ago = A.withArray "setDeviceMetricsOverride" $ \_a -> SetDeviceMetricsOverride
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)
            <*> P.traverse A.parseJSON (_a !? 5)
            <*> P.traverse A.parseJSON (_a !? 6)
            <*> P.traverse A.parseJSON (_a !? 7)
            <*> P.traverse A.parseJSON (_a !? 8)
            <*> P.traverse A.parseJSON (_a !? 9)
            <*> P.traverse A.parseJSON (_a !? 10)
            <*> P.traverse A.parseJSON (_a !? 11)


------------------------------------------------------------------------------
instance A.ToJSON SetDeviceMetricsOverride where
    toEncoding (SetDeviceMetricsOverride _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "width" .= _0
        , P.pure $ "height" .= _1
        , P.pure $ "deviceScaleFactor" .= _2
        , P.pure $ "mobile" .= _3
        , ("scale" .=) <$> _4
        , ("screenWidth" .=) <$> _5
        , ("screenHeight" .=) <$> _6
        , ("positionX" .=) <$> _7
        , ("positionY" .=) <$> _8
        , ("dontSetVisibleSize" .=) <$> _9
        , ("screenOrientation" .=) <$> _10
        , ("viewport" .=) <$> _11
        ]
    toJSON (SetDeviceMetricsOverride _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11) = A.object $ P.catMaybes
        [ P.pure $ "width" .= _0
        , P.pure $ "height" .= _1
        , P.pure $ "deviceScaleFactor" .= _2
        , P.pure $ "mobile" .= _3
        , ("scale" .=) <$> _4
        , ("screenWidth" .=) <$> _5
        , ("screenHeight" .=) <$> _6
        , ("positionX" .=) <$> _7
        , ("positionY" .=) <$> _8
        , ("dontSetVisibleSize" .=) <$> _9
        , ("screenOrientation" .=) <$> _10
        , ("viewport" .=) <$> _11
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetDeviceMetricsOverride where
    SetDeviceMetricsOverride _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 <> SetDeviceMetricsOverride _ _ _ _ __4 __5 __6 __7 __8 __9 __10 __11 = SetDeviceMetricsOverride _0 _1 _2 _3 (_4 <|> __4) (_5 <|> __5) (_6 <|> __6) (_7 <|> __7) (_8 <|> __8) (_9 <|> __9) (_10 <|> __10) (_11 <|> __11)


------------------------------------------------------------------------------
instance M.Method SetDeviceMetricsOverride where
    type Result SetDeviceMetricsOverride = ()
    name _ = "Page.setDeviceMetricsOverride"


------------------------------------------------------------------------------
-- | Overrides the values of device screen dimensions (window.screen.width, window.screen.height,
-- window.innerWidth, window.innerHeight, and "device-width"\/"device-height"-related CSS media
-- query results).
{-# DEPRECATED setDeviceMetricsOverride "This may be removed in a future release." #-}
{-{-# WARNING setDeviceMetricsOverride "This feature is marked as EXPERIMENTAL." #-}-}
setDeviceMetricsOverride
    :: P.Int
    -- ^ Overriding width value in pixels (minimum 0, maximum 10000000). 0 disables the override.

    -> P.Int
    -- ^ Overriding height value in pixels (minimum 0, maximum 10000000). 0 disables the override.

    -> P.Double
    -- ^ Overriding device scale factor value. 0 disables the override.

    -> P.Bool
    -- ^ Whether to emulate mobile device. This includes viewport meta tag, overlay scrollbars, text

    -- autosizing and more.

    -> SetDeviceMetricsOverride
setDeviceMetricsOverride _0 _1 _2 _3 = SetDeviceMetricsOverride _0 _1 _2 _3 P.empty P.empty P.empty P.empty P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Overrides the Device Orientation.
{-# DEPRECATED SetDeviceOrientationOverride "This may be removed in a future release." #-}
{-{-# WARNING SetDeviceOrientationOverride "This feature is marked as EXPERIMENTAL." #-}-}
data SetDeviceOrientationOverride = SetDeviceOrientationOverride
    { -- | Mock alpha
      alpha :: !P.Double
      -- | Mock beta
    , beta :: !P.Double
      -- | Mock gamma
    , gamma :: !P.Double
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetDeviceOrientationOverride where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setDeviceOrientationOverride" $ \_o -> SetDeviceOrientationOverride
            <$> _o .: "alpha"
            <*> _o .: "beta"
            <*> _o .: "gamma"
        ago = A.withArray "setDeviceOrientationOverride" $ \_a -> SetDeviceOrientationOverride
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON SetDeviceOrientationOverride where
    toEncoding (SetDeviceOrientationOverride _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "alpha" .= _0
        , P.pure $ "beta" .= _1
        , P.pure $ "gamma" .= _2
        ]
    toJSON (SetDeviceOrientationOverride _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "alpha" .= _0
        , P.pure $ "beta" .= _1
        , P.pure $ "gamma" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetDeviceOrientationOverride where
    SetDeviceOrientationOverride _0 _1 _2 <> SetDeviceOrientationOverride _ _ _ = SetDeviceOrientationOverride _0 _1 _2


------------------------------------------------------------------------------
instance M.Method SetDeviceOrientationOverride where
    type Result SetDeviceOrientationOverride = ()
    name _ = "Page.setDeviceOrientationOverride"


------------------------------------------------------------------------------
-- | Overrides the Device Orientation.
{-# DEPRECATED setDeviceOrientationOverride "This may be removed in a future release." #-}
{-{-# WARNING setDeviceOrientationOverride "This feature is marked as EXPERIMENTAL." #-}-}
setDeviceOrientationOverride
    :: P.Double
    -- ^ Mock alpha

    -> P.Double
    -- ^ Mock beta

    -> P.Double
    -- ^ Mock gamma

    -> SetDeviceOrientationOverride
setDeviceOrientationOverride _0 _1 _2 = SetDeviceOrientationOverride _0 _1 _2


------------------------------------------------------------------------------
-- | Set generic font families.
{-# WARNING SetFontFamilies "This feature is marked as EXPERIMENTAL." #-}
data SetFontFamilies = SetFontFamilies
    { -- | Specifies font families to set. If a font family is not specified, it won't be changed.
      fontFamilies :: !FontFamilies
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetFontFamilies where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setFontFamilies" $ \_o -> SetFontFamilies
            <$> _o .: "fontFamilies"
        ago = A.withArray "setFontFamilies" $ \_a -> SetFontFamilies
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetFontFamilies where
    toEncoding (SetFontFamilies _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "fontFamilies" .= _0
        ]
    toJSON (SetFontFamilies _0) = A.object $ P.catMaybes
        [ P.pure $ "fontFamilies" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetFontFamilies where
    SetFontFamilies _0 <> SetFontFamilies _ = SetFontFamilies _0


------------------------------------------------------------------------------
instance M.Method SetFontFamilies where
    type Result SetFontFamilies = ()
    name _ = "Page.setFontFamilies"


------------------------------------------------------------------------------
-- | Set generic font families.
{-# WARNING setFontFamilies "This feature is marked as EXPERIMENTAL." #-}
setFontFamilies
    :: FontFamilies
    -- ^ Specifies font families to set. If a font family is not specified, it won't be changed.

    -> SetFontFamilies
setFontFamilies _0 = SetFontFamilies _0


------------------------------------------------------------------------------
-- | Set default font sizes.
{-# WARNING SetFontSizes "This feature is marked as EXPERIMENTAL." #-}
data SetFontSizes = SetFontSizes
    { -- | Specifies font sizes to set. If a font size is not specified, it won't be changed.
      fontSizes :: !FontSizes
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetFontSizes where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setFontSizes" $ \_o -> SetFontSizes
            <$> _o .: "fontSizes"
        ago = A.withArray "setFontSizes" $ \_a -> SetFontSizes
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetFontSizes where
    toEncoding (SetFontSizes _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "fontSizes" .= _0
        ]
    toJSON (SetFontSizes _0) = A.object $ P.catMaybes
        [ P.pure $ "fontSizes" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetFontSizes where
    SetFontSizes _0 <> SetFontSizes _ = SetFontSizes _0


------------------------------------------------------------------------------
instance M.Method SetFontSizes where
    type Result SetFontSizes = ()
    name _ = "Page.setFontSizes"


------------------------------------------------------------------------------
-- | Set default font sizes.
{-# WARNING setFontSizes "This feature is marked as EXPERIMENTAL." #-}
setFontSizes
    :: FontSizes
    -- ^ Specifies font sizes to set. If a font size is not specified, it won't be changed.

    -> SetFontSizes
setFontSizes _0 = SetFontSizes _0


------------------------------------------------------------------------------
-- | Sets given markup as the document's HTML.
data SetDocumentContent = SetDocumentContent
    { -- | Frame id to set HTML for.
      frameId :: !FrameId
      -- | HTML content to set.
    , html :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetDocumentContent where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setDocumentContent" $ \_o -> SetDocumentContent
            <$> _o .: "frameId"
            <*> _o .: "html"
        ago = A.withArray "setDocumentContent" $ \_a -> SetDocumentContent
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SetDocumentContent where
    toEncoding (SetDocumentContent _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        , P.pure $ "html" .= _1
        ]
    toJSON (SetDocumentContent _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        , P.pure $ "html" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetDocumentContent where
    SetDocumentContent _0 _1 <> SetDocumentContent _ _ = SetDocumentContent _0 _1


------------------------------------------------------------------------------
instance M.Method SetDocumentContent where
    type Result SetDocumentContent = ()
    name _ = "Page.setDocumentContent"


------------------------------------------------------------------------------
-- | Sets given markup as the document's HTML.
setDocumentContent
    :: FrameId
    -- ^ Frame id to set HTML for.

    -> T.Text
    -- ^ HTML content to set.

    -> SetDocumentContent
setDocumentContent _0 _1 = SetDocumentContent _0 _1


------------------------------------------------------------------------------
-- | Set the behavior when downloading a file.
{-# WARNING SetDownloadBehavior "This feature is marked as EXPERIMENTAL." #-}
data SetDownloadBehavior = SetDownloadBehavior
    { -- | Whether to allow all or deny all download requests, or use default Chrome behavior if
      -- available (otherwise deny).
      behavior :: !Behavior
      -- | The default path to save downloaded files to. This is requred if behavior is set to 'allow'
    , downloadPath :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetDownloadBehavior where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setDownloadBehavior" $ \_o -> SetDownloadBehavior
            <$> _o .: "behavior"
            <*> _o .:? "downloadPath"
        ago = A.withArray "setDownloadBehavior" $ \_a -> SetDownloadBehavior
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SetDownloadBehavior where
    toEncoding (SetDownloadBehavior _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "behavior" .= _0
        , ("downloadPath" .=) <$> _1
        ]
    toJSON (SetDownloadBehavior _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "behavior" .= _0
        , ("downloadPath" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetDownloadBehavior where
    SetDownloadBehavior _0 _1 <> SetDownloadBehavior _ __1 = SetDownloadBehavior _0 (_1 <|> __1)


------------------------------------------------------------------------------
data Behavior
    = Deny
    | Allow
    | Default
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Behavior where
    parseJSON = A.withText "Behavior" $ \t -> case t of
        "deny" -> P.pure Deny
        "allow" -> P.pure Allow
        "default" -> P.pure Default
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Behavior where
    toJSON Deny = "deny"
    toJSON Allow = "allow"
    toJSON Default = "default"


------------------------------------------------------------------------------
instance M.Method SetDownloadBehavior where
    type Result SetDownloadBehavior = ()
    name _ = "Page.setDownloadBehavior"


------------------------------------------------------------------------------
-- | Set the behavior when downloading a file.
{-# WARNING setDownloadBehavior "This feature is marked as EXPERIMENTAL." #-}
setDownloadBehavior
    :: Behavior
    -- ^ Whether to allow all or deny all download requests, or use default Chrome behavior if

    -- available (otherwise deny).

    -> SetDownloadBehavior
setDownloadBehavior _0 = SetDownloadBehavior _0 P.empty


------------------------------------------------------------------------------
-- | Overrides the Geolocation Position or Error. Omitting any of the parameters emulates position
-- unavailable.
{-# DEPRECATED SetGeolocationOverride "This may be removed in a future release." #-}
data SetGeolocationOverride = SetGeolocationOverride
    { -- | Mock latitude
      latitude :: !(P.Maybe P.Double)
      -- | Mock longitude
    , longitude :: !(P.Maybe P.Double)
      -- | Mock accuracy
    , accuracy :: !(P.Maybe P.Double)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetGeolocationOverride where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setGeolocationOverride" $ \_o -> SetGeolocationOverride
            <$> _o .:? "latitude"
            <*> _o .:? "longitude"
            <*> _o .:? "accuracy"
        ago = A.withArray "setGeolocationOverride" $ \_a -> SetGeolocationOverride
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON SetGeolocationOverride where
    toEncoding (SetGeolocationOverride _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ ("latitude" .=) <$> _0
        , ("longitude" .=) <$> _1
        , ("accuracy" .=) <$> _2
        ]
    toJSON (SetGeolocationOverride _0 _1 _2) = A.object $ P.catMaybes
        [ ("latitude" .=) <$> _0
        , ("longitude" .=) <$> _1
        , ("accuracy" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetGeolocationOverride where
    SetGeolocationOverride _0 _1 _2 <> SetGeolocationOverride __0 __1 __2 = SetGeolocationOverride (_0 <|> __0) (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
instance P.Monoid SetGeolocationOverride where
    mempty = SetGeolocationOverride P.empty P.empty P.empty


------------------------------------------------------------------------------
instance M.Method SetGeolocationOverride where
    type Result SetGeolocationOverride = ()
    name _ = "Page.setGeolocationOverride"


------------------------------------------------------------------------------
-- | Overrides the Geolocation Position or Error. Omitting any of the parameters emulates position
-- unavailable.
{-# DEPRECATED setGeolocationOverride "This may be removed in a future release." #-}
setGeolocationOverride
    :: SetGeolocationOverride
setGeolocationOverride = SetGeolocationOverride P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Controls whether page will emit lifecycle events.
{-# WARNING SetLifecycleEventsEnabled "This feature is marked as EXPERIMENTAL." #-}
data SetLifecycleEventsEnabled = SetLifecycleEventsEnabled
    { -- | If true, starts emitting lifecycle events.
      enabled :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetLifecycleEventsEnabled where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setLifecycleEventsEnabled" $ \_o -> SetLifecycleEventsEnabled
            <$> _o .: "enabled"
        ago = A.withArray "setLifecycleEventsEnabled" $ \_a -> SetLifecycleEventsEnabled
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetLifecycleEventsEnabled where
    toEncoding (SetLifecycleEventsEnabled _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "enabled" .= _0
        ]
    toJSON (SetLifecycleEventsEnabled _0) = A.object $ P.catMaybes
        [ P.pure $ "enabled" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetLifecycleEventsEnabled where
    SetLifecycleEventsEnabled _0 <> SetLifecycleEventsEnabled _ = SetLifecycleEventsEnabled _0


------------------------------------------------------------------------------
instance M.Method SetLifecycleEventsEnabled where
    type Result SetLifecycleEventsEnabled = ()
    name _ = "Page.setLifecycleEventsEnabled"


------------------------------------------------------------------------------
-- | Controls whether page will emit lifecycle events.
{-# WARNING setLifecycleEventsEnabled "This feature is marked as EXPERIMENTAL." #-}
setLifecycleEventsEnabled
    :: P.Bool
    -- ^ If true, starts emitting lifecycle events.

    -> SetLifecycleEventsEnabled
setLifecycleEventsEnabled _0 = SetLifecycleEventsEnabled _0


------------------------------------------------------------------------------
-- | Toggles mouse event-based touch event emulation.
{-# DEPRECATED SetTouchEmulationEnabled "This may be removed in a future release." #-}
{-{-# WARNING SetTouchEmulationEnabled "This feature is marked as EXPERIMENTAL." #-}-}
data SetTouchEmulationEnabled = SetTouchEmulationEnabled
    { -- | Whether the touch event emulation should be enabled.
      enabled :: !P.Bool
      -- | Touch\/gesture events configuration. Default: current platform.
    , configuration :: !(P.Maybe Configuration)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetTouchEmulationEnabled where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setTouchEmulationEnabled" $ \_o -> SetTouchEmulationEnabled
            <$> _o .: "enabled"
            <*> _o .:? "configuration"
        ago = A.withArray "setTouchEmulationEnabled" $ \_a -> SetTouchEmulationEnabled
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SetTouchEmulationEnabled where
    toEncoding (SetTouchEmulationEnabled _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "enabled" .= _0
        , ("configuration" .=) <$> _1
        ]
    toJSON (SetTouchEmulationEnabled _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "enabled" .= _0
        , ("configuration" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetTouchEmulationEnabled where
    SetTouchEmulationEnabled _0 _1 <> SetTouchEmulationEnabled _ __1 = SetTouchEmulationEnabled _0 (_1 <|> __1)


------------------------------------------------------------------------------
data Configuration
    = Mobile
    | Desktop
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Configuration where
    parseJSON = A.withText "Configuration" $ \t -> case t of
        "mobile" -> P.pure Mobile
        "desktop" -> P.pure Desktop
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Configuration where
    toJSON Mobile = "mobile"
    toJSON Desktop = "desktop"


------------------------------------------------------------------------------
instance M.Method SetTouchEmulationEnabled where
    type Result SetTouchEmulationEnabled = ()
    name _ = "Page.setTouchEmulationEnabled"


------------------------------------------------------------------------------
-- | Toggles mouse event-based touch event emulation.
{-# DEPRECATED setTouchEmulationEnabled "This may be removed in a future release." #-}
{-{-# WARNING setTouchEmulationEnabled "This feature is marked as EXPERIMENTAL." #-}-}
setTouchEmulationEnabled
    :: P.Bool
    -- ^ Whether the touch event emulation should be enabled.

    -> SetTouchEmulationEnabled
setTouchEmulationEnabled _0 = SetTouchEmulationEnabled _0 P.empty


------------------------------------------------------------------------------
-- | Starts sending each frame using the @screencastFrame@ event.
{-# WARNING StartScreencast "This feature is marked as EXPERIMENTAL." #-}
data StartScreencast = StartScreencast
    { -- | Image compression format.
      format :: !(P.Maybe Format)
      -- | Compression quality from range [0..100].
    , quality :: !(P.Maybe P.Int)
      -- | Maximum screenshot width.
    , maxWidth :: !(P.Maybe P.Int)
      -- | Maximum screenshot height.
    , maxHeight :: !(P.Maybe P.Int)
      -- | Send every n-th frame.
    , everyNthFrame :: !(P.Maybe P.Int)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StartScreencast where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "startScreencast" $ \_o -> StartScreencast
            <$> _o .:? "format"
            <*> _o .:? "quality"
            <*> _o .:? "maxWidth"
            <*> _o .:? "maxHeight"
            <*> _o .:? "everyNthFrame"
        ago = A.withArray "startScreencast" $ \_a -> StartScreencast
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON StartScreencast where
    toEncoding (StartScreencast _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ ("format" .=) <$> _0
        , ("quality" .=) <$> _1
        , ("maxWidth" .=) <$> _2
        , ("maxHeight" .=) <$> _3
        , ("everyNthFrame" .=) <$> _4
        ]
    toJSON (StartScreencast _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ ("format" .=) <$> _0
        , ("quality" .=) <$> _1
        , ("maxWidth" .=) <$> _2
        , ("maxHeight" .=) <$> _3
        , ("everyNthFrame" .=) <$> _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup StartScreencast where
    StartScreencast _0 _1 _2 _3 _4 <> StartScreencast __0 __1 __2 __3 __4 = StartScreencast (_0 <|> __0) (_1 <|> __1) (_2 <|> __2) (_3 <|> __3) (_4 <|> __4)


------------------------------------------------------------------------------
instance P.Monoid StartScreencast where
    mempty = StartScreencast P.empty P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
instance M.Method StartScreencast where
    type Result StartScreencast = ()
    name _ = "Page.startScreencast"


------------------------------------------------------------------------------
-- | Starts sending each frame using the @screencastFrame@ event.
{-# WARNING startScreencast "This feature is marked as EXPERIMENTAL." #-}
startScreencast
    :: StartScreencast
startScreencast = StartScreencast P.empty P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Force the page stop all navigations and pending resource fetches.
data StopLoading = StopLoading
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StopLoading where
    parseJSON A.Null = P.pure StopLoading
    parseJSON v = A.withArray "stopLoading" go v
        <|> A.withObject "stopLoading" go v
      where
        go _ = P.pure StopLoading


------------------------------------------------------------------------------
instance A.ToJSON StopLoading where
    toEncoding StopLoading = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON StopLoading = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup StopLoading where
    StopLoading <> StopLoading = StopLoading


------------------------------------------------------------------------------
instance P.Monoid StopLoading where
    mempty = StopLoading


------------------------------------------------------------------------------
instance M.Method StopLoading where
    type Result StopLoading = ()
    name _ = "Page.stopLoading"


------------------------------------------------------------------------------
-- | Force the page stop all navigations and pending resource fetches.
stopLoading
    :: StopLoading
stopLoading = StopLoading


------------------------------------------------------------------------------
-- | Crashes renderer on the IO thread, generates minidumps.
{-# WARNING Crash "This feature is marked as EXPERIMENTAL." #-}
data Crash = Crash
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Crash where
    parseJSON A.Null = P.pure Crash
    parseJSON v = A.withArray "crash" go v
        <|> A.withObject "crash" go v
      where
        go _ = P.pure Crash


------------------------------------------------------------------------------
instance A.ToJSON Crash where
    toEncoding Crash = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON Crash = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup Crash where
    Crash <> Crash = Crash


------------------------------------------------------------------------------
instance P.Monoid Crash where
    mempty = Crash


------------------------------------------------------------------------------
instance M.Method Crash where
    type Result Crash = ()
    name _ = "Page.crash"


------------------------------------------------------------------------------
-- | Crashes renderer on the IO thread, generates minidumps.
{-# WARNING crash "This feature is marked as EXPERIMENTAL." #-}
crash
    :: Crash
crash = Crash


------------------------------------------------------------------------------
-- | Tries to close page, running its beforeunload hooks, if any.
{-# WARNING Close "This feature is marked as EXPERIMENTAL." #-}
data Close = Close
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Close where
    parseJSON A.Null = P.pure Close
    parseJSON v = A.withArray "close" go v
        <|> A.withObject "close" go v
      where
        go _ = P.pure Close


------------------------------------------------------------------------------
instance A.ToJSON Close where
    toEncoding Close = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON Close = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup Close where
    Close <> Close = Close


------------------------------------------------------------------------------
instance P.Monoid Close where
    mempty = Close


------------------------------------------------------------------------------
instance M.Method Close where
    type Result Close = ()
    name _ = "Page.close"


------------------------------------------------------------------------------
-- | Tries to close page, running its beforeunload hooks, if any.
{-# WARNING close "This feature is marked as EXPERIMENTAL." #-}
close
    :: Close
close = Close


------------------------------------------------------------------------------
-- | Tries to update the web lifecycle state of the page.
-- It will transition the page to the given state according to:
-- https:\/\/github.com\/WICG\/web-lifecycle\/
{-# WARNING SetWebLifecycleState "This feature is marked as EXPERIMENTAL." #-}
data SetWebLifecycleState = SetWebLifecycleState
    { -- | Target lifecycle state
      state :: !State
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetWebLifecycleState where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setWebLifecycleState" $ \_o -> SetWebLifecycleState
            <$> _o .: "state"
        ago = A.withArray "setWebLifecycleState" $ \_a -> SetWebLifecycleState
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetWebLifecycleState where
    toEncoding (SetWebLifecycleState _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "state" .= _0
        ]
    toJSON (SetWebLifecycleState _0) = A.object $ P.catMaybes
        [ P.pure $ "state" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetWebLifecycleState where
    SetWebLifecycleState _0 <> SetWebLifecycleState _ = SetWebLifecycleState _0


------------------------------------------------------------------------------
data State
    = Frozen
    | Active
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON State where
    parseJSON = A.withText "State" $ \t -> case t of
        "frozen" -> P.pure Frozen
        "active" -> P.pure Active
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON State where
    toJSON Frozen = "frozen"
    toJSON Active = "active"


------------------------------------------------------------------------------
instance M.Method SetWebLifecycleState where
    type Result SetWebLifecycleState = ()
    name _ = "Page.setWebLifecycleState"


------------------------------------------------------------------------------
-- | Tries to update the web lifecycle state of the page.
-- It will transition the page to the given state according to:
-- https:\/\/github.com\/WICG\/web-lifecycle\/
{-# WARNING setWebLifecycleState "This feature is marked as EXPERIMENTAL." #-}
setWebLifecycleState
    :: State
    -- ^ Target lifecycle state

    -> SetWebLifecycleState
setWebLifecycleState _0 = SetWebLifecycleState _0


------------------------------------------------------------------------------
-- | Stops sending each frame in the @screencastFrame@.
{-# WARNING StopScreencast "This feature is marked as EXPERIMENTAL." #-}
data StopScreencast = StopScreencast
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON StopScreencast where
    parseJSON A.Null = P.pure StopScreencast
    parseJSON v = A.withArray "stopScreencast" go v
        <|> A.withObject "stopScreencast" go v
      where
        go _ = P.pure StopScreencast


------------------------------------------------------------------------------
instance A.ToJSON StopScreencast where
    toEncoding StopScreencast = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON StopScreencast = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup StopScreencast where
    StopScreencast <> StopScreencast = StopScreencast


------------------------------------------------------------------------------
instance P.Monoid StopScreencast where
    mempty = StopScreencast


------------------------------------------------------------------------------
instance M.Method StopScreencast where
    type Result StopScreencast = ()
    name _ = "Page.stopScreencast"


------------------------------------------------------------------------------
-- | Stops sending each frame in the @screencastFrame@.
{-# WARNING stopScreencast "This feature is marked as EXPERIMENTAL." #-}
stopScreencast
    :: StopScreencast
stopScreencast = StopScreencast


------------------------------------------------------------------------------
-- | Forces compilation cache to be generated for every subresource script.
{-# WARNING SetProduceCompilationCache "This feature is marked as EXPERIMENTAL." #-}
data SetProduceCompilationCache = SetProduceCompilationCache
    { enabled :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetProduceCompilationCache where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setProduceCompilationCache" $ \_o -> SetProduceCompilationCache
            <$> _o .: "enabled"
        ago = A.withArray "setProduceCompilationCache" $ \_a -> SetProduceCompilationCache
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetProduceCompilationCache where
    toEncoding (SetProduceCompilationCache _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "enabled" .= _0
        ]
    toJSON (SetProduceCompilationCache _0) = A.object $ P.catMaybes
        [ P.pure $ "enabled" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetProduceCompilationCache where
    SetProduceCompilationCache _0 <> SetProduceCompilationCache _ = SetProduceCompilationCache _0


------------------------------------------------------------------------------
instance M.Method SetProduceCompilationCache where
    type Result SetProduceCompilationCache = ()
    name _ = "Page.setProduceCompilationCache"


------------------------------------------------------------------------------
-- | Forces compilation cache to be generated for every subresource script.
{-# WARNING setProduceCompilationCache "This feature is marked as EXPERIMENTAL." #-}
setProduceCompilationCache
    :: P.Bool
    -> SetProduceCompilationCache
setProduceCompilationCache _0 = SetProduceCompilationCache _0


------------------------------------------------------------------------------
-- | Seeds compilation cache for given url. Compilation cache does not survive
-- cross-process navigation.
{-# WARNING AddCompilationCache "This feature is marked as EXPERIMENTAL." #-}
data AddCompilationCache = AddCompilationCache
    { url :: !T.Text
      -- | Base64-encoded data
    , data_ :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AddCompilationCache where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "addCompilationCache" $ \_o -> AddCompilationCache
            <$> _o .: "url"
            <*> _o .: "data"
        ago = A.withArray "addCompilationCache" $ \_a -> AddCompilationCache
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON AddCompilationCache where
    toEncoding (AddCompilationCache _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "url" .= _0
        , P.pure $ "data" .= _1
        ]
    toJSON (AddCompilationCache _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "url" .= _0
        , P.pure $ "data" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup AddCompilationCache where
    AddCompilationCache _0 _1 <> AddCompilationCache _ _ = AddCompilationCache _0 _1


------------------------------------------------------------------------------
instance M.Method AddCompilationCache where
    type Result AddCompilationCache = ()
    name _ = "Page.addCompilationCache"


------------------------------------------------------------------------------
-- | Seeds compilation cache for given url. Compilation cache does not survive
-- cross-process navigation.
{-# WARNING addCompilationCache "This feature is marked as EXPERIMENTAL." #-}
addCompilationCache
    :: T.Text
    -> T.Text
    -- ^ Base64-encoded data

    -> AddCompilationCache
addCompilationCache _0 _1 = AddCompilationCache _0 _1


------------------------------------------------------------------------------
-- | Clears seeded compilation cache.
{-# WARNING ClearCompilationCache "This feature is marked as EXPERIMENTAL." #-}
data ClearCompilationCache = ClearCompilationCache
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ClearCompilationCache where
    parseJSON A.Null = P.pure ClearCompilationCache
    parseJSON v = A.withArray "clearCompilationCache" go v
        <|> A.withObject "clearCompilationCache" go v
      where
        go _ = P.pure ClearCompilationCache


------------------------------------------------------------------------------
instance A.ToJSON ClearCompilationCache where
    toEncoding ClearCompilationCache = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON ClearCompilationCache = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup ClearCompilationCache where
    ClearCompilationCache <> ClearCompilationCache = ClearCompilationCache


------------------------------------------------------------------------------
instance P.Monoid ClearCompilationCache where
    mempty = ClearCompilationCache


------------------------------------------------------------------------------
instance M.Method ClearCompilationCache where
    type Result ClearCompilationCache = ()
    name _ = "Page.clearCompilationCache"


------------------------------------------------------------------------------
-- | Clears seeded compilation cache.
{-# WARNING clearCompilationCache "This feature is marked as EXPERIMENTAL." #-}
clearCompilationCache
    :: ClearCompilationCache
clearCompilationCache = ClearCompilationCache


------------------------------------------------------------------------------
-- | Generates a report for testing.
{-# WARNING GenerateTestReport "This feature is marked as EXPERIMENTAL." #-}
data GenerateTestReport = GenerateTestReport
    { -- | Message to be displayed in the report.
      message :: !T.Text
      -- | Specifies the endpoint group to deliver the report to.
    , group :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GenerateTestReport where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "generateTestReport" $ \_o -> GenerateTestReport
            <$> _o .: "message"
            <*> _o .:? "group"
        ago = A.withArray "generateTestReport" $ \_a -> GenerateTestReport
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON GenerateTestReport where
    toEncoding (GenerateTestReport _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "message" .= _0
        , ("group" .=) <$> _1
        ]
    toJSON (GenerateTestReport _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "message" .= _0
        , ("group" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup GenerateTestReport where
    GenerateTestReport _0 _1 <> GenerateTestReport _ __1 = GenerateTestReport _0 (_1 <|> __1)


------------------------------------------------------------------------------
instance M.Method GenerateTestReport where
    type Result GenerateTestReport = ()
    name _ = "Page.generateTestReport"


------------------------------------------------------------------------------
-- | Generates a report for testing.
{-# WARNING generateTestReport "This feature is marked as EXPERIMENTAL." #-}
generateTestReport
    :: T.Text
    -- ^ Message to be displayed in the report.

    -> GenerateTestReport
generateTestReport _0 = GenerateTestReport _0 P.empty


------------------------------------------------------------------------------
-- | Pauses page execution. Can be resumed using generic Runtime.runIfWaitingForDebugger.
{-# WARNING WaitForDebugger "This feature is marked as EXPERIMENTAL." #-}
data WaitForDebugger = WaitForDebugger
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON WaitForDebugger where
    parseJSON A.Null = P.pure WaitForDebugger
    parseJSON v = A.withArray "waitForDebugger" go v
        <|> A.withObject "waitForDebugger" go v
      where
        go _ = P.pure WaitForDebugger


------------------------------------------------------------------------------
instance A.ToJSON WaitForDebugger where
    toEncoding WaitForDebugger = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON WaitForDebugger = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup WaitForDebugger where
    WaitForDebugger <> WaitForDebugger = WaitForDebugger


------------------------------------------------------------------------------
instance P.Monoid WaitForDebugger where
    mempty = WaitForDebugger


------------------------------------------------------------------------------
instance M.Method WaitForDebugger where
    type Result WaitForDebugger = ()
    name _ = "Page.waitForDebugger"


------------------------------------------------------------------------------
-- | Pauses page execution. Can be resumed using generic Runtime.runIfWaitingForDebugger.
{-# WARNING waitForDebugger "This feature is marked as EXPERIMENTAL." #-}
waitForDebugger
    :: WaitForDebugger
waitForDebugger = WaitForDebugger


------------------------------------------------------------------------------
-- | Intercept file chooser requests and transfer control to protocol clients.
-- When file chooser interception is enabled, native file chooser dialog is not shown.
-- Instead, a protocol event @Page.fileChooserOpened@ is emitted.
-- File chooser can be handled with @page.handleFileChooser@ command.
{-# WARNING SetInterceptFileChooserDialog "This feature is marked as EXPERIMENTAL." #-}
data SetInterceptFileChooserDialog = SetInterceptFileChooserDialog
    { enabled :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetInterceptFileChooserDialog where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setInterceptFileChooserDialog" $ \_o -> SetInterceptFileChooserDialog
            <$> _o .: "enabled"
        ago = A.withArray "setInterceptFileChooserDialog" $ \_a -> SetInterceptFileChooserDialog
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetInterceptFileChooserDialog where
    toEncoding (SetInterceptFileChooserDialog _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "enabled" .= _0
        ]
    toJSON (SetInterceptFileChooserDialog _0) = A.object $ P.catMaybes
        [ P.pure $ "enabled" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetInterceptFileChooserDialog where
    SetInterceptFileChooserDialog _0 <> SetInterceptFileChooserDialog _ = SetInterceptFileChooserDialog _0


------------------------------------------------------------------------------
instance M.Method SetInterceptFileChooserDialog where
    type Result SetInterceptFileChooserDialog = ()
    name _ = "Page.setInterceptFileChooserDialog"


------------------------------------------------------------------------------
-- | Intercept file chooser requests and transfer control to protocol clients.
-- When file chooser interception is enabled, native file chooser dialog is not shown.
-- Instead, a protocol event @Page.fileChooserOpened@ is emitted.
-- File chooser can be handled with @page.handleFileChooser@ command.
{-# WARNING setInterceptFileChooserDialog "This feature is marked as EXPERIMENTAL." #-}
setInterceptFileChooserDialog
    :: P.Bool
    -> SetInterceptFileChooserDialog
setInterceptFileChooserDialog _0 = SetInterceptFileChooserDialog _0


------------------------------------------------------------------------------
-- | Accepts or cancels an intercepted file chooser dialog.
{-# WARNING HandleFileChooser "This feature is marked as EXPERIMENTAL." #-}
data HandleFileChooser = HandleFileChooser
    { action :: !Action
      -- | Array of absolute file paths to set, only respected with @accept@ action.
    , files :: !(P.Maybe [T.Text])
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON HandleFileChooser where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "handleFileChooser" $ \_o -> HandleFileChooser
            <$> _o .: "action"
            <*> _o .:? "files"
        ago = A.withArray "handleFileChooser" $ \_a -> HandleFileChooser
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON HandleFileChooser where
    toEncoding (HandleFileChooser _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "action" .= _0
        , ("files" .=) <$> _1
        ]
    toJSON (HandleFileChooser _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "action" .= _0
        , ("files" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup HandleFileChooser where
    HandleFileChooser _0 _1 <> HandleFileChooser _ __1 = HandleFileChooser _0 (_1 <|> __1)


------------------------------------------------------------------------------
data Action
    = Accept
    | Cancel
    | Fallback
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Action where
    parseJSON = A.withText "Action" $ \t -> case t of
        "accept" -> P.pure Accept
        "cancel" -> P.pure Cancel
        "fallback" -> P.pure Fallback
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Action where
    toJSON Accept = "accept"
    toJSON Cancel = "cancel"
    toJSON Fallback = "fallback"


------------------------------------------------------------------------------
instance M.Method HandleFileChooser where
    type Result HandleFileChooser = ()
    name _ = "Page.handleFileChooser"


------------------------------------------------------------------------------
-- | Accepts or cancels an intercepted file chooser dialog.
{-# WARNING handleFileChooser "This feature is marked as EXPERIMENTAL." #-}
handleFileChooser
    :: Action
    -> HandleFileChooser
handleFileChooser _0 = HandleFileChooser _0 P.empty


------------------------------------------------------------------------------
data DomContentEventFired = DomContentEventFired
    { timestamp :: !Network.MonotonicTime
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DomContentEventFired where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "domContentEventFired" $ \_o -> DomContentEventFired
            <$> _o .: "timestamp"
        ago = A.withArray "domContentEventFired" $ \_a -> DomContentEventFired
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON DomContentEventFired where
    toEncoding (DomContentEventFired _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "timestamp" .= _0
        ]
    toJSON (DomContentEventFired _0) = A.object $ P.catMaybes
        [ P.pure $ "timestamp" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup DomContentEventFired where
    DomContentEventFired _0 <> DomContentEventFired _ = DomContentEventFired _0


------------------------------------------------------------------------------
instance E.Event DomContentEventFired where
    type Result DomContentEventFired = DomContentEventFired
    name _ = "Page.domContentEventFired"


------------------------------------------------------------------------------
domContentEventFired :: P.Proxy DomContentEventFired
domContentEventFired = P.Proxy


------------------------------------------------------------------------------
-- | Emitted only when @page.interceptFileChooser@ is enabled.
data FileChooserOpened = FileChooserOpened
    { mode :: !Mode
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON FileChooserOpened where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "fileChooserOpened" $ \_o -> FileChooserOpened
            <$> _o .: "mode"
        ago = A.withArray "fileChooserOpened" $ \_a -> FileChooserOpened
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON FileChooserOpened where
    toEncoding (FileChooserOpened _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "mode" .= _0
        ]
    toJSON (FileChooserOpened _0) = A.object $ P.catMaybes
        [ P.pure $ "mode" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup FileChooserOpened where
    FileChooserOpened _0 <> FileChooserOpened _ = FileChooserOpened _0


------------------------------------------------------------------------------
data Mode
    = SelectSingle
    | SelectMultiple
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Mode where
    parseJSON = A.withText "Mode" $ \t -> case t of
        "selectSingle" -> P.pure SelectSingle
        "selectMultiple" -> P.pure SelectMultiple
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON Mode where
    toJSON SelectSingle = "selectSingle"
    toJSON SelectMultiple = "selectMultiple"


------------------------------------------------------------------------------
instance E.Event FileChooserOpened where
    type Result FileChooserOpened = FileChooserOpened
    name _ = "Page.fileChooserOpened"


------------------------------------------------------------------------------
-- | Emitted only when @page.interceptFileChooser@ is enabled.
fileChooserOpened :: P.Proxy FileChooserOpened
fileChooserOpened = P.Proxy


------------------------------------------------------------------------------
-- | Fired when frame has been attached to its parent.
data FrameAttached = FrameAttached
    { -- | Id of the frame that has been attached.
      frameId :: !FrameId
      -- | Parent frame identifier.
    , parentFrameId :: !FrameId
      -- | JavaScript stack trace of when frame was attached, only set if frame initiated from script.
    , stack :: !(P.Maybe Runtime.StackTrace)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON FrameAttached where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "frameAttached" $ \_o -> FrameAttached
            <$> _o .: "frameId"
            <*> _o .: "parentFrameId"
            <*> _o .:? "stack"
        ago = A.withArray "frameAttached" $ \_a -> FrameAttached
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON FrameAttached where
    toEncoding (FrameAttached _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        , P.pure $ "parentFrameId" .= _1
        , ("stack" .=) <$> _2
        ]
    toJSON (FrameAttached _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        , P.pure $ "parentFrameId" .= _1
        , ("stack" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup FrameAttached where
    FrameAttached _0 _1 _2 <> FrameAttached _ _ __2 = FrameAttached _0 _1 (_2 <|> __2)


------------------------------------------------------------------------------
instance E.Event FrameAttached where
    type Result FrameAttached = FrameAttached
    name _ = "Page.frameAttached"


------------------------------------------------------------------------------
-- | Fired when frame has been attached to its parent.
frameAttached :: P.Proxy FrameAttached
frameAttached = P.Proxy


------------------------------------------------------------------------------
-- | Fired when frame no longer has a scheduled navigation.
{-# DEPRECATED FrameClearedScheduledNavigation "This may be removed in a future release." #-}
data FrameClearedScheduledNavigation = FrameClearedScheduledNavigation
    { -- | Id of the frame that has cleared its scheduled navigation.
      frameId :: !FrameId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON FrameClearedScheduledNavigation where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "frameClearedScheduledNavigation" $ \_o -> FrameClearedScheduledNavigation
            <$> _o .: "frameId"
        ago = A.withArray "frameClearedScheduledNavigation" $ \_a -> FrameClearedScheduledNavigation
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON FrameClearedScheduledNavigation where
    toEncoding (FrameClearedScheduledNavigation _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        ]
    toJSON (FrameClearedScheduledNavigation _0) = A.object $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup FrameClearedScheduledNavigation where
    FrameClearedScheduledNavigation _0 <> FrameClearedScheduledNavigation _ = FrameClearedScheduledNavigation _0


------------------------------------------------------------------------------
instance E.Event FrameClearedScheduledNavigation where
    type Result FrameClearedScheduledNavigation = FrameClearedScheduledNavigation
    name _ = "Page.frameClearedScheduledNavigation"


------------------------------------------------------------------------------
-- | Fired when frame no longer has a scheduled navigation.
{-# DEPRECATED frameClearedScheduledNavigation "This may be removed in a future release." #-}
frameClearedScheduledNavigation :: P.Proxy FrameClearedScheduledNavigation
frameClearedScheduledNavigation = P.Proxy


------------------------------------------------------------------------------
-- | Fired when frame has been detached from its parent.
data FrameDetached = FrameDetached
    { -- | Id of the frame that has been detached.
      frameId :: !FrameId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON FrameDetached where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "frameDetached" $ \_o -> FrameDetached
            <$> _o .: "frameId"
        ago = A.withArray "frameDetached" $ \_a -> FrameDetached
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON FrameDetached where
    toEncoding (FrameDetached _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        ]
    toJSON (FrameDetached _0) = A.object $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup FrameDetached where
    FrameDetached _0 <> FrameDetached _ = FrameDetached _0


------------------------------------------------------------------------------
instance E.Event FrameDetached where
    type Result FrameDetached = FrameDetached
    name _ = "Page.frameDetached"


------------------------------------------------------------------------------
-- | Fired when frame has been detached from its parent.
frameDetached :: P.Proxy FrameDetached
frameDetached = P.Proxy


------------------------------------------------------------------------------
-- | Fired once navigation of the frame has completed. Frame is now associated with the new loader.
data FrameNavigated = FrameNavigated
    { -- | Frame object.
      frame :: !Frame
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON FrameNavigated where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "frameNavigated" $ \_o -> FrameNavigated
            <$> _o .: "frame"
        ago = A.withArray "frameNavigated" $ \_a -> FrameNavigated
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON FrameNavigated where
    toEncoding (FrameNavigated _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "frame" .= _0
        ]
    toJSON (FrameNavigated _0) = A.object $ P.catMaybes
        [ P.pure $ "frame" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup FrameNavigated where
    FrameNavigated _0 <> FrameNavigated _ = FrameNavigated _0


------------------------------------------------------------------------------
instance E.Event FrameNavigated where
    type Result FrameNavigated = FrameNavigated
    name _ = "Page.frameNavigated"


------------------------------------------------------------------------------
-- | Fired once navigation of the frame has completed. Frame is now associated with the new loader.
frameNavigated :: P.Proxy FrameNavigated
frameNavigated = P.Proxy


------------------------------------------------------------------------------
{-# WARNING FrameResized "This feature is marked as EXPERIMENTAL." #-}
data FrameResized = FrameResized
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON FrameResized where
    parseJSON A.Null = P.pure FrameResized
    parseJSON v = A.withArray "frameResized" go v
        <|> A.withObject "frameResized" go v
      where
        go _ = P.pure FrameResized


------------------------------------------------------------------------------
instance A.ToJSON FrameResized where
    toEncoding FrameResized = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON FrameResized = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup FrameResized where
    FrameResized <> FrameResized = FrameResized


------------------------------------------------------------------------------
instance P.Monoid FrameResized where
    mempty = FrameResized


------------------------------------------------------------------------------
instance E.Event FrameResized where
    type Result FrameResized = ()
    name _ = "Page.frameResized"


------------------------------------------------------------------------------
{-# WARNING frameResized "This feature is marked as EXPERIMENTAL." #-}
frameResized :: P.Proxy FrameResized
frameResized = P.Proxy


------------------------------------------------------------------------------
-- | Fired when a renderer-initiated navigation is requested.
-- Navigation may still be cancelled after the event is issued.
{-# WARNING FrameRequestedNavigation "This feature is marked as EXPERIMENTAL." #-}
data FrameRequestedNavigation = FrameRequestedNavigation
    { -- | Id of the frame that is being navigated.
      frameId :: !FrameId
      -- | The reason for the navigation.
    , reason :: !ClientNavigationReason
      -- | The destination URL for the requested navigation.
    , url :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON FrameRequestedNavigation where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "frameRequestedNavigation" $ \_o -> FrameRequestedNavigation
            <$> _o .: "frameId"
            <*> _o .: "reason"
            <*> _o .: "url"
        ago = A.withArray "frameRequestedNavigation" $ \_a -> FrameRequestedNavigation
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON FrameRequestedNavigation where
    toEncoding (FrameRequestedNavigation _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        , P.pure $ "reason" .= _1
        , P.pure $ "url" .= _2
        ]
    toJSON (FrameRequestedNavigation _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        , P.pure $ "reason" .= _1
        , P.pure $ "url" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup FrameRequestedNavigation where
    FrameRequestedNavigation _0 _1 _2 <> FrameRequestedNavigation _ _ _ = FrameRequestedNavigation _0 _1 _2


------------------------------------------------------------------------------
instance E.Event FrameRequestedNavigation where
    type Result FrameRequestedNavigation = FrameRequestedNavigation
    name _ = "Page.frameRequestedNavigation"


------------------------------------------------------------------------------
-- | Fired when a renderer-initiated navigation is requested.
-- Navigation may still be cancelled after the event is issued.
{-# WARNING frameRequestedNavigation "This feature is marked as EXPERIMENTAL." #-}
frameRequestedNavigation :: P.Proxy FrameRequestedNavigation
frameRequestedNavigation = P.Proxy


------------------------------------------------------------------------------
-- | Fired when frame schedules a potential navigation.
{-# DEPRECATED FrameScheduledNavigation "This may be removed in a future release." #-}
data FrameScheduledNavigation = FrameScheduledNavigation
    { -- | Id of the frame that has scheduled a navigation.
      frameId :: !FrameId
      -- | Delay (in seconds) until the navigation is scheduled to begin. The navigation is not
      -- guaranteed to start.
    , delay :: !P.Double
      -- | The reason for the navigation.
    , reason :: !ClientNavigationReason
      -- | The destination URL for the scheduled navigation.
    , url :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON FrameScheduledNavigation where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "frameScheduledNavigation" $ \_o -> FrameScheduledNavigation
            <$> _o .: "frameId"
            <*> _o .: "delay"
            <*> _o .: "reason"
            <*> _o .: "url"
        ago = A.withArray "frameScheduledNavigation" $ \_a -> FrameScheduledNavigation
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON FrameScheduledNavigation where
    toEncoding (FrameScheduledNavigation _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        , P.pure $ "delay" .= _1
        , P.pure $ "reason" .= _2
        , P.pure $ "url" .= _3
        ]
    toJSON (FrameScheduledNavigation _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        , P.pure $ "delay" .= _1
        , P.pure $ "reason" .= _2
        , P.pure $ "url" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup FrameScheduledNavigation where
    FrameScheduledNavigation _0 _1 _2 _3 <> FrameScheduledNavigation _ _ _ _ = FrameScheduledNavigation _0 _1 _2 _3


------------------------------------------------------------------------------
instance E.Event FrameScheduledNavigation where
    type Result FrameScheduledNavigation = FrameScheduledNavigation
    name _ = "Page.frameScheduledNavigation"


------------------------------------------------------------------------------
-- | Fired when frame schedules a potential navigation.
{-# DEPRECATED frameScheduledNavigation "This may be removed in a future release." #-}
frameScheduledNavigation :: P.Proxy FrameScheduledNavigation
frameScheduledNavigation = P.Proxy


------------------------------------------------------------------------------
-- | Fired when frame has started loading.
{-# WARNING FrameStartedLoading "This feature is marked as EXPERIMENTAL." #-}
data FrameStartedLoading = FrameStartedLoading
    { -- | Id of the frame that has started loading.
      frameId :: !FrameId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON FrameStartedLoading where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "frameStartedLoading" $ \_o -> FrameStartedLoading
            <$> _o .: "frameId"
        ago = A.withArray "frameStartedLoading" $ \_a -> FrameStartedLoading
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON FrameStartedLoading where
    toEncoding (FrameStartedLoading _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        ]
    toJSON (FrameStartedLoading _0) = A.object $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup FrameStartedLoading where
    FrameStartedLoading _0 <> FrameStartedLoading _ = FrameStartedLoading _0


------------------------------------------------------------------------------
instance E.Event FrameStartedLoading where
    type Result FrameStartedLoading = FrameStartedLoading
    name _ = "Page.frameStartedLoading"


------------------------------------------------------------------------------
-- | Fired when frame has started loading.
{-# WARNING frameStartedLoading "This feature is marked as EXPERIMENTAL." #-}
frameStartedLoading :: P.Proxy FrameStartedLoading
frameStartedLoading = P.Proxy


------------------------------------------------------------------------------
-- | Fired when frame has stopped loading.
{-# WARNING FrameStoppedLoading "This feature is marked as EXPERIMENTAL." #-}
data FrameStoppedLoading = FrameStoppedLoading
    { -- | Id of the frame that has stopped loading.
      frameId :: !FrameId
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON FrameStoppedLoading where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "frameStoppedLoading" $ \_o -> FrameStoppedLoading
            <$> _o .: "frameId"
        ago = A.withArray "frameStoppedLoading" $ \_a -> FrameStoppedLoading
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON FrameStoppedLoading where
    toEncoding (FrameStoppedLoading _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        ]
    toJSON (FrameStoppedLoading _0) = A.object $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup FrameStoppedLoading where
    FrameStoppedLoading _0 <> FrameStoppedLoading _ = FrameStoppedLoading _0


------------------------------------------------------------------------------
instance E.Event FrameStoppedLoading where
    type Result FrameStoppedLoading = FrameStoppedLoading
    name _ = "Page.frameStoppedLoading"


------------------------------------------------------------------------------
-- | Fired when frame has stopped loading.
{-# WARNING frameStoppedLoading "This feature is marked as EXPERIMENTAL." #-}
frameStoppedLoading :: P.Proxy FrameStoppedLoading
frameStoppedLoading = P.Proxy


------------------------------------------------------------------------------
-- | Fired when page is about to start a download.
{-# WARNING DownloadWillBegin "This feature is marked as EXPERIMENTAL." #-}
data DownloadWillBegin = DownloadWillBegin
    { -- | Id of the frame that caused download to begin.
      frameId :: !FrameId
      -- | URL of the resource being downloaded.
    , url :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DownloadWillBegin where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "downloadWillBegin" $ \_o -> DownloadWillBegin
            <$> _o .: "frameId"
            <*> _o .: "url"
        ago = A.withArray "downloadWillBegin" $ \_a -> DownloadWillBegin
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON DownloadWillBegin where
    toEncoding (DownloadWillBegin _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        , P.pure $ "url" .= _1
        ]
    toJSON (DownloadWillBegin _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        , P.pure $ "url" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup DownloadWillBegin where
    DownloadWillBegin _0 _1 <> DownloadWillBegin _ _ = DownloadWillBegin _0 _1


------------------------------------------------------------------------------
instance E.Event DownloadWillBegin where
    type Result DownloadWillBegin = DownloadWillBegin
    name _ = "Page.downloadWillBegin"


------------------------------------------------------------------------------
-- | Fired when page is about to start a download.
{-# WARNING downloadWillBegin "This feature is marked as EXPERIMENTAL." #-}
downloadWillBegin :: P.Proxy DownloadWillBegin
downloadWillBegin = P.Proxy


------------------------------------------------------------------------------
-- | Fired when interstitial page was hidden
data InterstitialHidden = InterstitialHidden
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON InterstitialHidden where
    parseJSON A.Null = P.pure InterstitialHidden
    parseJSON v = A.withArray "interstitialHidden" go v
        <|> A.withObject "interstitialHidden" go v
      where
        go _ = P.pure InterstitialHidden


------------------------------------------------------------------------------
instance A.ToJSON InterstitialHidden where
    toEncoding InterstitialHidden = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON InterstitialHidden = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup InterstitialHidden where
    InterstitialHidden <> InterstitialHidden = InterstitialHidden


------------------------------------------------------------------------------
instance P.Monoid InterstitialHidden where
    mempty = InterstitialHidden


------------------------------------------------------------------------------
instance E.Event InterstitialHidden where
    type Result InterstitialHidden = ()
    name _ = "Page.interstitialHidden"


------------------------------------------------------------------------------
-- | Fired when interstitial page was hidden
interstitialHidden :: P.Proxy InterstitialHidden
interstitialHidden = P.Proxy


------------------------------------------------------------------------------
-- | Fired when interstitial page was shown
data InterstitialShown = InterstitialShown
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON InterstitialShown where
    parseJSON A.Null = P.pure InterstitialShown
    parseJSON v = A.withArray "interstitialShown" go v
        <|> A.withObject "interstitialShown" go v
      where
        go _ = P.pure InterstitialShown


------------------------------------------------------------------------------
instance A.ToJSON InterstitialShown where
    toEncoding InterstitialShown = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON InterstitialShown = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup InterstitialShown where
    InterstitialShown <> InterstitialShown = InterstitialShown


------------------------------------------------------------------------------
instance P.Monoid InterstitialShown where
    mempty = InterstitialShown


------------------------------------------------------------------------------
instance E.Event InterstitialShown where
    type Result InterstitialShown = ()
    name _ = "Page.interstitialShown"


------------------------------------------------------------------------------
-- | Fired when interstitial page was shown
interstitialShown :: P.Proxy InterstitialShown
interstitialShown = P.Proxy


------------------------------------------------------------------------------
-- | Fired when a JavaScript initiated dialog (alert, confirm, prompt, or onbeforeunload) has been
-- closed.
data JavascriptDialogClosed = JavascriptDialogClosed
    { -- | Whether dialog was confirmed.
      result :: !P.Bool
      -- | User input in case of prompt.
    , userInput :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON JavascriptDialogClosed where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "javascriptDialogClosed" $ \_o -> JavascriptDialogClosed
            <$> _o .: "result"
            <*> _o .: "userInput"
        ago = A.withArray "javascriptDialogClosed" $ \_a -> JavascriptDialogClosed
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON JavascriptDialogClosed where
    toEncoding (JavascriptDialogClosed _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "result" .= _0
        , P.pure $ "userInput" .= _1
        ]
    toJSON (JavascriptDialogClosed _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "result" .= _0
        , P.pure $ "userInput" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup JavascriptDialogClosed where
    JavascriptDialogClosed _0 _1 <> JavascriptDialogClosed _ _ = JavascriptDialogClosed _0 _1


------------------------------------------------------------------------------
instance E.Event JavascriptDialogClosed where
    type Result JavascriptDialogClosed = JavascriptDialogClosed
    name _ = "Page.javascriptDialogClosed"


------------------------------------------------------------------------------
-- | Fired when a JavaScript initiated dialog (alert, confirm, prompt, or onbeforeunload) has been
-- closed.
javascriptDialogClosed :: P.Proxy JavascriptDialogClosed
javascriptDialogClosed = P.Proxy


------------------------------------------------------------------------------
-- | Fired when a JavaScript initiated dialog (alert, confirm, prompt, or onbeforeunload) is about to
-- open.
data JavascriptDialogOpening = JavascriptDialogOpening
    { -- | Frame url.
      url :: !T.Text
      -- | Message that will be displayed by the dialog.
    , message :: !T.Text
      -- | Dialog type.
    , type_ :: !DialogType
      -- | True iff browser is capable showing or acting on the given dialog. When browser has no
      -- dialog handler for given target, calling alert while Page domain is engaged will stall
      -- the page execution. Execution can be resumed via calling Page.handleJavaScriptDialog.
    , hasBrowserHandler :: !P.Bool
      -- | Default dialog prompt.
    , defaultPrompt :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON JavascriptDialogOpening where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "javascriptDialogOpening" $ \_o -> JavascriptDialogOpening
            <$> _o .: "url"
            <*> _o .: "message"
            <*> _o .: "type"
            <*> _o .: "hasBrowserHandler"
            <*> _o .:? "defaultPrompt"
        ago = A.withArray "javascriptDialogOpening" $ \_a -> JavascriptDialogOpening
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON JavascriptDialogOpening where
    toEncoding (JavascriptDialogOpening _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "url" .= _0
        , P.pure $ "message" .= _1
        , P.pure $ "type" .= _2
        , P.pure $ "hasBrowserHandler" .= _3
        , ("defaultPrompt" .=) <$> _4
        ]
    toJSON (JavascriptDialogOpening _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ P.pure $ "url" .= _0
        , P.pure $ "message" .= _1
        , P.pure $ "type" .= _2
        , P.pure $ "hasBrowserHandler" .= _3
        , ("defaultPrompt" .=) <$> _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup JavascriptDialogOpening where
    JavascriptDialogOpening _0 _1 _2 _3 _4 <> JavascriptDialogOpening _ _ _ _ __4 = JavascriptDialogOpening _0 _1 _2 _3 (_4 <|> __4)


------------------------------------------------------------------------------
instance E.Event JavascriptDialogOpening where
    type Result JavascriptDialogOpening = JavascriptDialogOpening
    name _ = "Page.javascriptDialogOpening"


------------------------------------------------------------------------------
-- | Fired when a JavaScript initiated dialog (alert, confirm, prompt, or onbeforeunload) is about to
-- open.
javascriptDialogOpening :: P.Proxy JavascriptDialogOpening
javascriptDialogOpening = P.Proxy


------------------------------------------------------------------------------
-- | Fired for top level page lifecycle events such as navigation, load, paint, etc.
data LifecycleEvent = LifecycleEvent
    { -- | Id of the frame.
      frameId :: !FrameId
      -- | Loader identifier. Empty string if the request is fetched from worker.
    , loaderId :: !Network.LoaderId
    , name :: !T.Text
    , timestamp :: !Network.MonotonicTime
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON LifecycleEvent where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "lifecycleEvent" $ \_o -> LifecycleEvent
            <$> _o .: "frameId"
            <*> _o .: "loaderId"
            <*> _o .: "name"
            <*> _o .: "timestamp"
        ago = A.withArray "lifecycleEvent" $ \_a -> LifecycleEvent
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON LifecycleEvent where
    toEncoding (LifecycleEvent _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        , P.pure $ "loaderId" .= _1
        , P.pure $ "name" .= _2
        , P.pure $ "timestamp" .= _3
        ]
    toJSON (LifecycleEvent _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        , P.pure $ "loaderId" .= _1
        , P.pure $ "name" .= _2
        , P.pure $ "timestamp" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup LifecycleEvent where
    LifecycleEvent _0 _1 _2 _3 <> LifecycleEvent _ _ _ _ = LifecycleEvent _0 _1 _2 _3


------------------------------------------------------------------------------
instance E.Event LifecycleEvent where
    type Result LifecycleEvent = LifecycleEvent
    name _ = "Page.lifecycleEvent"


------------------------------------------------------------------------------
-- | Fired for top level page lifecycle events such as navigation, load, paint, etc.
lifecycleEvent :: P.Proxy LifecycleEvent
lifecycleEvent = P.Proxy


------------------------------------------------------------------------------
data LoadEventFired = LoadEventFired
    { timestamp :: !Network.MonotonicTime
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON LoadEventFired where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "loadEventFired" $ \_o -> LoadEventFired
            <$> _o .: "timestamp"
        ago = A.withArray "loadEventFired" $ \_a -> LoadEventFired
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON LoadEventFired where
    toEncoding (LoadEventFired _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "timestamp" .= _0
        ]
    toJSON (LoadEventFired _0) = A.object $ P.catMaybes
        [ P.pure $ "timestamp" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup LoadEventFired where
    LoadEventFired _0 <> LoadEventFired _ = LoadEventFired _0


------------------------------------------------------------------------------
instance E.Event LoadEventFired where
    type Result LoadEventFired = LoadEventFired
    name _ = "Page.loadEventFired"


------------------------------------------------------------------------------
loadEventFired :: P.Proxy LoadEventFired
loadEventFired = P.Proxy


------------------------------------------------------------------------------
-- | Fired when same-document navigation happens, e.g. due to history API usage or anchor navigation.
{-# WARNING NavigatedWithinDocument "This feature is marked as EXPERIMENTAL." #-}
data NavigatedWithinDocument = NavigatedWithinDocument
    { -- | Id of the frame.
      frameId :: !FrameId
      -- | Frame's new url.
    , url :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON NavigatedWithinDocument where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "navigatedWithinDocument" $ \_o -> NavigatedWithinDocument
            <$> _o .: "frameId"
            <*> _o .: "url"
        ago = A.withArray "navigatedWithinDocument" $ \_a -> NavigatedWithinDocument
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON NavigatedWithinDocument where
    toEncoding (NavigatedWithinDocument _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        , P.pure $ "url" .= _1
        ]
    toJSON (NavigatedWithinDocument _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "frameId" .= _0
        , P.pure $ "url" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup NavigatedWithinDocument where
    NavigatedWithinDocument _0 _1 <> NavigatedWithinDocument _ _ = NavigatedWithinDocument _0 _1


------------------------------------------------------------------------------
instance E.Event NavigatedWithinDocument where
    type Result NavigatedWithinDocument = NavigatedWithinDocument
    name _ = "Page.navigatedWithinDocument"


------------------------------------------------------------------------------
-- | Fired when same-document navigation happens, e.g. due to history API usage or anchor navigation.
{-# WARNING navigatedWithinDocument "This feature is marked as EXPERIMENTAL." #-}
navigatedWithinDocument :: P.Proxy NavigatedWithinDocument
navigatedWithinDocument = P.Proxy


------------------------------------------------------------------------------
-- | Compressed image data requested by the @startScreencast@.
{-# WARNING ScreencastFrame "This feature is marked as EXPERIMENTAL." #-}
data ScreencastFrame = ScreencastFrame
    { -- | Base64-encoded compressed image.
      data_ :: !T.Text
      -- | Screencast frame metadata.
    , metadata :: !ScreencastFrameMetadata
      -- | Frame number.
    , sessionId :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ScreencastFrame where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "screencastFrame" $ \_o -> ScreencastFrame
            <$> _o .: "data"
            <*> _o .: "metadata"
            <*> _o .: "sessionId"
        ago = A.withArray "screencastFrame" $ \_a -> ScreencastFrame
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON ScreencastFrame where
    toEncoding (ScreencastFrame _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "data" .= _0
        , P.pure $ "metadata" .= _1
        , P.pure $ "sessionId" .= _2
        ]
    toJSON (ScreencastFrame _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "data" .= _0
        , P.pure $ "metadata" .= _1
        , P.pure $ "sessionId" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup ScreencastFrame where
    ScreencastFrame _0 _1 _2 <> ScreencastFrame _ _ _ = ScreencastFrame _0 _1 _2


------------------------------------------------------------------------------
instance E.Event ScreencastFrame where
    type Result ScreencastFrame = ScreencastFrame
    name _ = "Page.screencastFrame"


------------------------------------------------------------------------------
-- | Compressed image data requested by the @startScreencast@.
{-# WARNING screencastFrame "This feature is marked as EXPERIMENTAL." #-}
screencastFrame :: P.Proxy ScreencastFrame
screencastFrame = P.Proxy


------------------------------------------------------------------------------
-- | Fired when the page with currently enabled screencast was shown or hidden @.
{-# WARNING ScreencastVisibilityChanged "This feature is marked as EXPERIMENTAL." #-}
data ScreencastVisibilityChanged = ScreencastVisibilityChanged
    { -- | True if the page is visible.
      visible :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ScreencastVisibilityChanged where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "screencastVisibilityChanged" $ \_o -> ScreencastVisibilityChanged
            <$> _o .: "visible"
        ago = A.withArray "screencastVisibilityChanged" $ \_a -> ScreencastVisibilityChanged
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON ScreencastVisibilityChanged where
    toEncoding (ScreencastVisibilityChanged _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "visible" .= _0
        ]
    toJSON (ScreencastVisibilityChanged _0) = A.object $ P.catMaybes
        [ P.pure $ "visible" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup ScreencastVisibilityChanged where
    ScreencastVisibilityChanged _0 <> ScreencastVisibilityChanged _ = ScreencastVisibilityChanged _0


------------------------------------------------------------------------------
instance E.Event ScreencastVisibilityChanged where
    type Result ScreencastVisibilityChanged = ScreencastVisibilityChanged
    name _ = "Page.screencastVisibilityChanged"


------------------------------------------------------------------------------
-- | Fired when the page with currently enabled screencast was shown or hidden @.
{-# WARNING screencastVisibilityChanged "This feature is marked as EXPERIMENTAL." #-}
screencastVisibilityChanged :: P.Proxy ScreencastVisibilityChanged
screencastVisibilityChanged = P.Proxy


------------------------------------------------------------------------------
-- | Fired when a new window is going to be opened, via window.open(), link click, form submission,
-- etc.
data WindowOpen = WindowOpen
    { -- | The URL for the new window.
      url :: !T.Text
      -- | Window name.
    , windowName :: !T.Text
      -- | An array of enabled window features.
    , windowFeatures :: ![T.Text]
      -- | Whether or not it was triggered by user gesture.
    , userGesture :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON WindowOpen where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "windowOpen" $ \_o -> WindowOpen
            <$> _o .: "url"
            <*> _o .: "windowName"
            <*> _o .: "windowFeatures"
            <*> _o .: "userGesture"
        ago = A.withArray "windowOpen" $ \_a -> WindowOpen
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON WindowOpen where
    toEncoding (WindowOpen _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "url" .= _0
        , P.pure $ "windowName" .= _1
        , P.pure $ "windowFeatures" .= _2
        , P.pure $ "userGesture" .= _3
        ]
    toJSON (WindowOpen _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "url" .= _0
        , P.pure $ "windowName" .= _1
        , P.pure $ "windowFeatures" .= _2
        , P.pure $ "userGesture" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup WindowOpen where
    WindowOpen _0 _1 _2 _3 <> WindowOpen _ _ _ _ = WindowOpen _0 _1 _2 _3


------------------------------------------------------------------------------
instance E.Event WindowOpen where
    type Result WindowOpen = WindowOpen
    name _ = "Page.windowOpen"


------------------------------------------------------------------------------
-- | Fired when a new window is going to be opened, via window.open(), link click, form submission,
-- etc.
windowOpen :: P.Proxy WindowOpen
windowOpen = P.Proxy


------------------------------------------------------------------------------
-- | Issued for every compilation cache generated. Is only available
-- if Page.setGenerateCompilationCache is enabled.
{-# WARNING CompilationCacheProduced "This feature is marked as EXPERIMENTAL." #-}
data CompilationCacheProduced = CompilationCacheProduced
    { url :: !T.Text
      -- | Base64-encoded data
    , data_ :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CompilationCacheProduced where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "compilationCacheProduced" $ \_o -> CompilationCacheProduced
            <$> _o .: "url"
            <*> _o .: "data"
        ago = A.withArray "compilationCacheProduced" $ \_a -> CompilationCacheProduced
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON CompilationCacheProduced where
    toEncoding (CompilationCacheProduced _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "url" .= _0
        , P.pure $ "data" .= _1
        ]
    toJSON (CompilationCacheProduced _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "url" .= _0
        , P.pure $ "data" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup CompilationCacheProduced where
    CompilationCacheProduced _0 _1 <> CompilationCacheProduced _ _ = CompilationCacheProduced _0 _1


------------------------------------------------------------------------------
instance E.Event CompilationCacheProduced where
    type Result CompilationCacheProduced = CompilationCacheProduced
    name _ = "Page.compilationCacheProduced"


------------------------------------------------------------------------------
-- | Issued for every compilation cache generated. Is only available
-- if Page.setGenerateCompilationCache is enabled.
{-# WARNING compilationCacheProduced "This feature is marked as EXPERIMENTAL." #-}
compilationCacheProduced :: P.Proxy CompilationCacheProduced
compilationCacheProduced = P.Proxy

