{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | This domain emulates different environments for the page.
module DevTools.API.Emulation
    ( module DevTools.API.Emulation.Types
    , module DevTools.API.Emulation
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
import           DevTools.API.Emulation.Types
import qualified DevTools.API.Network.Types as Network
import qualified DevTools.API.Page.Types as Page


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Tells whether emulation is supported.
data CanEmulate = CanEmulate
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CanEmulate where
    parseJSON A.Null = P.pure CanEmulate
    parseJSON v = A.withArray "canEmulate" go v
        <|> A.withObject "canEmulate" go v
      where
        go _ = P.pure CanEmulate


------------------------------------------------------------------------------
instance A.ToJSON CanEmulate where
    toEncoding CanEmulate = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON CanEmulate = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup CanEmulate where
    CanEmulate <> CanEmulate = CanEmulate


------------------------------------------------------------------------------
instance P.Monoid CanEmulate where
    mempty = CanEmulate


------------------------------------------------------------------------------
-- | Tells whether emulation is supported.
data CanEmulateResult = CanEmulateResult
    { -- | True if emulation is supported.
      result :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CanEmulateResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "canEmulateResult" $ \_o -> CanEmulateResult
            <$> _o .: "result"
        ago = A.withArray "canEmulateResult" $ \_a -> CanEmulateResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON CanEmulateResult where
    toEncoding (CanEmulateResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "result" .= _0
        ]
    toJSON (CanEmulateResult _0) = A.object $ P.catMaybes
        [ P.pure $ "result" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup CanEmulateResult where
    CanEmulateResult _0 <> CanEmulateResult _ = CanEmulateResult _0


------------------------------------------------------------------------------
instance M.Method CanEmulate where
    type Result CanEmulate = CanEmulateResult
    name _ = "Emulation.canEmulate"


------------------------------------------------------------------------------
-- | Tells whether emulation is supported.
canEmulate
    :: CanEmulate
canEmulate = CanEmulate


------------------------------------------------------------------------------
-- | Clears the overriden device metrics.
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
    name _ = "Emulation.clearDeviceMetricsOverride"


------------------------------------------------------------------------------
-- | Clears the overriden device metrics.
clearDeviceMetricsOverride
    :: ClearDeviceMetricsOverride
clearDeviceMetricsOverride = ClearDeviceMetricsOverride


------------------------------------------------------------------------------
-- | Clears the overriden Geolocation Position and Error.
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
    name _ = "Emulation.clearGeolocationOverride"


------------------------------------------------------------------------------
-- | Clears the overriden Geolocation Position and Error.
clearGeolocationOverride
    :: ClearGeolocationOverride
clearGeolocationOverride = ClearGeolocationOverride


------------------------------------------------------------------------------
-- | Requests that page scale factor is reset to initial values.
{-# WARNING ResetPageScaleFactor "This feature is marked as EXPERIMENTAL." #-}
data ResetPageScaleFactor = ResetPageScaleFactor
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ResetPageScaleFactor where
    parseJSON A.Null = P.pure ResetPageScaleFactor
    parseJSON v = A.withArray "resetPageScaleFactor" go v
        <|> A.withObject "resetPageScaleFactor" go v
      where
        go _ = P.pure ResetPageScaleFactor


------------------------------------------------------------------------------
instance A.ToJSON ResetPageScaleFactor where
    toEncoding ResetPageScaleFactor = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON ResetPageScaleFactor = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup ResetPageScaleFactor where
    ResetPageScaleFactor <> ResetPageScaleFactor = ResetPageScaleFactor


------------------------------------------------------------------------------
instance P.Monoid ResetPageScaleFactor where
    mempty = ResetPageScaleFactor


------------------------------------------------------------------------------
instance M.Method ResetPageScaleFactor where
    type Result ResetPageScaleFactor = ()
    name _ = "Emulation.resetPageScaleFactor"


------------------------------------------------------------------------------
-- | Requests that page scale factor is reset to initial values.
{-# WARNING resetPageScaleFactor "This feature is marked as EXPERIMENTAL." #-}
resetPageScaleFactor
    :: ResetPageScaleFactor
resetPageScaleFactor = ResetPageScaleFactor


------------------------------------------------------------------------------
-- | Enables or disables simulating a focused and active page.
{-# WARNING SetFocusEmulationEnabled "This feature is marked as EXPERIMENTAL." #-}
data SetFocusEmulationEnabled = SetFocusEmulationEnabled
    { -- | Whether to enable to disable focus emulation.
      enabled :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetFocusEmulationEnabled where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setFocusEmulationEnabled" $ \_o -> SetFocusEmulationEnabled
            <$> _o .: "enabled"
        ago = A.withArray "setFocusEmulationEnabled" $ \_a -> SetFocusEmulationEnabled
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetFocusEmulationEnabled where
    toEncoding (SetFocusEmulationEnabled _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "enabled" .= _0
        ]
    toJSON (SetFocusEmulationEnabled _0) = A.object $ P.catMaybes
        [ P.pure $ "enabled" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetFocusEmulationEnabled where
    SetFocusEmulationEnabled _0 <> SetFocusEmulationEnabled _ = SetFocusEmulationEnabled _0


------------------------------------------------------------------------------
instance M.Method SetFocusEmulationEnabled where
    type Result SetFocusEmulationEnabled = ()
    name _ = "Emulation.setFocusEmulationEnabled"


------------------------------------------------------------------------------
-- | Enables or disables simulating a focused and active page.
{-# WARNING setFocusEmulationEnabled "This feature is marked as EXPERIMENTAL." #-}
setFocusEmulationEnabled
    :: P.Bool
    -- ^ Whether to enable to disable focus emulation.

    -> SetFocusEmulationEnabled
setFocusEmulationEnabled _0 = SetFocusEmulationEnabled _0


------------------------------------------------------------------------------
-- | Enables CPU throttling to emulate slow CPUs.
{-# WARNING SetCPUThrottlingRate "This feature is marked as EXPERIMENTAL." #-}
data SetCPUThrottlingRate = SetCPUThrottlingRate
    { -- | Throttling rate as a slowdown factor (1 is no throttle, 2 is 2x slowdown, etc).
      rate :: !P.Double
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetCPUThrottlingRate where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setCPUThrottlingRate" $ \_o -> SetCPUThrottlingRate
            <$> _o .: "rate"
        ago = A.withArray "setCPUThrottlingRate" $ \_a -> SetCPUThrottlingRate
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetCPUThrottlingRate where
    toEncoding (SetCPUThrottlingRate _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "rate" .= _0
        ]
    toJSON (SetCPUThrottlingRate _0) = A.object $ P.catMaybes
        [ P.pure $ "rate" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetCPUThrottlingRate where
    SetCPUThrottlingRate _0 <> SetCPUThrottlingRate _ = SetCPUThrottlingRate _0


------------------------------------------------------------------------------
instance M.Method SetCPUThrottlingRate where
    type Result SetCPUThrottlingRate = ()
    name _ = "Emulation.setCPUThrottlingRate"


------------------------------------------------------------------------------
-- | Enables CPU throttling to emulate slow CPUs.
{-# WARNING setCPUThrottlingRate "This feature is marked as EXPERIMENTAL." #-}
setCPUThrottlingRate
    :: P.Double
    -- ^ Throttling rate as a slowdown factor (1 is no throttle, 2 is 2x slowdown, etc).

    -> SetCPUThrottlingRate
setCPUThrottlingRate _0 = SetCPUThrottlingRate _0


------------------------------------------------------------------------------
-- | Sets or clears an override of the default background color of the frame. This override is used
-- if the content does not specify one.
data SetDefaultBackgroundColorOverride = SetDefaultBackgroundColorOverride
    { -- | RGBA of the default background color. If not specified, any existing override will be
      -- cleared.
      color :: !(P.Maybe DOM.RGBA)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetDefaultBackgroundColorOverride where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setDefaultBackgroundColorOverride" $ \_o -> SetDefaultBackgroundColorOverride
            <$> _o .:? "color"
        ago = A.withArray "setDefaultBackgroundColorOverride" $ \_a -> SetDefaultBackgroundColorOverride
            <$> P.traverse A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetDefaultBackgroundColorOverride where
    toEncoding (SetDefaultBackgroundColorOverride _0) = A.pairs $ P.fold $ P.catMaybes
        [ ("color" .=) <$> _0
        ]
    toJSON (SetDefaultBackgroundColorOverride _0) = A.object $ P.catMaybes
        [ ("color" .=) <$> _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetDefaultBackgroundColorOverride where
    SetDefaultBackgroundColorOverride _0 <> SetDefaultBackgroundColorOverride __0 = SetDefaultBackgroundColorOverride (_0 <|> __0)


------------------------------------------------------------------------------
instance P.Monoid SetDefaultBackgroundColorOverride where
    mempty = SetDefaultBackgroundColorOverride P.empty


------------------------------------------------------------------------------
instance M.Method SetDefaultBackgroundColorOverride where
    type Result SetDefaultBackgroundColorOverride = ()
    name _ = "Emulation.setDefaultBackgroundColorOverride"


------------------------------------------------------------------------------
-- | Sets or clears an override of the default background color of the frame. This override is used
-- if the content does not specify one.
setDefaultBackgroundColorOverride
    :: SetDefaultBackgroundColorOverride
setDefaultBackgroundColorOverride = SetDefaultBackgroundColorOverride P.empty


------------------------------------------------------------------------------
-- | Overrides the values of device screen dimensions (window.screen.width, window.screen.height,
-- window.innerWidth, window.innerHeight, and "device-width"\/"device-height"-related CSS media
-- query results).
{-# WARNING scale, screenWidth, screenHeight, positionX, positionY, dontSetVisibleSize, viewport "This feature is marked as EXPERIMENTAL." #-}
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
    , screenOrientation :: !(P.Maybe ScreenOrientation)
      -- | If set, the visible area of the page will be overridden to this viewport. This viewport
      -- change is not observed by the page, e.g. viewport-relative elements do not change positions.
    , viewport :: !(P.Maybe Page.Viewport)
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
    name _ = "Emulation.setDeviceMetricsOverride"


------------------------------------------------------------------------------
-- | Overrides the values of device screen dimensions (window.screen.width, window.screen.height,
-- window.innerWidth, window.innerHeight, and "device-width"\/"device-height"-related CSS media
-- query results).
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
{-# WARNING SetScrollbarsHidden "This feature is marked as EXPERIMENTAL." #-}
data SetScrollbarsHidden = SetScrollbarsHidden
    { -- | Whether scrollbars should be always hidden.
      hidden :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetScrollbarsHidden where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setScrollbarsHidden" $ \_o -> SetScrollbarsHidden
            <$> _o .: "hidden"
        ago = A.withArray "setScrollbarsHidden" $ \_a -> SetScrollbarsHidden
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetScrollbarsHidden where
    toEncoding (SetScrollbarsHidden _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "hidden" .= _0
        ]
    toJSON (SetScrollbarsHidden _0) = A.object $ P.catMaybes
        [ P.pure $ "hidden" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetScrollbarsHidden where
    SetScrollbarsHidden _0 <> SetScrollbarsHidden _ = SetScrollbarsHidden _0


------------------------------------------------------------------------------
instance M.Method SetScrollbarsHidden where
    type Result SetScrollbarsHidden = ()
    name _ = "Emulation.setScrollbarsHidden"


------------------------------------------------------------------------------
{-# WARNING setScrollbarsHidden "This feature is marked as EXPERIMENTAL." #-}
setScrollbarsHidden
    :: P.Bool
    -- ^ Whether scrollbars should be always hidden.

    -> SetScrollbarsHidden
setScrollbarsHidden _0 = SetScrollbarsHidden _0


------------------------------------------------------------------------------
{-# WARNING SetDocumentCookieDisabled "This feature is marked as EXPERIMENTAL." #-}
data SetDocumentCookieDisabled = SetDocumentCookieDisabled
    { -- | Whether document.coookie API should be disabled.
      disabled :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetDocumentCookieDisabled where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setDocumentCookieDisabled" $ \_o -> SetDocumentCookieDisabled
            <$> _o .: "disabled"
        ago = A.withArray "setDocumentCookieDisabled" $ \_a -> SetDocumentCookieDisabled
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetDocumentCookieDisabled where
    toEncoding (SetDocumentCookieDisabled _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "disabled" .= _0
        ]
    toJSON (SetDocumentCookieDisabled _0) = A.object $ P.catMaybes
        [ P.pure $ "disabled" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetDocumentCookieDisabled where
    SetDocumentCookieDisabled _0 <> SetDocumentCookieDisabled _ = SetDocumentCookieDisabled _0


------------------------------------------------------------------------------
instance M.Method SetDocumentCookieDisabled where
    type Result SetDocumentCookieDisabled = ()
    name _ = "Emulation.setDocumentCookieDisabled"


------------------------------------------------------------------------------
{-# WARNING setDocumentCookieDisabled "This feature is marked as EXPERIMENTAL." #-}
setDocumentCookieDisabled
    :: P.Bool
    -- ^ Whether document.coookie API should be disabled.

    -> SetDocumentCookieDisabled
setDocumentCookieDisabled _0 = SetDocumentCookieDisabled _0


------------------------------------------------------------------------------
{-# WARNING SetEmitTouchEventsForMouse "This feature is marked as EXPERIMENTAL." #-}
data SetEmitTouchEventsForMouse = SetEmitTouchEventsForMouse
    { -- | Whether touch emulation based on mouse input should be enabled.
      enabled :: !P.Bool
      -- | Touch\/gesture events configuration. Default: current platform.
    , configuration :: !(P.Maybe Configuration)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetEmitTouchEventsForMouse where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setEmitTouchEventsForMouse" $ \_o -> SetEmitTouchEventsForMouse
            <$> _o .: "enabled"
            <*> _o .:? "configuration"
        ago = A.withArray "setEmitTouchEventsForMouse" $ \_a -> SetEmitTouchEventsForMouse
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SetEmitTouchEventsForMouse where
    toEncoding (SetEmitTouchEventsForMouse _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "enabled" .= _0
        , ("configuration" .=) <$> _1
        ]
    toJSON (SetEmitTouchEventsForMouse _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "enabled" .= _0
        , ("configuration" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetEmitTouchEventsForMouse where
    SetEmitTouchEventsForMouse _0 _1 <> SetEmitTouchEventsForMouse _ __1 = SetEmitTouchEventsForMouse _0 (_1 <|> __1)


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
instance M.Method SetEmitTouchEventsForMouse where
    type Result SetEmitTouchEventsForMouse = ()
    name _ = "Emulation.setEmitTouchEventsForMouse"


------------------------------------------------------------------------------
{-# WARNING setEmitTouchEventsForMouse "This feature is marked as EXPERIMENTAL." #-}
setEmitTouchEventsForMouse
    :: P.Bool
    -- ^ Whether touch emulation based on mouse input should be enabled.

    -> SetEmitTouchEventsForMouse
setEmitTouchEventsForMouse _0 = SetEmitTouchEventsForMouse _0 P.empty


------------------------------------------------------------------------------
-- | Emulates the given media for CSS media queries.
data SetEmulatedMedia = SetEmulatedMedia
    { -- | Media type to emulate. Empty string disables the override.
      media :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetEmulatedMedia where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setEmulatedMedia" $ \_o -> SetEmulatedMedia
            <$> _o .: "media"
        ago = A.withArray "setEmulatedMedia" $ \_a -> SetEmulatedMedia
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetEmulatedMedia where
    toEncoding (SetEmulatedMedia _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "media" .= _0
        ]
    toJSON (SetEmulatedMedia _0) = A.object $ P.catMaybes
        [ P.pure $ "media" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetEmulatedMedia where
    SetEmulatedMedia _0 <> SetEmulatedMedia _ = SetEmulatedMedia _0


------------------------------------------------------------------------------
instance M.Method SetEmulatedMedia where
    type Result SetEmulatedMedia = ()
    name _ = "Emulation.setEmulatedMedia"


------------------------------------------------------------------------------
-- | Emulates the given media for CSS media queries.
setEmulatedMedia
    :: T.Text
    -- ^ Media type to emulate. Empty string disables the override.

    -> SetEmulatedMedia
setEmulatedMedia _0 = SetEmulatedMedia _0


------------------------------------------------------------------------------
-- | Overrides the Geolocation Position or Error. Omitting any of the parameters emulates position
-- unavailable.
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
    name _ = "Emulation.setGeolocationOverride"


------------------------------------------------------------------------------
-- | Overrides the Geolocation Position or Error. Omitting any of the parameters emulates position
-- unavailable.
setGeolocationOverride
    :: SetGeolocationOverride
setGeolocationOverride = SetGeolocationOverride P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Overrides value returned by the javascript navigator object.
{-# DEPRECATED SetNavigatorOverrides "This may be removed in a future release." #-}
{-{-# WARNING SetNavigatorOverrides "This feature is marked as EXPERIMENTAL." #-}-}
data SetNavigatorOverrides = SetNavigatorOverrides
    { -- | The platform navigator.platform should return.
      platform :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetNavigatorOverrides where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setNavigatorOverrides" $ \_o -> SetNavigatorOverrides
            <$> _o .: "platform"
        ago = A.withArray "setNavigatorOverrides" $ \_a -> SetNavigatorOverrides
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetNavigatorOverrides where
    toEncoding (SetNavigatorOverrides _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "platform" .= _0
        ]
    toJSON (SetNavigatorOverrides _0) = A.object $ P.catMaybes
        [ P.pure $ "platform" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetNavigatorOverrides where
    SetNavigatorOverrides _0 <> SetNavigatorOverrides _ = SetNavigatorOverrides _0


------------------------------------------------------------------------------
instance M.Method SetNavigatorOverrides where
    type Result SetNavigatorOverrides = ()
    name _ = "Emulation.setNavigatorOverrides"


------------------------------------------------------------------------------
-- | Overrides value returned by the javascript navigator object.
{-# DEPRECATED setNavigatorOverrides "This may be removed in a future release." #-}
{-{-# WARNING setNavigatorOverrides "This feature is marked as EXPERIMENTAL." #-}-}
setNavigatorOverrides
    :: T.Text
    -- ^ The platform navigator.platform should return.

    -> SetNavigatorOverrides
setNavigatorOverrides _0 = SetNavigatorOverrides _0


------------------------------------------------------------------------------
-- | Sets a specified page scale factor.
{-# WARNING SetPageScaleFactor "This feature is marked as EXPERIMENTAL." #-}
data SetPageScaleFactor = SetPageScaleFactor
    { -- | Page scale factor.
      pageScaleFactor :: !P.Double
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetPageScaleFactor where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setPageScaleFactor" $ \_o -> SetPageScaleFactor
            <$> _o .: "pageScaleFactor"
        ago = A.withArray "setPageScaleFactor" $ \_a -> SetPageScaleFactor
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetPageScaleFactor where
    toEncoding (SetPageScaleFactor _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "pageScaleFactor" .= _0
        ]
    toJSON (SetPageScaleFactor _0) = A.object $ P.catMaybes
        [ P.pure $ "pageScaleFactor" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetPageScaleFactor where
    SetPageScaleFactor _0 <> SetPageScaleFactor _ = SetPageScaleFactor _0


------------------------------------------------------------------------------
instance M.Method SetPageScaleFactor where
    type Result SetPageScaleFactor = ()
    name _ = "Emulation.setPageScaleFactor"


------------------------------------------------------------------------------
-- | Sets a specified page scale factor.
{-# WARNING setPageScaleFactor "This feature is marked as EXPERIMENTAL." #-}
setPageScaleFactor
    :: P.Double
    -- ^ Page scale factor.

    -> SetPageScaleFactor
setPageScaleFactor _0 = SetPageScaleFactor _0


------------------------------------------------------------------------------
-- | Switches script execution in the page.
data SetScriptExecutionDisabled = SetScriptExecutionDisabled
    { -- | Whether script execution should be disabled in the page.
      value :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetScriptExecutionDisabled where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setScriptExecutionDisabled" $ \_o -> SetScriptExecutionDisabled
            <$> _o .: "value"
        ago = A.withArray "setScriptExecutionDisabled" $ \_a -> SetScriptExecutionDisabled
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetScriptExecutionDisabled where
    toEncoding (SetScriptExecutionDisabled _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "value" .= _0
        ]
    toJSON (SetScriptExecutionDisabled _0) = A.object $ P.catMaybes
        [ P.pure $ "value" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetScriptExecutionDisabled where
    SetScriptExecutionDisabled _0 <> SetScriptExecutionDisabled _ = SetScriptExecutionDisabled _0


------------------------------------------------------------------------------
instance M.Method SetScriptExecutionDisabled where
    type Result SetScriptExecutionDisabled = ()
    name _ = "Emulation.setScriptExecutionDisabled"


------------------------------------------------------------------------------
-- | Switches script execution in the page.
setScriptExecutionDisabled
    :: P.Bool
    -- ^ Whether script execution should be disabled in the page.

    -> SetScriptExecutionDisabled
setScriptExecutionDisabled _0 = SetScriptExecutionDisabled _0


------------------------------------------------------------------------------
-- | Enables touch on platforms which do not support them.
data SetTouchEmulationEnabled = SetTouchEmulationEnabled
    { -- | Whether the touch event emulation should be enabled.
      enabled :: !P.Bool
      -- | Maximum touch points supported. Defaults to one.
    , maxTouchPoints :: !(P.Maybe P.Int)
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
            <*> _o .:? "maxTouchPoints"
        ago = A.withArray "setTouchEmulationEnabled" $ \_a -> SetTouchEmulationEnabled
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SetTouchEmulationEnabled where
    toEncoding (SetTouchEmulationEnabled _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "enabled" .= _0
        , ("maxTouchPoints" .=) <$> _1
        ]
    toJSON (SetTouchEmulationEnabled _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "enabled" .= _0
        , ("maxTouchPoints" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetTouchEmulationEnabled where
    SetTouchEmulationEnabled _0 _1 <> SetTouchEmulationEnabled _ __1 = SetTouchEmulationEnabled _0 (_1 <|> __1)


------------------------------------------------------------------------------
instance M.Method SetTouchEmulationEnabled where
    type Result SetTouchEmulationEnabled = ()
    name _ = "Emulation.setTouchEmulationEnabled"


------------------------------------------------------------------------------
-- | Enables touch on platforms which do not support them.
setTouchEmulationEnabled
    :: P.Bool
    -- ^ Whether the touch event emulation should be enabled.

    -> SetTouchEmulationEnabled
setTouchEmulationEnabled _0 = SetTouchEmulationEnabled _0 P.empty


------------------------------------------------------------------------------
-- | Turns on virtual time for all frames (replacing real-time with a synthetic time source) and sets
-- the current virtual time policy.  Note this supersedes any previous time budget.
{-# WARNING SetVirtualTimePolicy "This feature is marked as EXPERIMENTAL." #-}
data SetVirtualTimePolicy = SetVirtualTimePolicy
    { policy :: !VirtualTimePolicy
      -- | If set, after this many virtual milliseconds have elapsed virtual time will be paused and a
      -- virtualTimeBudgetExpired event is sent.
    , budget :: !(P.Maybe P.Double)
      -- | If set this specifies the maximum number of tasks that can be run before virtual is forced
      -- forwards to prevent deadlock.
    , maxVirtualTimeTaskStarvationCount :: !(P.Maybe P.Int)
      -- | If set the virtual time policy change should be deferred until any frame starts navigating.
      -- Note any previous deferred policy change is superseded.
    , waitForNavigation :: !(P.Maybe P.Bool)
      -- | If set, base::Time::Now will be overriden to initially return this value.
    , initialVirtualTime :: !(P.Maybe Network.TimeSinceEpoch)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetVirtualTimePolicy where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setVirtualTimePolicy" $ \_o -> SetVirtualTimePolicy
            <$> _o .: "policy"
            <*> _o .:? "budget"
            <*> _o .:? "maxVirtualTimeTaskStarvationCount"
            <*> _o .:? "waitForNavigation"
            <*> _o .:? "initialVirtualTime"
        ago = A.withArray "setVirtualTimePolicy" $ \_a -> SetVirtualTimePolicy
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON SetVirtualTimePolicy where
    toEncoding (SetVirtualTimePolicy _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "policy" .= _0
        , ("budget" .=) <$> _1
        , ("maxVirtualTimeTaskStarvationCount" .=) <$> _2
        , ("waitForNavigation" .=) <$> _3
        , ("initialVirtualTime" .=) <$> _4
        ]
    toJSON (SetVirtualTimePolicy _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ P.pure $ "policy" .= _0
        , ("budget" .=) <$> _1
        , ("maxVirtualTimeTaskStarvationCount" .=) <$> _2
        , ("waitForNavigation" .=) <$> _3
        , ("initialVirtualTime" .=) <$> _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetVirtualTimePolicy where
    SetVirtualTimePolicy _0 _1 _2 _3 _4 <> SetVirtualTimePolicy _ __1 __2 __3 __4 = SetVirtualTimePolicy _0 (_1 <|> __1) (_2 <|> __2) (_3 <|> __3) (_4 <|> __4)


------------------------------------------------------------------------------
-- | Turns on virtual time for all frames (replacing real-time with a synthetic time source) and sets
-- the current virtual time policy.  Note this supersedes any previous time budget.
{-# WARNING SetVirtualTimePolicyResult "This feature is marked as EXPERIMENTAL." #-}
data SetVirtualTimePolicyResult = SetVirtualTimePolicyResult
    { -- | Absolute timestamp at which virtual time was first enabled (up time in milliseconds).
      virtualTimeTicksBase :: !P.Double
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetVirtualTimePolicyResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setVirtualTimePolicyResult" $ \_o -> SetVirtualTimePolicyResult
            <$> _o .: "virtualTimeTicksBase"
        ago = A.withArray "setVirtualTimePolicyResult" $ \_a -> SetVirtualTimePolicyResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetVirtualTimePolicyResult where
    toEncoding (SetVirtualTimePolicyResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "virtualTimeTicksBase" .= _0
        ]
    toJSON (SetVirtualTimePolicyResult _0) = A.object $ P.catMaybes
        [ P.pure $ "virtualTimeTicksBase" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetVirtualTimePolicyResult where
    SetVirtualTimePolicyResult _0 <> SetVirtualTimePolicyResult _ = SetVirtualTimePolicyResult _0


------------------------------------------------------------------------------
instance M.Method SetVirtualTimePolicy where
    type Result SetVirtualTimePolicy = SetVirtualTimePolicyResult
    name _ = "Emulation.setVirtualTimePolicy"


------------------------------------------------------------------------------
-- | Turns on virtual time for all frames (replacing real-time with a synthetic time source) and sets
-- the current virtual time policy.  Note this supersedes any previous time budget.
{-# WARNING setVirtualTimePolicy "This feature is marked as EXPERIMENTAL." #-}
setVirtualTimePolicy
    :: VirtualTimePolicy
    -> SetVirtualTimePolicy
setVirtualTimePolicy _0 = SetVirtualTimePolicy _0 P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Resizes the frame\/viewport of the page. Note that this does not affect the frame's container
-- (e.g. browser window). Can be used to produce screenshots of the specified size. Not supported
-- on Android.
{-# DEPRECATED SetVisibleSize "This may be removed in a future release." #-}
{-{-# WARNING SetVisibleSize "This feature is marked as EXPERIMENTAL." #-}-}
data SetVisibleSize = SetVisibleSize
    { -- | Frame width (DIP).
      width :: !P.Int
      -- | Frame height (DIP).
    , height :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetVisibleSize where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setVisibleSize" $ \_o -> SetVisibleSize
            <$> _o .: "width"
            <*> _o .: "height"
        ago = A.withArray "setVisibleSize" $ \_a -> SetVisibleSize
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SetVisibleSize where
    toEncoding (SetVisibleSize _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "width" .= _0
        , P.pure $ "height" .= _1
        ]
    toJSON (SetVisibleSize _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "width" .= _0
        , P.pure $ "height" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetVisibleSize where
    SetVisibleSize _0 _1 <> SetVisibleSize _ _ = SetVisibleSize _0 _1


------------------------------------------------------------------------------
instance M.Method SetVisibleSize where
    type Result SetVisibleSize = ()
    name _ = "Emulation.setVisibleSize"


------------------------------------------------------------------------------
-- | Resizes the frame\/viewport of the page. Note that this does not affect the frame's container
-- (e.g. browser window). Can be used to produce screenshots of the specified size. Not supported
-- on Android.
{-# DEPRECATED setVisibleSize "This may be removed in a future release." #-}
{-{-# WARNING setVisibleSize "This feature is marked as EXPERIMENTAL." #-}-}
setVisibleSize
    :: P.Int
    -- ^ Frame width (DIP).

    -> P.Int
    -- ^ Frame height (DIP).

    -> SetVisibleSize
setVisibleSize _0 _1 = SetVisibleSize _0 _1


------------------------------------------------------------------------------
-- | Allows overriding user agent with the given string.
data SetUserAgentOverride = SetUserAgentOverride
    { -- | User agent to use.
      userAgent :: !T.Text
      -- | Browser langugage to emulate.
    , acceptLanguage :: !(P.Maybe T.Text)
      -- | The platform navigator.platform should return.
    , platform :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetUserAgentOverride where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setUserAgentOverride" $ \_o -> SetUserAgentOverride
            <$> _o .: "userAgent"
            <*> _o .:? "acceptLanguage"
            <*> _o .:? "platform"
        ago = A.withArray "setUserAgentOverride" $ \_a -> SetUserAgentOverride
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON SetUserAgentOverride where
    toEncoding (SetUserAgentOverride _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "userAgent" .= _0
        , ("acceptLanguage" .=) <$> _1
        , ("platform" .=) <$> _2
        ]
    toJSON (SetUserAgentOverride _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "userAgent" .= _0
        , ("acceptLanguage" .=) <$> _1
        , ("platform" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetUserAgentOverride where
    SetUserAgentOverride _0 _1 _2 <> SetUserAgentOverride _ __1 __2 = SetUserAgentOverride _0 (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
instance M.Method SetUserAgentOverride where
    type Result SetUserAgentOverride = ()
    name _ = "Emulation.setUserAgentOverride"


------------------------------------------------------------------------------
-- | Allows overriding user agent with the given string.
setUserAgentOverride
    :: T.Text
    -- ^ User agent to use.

    -> SetUserAgentOverride
setUserAgentOverride _0 = SetUserAgentOverride _0 P.empty P.empty


------------------------------------------------------------------------------
-- | Notification sent after the virtual time budget for the current VirtualTimePolicy has run out.
{-# WARNING VirtualTimeBudgetExpired "This feature is marked as EXPERIMENTAL." #-}
data VirtualTimeBudgetExpired = VirtualTimeBudgetExpired
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON VirtualTimeBudgetExpired where
    parseJSON A.Null = P.pure VirtualTimeBudgetExpired
    parseJSON v = A.withArray "virtualTimeBudgetExpired" go v
        <|> A.withObject "virtualTimeBudgetExpired" go v
      where
        go _ = P.pure VirtualTimeBudgetExpired


------------------------------------------------------------------------------
instance A.ToJSON VirtualTimeBudgetExpired where
    toEncoding VirtualTimeBudgetExpired = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON VirtualTimeBudgetExpired = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup VirtualTimeBudgetExpired where
    VirtualTimeBudgetExpired <> VirtualTimeBudgetExpired = VirtualTimeBudgetExpired


------------------------------------------------------------------------------
instance P.Monoid VirtualTimeBudgetExpired where
    mempty = VirtualTimeBudgetExpired


------------------------------------------------------------------------------
instance E.Event VirtualTimeBudgetExpired where
    type Result VirtualTimeBudgetExpired = ()
    name _ = "Emulation.virtualTimeBudgetExpired"


------------------------------------------------------------------------------
-- | Notification sent after the virtual time budget for the current VirtualTimePolicy has run out.
{-# WARNING virtualTimeBudgetExpired "This feature is marked as EXPERIMENTAL." #-}
virtualTimeBudgetExpired :: P.Proxy VirtualTimeBudgetExpired
virtualTimeBudgetExpired = P.Proxy

