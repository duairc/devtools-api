{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | The Browser domain defines methods and events for browser managing.
module DevTools.API.Browser.Types
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


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
{-# WARNING WindowID "This feature is marked as EXPERIMENTAL." #-}
type WindowID = P.Int


------------------------------------------------------------------------------
-- | The state of the browser window.
{-# WARNING WindowState "This feature is marked as EXPERIMENTAL." #-}
data WindowState
    = Normal
    | Minimized
    | Maximized
    | Fullscreen
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON WindowState where
    parseJSON = A.withText "WindowState" $ \t -> case t of
        "normal" -> P.pure Normal
        "minimized" -> P.pure Minimized
        "maximized" -> P.pure Maximized
        "fullscreen" -> P.pure Fullscreen
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON WindowState where
    toJSON Normal = "normal"
    toJSON Minimized = "minimized"
    toJSON Maximized = "maximized"
    toJSON Fullscreen = "fullscreen"


------------------------------------------------------------------------------
-- | Browser window bounds information
{-# WARNING Bounds "This feature is marked as EXPERIMENTAL." #-}
data Bounds = Bounds
    { -- | The offset from the left edge of the screen to the window in pixels.
      left :: !(P.Maybe P.Int)
      -- | The offset from the top edge of the screen to the window in pixels.
    , top :: !(P.Maybe P.Int)
      -- | The window width in pixels.
    , width :: !(P.Maybe P.Int)
      -- | The window height in pixels.
    , height :: !(P.Maybe P.Int)
      -- | The window state. Default to normal.
    , windowState :: !(P.Maybe WindowState)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Bounds where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "Bounds" $ \_o -> Bounds
            <$> _o .:? "left"
            <*> _o .:? "top"
            <*> _o .:? "width"
            <*> _o .:? "height"
            <*> _o .:? "windowState"
        ago = A.withArray "Bounds" $ \_a -> Bounds
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON Bounds where
    toEncoding (Bounds _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ ("left" .=) <$> _0
        , ("top" .=) <$> _1
        , ("width" .=) <$> _2
        , ("height" .=) <$> _3
        , ("windowState" .=) <$> _4
        ]
    toJSON (Bounds _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ ("left" .=) <$> _0
        , ("top" .=) <$> _1
        , ("width" .=) <$> _2
        , ("height" .=) <$> _3
        , ("windowState" .=) <$> _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup Bounds where
    Bounds _0 _1 _2 _3 _4 <> Bounds __0 __1 __2 __3 __4 = Bounds (_0 <|> __0) (_1 <|> __1) (_2 <|> __2) (_3 <|> __3) (_4 <|> __4)


------------------------------------------------------------------------------
instance P.Monoid Bounds where
    mempty = Bounds P.empty P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
{-# WARNING PermissionType "This feature is marked as EXPERIMENTAL." #-}
data PermissionType
    = AccessibilityEvents
    | AudioCapture
    | BackgroundSync
    | BackgroundFetch
    | ClipboardRead
    | ClipboardWrite
    | DurableStorage
    | Flash
    | Geolocation
    | Midi
    | MidiSysex
    | Notifications
    | PaymentHandler
    | PeriodicBackgroundSync
    | ProtectedMediaIdentifier
    | Sensors
    | VideoCapture
    | IdleDetection
    | WakeLockScreen
    | WakeLockSystem
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PermissionType where
    parseJSON = A.withText "PermissionType" $ \t -> case t of
        "accessibilityEvents" -> P.pure AccessibilityEvents
        "audioCapture" -> P.pure AudioCapture
        "backgroundSync" -> P.pure BackgroundSync
        "backgroundFetch" -> P.pure BackgroundFetch
        "clipboardRead" -> P.pure ClipboardRead
        "clipboardWrite" -> P.pure ClipboardWrite
        "durableStorage" -> P.pure DurableStorage
        "flash" -> P.pure Flash
        "geolocation" -> P.pure Geolocation
        "midi" -> P.pure Midi
        "midiSysex" -> P.pure MidiSysex
        "notifications" -> P.pure Notifications
        "paymentHandler" -> P.pure PaymentHandler
        "periodicBackgroundSync" -> P.pure PeriodicBackgroundSync
        "protectedMediaIdentifier" -> P.pure ProtectedMediaIdentifier
        "sensors" -> P.pure Sensors
        "videoCapture" -> P.pure VideoCapture
        "idleDetection" -> P.pure IdleDetection
        "wakeLockScreen" -> P.pure WakeLockScreen
        "wakeLockSystem" -> P.pure WakeLockSystem
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON PermissionType where
    toJSON AccessibilityEvents = "accessibilityEvents"
    toJSON AudioCapture = "audioCapture"
    toJSON BackgroundSync = "backgroundSync"
    toJSON BackgroundFetch = "backgroundFetch"
    toJSON ClipboardRead = "clipboardRead"
    toJSON ClipboardWrite = "clipboardWrite"
    toJSON DurableStorage = "durableStorage"
    toJSON Flash = "flash"
    toJSON Geolocation = "geolocation"
    toJSON Midi = "midi"
    toJSON MidiSysex = "midiSysex"
    toJSON Notifications = "notifications"
    toJSON PaymentHandler = "paymentHandler"
    toJSON PeriodicBackgroundSync = "periodicBackgroundSync"
    toJSON ProtectedMediaIdentifier = "protectedMediaIdentifier"
    toJSON Sensors = "sensors"
    toJSON VideoCapture = "videoCapture"
    toJSON IdleDetection = "idleDetection"
    toJSON WakeLockScreen = "wakeLockScreen"
    toJSON WakeLockSystem = "wakeLockSystem"


------------------------------------------------------------------------------
{-# WARNING PermissionSetting "This feature is marked as EXPERIMENTAL." #-}
data PermissionSetting
    = Granted
    | Denied
    | Prompt
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PermissionSetting where
    parseJSON = A.withText "PermissionSetting" $ \t -> case t of
        "granted" -> P.pure Granted
        "denied" -> P.pure Denied
        "prompt" -> P.pure Prompt
        _ -> P.empty


------------------------------------------------------------------------------
instance A.ToJSON PermissionSetting where
    toJSON Granted = "granted"
    toJSON Denied = "denied"
    toJSON Prompt = "prompt"


------------------------------------------------------------------------------
-- | Definition of PermissionDescriptor defined in the Permissions API:
-- https:\/\/w3c.github.io\/permissions\/#dictdef-permissiondescriptor.
{-# WARNING PermissionDescriptor "This feature is marked as EXPERIMENTAL." #-}
data PermissionDescriptor = PermissionDescriptor
    { -- | Name of permission.
      -- See https:\/\/cs.chromium.org\/chromium\/src\/third_party\/blink\/renderer\/modules\/permissions\/permission_descriptor.idl for valid permission names.
      name :: !T.Text
      -- | For "midi" permission, may also specify sysex control.
    , sysex :: !(P.Maybe P.Bool)
      -- | For "push" permission, may specify userVisibleOnly.
      -- Note that userVisibleOnly = true is the only currently supported type.
    , userVisibleOnly :: !(P.Maybe P.Bool)
      -- | For "wake-lock" permission, must specify type as either "screen" or "system".
    , type_ :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON PermissionDescriptor where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "PermissionDescriptor" $ \_o -> PermissionDescriptor
            <$> _o .: "name"
            <*> _o .:? "sysex"
            <*> _o .:? "userVisibleOnly"
            <*> _o .:? "type"
        ago = A.withArray "PermissionDescriptor" $ \_a -> PermissionDescriptor
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON PermissionDescriptor where
    toEncoding (PermissionDescriptor _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "name" .= _0
        , ("sysex" .=) <$> _1
        , ("userVisibleOnly" .=) <$> _2
        , ("type" .=) <$> _3
        ]
    toJSON (PermissionDescriptor _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "name" .= _0
        , ("sysex" .=) <$> _1
        , ("userVisibleOnly" .=) <$> _2
        , ("type" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup PermissionDescriptor where
    PermissionDescriptor _0 _1 _2 _3 <> PermissionDescriptor _ __1 __2 __3 = PermissionDescriptor _0 (_1 <|> __1) (_2 <|> __2) (_3 <|> __3)


------------------------------------------------------------------------------
-- | Chrome histogram bucket.
{-# WARNING Bucket "This feature is marked as EXPERIMENTAL." #-}
data Bucket = Bucket
    { -- | Minimum value (inclusive).
      low :: !P.Int
      -- | Maximum value (exclusive).
    , high :: !P.Int
      -- | Number of samples.
    , count :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Bucket where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "Bucket" $ \_o -> Bucket
            <$> _o .: "low"
            <*> _o .: "high"
            <*> _o .: "count"
        ago = A.withArray "Bucket" $ \_a -> Bucket
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON Bucket where
    toEncoding (Bucket _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "low" .= _0
        , P.pure $ "high" .= _1
        , P.pure $ "count" .= _2
        ]
    toJSON (Bucket _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "low" .= _0
        , P.pure $ "high" .= _1
        , P.pure $ "count" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup Bucket where
    Bucket _0 _1 _2 <> Bucket _ _ _ = Bucket _0 _1 _2


------------------------------------------------------------------------------
-- | Chrome histogram.
{-# WARNING Histogram "This feature is marked as EXPERIMENTAL." #-}
data Histogram = Histogram
    { -- | Name.
      name :: !T.Text
      -- | Sum of sample values.
    , sum :: !P.Int
      -- | Total number of samples.
    , count :: !P.Int
      -- | Buckets.
    , buckets :: ![Bucket]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON Histogram where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "Histogram" $ \_o -> Histogram
            <$> _o .: "name"
            <*> _o .: "sum"
            <*> _o .: "count"
            <*> _o .: "buckets"
        ago = A.withArray "Histogram" $ \_a -> Histogram
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON Histogram where
    toEncoding (Histogram _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "sum" .= _1
        , P.pure $ "count" .= _2
        , P.pure $ "buckets" .= _3
        ]
    toJSON (Histogram _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "name" .= _0
        , P.pure $ "sum" .= _1
        , P.pure $ "count" .= _2
        , P.pure $ "buckets" .= _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup Histogram where
    Histogram _0 _1 _2 _3 <> Histogram _ _ _ _ = Histogram _0 _1 _2 _3

