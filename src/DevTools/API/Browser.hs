{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | The Browser domain defines methods and events for browser managing.
module DevTools.API.Browser
    ( module DevTools.API.Browser.Types
    , module DevTools.API.Browser
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
import           DevTools.API.Browser.Types
import qualified DevTools.API.Target.Types as Target


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Set permission settings for given origin.
{-# WARNING SetPermission "This feature is marked as EXPERIMENTAL." #-}
data SetPermission = SetPermission
    { -- | Origin the permission applies to.
      origin :: !T.Text
      -- | Descriptor of permission to override.
    , permission :: !PermissionDescriptor
      -- | Setting of the permission.
    , setting :: !PermissionSetting
      -- | Context to override. When omitted, default browser context is used.
    , browserContextId :: !(P.Maybe Target.TargetID)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetPermission where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setPermission" $ \_o -> SetPermission
            <$> _o .: "origin"
            <*> _o .: "permission"
            <*> _o .: "setting"
            <*> _o .:? "browserContextId"
        ago = A.withArray "setPermission" $ \_a -> SetPermission
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON SetPermission where
    toEncoding (SetPermission _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "origin" .= _0
        , P.pure $ "permission" .= _1
        , P.pure $ "setting" .= _2
        , ("browserContextId" .=) <$> _3
        ]
    toJSON (SetPermission _0 _1 _2 _3) = A.object $ P.catMaybes
        [ P.pure $ "origin" .= _0
        , P.pure $ "permission" .= _1
        , P.pure $ "setting" .= _2
        , ("browserContextId" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetPermission where
    SetPermission _0 _1 _2 _3 <> SetPermission _ _ _ __3 = SetPermission _0 _1 _2 (_3 <|> __3)


------------------------------------------------------------------------------
instance M.Method SetPermission where
    type Result SetPermission = ()
    name _ = "Browser.setPermission"


------------------------------------------------------------------------------
-- | Set permission settings for given origin.
{-# WARNING setPermission "This feature is marked as EXPERIMENTAL." #-}
setPermission
    :: T.Text
    -- ^ Origin the permission applies to.

    -> PermissionDescriptor
    -- ^ Descriptor of permission to override.

    -> PermissionSetting
    -- ^ Setting of the permission.

    -> SetPermission
setPermission _0 _1 _2 = SetPermission _0 _1 _2 P.empty


------------------------------------------------------------------------------
-- | Grant specific permissions to the given origin and reject all others.
{-# WARNING GrantPermissions "This feature is marked as EXPERIMENTAL." #-}
data GrantPermissions = GrantPermissions
    { origin :: !T.Text
    , permissions :: ![PermissionType]
      -- | BrowserContext to override permissions. When omitted, default browser context is used.
    , browserContextId :: !(P.Maybe Target.BrowserContextID)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GrantPermissions where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "grantPermissions" $ \_o -> GrantPermissions
            <$> _o .: "origin"
            <*> _o .: "permissions"
            <*> _o .:? "browserContextId"
        ago = A.withArray "grantPermissions" $ \_a -> GrantPermissions
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON GrantPermissions where
    toEncoding (GrantPermissions _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "origin" .= _0
        , P.pure $ "permissions" .= _1
        , ("browserContextId" .=) <$> _2
        ]
    toJSON (GrantPermissions _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "origin" .= _0
        , P.pure $ "permissions" .= _1
        , ("browserContextId" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup GrantPermissions where
    GrantPermissions _0 _1 _2 <> GrantPermissions _ _ __2 = GrantPermissions _0 _1 (_2 <|> __2)


------------------------------------------------------------------------------
instance M.Method GrantPermissions where
    type Result GrantPermissions = ()
    name _ = "Browser.grantPermissions"


------------------------------------------------------------------------------
-- | Grant specific permissions to the given origin and reject all others.
{-# WARNING grantPermissions "This feature is marked as EXPERIMENTAL." #-}
grantPermissions
    :: T.Text
    -> [PermissionType]
    -> GrantPermissions
grantPermissions _0 _1 = GrantPermissions _0 _1 P.empty


------------------------------------------------------------------------------
-- | Reset all permission management for all origins.
{-# WARNING ResetPermissions "This feature is marked as EXPERIMENTAL." #-}
data ResetPermissions = ResetPermissions
    { -- | BrowserContext to reset permissions. When omitted, default browser context is used.
      browserContextId :: !(P.Maybe Target.BrowserContextID)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ResetPermissions where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "resetPermissions" $ \_o -> ResetPermissions
            <$> _o .:? "browserContextId"
        ago = A.withArray "resetPermissions" $ \_a -> ResetPermissions
            <$> P.traverse A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON ResetPermissions where
    toEncoding (ResetPermissions _0) = A.pairs $ P.fold $ P.catMaybes
        [ ("browserContextId" .=) <$> _0
        ]
    toJSON (ResetPermissions _0) = A.object $ P.catMaybes
        [ ("browserContextId" .=) <$> _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup ResetPermissions where
    ResetPermissions _0 <> ResetPermissions __0 = ResetPermissions (_0 <|> __0)


------------------------------------------------------------------------------
instance P.Monoid ResetPermissions where
    mempty = ResetPermissions P.empty


------------------------------------------------------------------------------
instance M.Method ResetPermissions where
    type Result ResetPermissions = ()
    name _ = "Browser.resetPermissions"


------------------------------------------------------------------------------
-- | Reset all permission management for all origins.
{-# WARNING resetPermissions "This feature is marked as EXPERIMENTAL." #-}
resetPermissions
    :: ResetPermissions
resetPermissions = ResetPermissions P.empty


------------------------------------------------------------------------------
-- | Close browser gracefully.
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
    name _ = "Browser.close"


------------------------------------------------------------------------------
-- | Close browser gracefully.
close
    :: Close
close = Close


------------------------------------------------------------------------------
-- | Crashes browser on the main thread.
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
    name _ = "Browser.crash"


------------------------------------------------------------------------------
-- | Crashes browser on the main thread.
{-# WARNING crash "This feature is marked as EXPERIMENTAL." #-}
crash
    :: Crash
crash = Crash


------------------------------------------------------------------------------
-- | Crashes GPU process.
{-# WARNING CrashGpuProcess "This feature is marked as EXPERIMENTAL." #-}
data CrashGpuProcess = CrashGpuProcess
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CrashGpuProcess where
    parseJSON A.Null = P.pure CrashGpuProcess
    parseJSON v = A.withArray "crashGpuProcess" go v
        <|> A.withObject "crashGpuProcess" go v
      where
        go _ = P.pure CrashGpuProcess


------------------------------------------------------------------------------
instance A.ToJSON CrashGpuProcess where
    toEncoding CrashGpuProcess = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON CrashGpuProcess = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup CrashGpuProcess where
    CrashGpuProcess <> CrashGpuProcess = CrashGpuProcess


------------------------------------------------------------------------------
instance P.Monoid CrashGpuProcess where
    mempty = CrashGpuProcess


------------------------------------------------------------------------------
instance M.Method CrashGpuProcess where
    type Result CrashGpuProcess = ()
    name _ = "Browser.crashGpuProcess"


------------------------------------------------------------------------------
-- | Crashes GPU process.
{-# WARNING crashGpuProcess "This feature is marked as EXPERIMENTAL." #-}
crashGpuProcess
    :: CrashGpuProcess
crashGpuProcess = CrashGpuProcess


------------------------------------------------------------------------------
-- | Returns version information.
data GetVersion = GetVersion
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetVersion where
    parseJSON A.Null = P.pure GetVersion
    parseJSON v = A.withArray "getVersion" go v
        <|> A.withObject "getVersion" go v
      where
        go _ = P.pure GetVersion


------------------------------------------------------------------------------
instance A.ToJSON GetVersion where
    toEncoding GetVersion = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetVersion = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetVersion where
    GetVersion <> GetVersion = GetVersion


------------------------------------------------------------------------------
instance P.Monoid GetVersion where
    mempty = GetVersion


------------------------------------------------------------------------------
-- | Returns version information.
data GetVersionResult = GetVersionResult
    { -- | Protocol version.
      protocolVersion :: !T.Text
      -- | Product name.
    , product :: !T.Text
      -- | Product revision.
    , revision :: !T.Text
      -- | User-Agent.
    , userAgent :: !T.Text
      -- | V8 version.
    , jsVersion :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetVersionResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getVersionResult" $ \_o -> GetVersionResult
            <$> _o .: "protocolVersion"
            <*> _o .: "product"
            <*> _o .: "revision"
            <*> _o .: "userAgent"
            <*> _o .: "jsVersion"
        ago = A.withArray "getVersionResult" $ \_a -> GetVersionResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)
            <*> P.maybe P.empty A.parseJSON (_a !? 3)
            <*> P.maybe P.empty A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON GetVersionResult where
    toEncoding (GetVersionResult _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "protocolVersion" .= _0
        , P.pure $ "product" .= _1
        , P.pure $ "revision" .= _2
        , P.pure $ "userAgent" .= _3
        , P.pure $ "jsVersion" .= _4
        ]
    toJSON (GetVersionResult _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ P.pure $ "protocolVersion" .= _0
        , P.pure $ "product" .= _1
        , P.pure $ "revision" .= _2
        , P.pure $ "userAgent" .= _3
        , P.pure $ "jsVersion" .= _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetVersionResult where
    GetVersionResult _0 _1 _2 _3 _4 <> GetVersionResult _ _ _ _ _ = GetVersionResult _0 _1 _2 _3 _4


------------------------------------------------------------------------------
instance M.Method GetVersion where
    type Result GetVersion = GetVersionResult
    name _ = "Browser.getVersion"


------------------------------------------------------------------------------
-- | Returns version information.
getVersion
    :: GetVersion
getVersion = GetVersion


------------------------------------------------------------------------------
-- | Returns the command line switches for the browser process if, and only if
-- --enable-automation is on the commandline.
{-# WARNING GetBrowserCommandLine "This feature is marked as EXPERIMENTAL." #-}
data GetBrowserCommandLine = GetBrowserCommandLine
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetBrowserCommandLine where
    parseJSON A.Null = P.pure GetBrowserCommandLine
    parseJSON v = A.withArray "getBrowserCommandLine" go v
        <|> A.withObject "getBrowserCommandLine" go v
      where
        go _ = P.pure GetBrowserCommandLine


------------------------------------------------------------------------------
instance A.ToJSON GetBrowserCommandLine where
    toEncoding GetBrowserCommandLine = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetBrowserCommandLine = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetBrowserCommandLine where
    GetBrowserCommandLine <> GetBrowserCommandLine = GetBrowserCommandLine


------------------------------------------------------------------------------
instance P.Monoid GetBrowserCommandLine where
    mempty = GetBrowserCommandLine


------------------------------------------------------------------------------
-- | Returns the command line switches for the browser process if, and only if
-- --enable-automation is on the commandline.
{-# WARNING GetBrowserCommandLineResult "This feature is marked as EXPERIMENTAL." #-}
data GetBrowserCommandLineResult = GetBrowserCommandLineResult
    { -- | Commandline parameters
      arguments :: ![T.Text]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetBrowserCommandLineResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getBrowserCommandLineResult" $ \_o -> GetBrowserCommandLineResult
            <$> _o .: "arguments"
        ago = A.withArray "getBrowserCommandLineResult" $ \_a -> GetBrowserCommandLineResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetBrowserCommandLineResult where
    toEncoding (GetBrowserCommandLineResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "arguments" .= _0
        ]
    toJSON (GetBrowserCommandLineResult _0) = A.object $ P.catMaybes
        [ P.pure $ "arguments" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetBrowserCommandLineResult where
    GetBrowserCommandLineResult _0 <> GetBrowserCommandLineResult _ = GetBrowserCommandLineResult _0


------------------------------------------------------------------------------
instance M.Method GetBrowserCommandLine where
    type Result GetBrowserCommandLine = GetBrowserCommandLineResult
    name _ = "Browser.getBrowserCommandLine"


------------------------------------------------------------------------------
-- | Returns the command line switches for the browser process if, and only if
-- --enable-automation is on the commandline.
{-# WARNING getBrowserCommandLine "This feature is marked as EXPERIMENTAL." #-}
getBrowserCommandLine
    :: GetBrowserCommandLine
getBrowserCommandLine = GetBrowserCommandLine


------------------------------------------------------------------------------
-- | Get Chrome histograms.
{-# WARNING GetHistograms "This feature is marked as EXPERIMENTAL." #-}
data GetHistograms = GetHistograms
    { -- | Requested substring in name. Only histograms which have query as a
      -- substring in their name are extracted. An empty or absent query returns
      -- all histograms.
      query :: !(P.Maybe T.Text)
      -- | If true, retrieve delta since last call.
    , delta :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetHistograms where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getHistograms" $ \_o -> GetHistograms
            <$> _o .:? "query"
            <*> _o .:? "delta"
        ago = A.withArray "getHistograms" $ \_a -> GetHistograms
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON GetHistograms where
    toEncoding (GetHistograms _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ ("query" .=) <$> _0
        , ("delta" .=) <$> _1
        ]
    toJSON (GetHistograms _0 _1) = A.object $ P.catMaybes
        [ ("query" .=) <$> _0
        , ("delta" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetHistograms where
    GetHistograms _0 _1 <> GetHistograms __0 __1 = GetHistograms (_0 <|> __0) (_1 <|> __1)


------------------------------------------------------------------------------
instance P.Monoid GetHistograms where
    mempty = GetHistograms P.empty P.empty


------------------------------------------------------------------------------
-- | Get Chrome histograms.
{-# WARNING GetHistogramsResult "This feature is marked as EXPERIMENTAL." #-}
data GetHistogramsResult = GetHistogramsResult
    { -- | Histograms.
      histograms :: ![Histogram]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetHistogramsResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getHistogramsResult" $ \_o -> GetHistogramsResult
            <$> _o .: "histograms"
        ago = A.withArray "getHistogramsResult" $ \_a -> GetHistogramsResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetHistogramsResult where
    toEncoding (GetHistogramsResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "histograms" .= _0
        ]
    toJSON (GetHistogramsResult _0) = A.object $ P.catMaybes
        [ P.pure $ "histograms" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetHistogramsResult where
    GetHistogramsResult _0 <> GetHistogramsResult _ = GetHistogramsResult _0


------------------------------------------------------------------------------
instance M.Method GetHistograms where
    type Result GetHistograms = GetHistogramsResult
    name _ = "Browser.getHistograms"


------------------------------------------------------------------------------
-- | Get Chrome histograms.
{-# WARNING getHistograms "This feature is marked as EXPERIMENTAL." #-}
getHistograms
    :: GetHistograms
getHistograms = GetHistograms P.empty P.empty


------------------------------------------------------------------------------
-- | Get a Chrome histogram by name.
{-# WARNING GetHistogram "This feature is marked as EXPERIMENTAL." #-}
data GetHistogram = GetHistogram
    { -- | Requested histogram name.
      name :: !T.Text
      -- | If true, retrieve delta since last call.
    , delta :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetHistogram where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getHistogram" $ \_o -> GetHistogram
            <$> _o .: "name"
            <*> _o .:? "delta"
        ago = A.withArray "getHistogram" $ \_a -> GetHistogram
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON GetHistogram where
    toEncoding (GetHistogram _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "name" .= _0
        , ("delta" .=) <$> _1
        ]
    toJSON (GetHistogram _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "name" .= _0
        , ("delta" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetHistogram where
    GetHistogram _0 _1 <> GetHistogram _ __1 = GetHistogram _0 (_1 <|> __1)


------------------------------------------------------------------------------
-- | Get a Chrome histogram by name.
{-# WARNING GetHistogramResult "This feature is marked as EXPERIMENTAL." #-}
data GetHistogramResult = GetHistogramResult
    { -- | Histogram.
      histogram :: !Histogram
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetHistogramResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getHistogramResult" $ \_o -> GetHistogramResult
            <$> _o .: "histogram"
        ago = A.withArray "getHistogramResult" $ \_a -> GetHistogramResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetHistogramResult where
    toEncoding (GetHistogramResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "histogram" .= _0
        ]
    toJSON (GetHistogramResult _0) = A.object $ P.catMaybes
        [ P.pure $ "histogram" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetHistogramResult where
    GetHistogramResult _0 <> GetHistogramResult _ = GetHistogramResult _0


------------------------------------------------------------------------------
instance M.Method GetHistogram where
    type Result GetHistogram = GetHistogramResult
    name _ = "Browser.getHistogram"


------------------------------------------------------------------------------
-- | Get a Chrome histogram by name.
{-# WARNING getHistogram "This feature is marked as EXPERIMENTAL." #-}
getHistogram
    :: T.Text
    -- ^ Requested histogram name.

    -> GetHistogram
getHistogram _0 = GetHistogram _0 P.empty


------------------------------------------------------------------------------
-- | Get position and size of the browser window.
{-# WARNING GetWindowBounds "This feature is marked as EXPERIMENTAL." #-}
data GetWindowBounds = GetWindowBounds
    { -- | Browser window id.
      windowId :: !WindowID
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetWindowBounds where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getWindowBounds" $ \_o -> GetWindowBounds
            <$> _o .: "windowId"
        ago = A.withArray "getWindowBounds" $ \_a -> GetWindowBounds
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetWindowBounds where
    toEncoding (GetWindowBounds _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "windowId" .= _0
        ]
    toJSON (GetWindowBounds _0) = A.object $ P.catMaybes
        [ P.pure $ "windowId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetWindowBounds where
    GetWindowBounds _0 <> GetWindowBounds _ = GetWindowBounds _0


------------------------------------------------------------------------------
-- | Get position and size of the browser window.
{-# WARNING GetWindowBoundsResult "This feature is marked as EXPERIMENTAL." #-}
data GetWindowBoundsResult = GetWindowBoundsResult
    { -- | Bounds information of the window. When window state is 'minimized', the restored window
      -- position and size are returned.
      bounds :: !Bounds
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetWindowBoundsResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getWindowBoundsResult" $ \_o -> GetWindowBoundsResult
            <$> _o .: "bounds"
        ago = A.withArray "getWindowBoundsResult" $ \_a -> GetWindowBoundsResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetWindowBoundsResult where
    toEncoding (GetWindowBoundsResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "bounds" .= _0
        ]
    toJSON (GetWindowBoundsResult _0) = A.object $ P.catMaybes
        [ P.pure $ "bounds" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetWindowBoundsResult where
    GetWindowBoundsResult _0 <> GetWindowBoundsResult _ = GetWindowBoundsResult _0


------------------------------------------------------------------------------
instance M.Method GetWindowBounds where
    type Result GetWindowBounds = GetWindowBoundsResult
    name _ = "Browser.getWindowBounds"


------------------------------------------------------------------------------
-- | Get position and size of the browser window.
{-# WARNING getWindowBounds "This feature is marked as EXPERIMENTAL." #-}
getWindowBounds
    :: WindowID
    -- ^ Browser window id.

    -> GetWindowBounds
getWindowBounds _0 = GetWindowBounds _0


------------------------------------------------------------------------------
-- | Get the browser window that contains the devtools target.
{-# WARNING GetWindowForTarget "This feature is marked as EXPERIMENTAL." #-}
data GetWindowForTarget = GetWindowForTarget
    { -- | Devtools agent host id. If called as a part of the session, associated targetId is used.
      targetId :: !(P.Maybe Target.TargetID)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetWindowForTarget where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getWindowForTarget" $ \_o -> GetWindowForTarget
            <$> _o .:? "targetId"
        ago = A.withArray "getWindowForTarget" $ \_a -> GetWindowForTarget
            <$> P.traverse A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetWindowForTarget where
    toEncoding (GetWindowForTarget _0) = A.pairs $ P.fold $ P.catMaybes
        [ ("targetId" .=) <$> _0
        ]
    toJSON (GetWindowForTarget _0) = A.object $ P.catMaybes
        [ ("targetId" .=) <$> _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetWindowForTarget where
    GetWindowForTarget _0 <> GetWindowForTarget __0 = GetWindowForTarget (_0 <|> __0)


------------------------------------------------------------------------------
instance P.Monoid GetWindowForTarget where
    mempty = GetWindowForTarget P.empty


------------------------------------------------------------------------------
-- | Get the browser window that contains the devtools target.
{-# WARNING GetWindowForTargetResult "This feature is marked as EXPERIMENTAL." #-}
data GetWindowForTargetResult = GetWindowForTargetResult
    { -- | Browser window id.
      windowId :: !WindowID
      -- | Bounds information of the window. When window state is 'minimized', the restored window
      -- position and size are returned.
    , bounds :: !Bounds
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetWindowForTargetResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getWindowForTargetResult" $ \_o -> GetWindowForTargetResult
            <$> _o .: "windowId"
            <*> _o .: "bounds"
        ago = A.withArray "getWindowForTargetResult" $ \_a -> GetWindowForTargetResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON GetWindowForTargetResult where
    toEncoding (GetWindowForTargetResult _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "windowId" .= _0
        , P.pure $ "bounds" .= _1
        ]
    toJSON (GetWindowForTargetResult _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "windowId" .= _0
        , P.pure $ "bounds" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetWindowForTargetResult where
    GetWindowForTargetResult _0 _1 <> GetWindowForTargetResult _ _ = GetWindowForTargetResult _0 _1


------------------------------------------------------------------------------
instance M.Method GetWindowForTarget where
    type Result GetWindowForTarget = GetWindowForTargetResult
    name _ = "Browser.getWindowForTarget"


------------------------------------------------------------------------------
-- | Get the browser window that contains the devtools target.
{-# WARNING getWindowForTarget "This feature is marked as EXPERIMENTAL." #-}
getWindowForTarget
    :: GetWindowForTarget
getWindowForTarget = GetWindowForTarget P.empty


------------------------------------------------------------------------------
-- | Set position and\/or size of the browser window.
{-# WARNING SetWindowBounds "This feature is marked as EXPERIMENTAL." #-}
data SetWindowBounds = SetWindowBounds
    { -- | Browser window id.
      windowId :: !WindowID
      -- | New window bounds. The 'minimized', 'maximized' and 'fullscreen' states cannot be combined
      -- with 'left', 'top', 'width' or 'height'. Leaves unspecified fields unchanged.
    , bounds :: !Bounds
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetWindowBounds where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setWindowBounds" $ \_o -> SetWindowBounds
            <$> _o .: "windowId"
            <*> _o .: "bounds"
        ago = A.withArray "setWindowBounds" $ \_a -> SetWindowBounds
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SetWindowBounds where
    toEncoding (SetWindowBounds _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "windowId" .= _0
        , P.pure $ "bounds" .= _1
        ]
    toJSON (SetWindowBounds _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "windowId" .= _0
        , P.pure $ "bounds" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetWindowBounds where
    SetWindowBounds _0 _1 <> SetWindowBounds _ _ = SetWindowBounds _0 _1


------------------------------------------------------------------------------
instance M.Method SetWindowBounds where
    type Result SetWindowBounds = ()
    name _ = "Browser.setWindowBounds"


------------------------------------------------------------------------------
-- | Set position and\/or size of the browser window.
{-# WARNING setWindowBounds "This feature is marked as EXPERIMENTAL." #-}
setWindowBounds
    :: WindowID
    -- ^ Browser window id.

    -> Bounds
    -- ^ New window bounds. The 'minimized', 'maximized' and 'fullscreen' states cannot be combined

    -- with 'left', 'top', 'width' or 'height'. Leaves unspecified fields unchanged.

    -> SetWindowBounds
setWindowBounds _0 _1 = SetWindowBounds _0 _1


------------------------------------------------------------------------------
-- | Set dock tile details, platform-specific.
{-# WARNING SetDockTile "This feature is marked as EXPERIMENTAL." #-}
data SetDockTile = SetDockTile
    { badgeLabel :: !(P.Maybe T.Text)
      -- | Png encoded image.
    , image :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetDockTile where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setDockTile" $ \_o -> SetDockTile
            <$> _o .:? "badgeLabel"
            <*> _o .:? "image"
        ago = A.withArray "setDockTile" $ \_a -> SetDockTile
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SetDockTile where
    toEncoding (SetDockTile _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ ("badgeLabel" .=) <$> _0
        , ("image" .=) <$> _1
        ]
    toJSON (SetDockTile _0 _1) = A.object $ P.catMaybes
        [ ("badgeLabel" .=) <$> _0
        , ("image" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetDockTile where
    SetDockTile _0 _1 <> SetDockTile __0 __1 = SetDockTile (_0 <|> __0) (_1 <|> __1)


------------------------------------------------------------------------------
instance P.Monoid SetDockTile where
    mempty = SetDockTile P.empty P.empty


------------------------------------------------------------------------------
instance M.Method SetDockTile where
    type Result SetDockTile = ()
    name _ = "Browser.setDockTile"


------------------------------------------------------------------------------
-- | Set dock tile details, platform-specific.
{-# WARNING setDockTile "This feature is marked as EXPERIMENTAL." #-}
setDockTile
    :: SetDockTile
setDockTile = SetDockTile P.empty P.empty

