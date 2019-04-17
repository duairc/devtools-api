{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Supports additional targets discovery and allows to attach to them.
module DevTools.API.Target
    ( module DevTools.API.Target.Types
    , module DevTools.API.Target
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
import           DevTools.API.Target.Types


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Activates (focuses) the target.
data ActivateTarget = ActivateTarget
    { targetId :: !TargetID
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ActivateTarget where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "activateTarget" $ \_o -> ActivateTarget
            <$> _o .: "targetId"
        ago = A.withArray "activateTarget" $ \_a -> ActivateTarget
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON ActivateTarget where
    toEncoding (ActivateTarget _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "targetId" .= _0
        ]
    toJSON (ActivateTarget _0) = A.object $ P.catMaybes
        [ P.pure $ "targetId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup ActivateTarget where
    ActivateTarget _0 <> ActivateTarget _ = ActivateTarget _0


------------------------------------------------------------------------------
instance M.Method ActivateTarget where
    type Result ActivateTarget = ()
    name _ = "Target.activateTarget"


------------------------------------------------------------------------------
-- | Activates (focuses) the target.
activateTarget
    :: TargetID
    -> ActivateTarget
activateTarget _0 = ActivateTarget _0


------------------------------------------------------------------------------
-- | Attaches to the target with given id.
{-# WARNING flatten "This feature is marked as EXPERIMENTAL." #-}
data AttachToTarget = AttachToTarget
    { targetId :: !TargetID
      -- | Enables "flat" access to the session via specifying sessionId attribute in the commands.
    , flatten :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AttachToTarget where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "attachToTarget" $ \_o -> AttachToTarget
            <$> _o .: "targetId"
            <*> _o .:? "flatten"
        ago = A.withArray "attachToTarget" $ \_a -> AttachToTarget
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON AttachToTarget where
    toEncoding (AttachToTarget _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "targetId" .= _0
        , ("flatten" .=) <$> _1
        ]
    toJSON (AttachToTarget _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "targetId" .= _0
        , ("flatten" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup AttachToTarget where
    AttachToTarget _0 _1 <> AttachToTarget _ __1 = AttachToTarget _0 (_1 <|> __1)


------------------------------------------------------------------------------
-- | Attaches to the target with given id.
data AttachToTargetResult = AttachToTargetResult
    { -- | Id assigned to the session.
      sessionId :: !SessionID
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AttachToTargetResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "attachToTargetResult" $ \_o -> AttachToTargetResult
            <$> _o .: "sessionId"
        ago = A.withArray "attachToTargetResult" $ \_a -> AttachToTargetResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON AttachToTargetResult where
    toEncoding (AttachToTargetResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "sessionId" .= _0
        ]
    toJSON (AttachToTargetResult _0) = A.object $ P.catMaybes
        [ P.pure $ "sessionId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup AttachToTargetResult where
    AttachToTargetResult _0 <> AttachToTargetResult _ = AttachToTargetResult _0


------------------------------------------------------------------------------
instance M.Method AttachToTarget where
    type Result AttachToTarget = AttachToTargetResult
    name _ = "Target.attachToTarget"


------------------------------------------------------------------------------
-- | Attaches to the target with given id.
attachToTarget
    :: TargetID
    -> AttachToTarget
attachToTarget _0 = AttachToTarget _0 P.empty


------------------------------------------------------------------------------
-- | Attaches to the browser target, only uses flat sessionId mode.
{-# WARNING AttachToBrowserTarget "This feature is marked as EXPERIMENTAL." #-}
data AttachToBrowserTarget = AttachToBrowserTarget
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AttachToBrowserTarget where
    parseJSON A.Null = P.pure AttachToBrowserTarget
    parseJSON v = A.withArray "attachToBrowserTarget" go v
        <|> A.withObject "attachToBrowserTarget" go v
      where
        go _ = P.pure AttachToBrowserTarget


------------------------------------------------------------------------------
instance A.ToJSON AttachToBrowserTarget where
    toEncoding AttachToBrowserTarget = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON AttachToBrowserTarget = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup AttachToBrowserTarget where
    AttachToBrowserTarget <> AttachToBrowserTarget = AttachToBrowserTarget


------------------------------------------------------------------------------
instance P.Monoid AttachToBrowserTarget where
    mempty = AttachToBrowserTarget


------------------------------------------------------------------------------
-- | Attaches to the browser target, only uses flat sessionId mode.
{-# WARNING AttachToBrowserTargetResult "This feature is marked as EXPERIMENTAL." #-}
data AttachToBrowserTargetResult = AttachToBrowserTargetResult
    { -- | Id assigned to the session.
      sessionId :: !SessionID
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AttachToBrowserTargetResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "attachToBrowserTargetResult" $ \_o -> AttachToBrowserTargetResult
            <$> _o .: "sessionId"
        ago = A.withArray "attachToBrowserTargetResult" $ \_a -> AttachToBrowserTargetResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON AttachToBrowserTargetResult where
    toEncoding (AttachToBrowserTargetResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "sessionId" .= _0
        ]
    toJSON (AttachToBrowserTargetResult _0) = A.object $ P.catMaybes
        [ P.pure $ "sessionId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup AttachToBrowserTargetResult where
    AttachToBrowserTargetResult _0 <> AttachToBrowserTargetResult _ = AttachToBrowserTargetResult _0


------------------------------------------------------------------------------
instance M.Method AttachToBrowserTarget where
    type Result AttachToBrowserTarget = AttachToBrowserTargetResult
    name _ = "Target.attachToBrowserTarget"


------------------------------------------------------------------------------
-- | Attaches to the browser target, only uses flat sessionId mode.
{-# WARNING attachToBrowserTarget "This feature is marked as EXPERIMENTAL." #-}
attachToBrowserTarget
    :: AttachToBrowserTarget
attachToBrowserTarget = AttachToBrowserTarget


------------------------------------------------------------------------------
-- | Closes the target. If the target is a page that gets closed too.
data CloseTarget = CloseTarget
    { targetId :: !TargetID
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CloseTarget where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "closeTarget" $ \_o -> CloseTarget
            <$> _o .: "targetId"
        ago = A.withArray "closeTarget" $ \_a -> CloseTarget
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON CloseTarget where
    toEncoding (CloseTarget _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "targetId" .= _0
        ]
    toJSON (CloseTarget _0) = A.object $ P.catMaybes
        [ P.pure $ "targetId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup CloseTarget where
    CloseTarget _0 <> CloseTarget _ = CloseTarget _0


------------------------------------------------------------------------------
-- | Closes the target. If the target is a page that gets closed too.
data CloseTargetResult = CloseTargetResult
    { success :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CloseTargetResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "closeTargetResult" $ \_o -> CloseTargetResult
            <$> _o .: "success"
        ago = A.withArray "closeTargetResult" $ \_a -> CloseTargetResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON CloseTargetResult where
    toEncoding (CloseTargetResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "success" .= _0
        ]
    toJSON (CloseTargetResult _0) = A.object $ P.catMaybes
        [ P.pure $ "success" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup CloseTargetResult where
    CloseTargetResult _0 <> CloseTargetResult _ = CloseTargetResult _0


------------------------------------------------------------------------------
instance M.Method CloseTarget where
    type Result CloseTarget = CloseTargetResult
    name _ = "Target.closeTarget"


------------------------------------------------------------------------------
-- | Closes the target. If the target is a page that gets closed too.
closeTarget
    :: TargetID
    -> CloseTarget
closeTarget _0 = CloseTarget _0


------------------------------------------------------------------------------
-- | Inject object to the target's main frame that provides a communication
-- channel with browser target.
-- 
-- Injected object will be available as @window[bindingName]@.
-- 
-- The object has the follwing API:
-- - @binding.send(json)@ - a method to send messages over the remote debugging protocol
-- - @binding.onmessage = json => handleMessage(json)@ - a callback that will be called for the protocol notifications and command responses.
{-# WARNING ExposeDevToolsProtocol "This feature is marked as EXPERIMENTAL." #-}
data ExposeDevToolsProtocol = ExposeDevToolsProtocol
    { targetId :: !TargetID
      -- | Binding name, 'cdp' if not specified.
    , bindingName :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ExposeDevToolsProtocol where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "exposeDevToolsProtocol" $ \_o -> ExposeDevToolsProtocol
            <$> _o .: "targetId"
            <*> _o .:? "bindingName"
        ago = A.withArray "exposeDevToolsProtocol" $ \_a -> ExposeDevToolsProtocol
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON ExposeDevToolsProtocol where
    toEncoding (ExposeDevToolsProtocol _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "targetId" .= _0
        , ("bindingName" .=) <$> _1
        ]
    toJSON (ExposeDevToolsProtocol _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "targetId" .= _0
        , ("bindingName" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup ExposeDevToolsProtocol where
    ExposeDevToolsProtocol _0 _1 <> ExposeDevToolsProtocol _ __1 = ExposeDevToolsProtocol _0 (_1 <|> __1)


------------------------------------------------------------------------------
instance M.Method ExposeDevToolsProtocol where
    type Result ExposeDevToolsProtocol = ()
    name _ = "Target.exposeDevToolsProtocol"


------------------------------------------------------------------------------
-- | Inject object to the target's main frame that provides a communication
-- channel with browser target.
-- 
-- Injected object will be available as @window[bindingName]@.
-- 
-- The object has the follwing API:
-- - @binding.send(json)@ - a method to send messages over the remote debugging protocol
-- - @binding.onmessage = json => handleMessage(json)@ - a callback that will be called for the protocol notifications and command responses.
{-# WARNING exposeDevToolsProtocol "This feature is marked as EXPERIMENTAL." #-}
exposeDevToolsProtocol
    :: TargetID
    -> ExposeDevToolsProtocol
exposeDevToolsProtocol _0 = ExposeDevToolsProtocol _0 P.empty


------------------------------------------------------------------------------
-- | Creates a new empty BrowserContext. Similar to an incognito profile but you can have more than
-- one.
{-# WARNING CreateBrowserContext "This feature is marked as EXPERIMENTAL." #-}
data CreateBrowserContext = CreateBrowserContext
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CreateBrowserContext where
    parseJSON A.Null = P.pure CreateBrowserContext
    parseJSON v = A.withArray "createBrowserContext" go v
        <|> A.withObject "createBrowserContext" go v
      where
        go _ = P.pure CreateBrowserContext


------------------------------------------------------------------------------
instance A.ToJSON CreateBrowserContext where
    toEncoding CreateBrowserContext = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON CreateBrowserContext = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup CreateBrowserContext where
    CreateBrowserContext <> CreateBrowserContext = CreateBrowserContext


------------------------------------------------------------------------------
instance P.Monoid CreateBrowserContext where
    mempty = CreateBrowserContext


------------------------------------------------------------------------------
-- | Creates a new empty BrowserContext. Similar to an incognito profile but you can have more than
-- one.
{-# WARNING CreateBrowserContextResult "This feature is marked as EXPERIMENTAL." #-}
data CreateBrowserContextResult = CreateBrowserContextResult
    { -- | The id of the context created.
      browserContextId :: !BrowserContextID
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CreateBrowserContextResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "createBrowserContextResult" $ \_o -> CreateBrowserContextResult
            <$> _o .: "browserContextId"
        ago = A.withArray "createBrowserContextResult" $ \_a -> CreateBrowserContextResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON CreateBrowserContextResult where
    toEncoding (CreateBrowserContextResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "browserContextId" .= _0
        ]
    toJSON (CreateBrowserContextResult _0) = A.object $ P.catMaybes
        [ P.pure $ "browserContextId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup CreateBrowserContextResult where
    CreateBrowserContextResult _0 <> CreateBrowserContextResult _ = CreateBrowserContextResult _0


------------------------------------------------------------------------------
instance M.Method CreateBrowserContext where
    type Result CreateBrowserContext = CreateBrowserContextResult
    name _ = "Target.createBrowserContext"


------------------------------------------------------------------------------
-- | Creates a new empty BrowserContext. Similar to an incognito profile but you can have more than
-- one.
{-# WARNING createBrowserContext "This feature is marked as EXPERIMENTAL." #-}
createBrowserContext
    :: CreateBrowserContext
createBrowserContext = CreateBrowserContext


------------------------------------------------------------------------------
-- | Returns all browser contexts created with @Target.createBrowserContext@ method.
{-# WARNING GetBrowserContexts "This feature is marked as EXPERIMENTAL." #-}
data GetBrowserContexts = GetBrowserContexts
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetBrowserContexts where
    parseJSON A.Null = P.pure GetBrowserContexts
    parseJSON v = A.withArray "getBrowserContexts" go v
        <|> A.withObject "getBrowserContexts" go v
      where
        go _ = P.pure GetBrowserContexts


------------------------------------------------------------------------------
instance A.ToJSON GetBrowserContexts where
    toEncoding GetBrowserContexts = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetBrowserContexts = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetBrowserContexts where
    GetBrowserContexts <> GetBrowserContexts = GetBrowserContexts


------------------------------------------------------------------------------
instance P.Monoid GetBrowserContexts where
    mempty = GetBrowserContexts


------------------------------------------------------------------------------
-- | Returns all browser contexts created with @Target.createBrowserContext@ method.
{-# WARNING GetBrowserContextsResult "This feature is marked as EXPERIMENTAL." #-}
data GetBrowserContextsResult = GetBrowserContextsResult
    { -- | An array of browser context ids.
      browserContextIds :: ![BrowserContextID]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetBrowserContextsResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getBrowserContextsResult" $ \_o -> GetBrowserContextsResult
            <$> _o .: "browserContextIds"
        ago = A.withArray "getBrowserContextsResult" $ \_a -> GetBrowserContextsResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetBrowserContextsResult where
    toEncoding (GetBrowserContextsResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "browserContextIds" .= _0
        ]
    toJSON (GetBrowserContextsResult _0) = A.object $ P.catMaybes
        [ P.pure $ "browserContextIds" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetBrowserContextsResult where
    GetBrowserContextsResult _0 <> GetBrowserContextsResult _ = GetBrowserContextsResult _0


------------------------------------------------------------------------------
instance M.Method GetBrowserContexts where
    type Result GetBrowserContexts = GetBrowserContextsResult
    name _ = "Target.getBrowserContexts"


------------------------------------------------------------------------------
-- | Returns all browser contexts created with @Target.createBrowserContext@ method.
{-# WARNING getBrowserContexts "This feature is marked as EXPERIMENTAL." #-}
getBrowserContexts
    :: GetBrowserContexts
getBrowserContexts = GetBrowserContexts


------------------------------------------------------------------------------
-- | Creates a new page.
{-# WARNING enableBeginFrameControl "This feature is marked as EXPERIMENTAL." #-}
data CreateTarget = CreateTarget
    { -- | The initial URL the page will be navigated to.
      url :: !T.Text
      -- | Frame width in DIP (headless chrome only).
    , width :: !(P.Maybe P.Int)
      -- | Frame height in DIP (headless chrome only).
    , height :: !(P.Maybe P.Int)
      -- | The browser context to create the page in.
    , browserContextId :: !(P.Maybe BrowserContextID)
      -- | Whether BeginFrames for this target will be controlled via DevTools (headless chrome only,
      -- not supported on MacOS yet, false by default).
    , enableBeginFrameControl :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CreateTarget where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "createTarget" $ \_o -> CreateTarget
            <$> _o .: "url"
            <*> _o .:? "width"
            <*> _o .:? "height"
            <*> _o .:? "browserContextId"
            <*> _o .:? "enableBeginFrameControl"
        ago = A.withArray "createTarget" $ \_a -> CreateTarget
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)
            <*> P.traverse A.parseJSON (_a !? 4)


------------------------------------------------------------------------------
instance A.ToJSON CreateTarget where
    toEncoding (CreateTarget _0 _1 _2 _3 _4) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "url" .= _0
        , ("width" .=) <$> _1
        , ("height" .=) <$> _2
        , ("browserContextId" .=) <$> _3
        , ("enableBeginFrameControl" .=) <$> _4
        ]
    toJSON (CreateTarget _0 _1 _2 _3 _4) = A.object $ P.catMaybes
        [ P.pure $ "url" .= _0
        , ("width" .=) <$> _1
        , ("height" .=) <$> _2
        , ("browserContextId" .=) <$> _3
        , ("enableBeginFrameControl" .=) <$> _4
        ]


------------------------------------------------------------------------------
instance P.Semigroup CreateTarget where
    CreateTarget _0 _1 _2 _3 _4 <> CreateTarget _ __1 __2 __3 __4 = CreateTarget _0 (_1 <|> __1) (_2 <|> __2) (_3 <|> __3) (_4 <|> __4)


------------------------------------------------------------------------------
-- | Creates a new page.
data CreateTargetResult = CreateTargetResult
    { -- | The id of the page opened.
      targetId :: !TargetID
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON CreateTargetResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "createTargetResult" $ \_o -> CreateTargetResult
            <$> _o .: "targetId"
        ago = A.withArray "createTargetResult" $ \_a -> CreateTargetResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON CreateTargetResult where
    toEncoding (CreateTargetResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "targetId" .= _0
        ]
    toJSON (CreateTargetResult _0) = A.object $ P.catMaybes
        [ P.pure $ "targetId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup CreateTargetResult where
    CreateTargetResult _0 <> CreateTargetResult _ = CreateTargetResult _0


------------------------------------------------------------------------------
instance M.Method CreateTarget where
    type Result CreateTarget = CreateTargetResult
    name _ = "Target.createTarget"


------------------------------------------------------------------------------
-- | Creates a new page.
createTarget
    :: T.Text
    -- ^ The initial URL the page will be navigated to.

    -> CreateTarget
createTarget _0 = CreateTarget _0 P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Detaches session with given id.
{-# DEPRECATED targetId "This may be removed in a future release." #-}
data DetachFromTarget = DetachFromTarget
    { -- | Session to detach.
      sessionId :: !(P.Maybe SessionID)
      -- | Deprecated.
    , targetId :: !(P.Maybe TargetID)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DetachFromTarget where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "detachFromTarget" $ \_o -> DetachFromTarget
            <$> _o .:? "sessionId"
            <*> _o .:? "targetId"
        ago = A.withArray "detachFromTarget" $ \_a -> DetachFromTarget
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON DetachFromTarget where
    toEncoding (DetachFromTarget _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ ("sessionId" .=) <$> _0
        , ("targetId" .=) <$> _1
        ]
    toJSON (DetachFromTarget _0 _1) = A.object $ P.catMaybes
        [ ("sessionId" .=) <$> _0
        , ("targetId" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup DetachFromTarget where
    DetachFromTarget _0 _1 <> DetachFromTarget __0 __1 = DetachFromTarget (_0 <|> __0) (_1 <|> __1)


------------------------------------------------------------------------------
instance P.Monoid DetachFromTarget where
    mempty = DetachFromTarget P.empty P.empty


------------------------------------------------------------------------------
instance M.Method DetachFromTarget where
    type Result DetachFromTarget = ()
    name _ = "Target.detachFromTarget"


------------------------------------------------------------------------------
-- | Detaches session with given id.
detachFromTarget
    :: DetachFromTarget
detachFromTarget = DetachFromTarget P.empty P.empty


------------------------------------------------------------------------------
-- | Deletes a BrowserContext. All the belonging pages will be closed without calling their
-- beforeunload hooks.
{-# WARNING DisposeBrowserContext "This feature is marked as EXPERIMENTAL." #-}
data DisposeBrowserContext = DisposeBrowserContext
    { browserContextId :: !BrowserContextID
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DisposeBrowserContext where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "disposeBrowserContext" $ \_o -> DisposeBrowserContext
            <$> _o .: "browserContextId"
        ago = A.withArray "disposeBrowserContext" $ \_a -> DisposeBrowserContext
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON DisposeBrowserContext where
    toEncoding (DisposeBrowserContext _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "browserContextId" .= _0
        ]
    toJSON (DisposeBrowserContext _0) = A.object $ P.catMaybes
        [ P.pure $ "browserContextId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup DisposeBrowserContext where
    DisposeBrowserContext _0 <> DisposeBrowserContext _ = DisposeBrowserContext _0


------------------------------------------------------------------------------
instance M.Method DisposeBrowserContext where
    type Result DisposeBrowserContext = ()
    name _ = "Target.disposeBrowserContext"


------------------------------------------------------------------------------
-- | Deletes a BrowserContext. All the belonging pages will be closed without calling their
-- beforeunload hooks.
{-# WARNING disposeBrowserContext "This feature is marked as EXPERIMENTAL." #-}
disposeBrowserContext
    :: BrowserContextID
    -> DisposeBrowserContext
disposeBrowserContext _0 = DisposeBrowserContext _0


------------------------------------------------------------------------------
-- | Returns information about a target.
{-# WARNING GetTargetInfo "This feature is marked as EXPERIMENTAL." #-}
data GetTargetInfo = GetTargetInfo
    { targetId :: !(P.Maybe TargetID)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetTargetInfo where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getTargetInfo" $ \_o -> GetTargetInfo
            <$> _o .:? "targetId"
        ago = A.withArray "getTargetInfo" $ \_a -> GetTargetInfo
            <$> P.traverse A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetTargetInfo where
    toEncoding (GetTargetInfo _0) = A.pairs $ P.fold $ P.catMaybes
        [ ("targetId" .=) <$> _0
        ]
    toJSON (GetTargetInfo _0) = A.object $ P.catMaybes
        [ ("targetId" .=) <$> _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetTargetInfo where
    GetTargetInfo _0 <> GetTargetInfo __0 = GetTargetInfo (_0 <|> __0)


------------------------------------------------------------------------------
instance P.Monoid GetTargetInfo where
    mempty = GetTargetInfo P.empty


------------------------------------------------------------------------------
-- | Returns information about a target.
{-# WARNING GetTargetInfoResult "This feature is marked as EXPERIMENTAL." #-}
data GetTargetInfoResult = GetTargetInfoResult
    { targetInfo :: !TargetInfo
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetTargetInfoResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getTargetInfoResult" $ \_o -> GetTargetInfoResult
            <$> _o .: "targetInfo"
        ago = A.withArray "getTargetInfoResult" $ \_a -> GetTargetInfoResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetTargetInfoResult where
    toEncoding (GetTargetInfoResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "targetInfo" .= _0
        ]
    toJSON (GetTargetInfoResult _0) = A.object $ P.catMaybes
        [ P.pure $ "targetInfo" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetTargetInfoResult where
    GetTargetInfoResult _0 <> GetTargetInfoResult _ = GetTargetInfoResult _0


------------------------------------------------------------------------------
instance M.Method GetTargetInfo where
    type Result GetTargetInfo = GetTargetInfoResult
    name _ = "Target.getTargetInfo"


------------------------------------------------------------------------------
-- | Returns information about a target.
{-# WARNING getTargetInfo "This feature is marked as EXPERIMENTAL." #-}
getTargetInfo
    :: GetTargetInfo
getTargetInfo = GetTargetInfo P.empty


------------------------------------------------------------------------------
-- | Retrieves a list of available targets.
data GetTargets = GetTargets
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetTargets where
    parseJSON A.Null = P.pure GetTargets
    parseJSON v = A.withArray "getTargets" go v
        <|> A.withObject "getTargets" go v
      where
        go _ = P.pure GetTargets


------------------------------------------------------------------------------
instance A.ToJSON GetTargets where
    toEncoding GetTargets = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetTargets = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetTargets where
    GetTargets <> GetTargets = GetTargets


------------------------------------------------------------------------------
instance P.Monoid GetTargets where
    mempty = GetTargets


------------------------------------------------------------------------------
-- | Retrieves a list of available targets.
data GetTargetsResult = GetTargetsResult
    { -- | The list of targets.
      targetInfos :: ![TargetInfo]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetTargetsResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getTargetsResult" $ \_o -> GetTargetsResult
            <$> _o .: "targetInfos"
        ago = A.withArray "getTargetsResult" $ \_a -> GetTargetsResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetTargetsResult where
    toEncoding (GetTargetsResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "targetInfos" .= _0
        ]
    toJSON (GetTargetsResult _0) = A.object $ P.catMaybes
        [ P.pure $ "targetInfos" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetTargetsResult where
    GetTargetsResult _0 <> GetTargetsResult _ = GetTargetsResult _0


------------------------------------------------------------------------------
instance M.Method GetTargets where
    type Result GetTargets = GetTargetsResult
    name _ = "Target.getTargets"


------------------------------------------------------------------------------
-- | Retrieves a list of available targets.
getTargets
    :: GetTargets
getTargets = GetTargets


------------------------------------------------------------------------------
-- | Sends protocol message over session with given id.
{-{-# DEPRECATED targetId "This may be removed in a future release." #-}-}
data SendMessageToTarget = SendMessageToTarget
    { message :: !T.Text
      -- | Identifier of the session.
    , sessionId :: !(P.Maybe SessionID)
      -- | Deprecated.
    , targetId :: !(P.Maybe TargetID)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SendMessageToTarget where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "sendMessageToTarget" $ \_o -> SendMessageToTarget
            <$> _o .: "message"
            <*> _o .:? "sessionId"
            <*> _o .:? "targetId"
        ago = A.withArray "sendMessageToTarget" $ \_a -> SendMessageToTarget
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON SendMessageToTarget where
    toEncoding (SendMessageToTarget _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "message" .= _0
        , ("sessionId" .=) <$> _1
        , ("targetId" .=) <$> _2
        ]
    toJSON (SendMessageToTarget _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "message" .= _0
        , ("sessionId" .=) <$> _1
        , ("targetId" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup SendMessageToTarget where
    SendMessageToTarget _0 _1 _2 <> SendMessageToTarget _ __1 __2 = SendMessageToTarget _0 (_1 <|> __1) (_2 <|> __2)


------------------------------------------------------------------------------
instance M.Method SendMessageToTarget where
    type Result SendMessageToTarget = ()
    name _ = "Target.sendMessageToTarget"


------------------------------------------------------------------------------
-- | Sends protocol message over session with given id.
sendMessageToTarget
    :: T.Text
    -> SendMessageToTarget
sendMessageToTarget _0 = SendMessageToTarget _0 P.empty P.empty


------------------------------------------------------------------------------
-- | Controls whether to automatically attach to new targets which are considered to be related to
-- this one. When turned on, attaches to all existing related targets as well. When turned off,
-- automatically detaches from all currently attached targets.
{-# WARNING SetAutoAttach "This feature is marked as EXPERIMENTAL." #-}
{-{-# WARNING flatten "This feature is marked as EXPERIMENTAL." #-}-}
data SetAutoAttach = SetAutoAttach
    { -- | Whether to auto-attach to related targets.
      autoAttach :: !P.Bool
      -- | Whether to pause new targets when attaching to them. Use @Runtime.runIfWaitingForDebugger@
      -- to run paused targets.
    , waitForDebuggerOnStart :: !P.Bool
      -- | Enables "flat" access to the session via specifying sessionId attribute in the commands.
    , flatten :: !(P.Maybe P.Bool)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetAutoAttach where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setAutoAttach" $ \_o -> SetAutoAttach
            <$> _o .: "autoAttach"
            <*> _o .: "waitForDebuggerOnStart"
            <*> _o .:? "flatten"
        ago = A.withArray "setAutoAttach" $ \_a -> SetAutoAttach
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON SetAutoAttach where
    toEncoding (SetAutoAttach _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "autoAttach" .= _0
        , P.pure $ "waitForDebuggerOnStart" .= _1
        , ("flatten" .=) <$> _2
        ]
    toJSON (SetAutoAttach _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "autoAttach" .= _0
        , P.pure $ "waitForDebuggerOnStart" .= _1
        , ("flatten" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetAutoAttach where
    SetAutoAttach _0 _1 _2 <> SetAutoAttach _ _ __2 = SetAutoAttach _0 _1 (_2 <|> __2)


------------------------------------------------------------------------------
instance M.Method SetAutoAttach where
    type Result SetAutoAttach = ()
    name _ = "Target.setAutoAttach"


------------------------------------------------------------------------------
-- | Controls whether to automatically attach to new targets which are considered to be related to
-- this one. When turned on, attaches to all existing related targets as well. When turned off,
-- automatically detaches from all currently attached targets.
{-# WARNING setAutoAttach "This feature is marked as EXPERIMENTAL." #-}
setAutoAttach
    :: P.Bool
    -- ^ Whether to auto-attach to related targets.

    -> P.Bool
    -- ^ Whether to pause new targets when attaching to them. Use @Runtime.runIfWaitingForDebugger@

    -- to run paused targets.

    -> SetAutoAttach
setAutoAttach _0 _1 = SetAutoAttach _0 _1 P.empty


------------------------------------------------------------------------------
-- | Controls whether to discover available targets and notify via
-- @targetCreated\/targetInfoChanged\/targetDestroyed@ events.
data SetDiscoverTargets = SetDiscoverTargets
    { -- | Whether to discover available targets.
      discover :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetDiscoverTargets where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setDiscoverTargets" $ \_o -> SetDiscoverTargets
            <$> _o .: "discover"
        ago = A.withArray "setDiscoverTargets" $ \_a -> SetDiscoverTargets
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetDiscoverTargets where
    toEncoding (SetDiscoverTargets _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "discover" .= _0
        ]
    toJSON (SetDiscoverTargets _0) = A.object $ P.catMaybes
        [ P.pure $ "discover" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetDiscoverTargets where
    SetDiscoverTargets _0 <> SetDiscoverTargets _ = SetDiscoverTargets _0


------------------------------------------------------------------------------
instance M.Method SetDiscoverTargets where
    type Result SetDiscoverTargets = ()
    name _ = "Target.setDiscoverTargets"


------------------------------------------------------------------------------
-- | Controls whether to discover available targets and notify via
-- @targetCreated\/targetInfoChanged\/targetDestroyed@ events.
setDiscoverTargets
    :: P.Bool
    -- ^ Whether to discover available targets.

    -> SetDiscoverTargets
setDiscoverTargets _0 = SetDiscoverTargets _0


------------------------------------------------------------------------------
-- | Enables target discovery for the specified locations, when @setDiscoverTargets@ was set to
-- @true@.
{-# WARNING SetRemoteLocations "This feature is marked as EXPERIMENTAL." #-}
data SetRemoteLocations = SetRemoteLocations
    { -- | List of remote locations.
      locations :: ![RemoteLocation]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetRemoteLocations where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setRemoteLocations" $ \_o -> SetRemoteLocations
            <$> _o .: "locations"
        ago = A.withArray "setRemoteLocations" $ \_a -> SetRemoteLocations
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetRemoteLocations where
    toEncoding (SetRemoteLocations _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "locations" .= _0
        ]
    toJSON (SetRemoteLocations _0) = A.object $ P.catMaybes
        [ P.pure $ "locations" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetRemoteLocations where
    SetRemoteLocations _0 <> SetRemoteLocations _ = SetRemoteLocations _0


------------------------------------------------------------------------------
instance M.Method SetRemoteLocations where
    type Result SetRemoteLocations = ()
    name _ = "Target.setRemoteLocations"


------------------------------------------------------------------------------
-- | Enables target discovery for the specified locations, when @setDiscoverTargets@ was set to
-- @true@.
{-# WARNING setRemoteLocations "This feature is marked as EXPERIMENTAL." #-}
setRemoteLocations
    :: [RemoteLocation]
    -- ^ List of remote locations.

    -> SetRemoteLocations
setRemoteLocations _0 = SetRemoteLocations _0


------------------------------------------------------------------------------
-- | Issued when attached to target because of auto-attach or @attachToTarget@ command.
{-# WARNING AttachedToTarget "This feature is marked as EXPERIMENTAL." #-}
data AttachedToTarget = AttachedToTarget
    { -- | Identifier assigned to the session used to send\/receive messages.
      sessionId :: !SessionID
    , targetInfo :: !TargetInfo
    , waitingForDebugger :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AttachedToTarget where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "attachedToTarget" $ \_o -> AttachedToTarget
            <$> _o .: "sessionId"
            <*> _o .: "targetInfo"
            <*> _o .: "waitingForDebugger"
        ago = A.withArray "attachedToTarget" $ \_a -> AttachedToTarget
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON AttachedToTarget where
    toEncoding (AttachedToTarget _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "sessionId" .= _0
        , P.pure $ "targetInfo" .= _1
        , P.pure $ "waitingForDebugger" .= _2
        ]
    toJSON (AttachedToTarget _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "sessionId" .= _0
        , P.pure $ "targetInfo" .= _1
        , P.pure $ "waitingForDebugger" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup AttachedToTarget where
    AttachedToTarget _0 _1 _2 <> AttachedToTarget _ _ _ = AttachedToTarget _0 _1 _2


------------------------------------------------------------------------------
instance E.Event AttachedToTarget where
    type Result AttachedToTarget = AttachedToTarget
    name _ = "Target.attachedToTarget"


------------------------------------------------------------------------------
-- | Issued when attached to target because of auto-attach or @attachToTarget@ command.
{-# WARNING attachedToTarget "This feature is marked as EXPERIMENTAL." #-}
attachedToTarget :: P.Proxy AttachedToTarget
attachedToTarget = P.Proxy


------------------------------------------------------------------------------
-- | Issued when detached from target for any reason (including @detachFromTarget@ command). Can be
-- issued multiple times per target if multiple sessions have been attached to it.
{-# WARNING DetachedFromTarget "This feature is marked as EXPERIMENTAL." #-}
{-{-# DEPRECATED targetId "This may be removed in a future release." #-}-}
data DetachedFromTarget = DetachedFromTarget
    { -- | Detached session identifier.
      sessionId :: !SessionID
      -- | Deprecated.
    , targetId :: !(P.Maybe TargetID)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON DetachedFromTarget where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "detachedFromTarget" $ \_o -> DetachedFromTarget
            <$> _o .: "sessionId"
            <*> _o .:? "targetId"
        ago = A.withArray "detachedFromTarget" $ \_a -> DetachedFromTarget
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON DetachedFromTarget where
    toEncoding (DetachedFromTarget _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "sessionId" .= _0
        , ("targetId" .=) <$> _1
        ]
    toJSON (DetachedFromTarget _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "sessionId" .= _0
        , ("targetId" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup DetachedFromTarget where
    DetachedFromTarget _0 _1 <> DetachedFromTarget _ __1 = DetachedFromTarget _0 (_1 <|> __1)


------------------------------------------------------------------------------
instance E.Event DetachedFromTarget where
    type Result DetachedFromTarget = DetachedFromTarget
    name _ = "Target.detachedFromTarget"


------------------------------------------------------------------------------
-- | Issued when detached from target for any reason (including @detachFromTarget@ command). Can be
-- issued multiple times per target if multiple sessions have been attached to it.
{-# WARNING detachedFromTarget "This feature is marked as EXPERIMENTAL." #-}
detachedFromTarget :: P.Proxy DetachedFromTarget
detachedFromTarget = P.Proxy


------------------------------------------------------------------------------
-- | Notifies about a new protocol message received from the session (as reported in
-- @attachedToTarget@ event).
{-{-# DEPRECATED targetId "This may be removed in a future release." #-}-}
data ReceivedMessageFromTarget = ReceivedMessageFromTarget
    { -- | Identifier of a session which sends a message.
      sessionId :: !SessionID
    , message :: !T.Text
      -- | Deprecated.
    , targetId :: !(P.Maybe TargetID)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ReceivedMessageFromTarget where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "receivedMessageFromTarget" $ \_o -> ReceivedMessageFromTarget
            <$> _o .: "sessionId"
            <*> _o .: "message"
            <*> _o .:? "targetId"
        ago = A.withArray "receivedMessageFromTarget" $ \_a -> ReceivedMessageFromTarget
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON ReceivedMessageFromTarget where
    toEncoding (ReceivedMessageFromTarget _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "sessionId" .= _0
        , P.pure $ "message" .= _1
        , ("targetId" .=) <$> _2
        ]
    toJSON (ReceivedMessageFromTarget _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "sessionId" .= _0
        , P.pure $ "message" .= _1
        , ("targetId" .=) <$> _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup ReceivedMessageFromTarget where
    ReceivedMessageFromTarget _0 _1 _2 <> ReceivedMessageFromTarget _ _ __2 = ReceivedMessageFromTarget _0 _1 (_2 <|> __2)


------------------------------------------------------------------------------
instance E.Event ReceivedMessageFromTarget where
    type Result ReceivedMessageFromTarget = ReceivedMessageFromTarget
    name _ = "Target.receivedMessageFromTarget"


------------------------------------------------------------------------------
-- | Notifies about a new protocol message received from the session (as reported in
-- @attachedToTarget@ event).
receivedMessageFromTarget :: P.Proxy ReceivedMessageFromTarget
receivedMessageFromTarget = P.Proxy


------------------------------------------------------------------------------
-- | Issued when a possible inspection target is created.
data TargetCreated = TargetCreated
    { targetInfo :: !TargetInfo
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TargetCreated where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "targetCreated" $ \_o -> TargetCreated
            <$> _o .: "targetInfo"
        ago = A.withArray "targetCreated" $ \_a -> TargetCreated
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON TargetCreated where
    toEncoding (TargetCreated _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "targetInfo" .= _0
        ]
    toJSON (TargetCreated _0) = A.object $ P.catMaybes
        [ P.pure $ "targetInfo" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup TargetCreated where
    TargetCreated _0 <> TargetCreated _ = TargetCreated _0


------------------------------------------------------------------------------
instance E.Event TargetCreated where
    type Result TargetCreated = TargetCreated
    name _ = "Target.targetCreated"


------------------------------------------------------------------------------
-- | Issued when a possible inspection target is created.
targetCreated :: P.Proxy TargetCreated
targetCreated = P.Proxy


------------------------------------------------------------------------------
-- | Issued when a target is destroyed.
data TargetDestroyed = TargetDestroyed
    { targetId :: !TargetID
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TargetDestroyed where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "targetDestroyed" $ \_o -> TargetDestroyed
            <$> _o .: "targetId"
        ago = A.withArray "targetDestroyed" $ \_a -> TargetDestroyed
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON TargetDestroyed where
    toEncoding (TargetDestroyed _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "targetId" .= _0
        ]
    toJSON (TargetDestroyed _0) = A.object $ P.catMaybes
        [ P.pure $ "targetId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup TargetDestroyed where
    TargetDestroyed _0 <> TargetDestroyed _ = TargetDestroyed _0


------------------------------------------------------------------------------
instance E.Event TargetDestroyed where
    type Result TargetDestroyed = TargetDestroyed
    name _ = "Target.targetDestroyed"


------------------------------------------------------------------------------
-- | Issued when a target is destroyed.
targetDestroyed :: P.Proxy TargetDestroyed
targetDestroyed = P.Proxy


------------------------------------------------------------------------------
-- | Issued when a target has crashed.
data TargetCrashed = TargetCrashed
    { targetId :: !TargetID
      -- | Termination status type.
    , status :: !T.Text
      -- | Termination error code.
    , errorCode :: !P.Int
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TargetCrashed where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "targetCrashed" $ \_o -> TargetCrashed
            <$> _o .: "targetId"
            <*> _o .: "status"
            <*> _o .: "errorCode"
        ago = A.withArray "targetCrashed" $ \_a -> TargetCrashed
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON TargetCrashed where
    toEncoding (TargetCrashed _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "targetId" .= _0
        , P.pure $ "status" .= _1
        , P.pure $ "errorCode" .= _2
        ]
    toJSON (TargetCrashed _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "targetId" .= _0
        , P.pure $ "status" .= _1
        , P.pure $ "errorCode" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup TargetCrashed where
    TargetCrashed _0 _1 _2 <> TargetCrashed _ _ _ = TargetCrashed _0 _1 _2


------------------------------------------------------------------------------
instance E.Event TargetCrashed where
    type Result TargetCrashed = TargetCrashed
    name _ = "Target.targetCrashed"


------------------------------------------------------------------------------
-- | Issued when a target has crashed.
targetCrashed :: P.Proxy TargetCrashed
targetCrashed = P.Proxy


------------------------------------------------------------------------------
-- | Issued when some information about a target has changed. This only happens between
-- @targetCreated@ and @targetDestroyed@.
data TargetInfoChanged = TargetInfoChanged
    { targetInfo :: !TargetInfo
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON TargetInfoChanged where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "targetInfoChanged" $ \_o -> TargetInfoChanged
            <$> _o .: "targetInfo"
        ago = A.withArray "targetInfoChanged" $ \_a -> TargetInfoChanged
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON TargetInfoChanged where
    toEncoding (TargetInfoChanged _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "targetInfo" .= _0
        ]
    toJSON (TargetInfoChanged _0) = A.object $ P.catMaybes
        [ P.pure $ "targetInfo" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup TargetInfoChanged where
    TargetInfoChanged _0 <> TargetInfoChanged _ = TargetInfoChanged _0


------------------------------------------------------------------------------
instance E.Event TargetInfoChanged where
    type Result TargetInfoChanged = TargetInfoChanged
    name _ = "Target.targetInfoChanged"


------------------------------------------------------------------------------
-- | Issued when some information about a target has changed. This only happens between
-- @targetCreated@ and @targetDestroyed@.
targetInfoChanged :: P.Proxy TargetInfoChanged
targetInfoChanged = P.Proxy

