{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | This domain provides experimental commands only supported in headless mode.
module DevTools.API.HeadlessExperimental{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

    ( module DevTools.API.HeadlessExperimental.Types
    , module DevTools.API.HeadlessExperimental
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
import           DevTools.API.HeadlessExperimental.Types


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Sends a BeginFrame to the target and returns when the frame was completed. Optionally captures a
-- screenshot from the resulting frame. Requires that the target was created with enabled
-- BeginFrameControl. Designed for use with --run-all-compositor-stages-before-draw, see also
-- https:\/\/goo.gl\/3zHXhB for more background.
data BeginFrame = BeginFrame
    { -- | Timestamp of this BeginFrame in Renderer TimeTicks (milliseconds of uptime). If not set,
      -- the current time will be used.
      frameTimeTicks :: !(P.Maybe P.Double)
      -- | The interval between BeginFrames that is reported to the compositor, in milliseconds.
      -- Defaults to a 60 frames\/second interval, i.e. about 16.666 milliseconds.
    , interval :: !(P.Maybe P.Double)
      -- | Whether updates should not be committed and drawn onto the display. False by default. If
      -- true, only side effects of the BeginFrame will be run, such as layout and animations, but
      -- any visual updates may not be visible on the display or in screenshots.
    , noDisplayUpdates :: !(P.Maybe P.Bool)
      -- | If set, a screenshot of the frame will be captured and returned in the response. Otherwise,
      -- no screenshot will be captured. Note that capturing a screenshot can fail, for example,
      -- during renderer initialization. In such a case, no screenshot data will be returned.
    , screenshot :: !(P.Maybe ScreenshotParams)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON BeginFrame where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "beginFrame" $ \_o -> BeginFrame
            <$> _o .:? "frameTimeTicks"
            <*> _o .:? "interval"
            <*> _o .:? "noDisplayUpdates"
            <*> _o .:? "screenshot"
        ago = A.withArray "beginFrame" $ \_a -> BeginFrame
            <$> P.traverse A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)
            <*> P.traverse A.parseJSON (_a !? 2)
            <*> P.traverse A.parseJSON (_a !? 3)


------------------------------------------------------------------------------
instance A.ToJSON BeginFrame where
    toEncoding (BeginFrame _0 _1 _2 _3) = A.pairs $ P.fold $ P.catMaybes
        [ ("frameTimeTicks" .=) <$> _0
        , ("interval" .=) <$> _1
        , ("noDisplayUpdates" .=) <$> _2
        , ("screenshot" .=) <$> _3
        ]
    toJSON (BeginFrame _0 _1 _2 _3) = A.object $ P.catMaybes
        [ ("frameTimeTicks" .=) <$> _0
        , ("interval" .=) <$> _1
        , ("noDisplayUpdates" .=) <$> _2
        , ("screenshot" .=) <$> _3
        ]


------------------------------------------------------------------------------
instance P.Semigroup BeginFrame where
    BeginFrame _0 _1 _2 _3 <> BeginFrame __0 __1 __2 __3 = BeginFrame (_0 <|> __0) (_1 <|> __1) (_2 <|> __2) (_3 <|> __3)


------------------------------------------------------------------------------
instance P.Monoid BeginFrame where
    mempty = BeginFrame P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Sends a BeginFrame to the target and returns when the frame was completed. Optionally captures a
-- screenshot from the resulting frame. Requires that the target was created with enabled
-- BeginFrameControl. Designed for use with --run-all-compositor-stages-before-draw, see also
-- https:\/\/goo.gl\/3zHXhB for more background.
data BeginFrameResult = BeginFrameResult
    { -- | Whether the BeginFrame resulted in damage and, thus, a new frame was committed to the
      -- display. Reported for diagnostic uses, may be removed in the future.
      hasDamage :: !P.Bool
      -- | Base64-encoded image data of the screenshot, if one was requested and successfully taken.
    , screenshotData :: !(P.Maybe T.Text)
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON BeginFrameResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "beginFrameResult" $ \_o -> BeginFrameResult
            <$> _o .: "hasDamage"
            <*> _o .:? "screenshotData"
        ago = A.withArray "beginFrameResult" $ \_a -> BeginFrameResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.traverse A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON BeginFrameResult where
    toEncoding (BeginFrameResult _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "hasDamage" .= _0
        , ("screenshotData" .=) <$> _1
        ]
    toJSON (BeginFrameResult _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "hasDamage" .= _0
        , ("screenshotData" .=) <$> _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup BeginFrameResult where
    BeginFrameResult _0 _1 <> BeginFrameResult _ __1 = BeginFrameResult _0 (_1 <|> __1)


------------------------------------------------------------------------------
instance M.Method BeginFrame where
    type Result BeginFrame = BeginFrameResult
    name _ = "HeadlessExperimental.beginFrame"


------------------------------------------------------------------------------
-- | Sends a BeginFrame to the target and returns when the frame was completed. Optionally captures a
-- screenshot from the resulting frame. Requires that the target was created with enabled
-- BeginFrameControl. Designed for use with --run-all-compositor-stages-before-draw, see also
-- https:\/\/goo.gl\/3zHXhB for more background.
beginFrame
    :: BeginFrame
beginFrame = BeginFrame P.empty P.empty P.empty P.empty


------------------------------------------------------------------------------
-- | Disables headless events for the target.
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
    name _ = "HeadlessExperimental.disable"


------------------------------------------------------------------------------
-- | Disables headless events for the target.
disable
    :: Disable
disable = Disable


------------------------------------------------------------------------------
-- | Enables headless events for the target.
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
    name _ = "HeadlessExperimental.enable"


------------------------------------------------------------------------------
-- | Enables headless events for the target.
enable
    :: Enable
enable = Enable


------------------------------------------------------------------------------
-- | Issued when the target starts or stops needing BeginFrames.
data NeedsBeginFramesChanged = NeedsBeginFramesChanged
    { -- | True if BeginFrames are needed, false otherwise.
      needsBeginFrames :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON NeedsBeginFramesChanged where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "needsBeginFramesChanged" $ \_o -> NeedsBeginFramesChanged
            <$> _o .: "needsBeginFrames"
        ago = A.withArray "needsBeginFramesChanged" $ \_a -> NeedsBeginFramesChanged
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON NeedsBeginFramesChanged where
    toEncoding (NeedsBeginFramesChanged _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "needsBeginFrames" .= _0
        ]
    toJSON (NeedsBeginFramesChanged _0) = A.object $ P.catMaybes
        [ P.pure $ "needsBeginFrames" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup NeedsBeginFramesChanged where
    NeedsBeginFramesChanged _0 <> NeedsBeginFramesChanged _ = NeedsBeginFramesChanged _0


------------------------------------------------------------------------------
instance E.Event NeedsBeginFramesChanged where
    type Result NeedsBeginFramesChanged = NeedsBeginFramesChanged
    name _ = "HeadlessExperimental.needsBeginFramesChanged"


------------------------------------------------------------------------------
-- | Issued when the target starts or stops needing BeginFrames.
needsBeginFramesChanged :: P.Proxy NeedsBeginFramesChanged
needsBeginFramesChanged = P.Proxy

