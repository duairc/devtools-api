{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DevTools.API.Animation{-{-# WARNING "This feature is marked as EXPERIMENTAL." #-}-}

    ( module DevTools.API.Animation.Types
    , module DevTools.API.Animation
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
import           DevTools.API.Animation.Types
import qualified DevTools.API.Runtime.Types as Runtime


-- hashable-------------------------------------------------------------------
import qualified Data.Hashable as H (Hashable)


-- text-----------------------------------------------------------------------
import qualified Data.Text as T


-- vector---------------------------------------------------------------------
import           Data.Vector ((!?))


------------------------------------------------------------------------------
-- | Disables animation domain notifications.
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
    name _ = "Animation.disable"


------------------------------------------------------------------------------
-- | Disables animation domain notifications.
disable
    :: Disable
disable = Disable


------------------------------------------------------------------------------
-- | Enables animation domain notifications.
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
    name _ = "Animation.enable"


------------------------------------------------------------------------------
-- | Enables animation domain notifications.
enable
    :: Enable
enable = Enable


------------------------------------------------------------------------------
-- | Returns the current time of the an animation.
data GetCurrentTime = GetCurrentTime
    { -- | Id of animation.
      id :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetCurrentTime where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getCurrentTime" $ \_o -> GetCurrentTime
            <$> _o .: "id"
        ago = A.withArray "getCurrentTime" $ \_a -> GetCurrentTime
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetCurrentTime where
    toEncoding (GetCurrentTime _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "id" .= _0
        ]
    toJSON (GetCurrentTime _0) = A.object $ P.catMaybes
        [ P.pure $ "id" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetCurrentTime where
    GetCurrentTime _0 <> GetCurrentTime _ = GetCurrentTime _0


------------------------------------------------------------------------------
-- | Returns the current time of the an animation.
data GetCurrentTimeResult = GetCurrentTimeResult
    { -- | Current time of the page.
      currentTime :: !P.Double
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetCurrentTimeResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getCurrentTimeResult" $ \_o -> GetCurrentTimeResult
            <$> _o .: "currentTime"
        ago = A.withArray "getCurrentTimeResult" $ \_a -> GetCurrentTimeResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetCurrentTimeResult where
    toEncoding (GetCurrentTimeResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "currentTime" .= _0
        ]
    toJSON (GetCurrentTimeResult _0) = A.object $ P.catMaybes
        [ P.pure $ "currentTime" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetCurrentTimeResult where
    GetCurrentTimeResult _0 <> GetCurrentTimeResult _ = GetCurrentTimeResult _0


------------------------------------------------------------------------------
instance M.Method GetCurrentTime where
    type Result GetCurrentTime = GetCurrentTimeResult
    name _ = "Animation.getCurrentTime"


------------------------------------------------------------------------------
-- | Returns the current time of the an animation.
getCurrentTime
    :: T.Text
    -- ^ Id of animation.

    -> GetCurrentTime
getCurrentTime _0 = GetCurrentTime _0


------------------------------------------------------------------------------
-- | Gets the playback rate of the document timeline.
data GetPlaybackRate = GetPlaybackRate
    {
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetPlaybackRate where
    parseJSON A.Null = P.pure GetPlaybackRate
    parseJSON v = A.withArray "getPlaybackRate" go v
        <|> A.withObject "getPlaybackRate" go v
      where
        go _ = P.pure GetPlaybackRate


------------------------------------------------------------------------------
instance A.ToJSON GetPlaybackRate where
    toEncoding GetPlaybackRate = A.pairs $ P.fold $ P.catMaybes
        [
        ]
    toJSON GetPlaybackRate = A.object $ P.catMaybes
        [
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetPlaybackRate where
    GetPlaybackRate <> GetPlaybackRate = GetPlaybackRate


------------------------------------------------------------------------------
instance P.Monoid GetPlaybackRate where
    mempty = GetPlaybackRate


------------------------------------------------------------------------------
-- | Gets the playback rate of the document timeline.
data GetPlaybackRateResult = GetPlaybackRateResult
    { -- | Playback rate for animations on page.
      playbackRate :: !P.Double
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON GetPlaybackRateResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "getPlaybackRateResult" $ \_o -> GetPlaybackRateResult
            <$> _o .: "playbackRate"
        ago = A.withArray "getPlaybackRateResult" $ \_a -> GetPlaybackRateResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON GetPlaybackRateResult where
    toEncoding (GetPlaybackRateResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "playbackRate" .= _0
        ]
    toJSON (GetPlaybackRateResult _0) = A.object $ P.catMaybes
        [ P.pure $ "playbackRate" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup GetPlaybackRateResult where
    GetPlaybackRateResult _0 <> GetPlaybackRateResult _ = GetPlaybackRateResult _0


------------------------------------------------------------------------------
instance M.Method GetPlaybackRate where
    type Result GetPlaybackRate = GetPlaybackRateResult
    name _ = "Animation.getPlaybackRate"


------------------------------------------------------------------------------
-- | Gets the playback rate of the document timeline.
getPlaybackRate
    :: GetPlaybackRate
getPlaybackRate = GetPlaybackRate


------------------------------------------------------------------------------
-- | Releases a set of animations to no longer be manipulated.
data ReleaseAnimations = ReleaseAnimations
    { -- | List of animation ids to seek.
      animations :: ![T.Text]
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ReleaseAnimations where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "releaseAnimations" $ \_o -> ReleaseAnimations
            <$> _o .: "animations"
        ago = A.withArray "releaseAnimations" $ \_a -> ReleaseAnimations
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON ReleaseAnimations where
    toEncoding (ReleaseAnimations _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "animations" .= _0
        ]
    toJSON (ReleaseAnimations _0) = A.object $ P.catMaybes
        [ P.pure $ "animations" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup ReleaseAnimations where
    ReleaseAnimations _0 <> ReleaseAnimations _ = ReleaseAnimations _0


------------------------------------------------------------------------------
instance M.Method ReleaseAnimations where
    type Result ReleaseAnimations = ()
    name _ = "Animation.releaseAnimations"


------------------------------------------------------------------------------
-- | Releases a set of animations to no longer be manipulated.
releaseAnimations
    :: [T.Text]
    -- ^ List of animation ids to seek.

    -> ReleaseAnimations
releaseAnimations _0 = ReleaseAnimations _0


------------------------------------------------------------------------------
-- | Gets the remote object of the Animation.
data ResolveAnimation = ResolveAnimation
    { -- | Animation id.
      animationId :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ResolveAnimation where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "resolveAnimation" $ \_o -> ResolveAnimation
            <$> _o .: "animationId"
        ago = A.withArray "resolveAnimation" $ \_a -> ResolveAnimation
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON ResolveAnimation where
    toEncoding (ResolveAnimation _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "animationId" .= _0
        ]
    toJSON (ResolveAnimation _0) = A.object $ P.catMaybes
        [ P.pure $ "animationId" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup ResolveAnimation where
    ResolveAnimation _0 <> ResolveAnimation _ = ResolveAnimation _0


------------------------------------------------------------------------------
-- | Gets the remote object of the Animation.
data ResolveAnimationResult = ResolveAnimationResult
    { -- | Corresponding remote object.
      remoteObject :: !Runtime.RemoteObject
    }
  deriving
    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON ResolveAnimationResult where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "resolveAnimationResult" $ \_o -> ResolveAnimationResult
            <$> _o .: "remoteObject"
        ago = A.withArray "resolveAnimationResult" $ \_a -> ResolveAnimationResult
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON ResolveAnimationResult where
    toEncoding (ResolveAnimationResult _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "remoteObject" .= _0
        ]
    toJSON (ResolveAnimationResult _0) = A.object $ P.catMaybes
        [ P.pure $ "remoteObject" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup ResolveAnimationResult where
    ResolveAnimationResult _0 <> ResolveAnimationResult _ = ResolveAnimationResult _0


------------------------------------------------------------------------------
instance M.Method ResolveAnimation where
    type Result ResolveAnimation = ResolveAnimationResult
    name _ = "Animation.resolveAnimation"


------------------------------------------------------------------------------
-- | Gets the remote object of the Animation.
resolveAnimation
    :: T.Text
    -- ^ Animation id.

    -> ResolveAnimation
resolveAnimation _0 = ResolveAnimation _0


------------------------------------------------------------------------------
-- | Seek a set of animations to a particular time within each animation.
data SeekAnimations = SeekAnimations
    { -- | List of animation ids to seek.
      animations :: ![T.Text]
      -- | Set the current time of each animation.
    , currentTime :: !P.Double
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SeekAnimations where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "seekAnimations" $ \_o -> SeekAnimations
            <$> _o .: "animations"
            <*> _o .: "currentTime"
        ago = A.withArray "seekAnimations" $ \_a -> SeekAnimations
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SeekAnimations where
    toEncoding (SeekAnimations _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "animations" .= _0
        , P.pure $ "currentTime" .= _1
        ]
    toJSON (SeekAnimations _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "animations" .= _0
        , P.pure $ "currentTime" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SeekAnimations where
    SeekAnimations _0 _1 <> SeekAnimations _ _ = SeekAnimations _0 _1


------------------------------------------------------------------------------
instance M.Method SeekAnimations where
    type Result SeekAnimations = ()
    name _ = "Animation.seekAnimations"


------------------------------------------------------------------------------
-- | Seek a set of animations to a particular time within each animation.
seekAnimations
    :: [T.Text]
    -- ^ List of animation ids to seek.

    -> P.Double
    -- ^ Set the current time of each animation.

    -> SeekAnimations
seekAnimations _0 _1 = SeekAnimations _0 _1


------------------------------------------------------------------------------
-- | Sets the paused state of a set of animations.
data SetPaused = SetPaused
    { -- | Animations to set the pause state of.
      animations :: ![T.Text]
      -- | Paused state to set to.
    , paused :: !P.Bool
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetPaused where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setPaused" $ \_o -> SetPaused
            <$> _o .: "animations"
            <*> _o .: "paused"
        ago = A.withArray "setPaused" $ \_a -> SetPaused
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)


------------------------------------------------------------------------------
instance A.ToJSON SetPaused where
    toEncoding (SetPaused _0 _1) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "animations" .= _0
        , P.pure $ "paused" .= _1
        ]
    toJSON (SetPaused _0 _1) = A.object $ P.catMaybes
        [ P.pure $ "animations" .= _0
        , P.pure $ "paused" .= _1
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetPaused where
    SetPaused _0 _1 <> SetPaused _ _ = SetPaused _0 _1


------------------------------------------------------------------------------
instance M.Method SetPaused where
    type Result SetPaused = ()
    name _ = "Animation.setPaused"


------------------------------------------------------------------------------
-- | Sets the paused state of a set of animations.
setPaused
    :: [T.Text]
    -- ^ Animations to set the pause state of.

    -> P.Bool
    -- ^ Paused state to set to.

    -> SetPaused
setPaused _0 _1 = SetPaused _0 _1


------------------------------------------------------------------------------
-- | Sets the playback rate of the document timeline.
data SetPlaybackRate = SetPlaybackRate
    { -- | Playback rate for animations on page
      playbackRate :: !P.Double
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetPlaybackRate where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setPlaybackRate" $ \_o -> SetPlaybackRate
            <$> _o .: "playbackRate"
        ago = A.withArray "setPlaybackRate" $ \_a -> SetPlaybackRate
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON SetPlaybackRate where
    toEncoding (SetPlaybackRate _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "playbackRate" .= _0
        ]
    toJSON (SetPlaybackRate _0) = A.object $ P.catMaybes
        [ P.pure $ "playbackRate" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetPlaybackRate where
    SetPlaybackRate _0 <> SetPlaybackRate _ = SetPlaybackRate _0


------------------------------------------------------------------------------
instance M.Method SetPlaybackRate where
    type Result SetPlaybackRate = ()
    name _ = "Animation.setPlaybackRate"


------------------------------------------------------------------------------
-- | Sets the playback rate of the document timeline.
setPlaybackRate
    :: P.Double
    -- ^ Playback rate for animations on page

    -> SetPlaybackRate
setPlaybackRate _0 = SetPlaybackRate _0


------------------------------------------------------------------------------
-- | Sets the timing of an animation node.
data SetTiming = SetTiming
    { -- | Animation id.
      animationId :: !T.Text
      -- | Duration of the animation.
    , duration :: !P.Double
      -- | Delay of the animation.
    , delay :: !P.Double
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON SetTiming where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "setTiming" $ \_o -> SetTiming
            <$> _o .: "animationId"
            <*> _o .: "duration"
            <*> _o .: "delay"
        ago = A.withArray "setTiming" $ \_a -> SetTiming
            <$> P.maybe P.empty A.parseJSON (_a !? 0)
            <*> P.maybe P.empty A.parseJSON (_a !? 1)
            <*> P.maybe P.empty A.parseJSON (_a !? 2)


------------------------------------------------------------------------------
instance A.ToJSON SetTiming where
    toEncoding (SetTiming _0 _1 _2) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "animationId" .= _0
        , P.pure $ "duration" .= _1
        , P.pure $ "delay" .= _2
        ]
    toJSON (SetTiming _0 _1 _2) = A.object $ P.catMaybes
        [ P.pure $ "animationId" .= _0
        , P.pure $ "duration" .= _1
        , P.pure $ "delay" .= _2
        ]


------------------------------------------------------------------------------
instance P.Semigroup SetTiming where
    SetTiming _0 _1 _2 <> SetTiming _ _ _ = SetTiming _0 _1 _2


------------------------------------------------------------------------------
instance M.Method SetTiming where
    type Result SetTiming = ()
    name _ = "Animation.setTiming"


------------------------------------------------------------------------------
-- | Sets the timing of an animation node.
setTiming
    :: T.Text
    -- ^ Animation id.

    -> P.Double
    -- ^ Duration of the animation.

    -> P.Double
    -- ^ Delay of the animation.

    -> SetTiming
setTiming _0 _1 _2 = SetTiming _0 _1 _2


------------------------------------------------------------------------------
-- | Event for when an animation has been cancelled.
data AnimationCanceled = AnimationCanceled
    { -- | Id of the animation that was cancelled.
      id :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AnimationCanceled where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "animationCanceled" $ \_o -> AnimationCanceled
            <$> _o .: "id"
        ago = A.withArray "animationCanceled" $ \_a -> AnimationCanceled
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON AnimationCanceled where
    toEncoding (AnimationCanceled _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "id" .= _0
        ]
    toJSON (AnimationCanceled _0) = A.object $ P.catMaybes
        [ P.pure $ "id" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup AnimationCanceled where
    AnimationCanceled _0 <> AnimationCanceled _ = AnimationCanceled _0


------------------------------------------------------------------------------
instance E.Event AnimationCanceled where
    type Result AnimationCanceled = AnimationCanceled
    name _ = "Animation.animationCanceled"


------------------------------------------------------------------------------
-- | Event for when an animation has been cancelled.
animationCanceled :: P.Proxy AnimationCanceled
animationCanceled = P.Proxy


------------------------------------------------------------------------------
-- | Event for each animation that has been created.
data AnimationCreated = AnimationCreated
    { -- | Id of the animation that was created.
      id :: !T.Text
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AnimationCreated where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "animationCreated" $ \_o -> AnimationCreated
            <$> _o .: "id"
        ago = A.withArray "animationCreated" $ \_a -> AnimationCreated
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON AnimationCreated where
    toEncoding (AnimationCreated _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "id" .= _0
        ]
    toJSON (AnimationCreated _0) = A.object $ P.catMaybes
        [ P.pure $ "id" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup AnimationCreated where
    AnimationCreated _0 <> AnimationCreated _ = AnimationCreated _0


------------------------------------------------------------------------------
instance E.Event AnimationCreated where
    type Result AnimationCreated = AnimationCreated
    name _ = "Animation.animationCreated"


------------------------------------------------------------------------------
-- | Event for each animation that has been created.
animationCreated :: P.Proxy AnimationCreated
animationCreated = P.Proxy


------------------------------------------------------------------------------
-- | Event for animation that has been started.
data AnimationStarted = AnimationStarted
    { -- | Animation that was started.
      animation :: !Animation
    }
  deriving
    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable
    , D.NFData, H.Hashable
    )


------------------------------------------------------------------------------
instance A.FromJSON AnimationStarted where
    parseJSON v = ago v <|> ogo v
      where
        ogo = A.withObject "animationStarted" $ \_o -> AnimationStarted
            <$> _o .: "animation"
        ago = A.withArray "animationStarted" $ \_a -> AnimationStarted
            <$> P.maybe P.empty A.parseJSON (_a !? 0)


------------------------------------------------------------------------------
instance A.ToJSON AnimationStarted where
    toEncoding (AnimationStarted _0) = A.pairs $ P.fold $ P.catMaybes
        [ P.pure $ "animation" .= _0
        ]
    toJSON (AnimationStarted _0) = A.object $ P.catMaybes
        [ P.pure $ "animation" .= _0
        ]


------------------------------------------------------------------------------
instance P.Semigroup AnimationStarted where
    AnimationStarted _0 <> AnimationStarted _ = AnimationStarted _0


------------------------------------------------------------------------------
instance E.Event AnimationStarted where
    type Result AnimationStarted = AnimationStarted
    name _ = "Animation.animationStarted"


------------------------------------------------------------------------------
-- | Event for animation that has been started.
animationStarted :: P.Proxy AnimationStarted
animationStarted = P.Proxy

