-- | This library defines a general API for communicating with iOS, Android and WPhone powered devices, sending Push Notifications.

module PushNotify.General
    ( 
    -- * Push Service
      startPushService
    , closePushService
    , sendPush
    -- * Yesod Push Subsite
    , PushAppSub
    -- * Push Settings
    , Device(..)
    , PushServiceConfig(..)
    , RegisterResult(..)
    , PushAppConfig(..)
    , PushManager
    -- * Push Message
    , PushNotification(..)
    -- * Push Result
    , PushResult(..)
    , IsPushResult(..)
    ) where

import PushNotify.General.Send
import PushNotify.General.Types
import PushNotify.General.YesodPushApp
