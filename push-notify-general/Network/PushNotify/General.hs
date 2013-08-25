-- | This library defines a general API for communicating with iOS, Android and WPhone powered devices, sending Push Notifications.

module Network.PushNotify.General
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

import Network.PushNotify.General.Send
import Network.PushNotify.General.Types
import Network.PushNotify.General.YesodPushApp
