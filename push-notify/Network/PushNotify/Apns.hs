-- GSoC 2013 - Communicating with mobile devices.

-- | This library defines an API for communicating with iOS powered devices, sending Push Notifications through Apple Push Notification Service.

module Network.PushNotify.Apns
    ( 
    -- * APNS Service
      sendAPNS
    , startAPNS
    , closeAPNS
    , feedBackAPNS
    -- * APNS Settings
    , APNSAppConfig(..)
    , APNSManager(..)
    , DeviceToken
    , Env(..)
    -- * APNS Messages
    , APNSmessage(..)
    , AlertDictionary(..)
    -- * APNS Results
    , APNSresult(..)
    , APNSFeedBackresult(..)
    ) where

import Network.PushNotify.Apns.Types
import Network.PushNotify.Apns.Send
