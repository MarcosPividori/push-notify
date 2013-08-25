-- GSoC 2013 - Communicating with mobile devices.

-- | This library defines an API for communicating with WPhone powered devices, sending Push Notifications through Microsoft Push Notification Service.

module Network.PushNotify.Mpns
    ( 
    -- * MPNS Service
      sendMPNS
    -- * MPNS Settings
    , MPNSAppConfig(..)
    , DeviceURI
    -- * MPNS Messages
    , MPNSmessage(..)
    , MPNSType(..)
    , MPNSInterval(..)
    -- * MPNS Result
    , MPNSresult(..)
    , MPNSinfo(..)
    ) where

import Network.PushNotify.Mpns.Send
import Network.PushNotify.Mpns.Types
