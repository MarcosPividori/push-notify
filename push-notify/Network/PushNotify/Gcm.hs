-- GSoC 2013 - Communicating with mobile devices.

-- | This library defines an API for communicating with Android powered devices, sending Push Notifications through Google Cloud Messaging (HTTP connection).

module Network.PushNotify.Gcm
    (
    -- * GCM Service
      sendGCM
    -- * GCM Settings
    , GCMHttpConfig(..)
    , RegId
    -- * GCM Messages
    , GCMmessage(..)
    -- * GCM Result
    , GCMresult(..)
    ) where

import Network.PushNotify.Gcm.Types
import Network.PushNotify.Gcm.Send
