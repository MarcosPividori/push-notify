-- GSoC 2013 - Communicating with mobile devices.

-- | This library defines an API for communicating with Android powered devices, sending Push Notifications through Cloud Connection Server (GCM).
--
--The GCM Cloud Connection Server (CCS) is a connection server based on XMPP.
--
--CCS allows 3rd-party app servers to communicate with Android devices by establishing a persistent TCP connection with Google servers using the XMPP protocol. This communication is asynchronous and bidirectional.
--
--To establish a XMPP connection, this library uses the Pontarius library ("Network.Xmpp").

module Network.PushNotify.Ccs
    (
    -- * CCS Service
      sendCCS
    , startCCS
    , closeCCS
    -- * CCS Settings
    , GCMAppConfig(..)
    , RegId
    , CCSManager
    -- * CCS Messages
    , GCMmessage(..)
    -- * CCS Results
    , GCMresult(..)
    ) where

import Network.PushNotify.Ccs.Types
import Network.PushNotify.Ccs.Send
import Network.PushNotify.Gcm.Types
