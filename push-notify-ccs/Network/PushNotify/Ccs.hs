-- GSoC 2013 - Communicating with mobile devices.

-- | This library defines an API for communicating with Android powered devices, sending Push Notifications through Cloud Connection Server (GCM).
--
--The GCM Cloud Connection Server (CCS) is a connection server based on XMPP.
--
--CCS allows 3rd-party app servers to communicate with Android devices by establishing a persistent TCP connection with Google servers using the XMPP protocol. This communication is asynchronous and bidirectional.
--
--To establish a XMPP connection, this library uses Pontarius XMPP library.

module Network.PushNotify.Ccs
    (
    -- * CCS Service
      startCCS
    , closeCCS
    , sendCCS
    -- * CCS Settings
    , GCMCcsConfig(..)
    -- * CCS Utilities
    , withCCS
    , withCCS'
    , CCSManager
    , module Network.PushNotify.Gcm.Types
    ) where

import Network.PushNotify.Gcm.Types
import Network.PushNotify.Ccs.Types
import Network.PushNotify.Ccs.Send
