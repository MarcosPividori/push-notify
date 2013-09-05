-- GSoC 2013 - Communicating with mobile devices.

{-# LANGUAGE OverloadedStrings #-}

-- | This Module define the main data types for sending Push Notifications through Microsoft Push Notification Service.
module Network.PushNotify.Mpns.Types
    ( -- * MPNS Settings
      MPNSAppConfig(..)
    , DeviceURI
      -- * MPNS Messages
    , MPNSType(..)
    , MPNSInterval(..)
    , MPNSmessage(..)
      -- * MPNS Result
    , MPNSresult(..)
    , MPNSinfo(..)
    , MPNSnotifStatus(..)
    , MPNSsubStatus(..)
    , MPNSconStatus(..)
    ) where

import Network.PushNotify.Mpns.Constants
import Data.Default
import Data.Text
import Text.XML
import Control.Monad.Writer
import qualified Control.Exception as CE

-- | 'MPNSAppConfig' represents the main necessary information for sending notifications through MPNS.
-- If it is not necessary a secure connection, the default value can be used.
data MPNSAppConfig = MPNSAppConfig{
        numRet       :: Int    -- ^ Number of attemps to send the message to the server.
    ,   useSecure    :: Bool   -- ^ To set a secure connection (HTTPS).
    ,   certificate  :: String -- ^ Certificate (only necessary for secure connections).
    ,   privateKey   :: String -- ^ Private key (only necessary for secure connections).
    }   deriving Show

instance Default MPNSAppConfig where
    def = MPNSAppConfig{
        numRet      = 5
    ,   useSecure   = False
    ,   certificate = ""
    ,   privateKey  = ""
    }

-- | 'DeviceURI' is an unique identifier of an app/device, provided by MPNS.
type DeviceURI = Text

-- | 'MPNSType' represents the three different kind of notifications.
data MPNSType = Toast | Raw | Tile deriving Show

-- | 'MPNSType' represents the batching interval.
data MPNSInterval = Immediate -- ^ Immediate delivery.
                  | Sec450    -- ^ Delivered within 450 seconds.
                  | Sec900    -- ^ Delivered within 900 seconds.
                  deriving Show

-- | 'MPNSmessage' represents a message to be sent through MPNS.
data MPNSmessage = MPNSmessage{
        deviceURIs          :: [DeviceURI]  -- ^ Destination.
    ,   batching_interval   :: MPNSInterval -- ^ When to deliver the notification.
    ,   target              :: MPNSType     -- ^ The kind of notification.
    ,   restXML             :: Document     -- ^ The XML data content to be sent.
    } deriving Show

instance Default MPNSmessage where
    def = MPNSmessage {
        deviceURIs          = []
    ,   batching_interval   = Immediate
    ,   target              = Raw
    ,   restXML             = parseText_ def ""
    }


-- | 'MPNSresult' represents information about messages after a communication with MPNS Servers.
data MPNSresult = MPNSresult{
        sucessfullResults :: [(DeviceURI,MPNSinfo)]         -- ^ Notifications that were successfully sent.
    ,   errorException    :: [(DeviceURI,CE.SomeException)] -- ^ Failed notifications that you need to resend,
                                                            -- because there was a problem connecting with MPNS servers.
    } deriving Show

-- | 'MPNSnotifStatus' represents the status of a notification which has been sent.
data MPNSnotifStatus = Received  | Dropped  | QueueFull deriving Show

-- | 'MPNSsubStatus' represents the status of a subscription.
data MPNSsubStatus   = Active    | Expired              deriving Show

-- | 'MPNSconStatus' represents the status of a connection.
data MPNSconStatus   = Connected | InActive | Disconnected | TempDisconnected deriving Show

-- | 'MPNSinfo' represents information about a specific notification and device, after a communication with MPNS Servers.
data MPNSinfo = MPNSinfo {
        notificationStatus :: Maybe MPNSnotifStatus
    ,   subscriptionStatus :: Maybe MPNSsubStatus
    ,   connectionStatus   :: Maybe MPNSconStatus
    } deriving Show
