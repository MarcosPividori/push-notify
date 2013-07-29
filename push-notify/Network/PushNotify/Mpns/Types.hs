-- GSoC 2013 - Communicating with mobile devices.
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
-- | This Module define the main data types for sending Push Notifications through Microsoft Push Notification Service.
module Network.PushNotify.Mpns.Types
    ( MPNSAppConfig(..)
    , MPNSmessage(..)
    , MPNSresult(..)
    , MPNSinfo(..)
    , MPNSType(..)
    , MPNSInterval(..)
    , DeviceURI
    ) where

import Network.PushNotify.Mpns.Constants
import Data.Default
import Data.Text
import Text.XML
import Control.Monad.Writer
import qualified Control.Exception as CE

-- | 'MPNSAppConfig' represents the main necessary information for sending notifications through MPNS.
data MPNSAppConfig = MPNSAppConfig{
        numRet       :: Int
    ,   useSecure    :: Bool
    ,   certificate  :: String
    ,   privateKey   :: String
    }   deriving Show

instance Default MPNSAppConfig where
    def = MPNSAppConfig{
        numRet      = 5
    ,   useSecure   = False
    ,   certificate = ""
    ,   privateKey  = ""
    }

type DeviceURI = Text

data MPNSType = Toast | Raw | Tile deriving Show

data MPNSInterval = Immediate | Sec450 | Sec900 deriving Show

-- | 'MPNSmessage' represents a message to be sent through MPNS.
data MPNSmessage = MPNSmessage{
        deviceURIs          :: [DeviceURI]
    ,   batching_interval   :: MPNSInterval
    ,   target              :: MPNSType
    ,   restXML             :: Document
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
        sucessfullResults :: [(DeviceURI,MPNSinfo)]
    ,   errorException    :: [(DeviceURI,CE.SomeException)]
    } deriving Show

data MPNSinfo = MPNSinfo {
        notificationStatus :: Maybe Text -- Received|Dropped|QueueFull
    ,   subscriptionStatus :: Maybe Text -- Active|Expired
    ,   connectionStatus   :: Maybe Text -- Connected|InActive|Disconnected|TempDisconnected
    } deriving Show
