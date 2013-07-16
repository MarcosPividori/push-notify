-- GSoC 2013 - Communicating with mobile devices.
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
-- | This Module define the main data types for sending Push Notifications through Microsoft Push Notification Service.
module Types
    ( MPNSAppConfig(..)
    , MPNSmessage(..)
    , MPNSresult(..)
    , MPNSinfo(..)
    , MPNSType(..)
    , MPNSInterval(..)
    , DeviceURI
    ) where

import Constants
import Data.Default
import Data.Text
import Text.XML
import Control.Monad.Writer


-- | 'MPNSAppConfig' represents the main necessary information for sending notifications through MPNS.
data MPNSAppConfig = MPNSAppConfig{
        numRet :: Int-- to be completed
    }   deriving Show

instance Default MPNSAppConfig where
    def = MPNSAppConfig{
        numRet = 5
    }

type DeviceURI = Text

data MPNSType = Toast | Raw | Tile deriving Show

data MPNSInterval = Immediate | Sec450 | Sec900 deriving Show

-- | 'MPNSmessage' represents a message to be sent through MPNS.
data MPNSmessage = MPNSmessage{
        deviceURIs          :: [DeviceURI]
    ,   batching_interval   :: MPNSInterval
    ,   target              :: MPNSType
    ,   rest                :: Document
    } deriving Show

instance Default MPNSmessage where
    def = MPNSmessage {
        deviceURIs          = []
    ,   batching_interval   = Immediate
    ,   target              = Raw
    ,   rest                = parseText_ def ""
    }


-- | 'MPNSresult' represents information about messages after a communication with MPNS Servers.
data MPNSresult = MPNSresult{
        results :: [(DeviceURI,MPNSinfo)]
    } deriving Show

data MPNSinfo = MPNSinfo {
        notificationStatus :: Maybe Text -- Received|Dropped|QueueFull
    ,   subscriptionStatus :: Maybe Text -- Active|Expired
    ,   connectionStatus   :: Maybe Text -- Connected|InActive|Disconnected|TempDisconnected
    } deriving Show
