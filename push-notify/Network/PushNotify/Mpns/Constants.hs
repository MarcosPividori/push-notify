-- GSoC 2013 - Communicating with mobile devices.

{-# LANGUAGE OverloadedStrings #-}

-- | This Module define the main contants for sending Push Notifications through Microsoft Push Notification Service.
module Network.PushNotify.Mpns.Constants where

import Network.HTTP.Types
import Data.ByteString
import Data.Text

cWindowsPhoneTarget :: HeaderName
cWindowsPhoneTarget = "X-WindowsPhone-Target"

cNotificationClass :: HeaderName
cNotificationClass = "X-NotificationClass"

cToken :: ByteString
cToken = "token"

cToast :: ByteString
cToast = "toast"

-- Fields for a Header response to a successful request.

cNotificationStatus :: HeaderName
cNotificationStatus = "X-NotificationStatus"

cSubscriptionStatus :: HeaderName
cSubscriptionStatus = "X-SubscriptionStatus"

cDeviceConnectionStatus :: HeaderName
cDeviceConnectionStatus = "X-DeviceConnectionStatus"

cNotifReceived :: Text
cNotifReceived = "Received"

cNotifDropped :: Text
cNotifDropped = "Dropped"

cNotifQueuefull :: Text
cNotifQueuefull ="QueueFull"

cSubActive :: Text
cSubActive = "Active"

cSubExpired :: Text
cSubExpired = "Expired"

cConnConnected :: Text
cConnConnected = "Connected"

cConnInactive :: Text
cConnInactive = "InActive"

cConnDisconnected :: Text
cConnDisconnected = "Disconnected"

cConnTempDisconn :: Text
cConnTempDisconn = "TempDisconnected"

