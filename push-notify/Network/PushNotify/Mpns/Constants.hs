-- GSoC 2013 - Communicating with mobile devices.
{-# LANGUAGE OverloadedStrings #-}

-- | This Module define the main contants for sending Push Notifications through Microsoft Push Notification Service.
module Constants where

import Network.HTTP.Types
import Data.ByteString

cWindowsPhoneTarget :: HeaderName
cWindowsPhoneTarget = "X-WindowsPhone-Target"

cNotificationClass :: HeaderName
cNotificationClass = "X-NotificationClass"

cToken :: ByteString
cToken = "token"

cToast :: ByteString
cToast = "toast"

-- Fields for a Header response to a sucessful request.

cNotificationStatus :: HeaderName
cNotificationStatus = "X-NotificationStatus"

cSubscriptionStatus :: HeaderName
cSubscriptionStatus = "X-SubscriptionStatus"

cDeviceConnectionStatus :: HeaderName
cDeviceConnectionStatus = "X-DeviceConnectionStatus"

