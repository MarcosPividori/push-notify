-- GSoC 2013 - Communicating with mobile devices.
{-# LANGUAGE OverloadedStrings #-}

-- | This Module define the main contants for sending Push Notifications through Google Cloud Messaging.
module Constants where

import Data.Text

cCCS_URL :: Text
cCCS_URL = "gcm.googleapis.com"

cCCS_PORT :: Integer
cCCS_PORT = 5235

cMessageId :: Text
cMessageId = "message_id"

cTo :: Text
cTo = "to"

cMessageType :: Text
cMessageType = "message_type"

cAck :: Text
cAck = "ack"

cFrom :: Text
cFrom = "from"

cData :: Text
cData = "data"

cError :: Text
cError = "error"

cBadRegistration :: Text
cBadRegistration = "BAD_REGISTRATION"

cDeviceUnregistered :: Text
cDeviceUnregistered = "DEVICE_UNREGISTERED"

cInternalServerError :: Text
cInternalServerError = "INTERNAL_SERVER_ERROR"

cServiceUnAvailable :: Text
cServiceUnAvailable = "SERVICE_UNAVAILABLE"
