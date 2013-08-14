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
