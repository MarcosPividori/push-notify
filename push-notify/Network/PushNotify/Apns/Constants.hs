-- GSoC 2013 - Communicating with mobile devices.
{-# LANGUAGE OverloadedStrings #-}

-- | This Module define the main contants for sending Push Notifications through Apple Push Notification Service.

module Network.PushNotify.Apns.Constants where

import Data.Text

cDEVELOPMENT_URL :: String
cDEVELOPMENT_URL = "localhost"

cDEVELOPMENT_PORT :: Integer
cDEVELOPMENT_PORT = 2195

cDEVELOPMENT_FEEDBACK_URL :: String
cDEVELOPMENT_FEEDBACK_URL = "localhost"

cDEVELOPMENT_FEEDBACK_PORT :: Integer
cDEVELOPMENT_FEEDBACK_PORT = 2196

cPRODUCTION_URL :: String
cPRODUCTION_URL = "gateway.push.apple.com"

cPRODUCTION_PORT :: Integer
cPRODUCTION_PORT = 2195

cPRODUCTION_FEEDBACK_URL :: String
cPRODUCTION_FEEDBACK_URL = "feedback.push.apple.com"

cPRODUCTION_FEEDBACK_PORT :: Integer
cPRODUCTION_FEEDBACK_PORT = 2196

-- Fields for JSON object to be sent to APNS servers.

cAPPS :: Text
cAPPS = "aps"

cALERT :: Text
cALERT = "alert"

cBADGE :: Text
cBADGE = "badge"

cSOUND :: Text
cSOUND = "sound"

cBODY :: Text
cBODY = "body"

cACTION_LOC_KEY :: Text
cACTION_LOC_KEY = "action-loc-key"

cLOC_KEY :: Text
cLOC_KEY = "loc-key"

cLOC_ARGS :: Text
cLOC_ARGS = "loc-args"

cLAUNCH_IMAGE :: Text
cLAUNCH_IMAGE = "launch-image"
