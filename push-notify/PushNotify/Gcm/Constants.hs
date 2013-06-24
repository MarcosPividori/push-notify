-- GSoC 2013 - Communicating with mobile devices.
{-# LANGUAGE OverloadedStrings #-}

-- | This Module define the main contants for sending Push Notifications through Google Cloud Messaging.
module PushNotify.Gcm.Constants where

import Data.Text

cPOST_URL :: Text
cPOST_URL = "https://android.googleapis.com/gcm/send"

cPOST_NOTIFICATION_URL :: Text
cPOST_NOTIFICATION_URL = "https://android.googleapis.com/gcm/notification"

-- Fields for JSON object in requests to GCM servers.

cREGISTRATION_IDS :: Text
cREGISTRATION_IDS = "registration_ids"

cNOTIFICATION_KEY :: Text
cNOTIFICATION_KEY = "notification_key"

cNOTIFICATION_KEY_NAME :: Text
cNOTIFICATION_KEY_NAME = "notification_key_name"

cCOLLAPSE_KEY :: Text
cCOLLAPSE_KEY = "collapse_key"

cDATA :: Text
cDATA = "data"

cDELAY_WHILE_IDLE :: Text
cDELAY_WHILE_IDLE = "delay_while_idle"

cTIME_TO_LIVE :: Text
cTIME_TO_LIVE = "time_to_live"

cRESTRICTED_PACKAGE_NAME :: Text
cRESTRICTED_PACKAGE_NAME = "restricted_package_name"

cDRY_RUN :: Text
cDRY_RUN = "dry_run"


-- Fields for a JSON response to a sucessful request.

cMULTICAST_ID :: Text
cMULTICAST_ID = "multicast_id"

cSUCESS :: Text
cSUCESS = "success"

cFAILURE :: Text
cFAILURE = "failure"

cCANONICAL_IDS :: Text
cCANONICAL_IDS = "canonical_ids"

cRESULTS :: Text
cRESULTS = "results"

cMESSAGE_ID :: Text
cMESSAGE_ID = "message_id"

cREGISTRATION_ID :: Text
cREGISTRATION_ID = "registration_id"

cERROR :: Text
cERROR = "error"

cNOT_REGISTERED :: Text
cNOT_REGISTERED = "NotRegistered"

cUNAVAILABLE :: Text
cUNAVAILABLE = "Unavailable"

-- More constants

cRETRY_AFTER :: Text
cRETRY_AFTER = "Retry-After"

