-- GSoC 2013 - Communicating with mobile devices.

-- | This Module define the main contants for sending Push Notifications through Google Cloud Messaging.
module Constants where

cPOST_URL :: String
cPOST_URL = "https://android.googleapis.com/gcm/send"

cPOST_NOTIFICATION_URL :: String
cPOST_NOTIFICATION_URL = "https://android.googleapis.com/gcm/notification"

-- Fields for JSON object in requests to GCM servers.

cREGISTRATION_IDS :: String
cREGISTRATION_IDS = "registration_ids"

cNOTIFICATION_KEY :: String
cNOTIFICATION_KEY = "notification_key"

cNOTIFICATION_KEY_NAME :: String
cNOTIFICATION_KEY_NAME = "notification_key_name"

cCOLLAPSE_KEY :: String
cCOLLAPSE_KEY = "collapse_key"

cDATA :: String
cDATA = "data"

cDELAY_WHILE_IDLE :: String
cDELAY_WHILE_IDLE = "delay_while_idle"

cTIME_TO_LIVE :: String
cTIME_TO_LIVE = "time_to_live"

cRESTRICTED_PACKAGE_NAME :: String
cRESTRICTED_PACKAGE_NAME = "restricted_package_name"

cDRY_RUN :: String
cDRY_RUN = "dry_run"


-- Fields for a JSON response to a sucessful request.

cMULTICAST_ID :: String
cMULTICAST_ID = "multicast_id"

cSUCESS :: String
cSUCESS = "success"

cFAILURE :: String
cFAILURE = "failure"

cCANONICAL_IDS :: String
cCANONICAL_IDS = "canonical_ids"

cRESULTS :: String
cRESULTS = "results"

cMESSAGE_ID :: String
cMESSAGE_ID = "message_id"

cREGISTRATION_ID :: String
cREGISTRATION_ID = "registration_id"

cERROR :: String
cERROR = "error"

-- More constants

cRETRY_AFTER = "Retry-After"

