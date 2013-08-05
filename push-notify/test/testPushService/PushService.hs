
module Network.PushNotify.PushService where

import Network.PushNotify.General
import YesodPushApp

import Yesod
import Network.PushNotify.Apns.Send
import Network.PushNotify.Apns.Types
import Network.HTTP.Conduit
import Data.Aeson
import Control.Concurrent


data PushServiceConfig = PushServiceConfig{
        pushConfig           :: PushAppConfig                -- Main configuration.
    ,   newMessageCallback   :: Device -> Value -> IO ()     -- The callback function to be called when receiving messages from devices.
    ,   newDeviceCallback    :: Device -> Value -> IO Bool   -- The callback function to be called when a new device try to register
                                                             -- on server, returns True in case of a successful registration.
    ,   unRegisteredCallback :: Device -> IO ()              -- The callback function to be called when a device unregisters.
    ,   newIdCallback        :: (Device,Device) -> IO ()     -- The callback function to be called when a device identifier changes.
  --,   yesodEntry           :: yesodRoute                   -- The yesod route to receive the messages from devices.
  --,   registerEntry        :: yesodRoute                   -- The yesod route to receive the registration from devices.
    }

data PushManager = PushManager {
        httpManager    :: Maybe Manager       -- Conduit manager for sending push notifications though POST requests.
    ,   apnsManager    :: Maybe APNSManager   -- Apns manager for sending push notifications though APNS servers.
  --,   ccsManager     :: Maybe CCSManager    -- Ccs manager for communicating with GCM through Cloud Connection Server.
    ,   serviceConfig  :: PushServiceConfig   -- Main configuration.
    ,   workerID       :: ThreadId            -- Id of the running yesod app.
    }

startPushService :: PushServiceConfig -> IO PushManager
startPushService pConfig = do
                       let cnfg = pushConfig pConfig
                       httpMan <- case (gcmAppConfig cnfg , mpnsAppConfig cnfg) of
                                      (Nothing,Nothing) -> return Nothing
                                      _                 -> do 
                                                               m <- newManager def
                                                               return (Just m)
                       apnsMan <- case apnsAppConfig cnfg of
                                      Just cnf -> do
                                                      m <- startAPNS cnf
                                                      return (Just m)
                                      Nothing  -> return Nothing
                    -- ccsMan  <- undefined
                       tID <- forkIO $ startYesodApp (newMessageCallback pConfig) (newDeviceCallback pConfig)
                       return $ PushManager httpMan apnsMan pConfig tID

closePushService :: PushManager -> IO ()
closePushService = undefined

sendPush :: [Device] -> PushNotification -> PushManager -> IO PushResult
sendPush = undefined
