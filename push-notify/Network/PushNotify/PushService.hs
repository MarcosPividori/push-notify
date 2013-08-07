    
module Network.PushNotify.PushService(
    PushServiceConfig(..)
  , PushManager(..)
  , startPushService
  , closePushService
  , sendPush
  , RegisterResult(..) -- from YesodPushApp
  , PushAppSub(..)     -- from YesodPushApp
  ) where

import Network.PushNotify.General
import Network.PushNotify.YesodPushApp

import Yesod
import Data.Aeson
import Data.Text                        (Text)
import Data.Monoid
import Control.Concurrent
import Control.Monad
import Network.HTTP.Conduit
import Network.PushNotify.Gcm.Types
import Network.PushNotify.Apns.Types
import Network.PushNotify.Mpns.Types
import Network.PushNotify.Gcm.Send
import Network.PushNotify.Apns.Send
import Network.PushNotify.Mpns.Send


data PushServiceConfig = PushServiceConfig {
        pushConfig           :: PushAppConfig                -- Main configuration.
    ,   newMessageCallback   :: Device -> Value -> IO ()     -- The callback function to be called when receiving messages from devices.
    ,   newDeviceCallback    :: Device -> Value -> IO RegisterResult -- The callback function to be called when a new device try to register on server.
    ,   unRegisteredCallback :: Device -> IO ()              -- The callback function to be called when a device unregisters.
    ,   newIdCallback        :: (Device,Device) -> IO ()     -- The callback function to be called when a device identifier changes.
    }

data PushManager = PushManager {
        httpManager    :: Maybe Manager       -- Conduit manager for sending push notifications though POST requests.
    ,   apnsManager    :: Maybe APNSManager   -- Apns manager for sending push notifications though APNS servers.
  --,   ccsManager     :: Maybe CCSManager    -- Ccs manager for communicating with GCM through Cloud Connection Server.
    ,   serviceConfig  :: PushServiceConfig   -- Main configuration.
    }

startPushService :: PushServiceConfig -> IO (PushManager,PushAppSub)
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
                       return ( PushManager httpMan apnsMan pConfig
                              , PushAppSub (newMessageCallback pConfig) (newDeviceCallback pConfig))

closePushService :: PushManager -> IO ()
closePushService man = do
                           case httpManager man of
                               Just m -> closeManager m
                               _      -> return ()
                           case apnsManager man of
                               Just m -> closeAPNS m
                               _      -> return ()
                     --    closeCCS


forgetConst :: Device -> Text
forgetConst (GCM  x) = x
forgetConst (APNS x) = x
forgetConst (MPNS x) = x

isGCM  (GCM  _) = True
isGCM  _        = False

isAPNS (APNS _) = True
isAPNS _        = False

isMPNS (MPNS _) = True
isMPNS _        = False

sendPush :: PushManager -> PushNotification -> [Device] -> IO PushResult
sendPush man notif devices = do
                let
                    gcmDevices  = map forgetConst $ filter isGCM  devices
                    apnsDevices = map forgetConst $ filter isAPNS devices
                    mpnsDevices = map forgetConst $ filter isMPNS devices
                    pConfig     = serviceConfig man
                    config      = pushConfig pConfig

                r1 <- case (gcmDevices , gcmAppConfig config , gcmNotif  notif , httpManager man) of
                          (_:_,Just cnf,Just msg,Just m) -> do
                                                               res <- sendGCM m cnf msg{registration_ids = gcmDevices}
                                                               return $ toPushResult res
                          _                              -> return def

                r2 <- case (apnsDevices , apnsNotif notif , apnsManager man) of
                          (_:_,Just msg,Just m) -> do
                                                       res <- sendAPNS m msg{deviceTokens = apnsDevices}
                                                       return $ toPushResult res
                          _                     -> return def

                r3 <- case (mpnsDevices , mpnsAppConfig config , mpnsNotif notif , httpManager man) of
                          (_:_,Just cnf,Just msg,Just m) -> do
                                                               res <- sendMPNS m cnf msg{deviceURIs = mpnsDevices}
                                                               return $ toPushResult res
                          _                              -> return def

                let res = r1 <> r2 <> r3

                when (unRegistered res /= []) $ mapM_ (unRegisteredCallback pConfig) (unRegistered res)

                when (newIds res /= [])       $ mapM_ (newIdCallback        pConfig) (newIds res)

                return res

