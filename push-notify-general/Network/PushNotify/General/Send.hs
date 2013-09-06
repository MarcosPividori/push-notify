-- | This Module define the main functions to send Push Notifications.
module Network.PushNotify.General.Send(
    startPushService
  , closePushService
  , sendPush
  ) where

import Network.PushNotify.General.Types
import Network.PushNotify.General.YesodPushApp

import Yesod
import Data.Text                        (Text)
import Data.Monoid
import Data.Default
import Control.Concurrent
import Control.Monad
import Control.Exception as CE
import Network.HTTP.Conduit
import Network.PushNotify.Gcm
import Network.PushNotify.Apns
import Network.PushNotify.Mpns
import Network.PushNotify.Ccs

-- | 'startPushService' starts the PushService creating a Manager and a Yesod subsite.
--
-- The Manager will be used to send notifications.
--
-- The Yesod subsite will be used to receive registrations and messages from devices as POST HTTP requests.
startPushService :: PushServiceConfig -> IO (PushManager,PushAppSub)
startPushService pConfig = do
                       let cnfg = pushConfig pConfig
                       httpMan <- case (gcmAppConfig cnfg , mpnsAppConfig cnfg) of
                                      (Nothing,Nothing) -> return Nothing
                                      _                 -> do
                                                             m <- newManager def
                                                             return (Just m)
                       apnsMan <- case apnsAppConfig cnfg of
                                      Just cnf          -> do
                                                             m <- startAPNS cnf
                                                             return (Just m)
                                      Nothing           -> return Nothing
                       ccsMan  <- case (gcmAppConfig cnfg,useCCS cnfg) of
                                      (Just cnf,True)   -> do
                                                             m <- startCCS cnf (\d -> (newMessageCallback pConfig) (GCM d))
                                                             return (Just m)
                                      _                 -> return Nothing
                       return ( PushManager httpMan apnsMan ccsMan pConfig
                              , PushAppSub (newMessageCallback pConfig) (newDeviceCallback pConfig))

-- | 'closePushService' stops the Push service.
closePushService :: PushManager -> IO ()
closePushService man = do
                         case httpManager man of
                             Just m -> closeManager m
                             _      -> return ()
                         case apnsManager man of
                             Just m -> closeAPNS m
                             _      -> return ()
                         case ccsManager man of
                             Just m -> closeCCS m
                             _      -> return ()

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

-- | 'sendPush' sends messages to the appropiate Push Servers.
sendPush :: PushManager -> PushNotification -> [Device] -> IO PushResult
sendPush man notif devices = do
                let
                    gcmDevices  = map forgetConst $ filter isGCM  devices
                    apnsDevices = map forgetConst $ filter isAPNS devices
                    mpnsDevices = map forgetConst $ filter isMPNS devices
                    pConfig     = serviceConfig man
                    config      = pushConfig pConfig

                r1 <- case (gcmDevices , gcmAppConfig config , gcmNotif  notif , httpManager man , ccsManager man) of
                          (_:_,Just cnf,Just msg,_,Just m)  -> do
                                                                 let msg' = msg{registration_ids = gcmDevices}
                                                                 res <- CE.catch (sendCCS m msg')
                                                                          (\e -> let _ = e :: CE.SomeException in
                                                                             case httpManager man of
                                                                               Just hman -> sendGCM hman cnf msg'
                                                                               _ -> throw e)
                                                                 return $ toPushResult res
                          (_:_,Just cnf,Just msg,Just m,_ ) -> do
                                                                 res <- sendGCM m cnf msg{registration_ids = gcmDevices}
                                                                 return $ toPushResult res
                          _                                 -> return def

                r2 <- case (apnsDevices , apnsNotif notif , apnsManager man) of
                          (_:_,Just msg,Just m)             -> do
                                                                 res <- sendAPNS m msg{deviceTokens = apnsDevices}
                                                                 return $ toPushResult res
                          _                                 -> return def

                r3 <- case (mpnsDevices , mpnsAppConfig config , mpnsNotif notif , httpManager man) of
                          (_:_,Just cnf,Just msg,Just m)    -> do
                                                                 res <- sendMPNS m cnf msg{deviceURIs = mpnsDevices}
                                                                 return $ toPushResult res
                          _                                 -> return def

                let res = r1 <> r2 <> r3

                when (unRegistered res /= []) $ mapM_ (unRegisteredCallback pConfig) (unRegistered res)

                when (newIds res /= [])       $ mapM_ (newIdCallback        pConfig) (newIds res)

                return res

