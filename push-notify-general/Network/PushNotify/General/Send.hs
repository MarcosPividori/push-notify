-- | This Module define the main functions to send Push Notifications.
{-# LANGUAGE ScopedTypeVariables #-}
module Network.PushNotify.General.Send(
    startPushService
  , closePushService
  , sendPush
  , withPushManager
  ) where

import Network.PushNotify.General.Types
import Network.PushNotify.General.YesodPushApp

import Yesod
import Data.Text                        (Text)
import Data.Maybe
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

-- | 'startPushService' starts the PushService creating a PushManager.
startPushService :: PushServiceConfig -> IO PushManager
startPushService pConfig = do
                let cnfg    = pushConfig pConfig
                    gcmflag = case gcmConfig cnfg of
                                Just (Http _) -> True
                                _             -> False
                httpMan <- if gcmflag || isJust (mpnsConfig cnfg)
                             then do
                                    m <- newManager def
                                    return (Just m)
                             else return Nothing
                apnsMan <- case apnsConfig cnfg of
                             Just cnf -> do
                                    m <- startAPNS cnf
                                    return (Just m)
                             Nothing  -> return Nothing
                ccsMan  <- case gcmConfig cnfg of
                             Just (Ccs cnf) -> do
                                    m <- startCCS cnf (\d -> (newMessageCallback pConfig) (GCM d))
                                    return (Just m)
                             _                    -> return Nothing
                return $ PushManager httpMan apnsMan ccsMan pConfig

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

-- | 'withPushManager' creates a new manager, uses it in the provided function, and then releases it.
withPushManager :: PushServiceConfig -> (PushManager -> IO a) -> IO a
withPushManager confg fun = CE.bracket (startPushService confg) closePushService fun

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
    let gcmDevices  = map forgetConst $ filter isGCM  devices
        apnsDevices = map forgetConst $ filter isAPNS devices
        mpnsDevices = map forgetConst $ filter isMPNS devices
        pConfig     = serviceConfig man
        config      = pushConfig pConfig

    r1 <- case (gcmDevices , gcmConfig config , gcmNotif  notif , httpManager man , ccsManager man) of
              (_:_,Just (Ccs cnf),Just msg,_,Just m)   -> do
                            let msg' = msg{registration_ids = gcmDevices}
                            CE.catch (sendCCS' m msg')
                              (\e -> let _ = e :: CE.SomeException in
                                     case httpManager man of
                                       Just hman -> sendGCM' hman def{apiKey = aPiKey cnf} msg'
                                       _ -> return $ exceptionResult (map (\d -> (GCM d,Right e)) $ registration_ids msg') )

              (_:_,Just (Http cnf),Just msg,Just m,_ ) -> sendGCM' m cnf msg{registration_ids = gcmDevices}

              _                                        -> return def

    r2 <- case (apnsDevices , apnsNotif notif , apnsManager man) of
              (_:_,Just msg,Just m) -> sendAPNS' m msg{deviceTokens = apnsDevices}
              _                     -> return def

    r3 <- case (mpnsDevices , mpnsConfig config , mpnsNotif notif , httpManager man) of
              (_:_,Just cnf,Just msg,Just m) -> sendMPNS' m cnf msg{deviceURIs = mpnsDevices}
              _                              -> return def

    let res = r1 <> r2 <> r3

    when (unRegistered res /= []) $ mapM_ (unRegisteredCallback pConfig) (unRegistered res)

    when (newIds res /= [])       $ mapM_ (newIdCallback        pConfig) (newIds res)

    return res

    where
      sendCCS'  a b   = sendCCS a b >>= return . toPushResult
      sendMPNS' a b c = sendMPNS a b c >>= return . toPushResult
      sendGCM'  a b c = CE.catch (sendGCM a b c >>= return . toPushResult)
                        (\(e :: CE.SomeException) -> return $ exceptionResult (map (\d -> (GCM d,Right e)) $ registration_ids c))
      sendAPNS' a b   = CE.catch (sendAPNS a b >>= return . toPushResult)
                        (\(e :: CE.SomeException) -> return $ exceptionResult (map (\d -> (APNS d,Right e)) $ deviceTokens b))
      exceptionResult l = PushResult [] l [] [] []
