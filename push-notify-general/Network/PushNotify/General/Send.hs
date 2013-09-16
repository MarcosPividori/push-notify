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
import Control.Exception                as CE
import qualified Data.HashMap.Strict    as HM
import qualified Data.HashSet           as HS
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
sendPush :: PushManager -> PushNotification -> HS.HashSet Device -> IO PushResult
sendPush man notif devices = do
    let gcmDevices  = HS.map forgetConst $ HS.filter isGCM  devices
        apnsDevices = HS.map forgetConst $ HS.filter isAPNS devices
        mpnsDevices = HS.map forgetConst $ HS.filter isMPNS devices
        pConfig     = serviceConfig man
        config      = pushConfig pConfig

    r1 <- case (HS.null gcmDevices , gcmConfig config , gcmNotif  notif , httpManager man , ccsManager man) of
              (False,Just (Ccs cnf),Just msg,_,Just m)   -> do
                            let msg' = msg{registration_ids = gcmDevices}
                            CE.catch (sendCCS' m msg')
                              (\e -> let _ = e :: CE.SomeException in
                                     case httpManager man of
                                       Just hman -> sendGCM' hman def{apiKey = aPiKey cnf} msg'
                                       _ -> return $ exceptionResult (HM.fromList $ map (\d -> (GCM d,Right e)) $
                                                                      HS.toList $ registration_ids msg') )

              (False,Just (Http cnf),Just msg,Just m,_ ) -> sendGCM' m cnf msg{registration_ids = gcmDevices}

              _                                          -> return def

    r2 <- case (HS.null apnsDevices , apnsNotif notif , apnsManager man) of
              (False,Just msg,Just m) -> sendAPNS' m msg{deviceTokens = apnsDevices}
              _                       -> return def

    r3 <- case (HS.null mpnsDevices , mpnsConfig config , mpnsNotif notif , httpManager man) of
              (False,Just cnf,Just msg,Just m) -> sendMPNS' m cnf msg{deviceURIs = mpnsDevices}
              _                                -> return def

    let res = r1 <> r2 <> r3

    when (not $ HS.null $ unRegistered res) $ mapM_ (unRegisteredCallback pConfig) (HS.toList $ unRegistered res)

    when (not $ HM.null $ newIds res)       $ mapM_ (newIdCallback        pConfig) (HM.toList $ newIds res)

    return res

    where
      sendCCS'  a b   = sendCCS a b >>= return . toPushResult
      sendMPNS' a b c = sendMPNS a b c >>= return . toPushResult
      sendGCM'  a b c = CE.catch (sendGCM a b c >>= return . toPushResult)
                        (\(e :: CE.SomeException) -> return $ exceptionResult ( HM.fromList $ map (\d -> (GCM d,Right e)) $ 
                                                                                HS.toList $ registration_ids c))
      sendAPNS' a b   = CE.catch (sendAPNS a b >>= return . toPushResult)
                        (\(e :: CE.SomeException) -> return $ exceptionResult ( HM.fromList $ map (\d -> (APNS d,Right e)) $
                                                                                HS.toList $ deviceTokens b))
      exceptionResult l = PushResult HS.empty l HS.empty HS.empty HM.empty
