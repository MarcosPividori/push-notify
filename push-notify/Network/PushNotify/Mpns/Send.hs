-- GSoC 2013 - Communicating with mobile devices.

{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

-- | This Module define the main function to send Push Notifications through Microsoft Push Notification Service.
module Network.PushNotify.Mpns.Send (sendMPNS) where

import Network.PushNotify.Mpns.Constants
import Network.PushNotify.Mpns.Types

import Data.Functor
import Data.String
import Data.Conduit                 (($$+-))
import Data.List
import Data.Text                    (Text, pack, unpack, empty)
import Data.Text.Encoding           (decodeUtf8)
import Text.XML
import qualified Control.Exception  as CE
import Control.Concurrent.Async
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Control  (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadResource,runResourceT)
import Control.Retry
import Network.HTTP.Types
import Network.HTTP.Conduit
import Network.TLS.Extra            (fileReadCertificate,fileReadPrivateKey)

retrySettingsMPNS = RetrySettings {
    backoff     = True
,   baseDelay   = 100
,   numRetries  = limitedRetries 1
}

-- | 'sendMPNS' sends the message to a MPNS Server.
sendMPNS :: Manager -> MPNSAppConfig -> MPNSmessage -> IO MPNSresult
sendMPNS manager cnfg msg = do
                        asyncs  <- mapM (async . send manager cnfg msg) $ deviceURIs msg
                        results <- mapM waitCatch asyncs
                        let list  = zip (deviceURIs msg) results
                            (r,l) = partition (isRight . snd) list
                        return $ MPNSresult{
                            sucessfullResults = map (\(x,Right y) -> (x,y)) r
                        ,   errorException    = map (\(x,Left e)  -> (x,e)) l
                        }
                    where
                        isRight (Right _) = True
                        isRight (Left  _) = False

send :: Manager -> MPNSAppConfig -> MPNSmessage -> DeviceURI -> IO MPNSinfo
send manager cnfg msg deviceUri = runResourceT $ do
    req' <- liftIO $ case useSecure cnfg of
                        False   -> parseUrl $ unpack deviceUri
                        True    -> do
                                     cert <- fileReadCertificate $ certificate cnfg
                                     key  <- fileReadPrivateKey  $ privateKey  cnfg
                                     r    <- (parseUrl $ unpack deviceUri)
                                     return r{
                                             clientCertificates = [(cert, Just key)]
                                         ,   secure             = True }
    let valueBS  = renderLBS def $ restXML msg
        interval = case target msg of
                        Tile      -> 1
                        Toast     -> 2
                        Raw       -> 3
                 + case batching_interval msg of
                        Immediate -> 0
                        Sec450    -> 10
                        Sec900    -> 20
        req = req' {
                method = "POST"
              , requestBody = RequestBodyLBS valueBS
              , requestHeaders = [
                          ("Content-Type", "text/xml")
                        , (cNotificationClass, fromString $ show interval)
                        ] ++ case target msg of
                                Tile  -> [(cWindowsPhoneTarget, cToken)]
                                Toast -> [(cWindowsPhoneTarget, cToast)]
                                Raw   -> []
              }
    info <- liftIO $ CE.catch (runResourceT $ retry req manager (numRet cnfg))
                              (\(StatusCodeException rStatus rHeaders c) -> case statusCode rStatus of
                                                                       404 -> return $ handleSuccessfulResponse rHeaders
                                                                       412 -> return $ handleSuccessfulResponse rHeaders
                                                                       _   -> CE.throw $ StatusCodeException rStatus rHeaders c )
    return info

-- 'retry' try numRet attemps to send the messages.
retry :: (MonadBaseControl IO m,MonadResource m)
      => Request m -> Manager -> Int -> m MPNSinfo
retry req manager numret = do
        response <- retrying (retrySettingsMPNS{numRetries = limitedRetries numret}) ifRetry $ http req manager
        responseBody response $$+- return ()
        if ifRetry response
          then CE.throw $ StatusCodeException (responseStatus response) (responseHeaders response) (responseCookieJar response)
          else return $ handleSuccessfulResponse $ responseHeaders response
        where
            ifRetry x = (statusCode $ responseStatus x) >= 500

-- 'handleSuccessfulResponse' analyzes the server response.
handleSuccessfulResponse :: ResponseHeaders -> MPNSinfo
handleSuccessfulResponse headers = MPNSinfo {
        notificationStatus = (decodeUtf8 <$> lookup cNotificationStatus headers    ) >>= case1
    ,   subscriptionStatus = (decodeUtf8 <$> lookup cSubscriptionStatus headers    ) >>= case2
    ,   connectionStatus   = (decodeUtf8 <$> lookup cDeviceConnectionStatus headers) >>= case3
    }

case1 :: Text -> Maybe MPNSnotifStatus
case1 m | m== cNotifReceived  = Just Received
        | m== cNotifDropped   = Just Dropped
        | m== cNotifQueuefull = Just QueueFull
        | otherwise = Nothing

case2 :: Text -> Maybe MPNSsubStatus
case2 m | m== cSubActive  = Just Active
        | m== cSubExpired = Just Expired
        | otherwise = Nothing

case3 :: Text -> Maybe MPNSconStatus
case3 m | m== cConnConnected    = Just Connected
        | m== cConnInactive     = Just InActive
        | m== cConnDisconnected = Just Disconnected
        | m== cConnTempDisconn  = Just TempDisconnected
        | otherwise = Nothing

