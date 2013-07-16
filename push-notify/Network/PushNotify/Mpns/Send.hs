-- GSoC 2013 - Communicating with mobile devices.

{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
             QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

-- | This Module define the main function to send Push Notifications through Microsoft Push Notification Service.
module Send (sendMPNS) where

import Constants
import Types

import Data.Functor
import Data.String
import Data.Text                    (Text, pack, unpack, empty)
import Data.Text.Encoding           (decodeUtf8)
import Text.XML
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Control  (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadResource)
import Control.Retry
import Network.HTTP.Types
import Network.HTTP.Conduit         (http, parseUrl, withManager, RequestBody (RequestBodyLBS),
                                     requestBody, requestHeaders, method, Response (..), Manager,
                                     Request)

retrySettingsMPNS = RetrySettings {
    backoff = True
,   baseDelay = 100
,   numRetries = limitedRetries 1
}

-- | 'sendMPNS' sends the message through a MPNS Server.
sendMPNS :: MPNSAppConfig -> MPNSmessage -> IO MPNSresult
sendMPNS cnfg msg = do
                        r <- mapM (send cnfg msg) $ deviceURIs msg
                        return $ MPNSresult r

send :: MPNSAppConfig -> MPNSmessage -> DeviceURI -> IO (DeviceURI , MPNSinfo)
send cnfg msg deviceUri = withManager $ \manager -> do
    let valueBS   = renderLBS def $ rest msg
    req' <- liftIO $ parseUrl $ unpack deviceUri
    let 
        interval = case target msg of
                        Tile        -> 1
                        Toast       -> 2
                        Raw         -> 3
                 + case batching_interval msg of
                        Immediate   -> 0
                        Sec450      -> 10
                        Sec900      -> 20
        req = req' {
                method = "POST"
              , requestBody = RequestBodyLBS valueBS
              , requestHeaders = [
                            ("Content-Type", "text/xml")
                        ,   (cNotificationClass, fromString $ show interval)
                        ] ++ case target msg of
                                Tile        -> [(cWindowsPhoneTarget, cToken)]
                                Toast       -> [(cWindowsPhoneTarget, cToast)]
                                Raw         -> []
              }
    info <- retry req manager (numRet cnfg)
    return (deviceUri,info)

-- 'retry' try numRet attemps to send the messages.
retry :: (MonadBaseControl IO m,MonadResource m)
      => Request m -> Manager -> Int -> m MPNSinfo
retry req manager numRet = do
        Response _ _ headers _ <- retrying (retrySettingsMPNS{numRetries = limitedRetries numRet})
                                           ifRetry $ http req manager
        return $ handleSuccessfulResponse headers

        where ifRetry x = (statusCode $ responseStatus x) >= 500

-- 'handleSuccessfulResponse' analyzes the server response and generates useful information.
handleSuccessfulResponse :: ResponseHeaders -> MPNSinfo
handleSuccessfulResponse headers = MPNSinfo {
        notificationStatus = decodeUtf8 <$> lookup cNotificationStatus     headers
    ,   subscriptionStatus = decodeUtf8 <$> lookup cSubscriptionStatus     headers
    ,   connectionStatus   = decodeUtf8 <$> lookup cDeviceConnectionStatus headers
    }

