-- GSoC 2013 - Communicating with mobile devices.

{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
             QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

-- | This Module define the main function to send Push Notifications through Microsoft Push Notification Service.
module Network.PushNotify.Mpns.Send (sendMPNS) where

import Network.PushNotify.Mpns.Constants
import Network.PushNotify.Mpns.Types

import Data.Functor
import Data.String
import Data.Conduit                 (($$+-))
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
import Network.TLS.Extra            (fileReadCertificate,fileReadPrivateKey)
import Network.HTTP.Conduit         (http, parseUrl, withManager, RequestBody (RequestBodyLBS),
                                     requestBody, requestHeaders, Response(..), method, Manager,
                                     Request(..), HttpException(..))

retrySettingsMPNS = RetrySettings {
    backoff     = True
,   baseDelay   = 100
,   numRetries  = limitedRetries 1
}

-- | 'sendMPNS' sends the message through a MPNS Server.
sendMPNS :: Manager -> MPNSAppConfig -> MPNSmessage -> IO MPNSresult
sendMPNS manager cnfg msg = do
                        asyncs  <- mapM (async . send manager cnfg msg) $ deviceURIs msg
                        results <- mapM waitCatch asyncs
                        let l   = zip (deviceURIs msg) results
                            res = map (\(x,Right y) -> (x,y)) $ filter (isRight . snd) l
                        return $ MPNSresult{
                            sucessfullResults = res
                        ,   errorException    = map (\(x,Left e)  -> (x,e)) $ filter (not . isRight . snd) l
                        }
                    where
                        isRight (Right _) = True
                        isRight (Left  _) = False

send :: Manager -> MPNSAppConfig -> MPNSmessage -> DeviceURI -> IO MPNSinfo
send manager cnfg msg deviceUri = runResourceT $ do
    let valueBS   = renderLBS def $ restXML msg
    req' <- liftIO $ case useSecure cnfg of
                        False   -> parseUrl $ unpack deviceUri
                        True    -> do
                                     cert <- fileReadCertificate $ certificate cnfg
                                     key  <- fileReadPrivateKey  $ privateKey  cnfg
                                     r    <- (parseUrl $ unpack deviceUri)
                                     return r{ 
                                             clientCertificates = [(cert, Just key)]
                                         ,   secure             = True }
    let 
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
        notificationStatus = decodeUtf8 <$> lookup cNotificationStatus     headers
    ,   subscriptionStatus = decodeUtf8 <$> lookup cSubscriptionStatus     headers
    ,   connectionStatus   = decodeUtf8 <$> lookup cDeviceConnectionStatus headers
    }
