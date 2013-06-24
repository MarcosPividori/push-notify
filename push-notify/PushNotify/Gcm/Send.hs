-- GSoC 2013 - Communicating with mobile devices.

{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
             QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

-- | This Module define the main function to send Push Notifications through Google Cloud Messaging.
module PushNotify.Gcm.Send (sendGCM) where

import PushNotify.Gcm.Types
import PushNotify.Gcm.Constants
import Control.Monad.Trans.Control  (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadResource)
import Network.HTTP.Conduit         (http, parseUrl, withManager, RequestBody (RequestBodyLBS),
                                     requestBody, requestHeaders, method, Response (..), Manager,
                                     newManager, def, Request)
import Network.HTTP.Types
import Data.Aeson.Parser            (json)
import Data.Conduit                 (($$+-))
import Data.Conduit.Attoparsec      (sinkParser)
import Control.Monad.IO.Class       (liftIO)
import Data.Aeson
import Data.Aeson.Types
import Data.Map                     (Map,lookup)
import Data.Text                    (Text, pack, unpack)
import Data.String
import Control.Retry
import Control.Concurrent
import qualified Data.ByteString.Char8 as B

retrySettingsGCM = RetrySettings {
    backoff = True
,   baseDelay = 100
}

-- | 'sendGCM' sends the message through a GCM Server.
sendGCM :: GCMAppConfig -> GCMmessage -> Int -> IO GCMresult
sendGCM cnfg msg numRet = withManager $ \manager -> do
    let
        value = toJSON msg
        valueBS = encode value
    req' <- liftIO $ parseUrl $ unpack cPOST_URL
    let req = req' {
                   method = "POST",
                   requestBody = RequestBodyLBS valueBS,
                   requestHeaders =
                        [ ("Content-Type", "application/json"),
                          ("Authorization", fromString $ apiKey cnfg) -- API Key. (provided by Google)
                        ]}
    retry req manager numRet msg

-- 'retry' try numRet attemps to send the messages.
retry :: (MonadBaseControl IO m,MonadResource m)
        => Request m -> Manager -> Int -> GCMmessage -> m GCMresult
retry req manager numRet msg = do
        Response status version headers body <- retrying (
                                retrySettingsGCM{numRetries = limitedRetries numRet}) ifRetry $ http req manager
        if (statusCode $ status) >= 500
          then
            case Prelude.lookup (fromString $ unpack cRETRY_AFTER) headers of
                Nothing ->  fail "Persistent server internal error after retrying"
                Just t  ->  do
                                let time = (read (B.unpack t)) :: Int -- I need to check this line
                                liftIO $ threadDelay (time*1000000) -- from seconds to microseconds
                                retry req manager (numRet-1) msg 
          else
            do
                resValue <- body $$+- sinkParser json
                liftIO $ handleSucessfulResponse resValue msg
        
        where ifRetry x = if (statusCode $ responseStatus x) >= 500
                            then case Prelude.lookup (fromString $ unpack cRETRY_AFTER) (responseHeaders x) of
                                    Nothing ->  True  -- Internal Server error, and don't specify a time to wait
                                    Just t  ->  False -- Internal Server error, and specify a time to wait
                            else False

getValue :: FromJSON b => Text -> Map Text Value -> Maybe b
getValue x xs = do
                    dat <-  Data.Map.lookup x xs
                    parseMaybe parseJSON dat

-- 'handleSucessfullResponse' analyzes the server response and generates useful information.
handleSucessfulResponse :: Value -> GCMmessage -> IO GCMresult
handleSucessfulResponse resValue msg =
       case (parseMaybe parseJSON resValue) :: Maybe (Map Text Value) of
           Just a -> let list  = case (getValue cRESULTS a) :: Maybe [(Map Text Value)] of
                                   Just xs ->  xs
                                   Nothing ->  []
                         mapMsg= case registration_ids msg of
                                           Just xs -> zip xs list
                                           Nothing -> []
                     in
                     return $ def {
                         multicast_id  = getValue cMULTICAST_ID a
                     ,   success       = getValue cSUCESS a
                     ,   failure       = getValue cFAILURE a
                     ,   canonical_ids = getValue cCANONICAL_IDS a
                     ,   results       = let
                                            f x = case (getValue cMESSAGE_ID x) :: Maybe String of
                                                     Just xs ->  GCMOk xs
                                                     Nothing ->  case (getValue cERROR x) :: Maybe String of
                                                                    Just y  -> GCMError y
                                                                    Nothing -> GCMError ""
                                         in map f list
                     ,   newRegids     = let
                                            g (x,list') = case (getValue cREGISTRATION_ID list') :: Maybe RegId of
                                                             Just xs ->  (x,xs)
                                                             Nothing ->  (x,[])
                                         in filter (\(x,y) -> y /= []) $ map g mapMsg
                     ,   unRegistered  = let
                                            g (x,list') = ((getValue cERROR list') :: Maybe Text) == Just cNOT_REGISTERED
                                         in map fst $ filter g mapMsg
                     ,   toReSend      = let
                                            g (x,list') = ((getValue cERROR list') :: Maybe Text) == Just cUNAVAILABLE
                                         in map fst $ filter g mapMsg
                     }
           _      -> fail "Error parsing Response"
