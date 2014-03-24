-- GSoC 2013 - Communicating with mobile devices.

{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

-- | This Module define the main function to send Push Notifications through Google Cloud Messaging (HTTP connection).
module Network.PushNotify.Gcm.Send (sendGCM) where

import Network.PushNotify.Gcm.Constants
import Network.PushNotify.Gcm.Types

import Data.Aeson
import Data.Aeson.Parser                (json)
import Data.Aeson.Types
import Data.Conduit                     (($$+-))
import Data.Conduit.Attoparsec          (sinkParser)
import Data.Default
import Data.Map                         (Map,lookup)
import Data.String
import Data.Text                        (Text, pack, unpack, empty)
import qualified Data.ByteString.Char8  as B
import qualified Data.HashMap.Strict    as HM
import qualified Data.HashSet           as HS
import Control.Concurrent
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Trans.Control      (MonadBaseControl)
import Control.Monad.Trans.Resource     (MonadResource,runResourceT)
import Control.Retry
import Network.HTTP.Types
import Network.HTTP.Conduit

retrySettingsGCM = RetrySettings {
    backoff     = True
,   baseDelay   = 100
,   numRetries  = limitedRetries 1
}

-- | 'sendGCM' sends the message to a GCM Server.
sendGCM :: Manager -> GCMHttpConfig -> GCMmessage -> IO GCMresult
sendGCM manager cnfg msg = runResourceT $ do
    req' <- liftIO $ parseUrl $ unpack cPOST_URL
    let value   = toJSON msg
        valueBS = encode value
        req = req' {
                method = "POST"
              , requestBody = RequestBodyLBS valueBS
              , requestHeaders = [
                          ("Content-Type", "application/json")
                        , ("Authorization", fromString $ "key=" ++ (unpack $ apiKey cnfg) ) -- API Key. (provided by Google)
                        ]
              }
    retry req manager (numRet cnfg) msg

-- 'retry' try numRet attemps to send the messages.
retry :: (MonadBaseControl IO m,MonadResource m)
      => Request -> Manager -> Int -> GCMmessage -> m GCMresult
retry req manager numret msg = do
        response <- retrying (retrySettingsGCM{numRetries = limitedRetries numret})
                             ifRetry $ http req manager
        if (statusCode $ responseStatus response) >= 500
          then
            case Prelude.lookup (fromString $ unpack cRETRY_AFTER) (responseHeaders response) of
                Nothing -> return $ def { success = Just 0
                                        , failure = Just $ HS.size $ registration_ids msg
                                        , errorToReSend = registration_ids msg
                                        } -- Persistent server internal error after retrying
                Just t  -> do
                             let time = (read (B.unpack t)) :: Int
                             liftIO $ threadDelay (time*1000000) -- from seconds to microseconds
                             retry req manager (numret-1) msg
          else
            do
              resValue <- responseBody response $$+- sinkParser json
              liftIO $ handleSucessfulResponse resValue msg

        where ifRetry x = if (statusCode $ responseStatus x) >= 500
                            then case Prelude.lookup (fromString $ unpack cRETRY_AFTER) (responseHeaders x) of
                                    Nothing ->  True  -- Internal Server error, and don't specify time to wait
                                    Just t  ->  False -- Internal Server error, and specify time to wait
                            else False

getValue :: FromJSON b => Text -> Map Text Value -> Maybe b
getValue x xs = do
                  dat <-  Data.Map.lookup x xs
                  parseMaybe parseJSON dat

-- 'handleSucessfullResponse' analyzes the server response and generates useful information.
handleSucessfulResponse :: Value -> GCMmessage -> IO GCMresult
handleSucessfulResponse resValue msg =
       case (parseMaybe parseJSON resValue) :: Maybe (Map Text Value) of
          Nothing -> fail "Error parsing Response"
          Just a  -> let list  = case (getValue cRESULTS a) :: Maybe [(Map Text Value)] of
                                   Just xs ->  xs
                                   Nothing ->  []
                         mapMsg= HM.fromList $ zip (HS.toList $ registration_ids msg) list
                     in
                     return $ def {
                         multicast_id  = getValue cMULTICAST_ID a
                     ,   success       = getValue cSUCESS a
                     ,   failure       = getValue cFAILURE a
                     ,   canonical_ids = getValue cCANONICAL_IDS a

                     ,   newRegids     = let g list' = case (getValue cREGISTRATION_ID list') :: Maybe RegId of
                                                         Just xs ->  xs
                                                         Nothing ->  empty
                                         in  HM.filter ((/=) empty) $ HM.map g mapMsg

                     ,   messagesIds   = let g list' = case (getValue cMESSAGE_ID list') :: Maybe Text of
                                                         Just xs ->  xs
                                                         Nothing ->  empty
                                         in  HM.filter ((/=) empty) $ HM.map g mapMsg

                     ,   errorUnRegistered = let g list' = ((getValue cERROR list') :: Maybe Text) == Just cNOT_REGISTERED
                                             in  HS.fromList $ HM.keys $ HM.filter g mapMsg

                     ,   errorToReSend = let g list' = ((getValue cERROR list') :: Maybe Text) == Just cUNAVAILABLE
                                         in  HS.fromList $ HM.keys $ HM.filter g mapMsg

                     ,   errorRest     = let g list' = case (getValue cERROR list') :: Maybe Text of
                                                         Just xs -> if xs /= cUNAVAILABLE && xs /= cNOT_REGISTERED
                                                                    then xs
                                                                    else empty
                                                         Nothing -> empty
                                         in  HM.filter ((/=) empty) $ HM.map g mapMsg
                     }
