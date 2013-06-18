-- GSoC 2013 - Communicating with mobile devices.

{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
             QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

-- | This Module define the main function to send Push Notifications through Google Cloud Messaging.
module Send (sendGCM) where

import Types
import ApiKey
             
import Network.HTTP.Conduit
    (http, parseUrl, withManager, RequestBody (RequestBodyLBS), requestBody, 
    requestHeaders,	 method, Response (..), Manager, newManager, def
    )
import Network.HTTP.Types
import Data.Aeson.Parser        (json)
import Data.Conduit             (($$+-))
import Data.Conduit.Attoparsec  (sinkParser)
import Control.Monad.IO.Class   (liftIO)
import Data.Aeson
import Data.Aeson.Types
import Data.Map                 (Map,lookup)
import Data.Text                (Text, pack)
import Data.String
import Control.Retry

retrySettingsGCM = RetrySettings {	 
    backoff = True
,   baseDelay = 100
}

gcmToJson :: GCMmessage -> IO Value
gcmToJson msg = return $ object $ gcmToObject msg

gcmToObject :: GCMmessage -> [Pair]
gcmToObject msg = let
                        el1 = case registration_ids msg of
                                Just regIds ->  [("registration_ids" .= (regIds :: [String] ))]
                                Nothing ->  [] -- to be continued ...
                        el2 = case collapse_key msg of
                                []  -> []
                                xs  -> [("collapse_key" .= xs)]
                        el3 = case data_object msg of 
                                Nothing  -> []
                                Just dat -> [("data" .= dat)]
                        el4 = case delay_while_idle msg of
                                True    -> [("delay_while_idle" .= True)]
                                False   -> []
                        el5 = case time_to_live msg of 
                                Nothing -> []
                                Just t  -> [("time_to_live" .= t)]
                        el6 = case restricted_package_name msg of
                                []  ->  []
                                xs  ->  [("restricted_package_name" .= xs)]
                        el7 = case dry_run msg of
                                True    -> [("dry_run" .= True)]
                                False   -> []
                  in 
                  el1 ++ el2 ++ el3 ++ el4 ++ el5 ++ el6 ++ el7

-- |'sendGCM' sends the message through a GCM Server.
sendGCM :: GCMmessage -> Int -> IO GCMresult
sendGCM msg numRet = withManager $ \manager -> do
    value <- liftIO $ makeValue msg (registration_ids msg)
    let valueBS = encode value
    req' <- liftIO $ parseUrl "https://android.googleapis.com/gcm/send"
    let req = req' {
                   method = "POST",
                   requestBody = RequestBodyLBS valueBS,
                   requestHeaders =
                        [ ("Content-Type", "application/json"),
                          ("Authorization", fromString apiKey) -- API Key. (provided by Google)
                        ]}
    Response status version headers body <- retrying (retrySettingsGCM{numRetries = limitedRetries numRet})
                                                     (\x -> (statusCode $ responseStatus x) >= 500) $ http req manager
                                                     -- If internal error in the GCM server I will retry.
    resValue <- body $$+- sinkParser json
    liftIO $ handleSucessfulResponse resValue msg

-- 'makeValue' creates the block to be sent.
makeValue :: GCMmessage -> Maybe [RegId] -> IO Value
makeValue msg (Just regIds) = do
                        dat <-  (gcmToJson msg)
                        return $ object [
                              ("registration_ids" .= (regIds :: [String] ))
                            , ("data" .= dat )]

getValue :: FromJSON b => String -> Map Text Value -> Maybe b
getValue xs x = do
                    dat <-  Data.Map.lookup (pack xs) x
                    parseMaybe parseJSON dat

-- | 'handleSucessfullResponse' analyzes the server response and generates useful information.
handleSucessfulResponse :: Value -> GCMmessage -> IO GCMresult
handleSucessfulResponse resValue msg =
                    case (parseMaybe parseJSON resValue) :: Maybe (Map Text Value) of
                        Just a -> let list  = case (getValue "results" a) :: Maybe [(Map Text Value)] of
                                                Just xs ->  xs
                                                Nothing ->  []
                                      mapMsg= case registration_ids msg of
                                                Just xs -> zip xs list
                                                Nothing -> []
                                  in
                                  return $ def {
                                      multicast_id  = (getValue "multicast_id" a) :: Maybe Integer
                                  ,   success       = (getValue "success" a) :: Maybe Int
                                  ,   failure       = (getValue "failure" a) :: Maybe Int
                                  ,   canonical_ids = (getValue "canonical_ids" a) :: Maybe Int
                                  ,   results       = let
                                                        f x = case (getValue "message_id" x) :: Maybe String of
                                                                Just xs ->  GCMOk xs
                                                                Nothing ->  case (getValue "error" x) :: Maybe String of
                                                                                Just y  -> GCMError y
                                                                                Nothing -> GCMError ""
                                                      in map f list
                                  ,   newRegids     = let
                                                        g (x,list') = case (getValue "registration_id" list') :: Maybe String of
                                                                            Just xs ->  (x,xs)
                                                                            Nothing ->  (x,[])
                                                      in filter (\(x,y) -> y /= []) $ map g mapMsg
                                  ,   unRegistered  = let
                                                        g (x,list') = case (getValue "error" list') :: Maybe String of
                                                                            Just "NotRegistered" ->  True
                                                                            _                    ->  False
                                                      in map fst $ filter g mapMsg
                                  ,   toReSend      = let
                                                        g (x,list') = case (getValue "error" list') :: Maybe String of
                                                                            Just "Unavailable"  ->  True
                                                                            _                   ->  False
                                                      in map fst $ filter g mapMsg
                                  }
                        _      -> fail "Error parsing Response"
