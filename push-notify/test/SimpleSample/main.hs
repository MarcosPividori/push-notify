
import Network.PushNotify.Gcm.Types
import Network.PushNotify.Gcm.Send
import System.Environment
import Data.Aeson
import Data.Text    (Text, pack)
import Data.Default
import Data.HashMap.Strict

{-# LANGUAGE OverloadedStrings #-}

myGcmApp :: GCMAppConfig
myGcmApp = GCMAppConfig
    {   apiKey = pack "key=" -- API Key. (provided by Google)
    ,   projectId = pack "" -- Project ID. (provided by Google)
    }

-- Simple example, main regId message, sends message to the regId.
main :: IO ()
main = do
            regId : msg : _ <-  getArgs
            let
                message = def {
                                registration_ids = Just [pack regId]
                            ,   data_object = Just (fromList [(pack "Message" .= (msg :: String))])
                            --,   delay_while_idle = True
                            --,   time_to_live = Just 120
                            --,   dry_run = True
                            }
            result          <-  sendGCM myGcmApp message 0
            print $ show result
