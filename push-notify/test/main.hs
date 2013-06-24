
import PushNotify.Gcm.Types
import PushNotify.Gcm.Send
import System.Environment
import Data.Aeson
import Data.Text    (Text, pack)
import Data.Default
import Data.HashMap.Strict

myGcmApp :: GCMAppConfig
myGcmApp = GCMAppConfig
    {   apiKey = "key=" -- API Key. (provided by Google)
    ,   projectId = "" -- Project ID. (provided by Google)
    }

-- Simple example, main regId message, sends message to the regId.
main :: IO ()
main = do
            regId : msg : _ <-  getArgs
            let
                message = def {
                                registration_ids = Just [regId]
                            ,   data_object = Just (fromList [(pack "Message" .= (msg :: String))])
                            ,   delay_while_idle = True
                            ,   time_to_live = Just 120
                            ,   dry_run = True
                            }
            result          <-  sendGCM myGcmApp message 0
            print $ show result
