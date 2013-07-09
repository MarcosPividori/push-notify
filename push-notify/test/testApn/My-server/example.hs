-- GSoC 2013 - Communicating with mobile devices.

import Network.PushNotify.Apns.Send
import Network.PushNotify.Apns.Types
import Data.Default
import Data.Text (pack)
import Data.Aeson.Types
import Data.Aeson

main :: IO ()
main = do
            let confg = APNSAppConfig{
                            certificate = "public-cert.pem"
                        ,   privateKey = "private-key.pem"
                        ,   environment = Development }
            putStrLn "A device token : "
            dtoken <- getLine
            putStrLn "An alert message : "
            alertMsg <- getLine
            let msg = def { deviceTokens = [pack dtoken], alert = Left $ pack alertMsg }
            print $ encode $ toJSON msg
            res <- sendAPNS confg msg
            print res
