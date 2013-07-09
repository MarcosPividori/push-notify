-- GSoC 2013 - Communicating with mobile devices.

import Send
import Types
import Data.Default
import Data.Text (pack)

main :: IO ()
main = do
            let confg = APNSAppConfig{
                            certificate = "public-cert.pem"
                        ,   privateKey = "private-key.pem"
                        ,   environment = Development }
            putStrLn "LET'S SEND A NOTIFICATION:"
            putStr "A device token (hexadecimal): "
            dtoken <- getLine
            putStr "An alert message: "
            alertMsg <- getLine
            let msg = def { deviceTokens = [pack dtoken], alert = Left $ pack alertMsg }
            res     <- sendAPNS confg msg
            putStrLn ("Result: " ++ show res)
            putStrLn "\nLET'S CONNECT WITH FEEDBACK SERVICE:"
            fres    <- feedBackAPNS confg
            putStrLn ("Result: " ++ show fres)
