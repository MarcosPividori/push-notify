-- GSoC 2013 - Communicating with mobile devices.

import Network.PushNotify.Apns.Send
import Network.PushNotify.Apns.Types
import Data.Default
import Data.Text (pack)
import Control.Concurrent

{-
main :: IO ()
main = do
            let confg = def{
                            certificate = "public-cert.pem"
                        ,   privateKey = "private-key.pem"
                        ,   environment = Development }
            putStrLn "LET'S SEND A NOTIFICATION:"
            putStr "A device token (hexadecimal): "
            dtoken      <- getLine
            putStr "An alert message: "
            alertMsg    <- getLine
            let msg = def { deviceTokens = [pack dtoken,pack "3242"], alert = Left $ pack alertMsg }
            newConfg    <- startAPNS confg
            res         <- sendAPNS newConfg msg
            putStrLn ("Result: " ++ show res)
            putStrLn "\nLET'S CONNECT WITH FEEDBACK SERVICE:"
            fres        <- feedBackAPNS confg
            putStrLn ("Result: " ++ show fres)
-}

main :: IO ()
main = do
            let confg = def{
                            certificate = "public-cert.pem"
                        ,   privateKey  = "private-key.pem"
                        ,   environment = Development 
                        ,   timeoutTime = 100000 }

            let msg = def{
                            deviceTokens = map (pack . show) [1 .. 100]
                        ,   alert        = Left $ pack "Hello World!" }
            newConfg    <- startAPNS confg
            var         <- newEmptyMVar
            putMVar var ()
            loop 5 msg newConfg var
            where
                loop 0 msg newConfg var = do
                                            threadDelay 200000
                                            closeAPNS newConfg
                loop n msg newConfg var = do            
                           forkIO $ do
                                        res <- sendAPNS newConfg msg
                                        takeMVar var
                                        putStrLn ("Result: " ++ show res)
                                        putMVar var ()
                           loop (n-1) msg newConfg var
