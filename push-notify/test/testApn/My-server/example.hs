-- GSoC 2013 - Communicating with mobile devices.

import Network.PushNotify.Apns.Send
import Network.PushNotify.Apns.Types
import Data.Default
import Data.Text (pack)
import Control.Concurrent

example1 :: IO ()
example1 = do
            let confg = def{
                            certificate = "public-cert.pem"
                        ,   privateKey = "private-key.pem"
                        ,   environment = Development }
            
            putStrLn "LET'S SEND A NOTIFICATION:"
            putStr "A device token (hexadecimal): "
            
            dtoken      <- getLine
            
            putStr "An alert message: "
            
            alertMsg    <- getLine
            
            let msg = def { deviceTokens = [pack dtoken], alert = Left $ pack alertMsg }
            manager     <- startAPNS confg
            res         <- sendAPNS manager msg
            putStrLn ("Result: " ++ show res)
            closeAPNS manager
            putStrLn "\nLET'S CONNECT WITH FEEDBACK SERVICE:"
            fres        <- feedBackAPNS confg
            putStrLn ("Result: " ++ show fres)

example2 :: IO ()
example2 = do
            let confg = def{
                            certificate = "public-cert.pem"
                        ,   privateKey  = "private-key.pem"
                        ,   environment = Development
                        }

            manager     <- startAPNS confg
            var         <- newEmptyMVar
            putMVar var ()
            loop 1000 manager var
            where
                loop 0 manager var = do
                                            threadDelay 50000000
                                            closeAPNS manager
                loop n manager var = do
                           forkIO $ do
                                        n <- myThreadId
                                        let msg = def{
                                                deviceTokens = map (pack . show) [11 .. 20]
                                            ,   alert        = Left $ pack ("Hello World! by: " ++ show n) }
                                        sending msg manager var
                           loop (n-1) manager var
                
                sending msg manager var = do
                                                res <- sendAPNS manager msg
                                                takeMVar var
                                                putStrLn ("Result: " ++ show res)
                                                putMVar var ()
                                                let rest = toReSendTokens res
                                                if rest /= []
                                                 then sending msg{deviceTokens = rest} manager var
                                                 else return ()
