-- GSoC 2013 - Communicating with mobile devices.

import Network.PushNotify.Apns.Send
import Network.PushNotify.Apns.Types
import Data.Default
import Data.Text (pack)
import Control.Concurrent

main :: IO ()
main = example1

example1 :: IO ()
example1 = do
             let confg = def{
                             certificate = "public-cert.pem"
                         ,   privateKey  = "private-key.pem"
                         ,   environment = Local }

             putStrLn "Let's send a notification:"

             putStrLn "A device token (hexadecimal): "
             dtoken  <- getLine

             putStrLn "An alert message: "            
             alertMsg <- getLine

             let msg = def { deviceTokens = [pack dtoken], alert = Left $ pack alertMsg }
             manager <- startAPNS confg
             res     <- sendAPNS manager msg
             putStrLn ("Result: " ++ show res)
             closeAPNS manager
             
             putStrLn "\nLet's connect to the Feedback Service:"
             fres    <- feedBackAPNS confg
             putStrLn ("Result: " ++ show fres)


example2 :: IO ()
example2 = do
             let confg = def{
                             certificate = "public-cert.pem"
                         ,   privateKey  = "private-key.pem"
                         ,   environment = Local
                         }

             manager     <- startAPNS confg
             var         <- newEmptyMVar
             var2        <- newEmptyMVar
             putMVar var ()
             forkIO $ loop 500 manager var var2
             mapM (\_-> takeMVar var2) [1..500]
             closeAPNS manager
             where
                loop 0 manager var var2 = return ()
                loop n manager var var2 = do
                            forkIO $ do
                                       id <- myThreadId
                                       let msg = def{
                                               deviceTokens = map (pack . show) [11 .. 20]
                                           ,   alert        = Left $ pack ("Hello World! by: " ++ show id) }
                                       sending msg manager var
                                       putMVar var2 ()
                            loop (n-1) manager var var2

                sending msg manager var = do
                                            res <- sendAPNS manager msg
                                            takeMVar var
                                            putStrLn ("Result: " ++ show res)
                                            putMVar var ()
                                            let rest = toReSendTokens res
                                            if rest /= []
                                            then sending msg{deviceTokens = rest} manager var
                                            else return ()
