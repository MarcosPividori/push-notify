-- GSoC 2013 - Communicating with mobile devices.

import Network.PushNotify.Apns.Send
import Network.PushNotify.Apns.Types
import Data.Default
import Data.Text (pack)
import Control.Concurrent

main :: IO ()
main = do
            let confg = def{
                            certificate = "public-cert.pem"
                        ,   privateKey  = "private-key.pem"
                        ,   environment = Development 
                        ,   timeoutTime = 1000000 
                        }

            newConfg    <- startAPNS confg
            var         <- newEmptyMVar
            putMVar var ()
            loop 10000 newConfg var
            where
                loop 0 newConfg var = do
                                            threadDelay 50000000
                                            closeAPNS newConfg
                loop n newConfg var = do
                           forkIO $ do
                                        n <- myThreadId
                                        let msg = def{
                                                deviceTokens = map (pack . show) [11 .. 20]
                                            ,   alert        = Left $ pack ("Hello World! by: " ++ show n) }
                                        sending msg newConfg var
                           loop (n-1) newConfg var
                
                sending msg newConfg var = do
                                                res <- sendAPNS newConfg msg
                                                takeMVar var
                                                putStrLn ("Result: " ++ show res)
                                                putMVar var ()
                                                let rest = toReSendTokens res
                                                if rest /= []
                                                then sending msg{deviceTokens = rest} newConfg var
                                                else return ()
