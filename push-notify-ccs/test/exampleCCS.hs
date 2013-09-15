
{-# LANGUAGE OverloadedStrings #-}

import Network.PushNotify.Ccs
import qualified Data.HashMap.Strict  as HM
import Data.Aeson
import Data.Default
import Data.Text
import Control.Concurrent

main :: IO ()
main = do
         man <- startCCS def{ aPiKey   = "apikey"
                            , senderID = "senderid"}
                         (\r dat -> do
                                      putStrLn "A new Message from device!: "
                                      putStrLn $ "\t From: " ++ show r
                                      putStrLn $ "\t Data: " ++ show dat
                                      return ())
         r <- sendCCS man def{
                       registration_ids = ["Here the regId of a device."]
                     , data_object = Just (HM.fromList [(pack "Message" .= pack "Hello world!")]) -- Example of JSON data.
                     }
         putStrLn $ "Result: " ++ show r
         threadDelay 20000000
         closeCCS man
