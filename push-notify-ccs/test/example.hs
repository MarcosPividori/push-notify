
{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
             QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

import Network.PushNotify.Ccs
import Data.Default
import qualified Data.HashMap.Strict  as HM
import Data.Aeson
import Data.Text

main :: IO ()
main = do
         man <- startCCS (GCMAppConfig "ApiKey" "senderId" 5)
                         (\r dat -> do
                                      putStrLn "A new Message from device!: " 
                                      putStrLn $ "\t From: " ++ show r
                                      putStrLn $ "\t Data: " ++ show dat
                                      return ())
         sendCCS man def{
                       registration_ids = ["Here the regId of a device."]
                     , data_object = Just (HM.fromList [(pack "Message" .= pack "hola")]) -- Example of JSON data.
                     }
         closeCCS man
