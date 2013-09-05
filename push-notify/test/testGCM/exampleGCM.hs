{-# LANGUAGE OverloadedStrings #-}

import Network.PushNotify.Gcm
import Network.HTTP.Conduit
import Network
import Data.Text
import Data.Aeson
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = send def

send :: GCMmessage -> IO ()
send msg = withSocketsDo $ do
           m    <- newManager def
           let gcmAppConfig  = def{ apiKey = "apikey" } -- Here you must complete with the correct Api Key.
           res  <- sendGCM m gcmAppConfig msg{
                                registration_ids = ["registrationId"] -- Here you must complete with the correct regId of an Android app.
                              , data_object      = Just (HM.fromList [(pack "Message" .= pack "Hello World!")]) }
           print res
           return ()
