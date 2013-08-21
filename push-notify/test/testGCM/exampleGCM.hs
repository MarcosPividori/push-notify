{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
               QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

import Network.PushNotify.Gcm.Send
import Network.PushNotify.Gcm.Types
import Network
import Network.HTTP.Conduit
import Data.Text
import Data.Aeson
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = send def

send :: GCMmessage -> IO ()
send msg = withSocketsDo $ do
           m    <- newManager def
           let gcmAppConfig  = GCMAppConfig "apikey" "senderId" 5 -- Here you must complete with the correct Api Key and SenderId.
           res  <- sendGCM m gcmAppConfig msg{
                                     registration_ids = ["registrationId"] -- Here you must complete with the correct regId of an Android app.
                                  ,  data_object      = Just (HM.fromList [(pack "Message" .= pack "Hello World!")]) }
           print res
           return ()
