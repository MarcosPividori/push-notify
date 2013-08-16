
{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
             QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

import Send
import Network.PushNotify.Gcm.Types
import Data.Default

main :: IO ()
main = do
         man <- startCCS (GCMAppConfig "ApiKey" "senderId" 5)
                         (\_ _ -> return ())
         sendCCS man def
         closeCCS man
