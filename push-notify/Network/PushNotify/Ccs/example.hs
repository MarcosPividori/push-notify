
{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
             QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

import Send
import Network.PushNotify.Gcm.Types
import Data.Default

main :: IO ()
main = do
         man <- startCCS (GCMAppConfig "key=" "21111121212" 5)
                         (\_ _ -> return ())
         sendCCS man def
         closeCCS man
