{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
               QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}
module General where

import Network.PushNotify.Gcm.Types
import Network.PushNotify.Gcm.Send
import Network.PushNotify.Mpns.Types
import Network.PushNotify.Mpns.Send
import Network.HTTP.Conduit
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Text

data Ident   = GCM Text | MPNS Text deriving (Show, Read, Eq)
derivePersistField "Ident"

data AppConfig = AppConfig{
            gcmAppConfig  :: GCMAppConfig
        ,   mpnsAppConfig :: MPNSAppConfig
        } deriving Show

data Message = Message {
            gcmMessage  :: GCMmessage
        ,   mpnsMessage :: MPNSmessage
        } deriving Show

data Result  = Result MPNSresult GCMresult deriving Show

send :: Manager -> AppConfig -> Message -> IO ()
send manager cnfg msg = do
                --sendGCM  manager (gcmAppConfig cnfg)  (gcmMessage msg)
                sendMPNS manager (mpnsAppConfig cnfg) (mpnsMessage msg)
                return ()
