{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell, QuasiQuotes #-}

module Network.PushNotify.General.YesodPushAppRoutes where

import Yesod
import Network.PushNotify.General.Types
import Control.Concurrent
import Data.Text

-- | Yesod subsite to be used for the registration and reception of messages from devices.
data PushAppSub = PushAppSub {
                            newmessageCallback :: Device -> Value -> IO ()
                         ,  newdeviceCallback  :: Device -> Value -> IO RegisterResult
                         }

mkYesodSubData "PushAppSub" [parseRoutes|
/register SubRegisterR POST
/messages SubMessagesR POST
|]
