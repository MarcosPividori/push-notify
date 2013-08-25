{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell, QuasiQuotes #-}

module Network.PushNotify.General.YesodPushAppRoutes where

import Yesod
import Network.PushNotify.General.Types
import Control.Concurrent
import Data.Text

data PushAppSub = PushAppSub {
                            newmessageCallback   :: Device -> Value -> IO ()
                         ,  newdeviceCallback    :: Device -> Value -> IO RegisterResult
                         }

mkYesodSubData "PushAppSub" [parseRoutes|
/register SubRegisterR POST
/messages SubMessagesR POST
|]
