{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell, FlexibleInstances, RankNTypes,
             QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

module Network.PushNotify.YesodPushAppRoutes where

import Yesod
import Network.PushNotify.General
import Control.Concurrent
import Data.Text

data RegisterResult = SuccessfulReg | ErrorReg Text

data PushAppSub = PushAppSub {
                            newmessageCallback   :: Device -> Value -> IO ()
                         ,  newdeviceCallback    :: Device -> Value -> IO RegisterResult
                         }

mkYesodSubData "PushAppSub" [parseRoutes|
/register SubRegisterR POST
/messages SubMessagesR POST
|]
