{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell, QuasiQuotes #-}

module Network.PushNotify.General.YesodPushAppRoutes where

import Yesod
import Network.PushNotify.General.Types
import Control.Concurrent
import Data.Text

-- Yesod subsite to be used for the registration and reception of messages from devices.
mkYesodSubData "PushManager" [parseRoutes|
/register SubRegisterR POST
/messages SubMessagesR POST
|]
