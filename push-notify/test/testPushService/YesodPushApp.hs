{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
             QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

module YesodPushApp where

import Yesod
import Network.PushNotify.General
import Control.Concurrent

data YesodPushApp = YesodPushApp {
                            newmessageCallback   :: Device -> Value -> IO ()
                         ,  newdeviceCallback    :: Device -> Value -> IO Bool
                         }

mkYesod "YesodPushApp" [parseRoutes|
/register RegisterR POST
/messages MessagesR POST
|]

-- Instances:

instance Yesod YesodPushApp

instance RenderMessage YesodPushApp FormMessage where
    renderMessage _ _ = defaultFormMessage

-- 'postRegister' allows a mobile device register. (POST messages to '/register')
postRegisterR :: Handler ()
postRegisterR = do
    regId  <- runInputPost $ ireq textField "regId"
    msys   <- runInputPost $ iopt textField "system"
    iden   <- case msys of
                Nothing       -> return $ GCM  regId -- We take GCM as default.
                Just "WPhone" -> return $ MPNS regId -- A WPhone device
                Just "iOS"    -> return $ APNS regId -- A iOS device.
                Just _        -> invalidArgs []
    value  <- parseJsonBody_
    YesodPushApp _ callback <- getYesod
    res <- liftIO $ callback iden value
    if res 
      then sendResponse $ RepJson emptyContent -- sucessful registration.
      else invalidArgs []                      -- error in registration.

-- 'postMessages' allows a mobile device to send a message. (POST messages to '/messages')
postMessagesR :: Handler ()
postMessagesR = do
    regId  <- runInputPost $ ireq textField "regId"
    msys   <- runInputPost $ iopt textField "system"
    iden   <- case msys of
                Nothing       -> return $ GCM  regId -- We take GCM as default.
                Just "WPhone" -> return $ MPNS regId -- A WPhone device.
                Just "iOS"    -> return $ APNS regId -- A iOS device.
                Just _        -> invalidArgs []
    value  <- parseJsonBody_
    YesodPushApp callback _ <- getYesod
    liftIO $ forkIO $ callback iden value
    sendResponse $ RepJson emptyContent

startYesodApp :: (Device -> Value -> IO ())
              -> (Device -> Value -> IO Bool)
              -> IO ()
startYesodApp cb1 cb2 = warp 3000 $ YesodPushApp cb1 cb2
