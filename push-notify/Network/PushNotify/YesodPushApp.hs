{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell, FlexibleInstances,
             QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

module YesodPushApp(
    RegisterResult(..)
  , PushAppSub(..)
  ) where

import Yesod
import YesodPushAppRoutes
import Network.PushNotify.General
import Control.Concurrent
import Data.Text

instance (RenderMessage master FormMessage, Yesod master) => YesodSubDispatch PushAppSub (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesPushAppSub)

-- 'postRegister' allows a mobile device to register. (POST messages to '/register')
postSubRegisterR :: (RenderMessage master FormMessage, Yesod master) => HandlerT PushAppSub (HandlerT master IO) ()
postSubRegisterR = do
    regId  <- lift $ runInputPost $ ireq textField "regId"
    msys   <- lift $ runInputPost $ iopt textField "system"
    iden   <- case msys of
                Nothing       -> return $ GCM  regId -- We take GCM as default.
                Just "WPhone" -> return $ MPNS regId -- A WPhone device
                Just "iOS"    -> return $ APNS regId -- A iOS device.
                Just _        -> invalidArgs []
    value  <- lift $ parseJsonBody_
    PushAppSub _ callback <- getYesod
    res <- liftIO $ callback iden value
    case res of
      SuccessfulReg -> sendResponse $ RepJson emptyContent -- successful registration.
      ErrorReg t    -> permissionDenied t                  -- error in registration.

-- 'postMessages' allows a mobile device to send a message. (POST messages to '/messages')
postSubMessagesR :: (RenderMessage master FormMessage, Yesod master) => HandlerT PushAppSub (HandlerT master IO) ()
postSubMessagesR = do
    regId  <- lift $ runInputPost $ ireq textField "regId"
    msys   <- lift $ runInputPost $ iopt textField "system"
    iden   <- case msys of
                Nothing       -> return $ GCM  regId -- We take GCM as default.
                Just "WPhone" -> return $ MPNS regId -- A WPhone device.
                Just "iOS"    -> return $ APNS regId -- A iOS device.
                Just _        -> invalidArgs []
    value  <- lift $ parseJsonBody_
    PushAppSub callback _ <- getYesod
    liftIO $ forkIO $ callback iden value
    sendResponse $ RepJson emptyContent   
