{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell, FlexibleInstances,
             QuasiQuotes, MultiParamTypeClasses, FlexibleContexts, GADTs #-}

module Network.PushNotify.YesodPushApp(
    RegisterResult(..)
  , PushAppSub(..)
  ) where

import Network.PushNotify.YesodPushAppRoutes
import Network.PushNotify.General

import Yesod
import Control.Concurrent
import Data.Text
import Data.Aeson
import qualified Data.HashMap.Strict as HM

instance (RenderMessage master FormMessage, Yesod master) => YesodSubDispatch PushAppSub (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesPushAppSub)

-- 'postRegister' allows a mobile device to register. (JSON POST messages to '/register')
postSubRegisterR :: (RenderMessage master FormMessage, Yesod master) => HandlerT PushAppSub (HandlerT master IO) ()
postSubRegisterR = do
    value  <- parseJsonBody_
    case value of
        Object v -> do
                        iden <- lookForIdentifier v
                        PushAppSub _ callback <- getYesod
                        res <- liftIO $ callback iden value
                        case res of
                            SuccessfulReg -> sendResponse $ RepJson emptyContent -- successful registration.
                            ErrorReg t    -> permissionDenied t                  -- error in registration.        
        _        -> invalidArgs []

lookForIdentifier :: Object -> HandlerT PushAppSub (HandlerT master IO) Device
lookForIdentifier v = do
                    regId <- case (HM.lookup "regId" v) of
                                 Just (String s) -> return s
                                 _               -> invalidArgs []
                    case (HM.lookup "system" v) of
                                 Nothing                -> return $ GCM  regId -- We take GCM as default.
                                 Just (String "WPhone") -> return $ MPNS regId -- A WPhone device.
                                 Just (String "iOS")    -> return $ APNS regId -- A iOS device.
                                 _                      -> invalidArgs []

-- 'postMessages' allows a mobile device to send a message. (JSON POST messages to '/messages')
postSubMessagesR :: (RenderMessage master FormMessage, Yesod master) => HandlerT PushAppSub (HandlerT master IO) ()
postSubMessagesR = do
    value  <- parseJsonBody_
    case value of
        Object v -> do
                        iden <- lookForIdentifier v
                        PushAppSub callback _ <- getYesod
                        liftIO $ forkIO $ callback iden value
                        sendResponse $ RepJson emptyContent
        _        -> invalidArgs []

