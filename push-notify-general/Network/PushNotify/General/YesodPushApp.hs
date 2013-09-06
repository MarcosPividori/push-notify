{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, QuasiQuotes #-}
-- | This Module defines the Yesod subsite to be used for the registration and reception of messages from devices.
module Network.PushNotify.General.YesodPushApp(
  PushAppSub(..)
  ) where

import Network.PushNotify.General.Types
import Yesod
import Control.Concurrent
import Data.Text
import Data.Aeson
import qualified Data.HashMap.Strict as HM

-- | Yesod subsite to be used for the registration and reception of messages from devices.
data PushAppSub = PushAppSub {
                            newmessageCallback :: Device -> Value -> IO ()
                         ,  newdeviceCallback  :: Device -> Value -> IO RegisterResult
                         }

mkYesodSubData "PushAppSub" [parseRoutes|
/register SubRegisterR POST
/messages SubMessagesR POST
|]

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
                      Just (String "ANDROID") -> return $ GCM  regId -- We take GCM as default.
                      Just (String "WPHONE")  -> return $ MPNS regId -- A WPhone device.
                      Just (String "IOS")     -> return $ APNS regId -- A iOS device.
                      _                       -> invalidArgs []

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
