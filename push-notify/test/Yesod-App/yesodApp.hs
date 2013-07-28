-- Test Example for Push Notifications.
-- This is a simple example of a Yesod server, where devices can register to receive
-- messages and users can send messages through the web service.
-- Before running this app, its necessary to complete the "approot" and the "apiKey" with the proper values.

{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
             QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

import Yesod
import Yesod.Static
import Database.Persist.Sqlite

import Database.Persist
import Data.Text.Internal           (empty)
import Data.Text                    (Text,pack,unpack,append)
import qualified Data.Text          as T
import Data.Default
import Data.HashMap.Strict          (fromList)
import Text.XML
import Text.Hamlet.XML
import qualified Data.Map           as M
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Logger
import Network.PushNotify.Gcm.Types
import Network.PushNotify.Gcm.Send
import Network.PushNotify.Mpns.Types
import Network.PushNotify.Mpns.Send
import Network.HTTP.Conduit
import qualified Control.Exception  as CE
import Data.Aeson.Types
import Data.Monoid                  ((<>))
import PushNotify

-- Data Base:

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Devices
    user Text
    password Text
    identifier Device
    UniqueUser user
    UniqueDevice identifier
    deriving Show
|]


-- Yesod App:

staticFiles "static"

data Messages = Messages {
                            connectionPool :: ConnectionPool -- Connection to the Database.
                         ,  getStatic :: Static -- Reference point of the static data.
                         ,  appConfig :: PushAppConfig -- configuration.
                         ,  manager   :: Manager
                         }

mkYesod "Messages" [parseRoutes|
/ RootR GET
/register RegisterR POST
/fromweb FromWebR POST
/static StaticR Static getStatic
|]

-- Instances:

instance Yesod Messages where
    approot = ApprootStatic "" -- Here you must complete with the correct route.

instance YesodPersist Messages where
    type YesodPersistBackend Messages = SqlPersistT
    runDB action = do
        Messages pool _ _ _ <- getYesod
        runSqlPool action pool

instance RenderMessage Messages FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Handlers:

-- 'getRootR' provides the main page.
getRootR :: Handler Html
getRootR = do
                his   <- lookupSession "history"
                list  <- runDB $ selectList [] [Desc DevicesUser]
                users <- return $ map (\a -> devicesUser(entityVal a)) list
                defaultLayout $ do
                                setTitle "Just a test example."
                                toWidget [hamlet|
                        $maybe msg <- his
                            <br><b>History:</b><br>
                            $forall message <- T.lines msg
                                <li>#{message}
                            <form method=post action=@{FromWebR}>
                                <input type="hidden" name="clear" value="clear" /> 
                                <input type=submit style="font-weight:bold;" value="Clear">
                        <br><b>Select a user:</b><br>
                        <form method=post action=@{FromWebR}>
                            $forall user <- users
                                <input type="radio" name="destination" value=#{user}> #{user} <br>
                            <input type="radio" name="destination" value="EveryOne" checked="yes" > Just to every one! <br>
                            <br><b>Write a message:</b><br>
                            <input type="text" name="message" />
                            <input type=submit style="font-weight:bold;" value="Send">
|]


-- 'postRegister' allows a mobile device register. (POST messages to '/register')
-- Receives a user name, password and a regId , if there isn't a "system" label, then it is an Android devices, else we should consider the diff options.
postRegisterR :: Handler ()
postRegisterR = do
    regId  <- runInputPost $ ireq textField "regId"
    usr    <- runInputPost $ ireq textField "user"
    pass   <- runInputPost $ ireq textField "password"
    msys   <- runInputPost $ iopt textField "system"
    iden   <- case msys of
                Nothing       -> return $ GCM  regId
                Just "WPhone" -> return $ MPNS regId
                Just _        -> invalidArgs []
    device <- runDB $ getBy $ UniqueUser usr
    $(logInfo) ("\nIntent for a new user!:\n-User: "<> usr <>"\n-Password: "<> pass <>"\n-Identifier: "<> (pack $ show iden) <>"\n")
    case device of
        Nothing -> do
                       dev  <-  runDB $ getBy $ UniqueDevice iden
                       case dev of
                           Just x  ->  do
                                           runDB $ update (entityKey (x)) [DevicesUser =. usr , DevicesPassword =. pass ]
                                           sendResponse $ RepJson emptyContent
                           Nothing ->  do
                                           runDB $ insert $ Devices usr pass iden
                                           sendResponse $ RepJson emptyContent
        Just a	-> case devicesPassword (entityVal (a)) == pass of
                       True  -> do -- regId has changed.
                                    runDB $ update (entityKey (a)) [DevicesIdentifier =. iden ]
                                    sendResponse $ RepJson emptyContent
                       False -> invalidArgs []

-- 'postFromWebR' receives messages from the website user. (POST messages to '/fromweb')
postFromWebR :: Handler ()
postFromWebR = do
  cl <- runInputPost $ iopt textField "clear"
  case cl of
   Just _ -> do
                 deleteSession "history"
                 redirect RootR
   _      -> do
    msg  <- runInputPost $ ireq textField "message" 
    dest <- runInputPost $ ireq textField "destination"
    list <- case dest of
                "EveryOne"  -> runDB $ selectList [] [Desc DevicesIdentifier]
                usr         -> do
                                   res <- runDB $ getBy $ UniqueUser usr
                                   case res of
                                       Just a  -> return [a]
                                       Nothing -> return []
    Messages _ _ appConfig man <- getYesod
    $(logInfo) ("\tA new message: \""<>msg<>"\"\t")
    let regIdsList = map (\a -> devicesIdentifier(entityVal a)) list
    if regIdsList /= []
    then do 
            let message = def {
                         gcmNotif  = Just $ def {data_object = Just (fromList [(pack "Message" .= msg)]) }
                     ,   mpnsNotif = Just $ def {target = Toast , rest = Document (Prologue [] Nothing []) (xmlMessage msg) []}
                     }
            result <- liftIO $ CE.catch -- I catch IO exceptions to avoid showing unsecure information.
                        (send man appConfig message regIdsList)
                        (\e -> do
                                   let _ = (e :: CE.SomeException)
                                   fail "Problem communicating with Push Servers")
            return ()
            handleResult message result
    else do
            return ()
    his <- lookupSession "history"
    case his of
        Just m  -> setSession "history" (m <> "\n" <> msg)
        Nothing -> setSession "history" msg
    redirect RootR
    where
        xmlMessage msg = Element (Name "Notification" (Just "WPNotification") (Just "wp")) (M.singleton "xmlns:wp" "WPNotification") [xml|
<wp:Toast>
    <wp:Text1>New message: 
    <wp:Text2>#{msg}
    <wp:Param>?msg=#{msg}
|]

-- Handle the result of the communication with the Push Servers.
handleResult :: PushNotification -> PushResult -> Handler ()
handleResult msg res = do
                        handleNewRegIDs    $ newIds       res
                        handleUnRegistered $ unRegistered res
                        handleToResend msg $ toResend     res

-- Handle the regIds that have changed, I need to actualize the DB.
handleNewRegIDs :: [(Device,Device)] -> Handler ()
handleNewRegIDs newRegIDs = do
                        $(logInfo) ("\tHandling NewRegIds: "<>pack (show newRegIDs)<>"\t")
                        let replaceOldRegId (old,new) = do
                                            dev  <-  runDB $ getBy $ UniqueDevice old
                                            case dev of
                                                Just x  ->  runDB $ update (entityKey (x)) [DevicesIdentifier =. new ]
                                                Nothing ->  return ()
                        foldr (>>) (return ()) $ map replaceOldRegId $ newRegIDs

-- Handle the regIds that have been unregistered, I neew to remove them from the DB. (Unregistered error)
handleUnRegistered :: [Device] -> Handler ()
handleUnRegistered unRegIDs = do
                        $(logInfo) ("\tHandling UnRegistered: "<>pack (show unRegIDs)<>"\t")
                        let removeUnRegUser regid = runDB $ deleteBy $ UniqueDevice regid
                        foldr (>>) (return ()) $ map removeUnRegUser $ unRegIDs

-- I decide to unregister all regId with error different to Unregistered or Unavailable.
-- Because these are non-recoverable error.
handleRest :: [(RegId,Text)] -> Handler ()
handleRest rest = do
                    $(logInfo) ("\tHandling RestError: "<>pack (show rest)<>"\t")
                    let removeUnRegUser (regid,_) = runDB $ deleteBy $ UniqueDevice (GCM regid)
                    foldr (>>) (return ()) $ map removeUnRegUser $ rest

-- Handle the regIds that I need to resend because of an internal server error. (Unavailable error)
handleToResend :: PushNotification -> [Device] -> Handler ()
handleToResend msg list = do
                    $(logInfo) ("\tHandling ToReSend: "<>pack (show list)<>"\t")
                    if list /= []
                    then do
                        Messages _ _ appConfig m <- getYesod
                        res <- liftIO $ CE.catch -- I catch IO exceptions to avoid showing unsecure information.
                                    (send m appConfig msg list)
                                    (\e -> do
                                              let _ = (e :: CE.SomeException)
                                              fail "Problem communicating with GCM Server")
                        handleResult msg res
                    else do
                        return ()


main :: IO ()
main = do
 runResourceT . Control.Monad.Logger.runNoLoggingT $ withSqlitePool "DevicesDateBase.db3" 10 $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    man <- liftIO $ newManager def
    liftIO $ do
                static@(Static settings) <- static "static"
                warp 3000 $ Messages pool static def{
                                                     gcmAppConfig  = Just $ GCMAppConfig "key=" 5 -- Here you must complete with the correct Api Key provided by Google.
                                                 ,   mpnsAppConfig = Just def
                                                 } man
