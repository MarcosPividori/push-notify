-- Test Example for GCM Api.
-- This is a simple example of a Yesod server, where devices can register to receive
-- GCM messages and users can send messages through the web service.
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
--import Network (withSocketsDo)
import Network.PushNotify.Gcm.Types
import Network.PushNotify.Gcm.Send
import Network.PushNotify.Mpns.Types
import Network.PushNotify.Mpns.Send
import Network.HTTP.Conduit
import qualified Control.Exception  as CE
import Data.Aeson.Types
import Data.Monoid                  ((<>))
import General

-- Data Base:

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Devices
    user Text
    password Text
    identifier Ident
    UniqueUser user
    UniqueIdent identifier
    deriving Show
|]


-- Yesod App:

staticFiles "static"

data Messages = Messages {
                            connectionPool :: ConnectionPool -- Connection to the Database.
                         ,  getStatic :: Static -- Reference point of the static data.
                         ,  appConfig :: AppConfig -- configuration.
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
    type YesodPersistBackend Messages = SqlPersist
    runDB action = do
        Messages pool _ _ _ <- getYesod
        runSqlPool action pool

instance RenderMessage Messages FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Handlers:

-- 'getRootR' provides the main page.
getRootR :: Handler RepHtml
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
    let iden = case msys of
		Nothing       -> GCM  regId
                Just "WPhone" -> MPNS regId
    device <- runDB $ getBy $ UniqueUser usr
    $(logInfo) ("\nIntent for a new user!:\n-User: "<> usr <>"\n-Password: "<> pass <>"\n-Identifier: "<> (pack $ show iden) <>"\n")
    case device of
        Nothing -> do
                        dev  <-  runDB $ getBy $ UniqueIdent iden
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
                    "EveryOne"  ->  runDB $ selectList [] [Desc DevicesIdentifier]
                    usr         ->  do
                                        res <- runDB $ getBy $ UniqueUser usr
                                        case res of
                                            Just a  -> return [a]
                                            Nothing -> return []
    Messages _ _ appConfig man <- getYesod
    $(logInfo) ("\tA new message: \""<>msg<>"\"\t")
    let regIdsList = map (\a -> devicesIdentifier(entityVal a)) list
    if regIdsList /= []
    then do 
            let message = Message{
                         gcmMessage  = def{registration_ids = Just (gcmDevices regIdsList) , data_object = Just (fromList [(pack "Message" .= msg)]) }
                     ,   mpnsMessage = def {deviceURIs = mpnsDevices regIdsList , target = Toast , rest = Document (Prologue [] Nothing []) (xmlMessage msg) []}
                     }
            result <- liftIO $ CE.catch -- I catch IO exceptions to avoid showing unsecure information.
                        (send man appConfig message)
                        (\e -> do
                                    let _ = (e :: CE.SomeException)
                                    fail "Problem communicating with Push Servers")
            return ()
            --handleResult gcmMessage gcmResult
    else do
            return ()
    his <- lookupSession "history"
    case his of
        Just m  -> setSession "history" (m <> "\n" <> msg)
        Nothing -> setSession "history" msg
    redirect RootR
    where
        gcmDevices  = map (\(GCM  x) -> x) . filter (\x -> case x of 
                                                         GCM _ -> True
                                                         _     -> False)
        mpnsDevices = map (\(MPNS x) -> x) . filter (\x -> case x of 
                                                         MPNS _ -> True
                                                         _      -> False)
        xmlMessage msg = Element (Name "Notification" (Just "WPNotification") (Just "wp")) (M.singleton "xmlns:wp" "WPNotification") [xml|
<wp:Toast>
    <wp:Text1>New message: 
    <wp:Text2>#{msg}
    <wp:Param>?msg=#{msg}
|]

-- Handle the result of the communication with the GCM Server.
handleResult :: GCMmessage -> GCMresult -> Handler ()
handleResult msg res = do
                        handleNewRegIDs $ newRegids res
                        handleUnRegistered $ errorUnRegistered res
                        handleToResend msg $ errorToReSend res
                        handleRest $ errorRest res
                        
-- Handle the regIds that have changed, I neew to actualize the DB.
handleNewRegIDs :: [(RegId,RegId)] -> Handler ()
handleNewRegIDs newRegIDs = do
                        $(logInfo) ("\tHandling NewRegIds: "<>pack (show newRegIDs)<>"\t")
                        let replaceOldRegId (old,new) = do
                                            dev  <-  runDB $ getBy $ UniqueIdent (GCM old)
                                            case dev of
                                                Just x  ->  runDB $ update (entityKey (x)) [DevicesIdentifier =. (GCM new) ]
                                                Nothing ->  return ()
                        foldr (>>) (return ()) $ map replaceOldRegId $ newRegIDs

-- Handle the regIds that have been unregistered, I neew to remove them from the DB. (Unregistered error)
handleUnRegistered :: [RegId] -> Handler ()
handleUnRegistered unRegIDs = do
                        $(logInfo) ("\tHandling UnRegistered: "<>pack (show unRegIDs)<>"\t")
                        let removeUnRegUser regid = runDB $ deleteBy $ UniqueIdent (GCM regid)
                        foldr (>>) (return ()) $ map removeUnRegUser $ unRegIDs

-- I decide to unregister all regId with error different to Unregistered or Unavailable.
-- Because these are non-recoverable error.
handleRest :: [(RegId,Text)] -> Handler ()
handleRest rest = do
                    $(logInfo) ("\tHandling RestError: "<>pack (show rest)<>"\t")
                    let removeUnRegUser (regid,_) = runDB $ deleteBy $ UniqueIdent (GCM regid)
                    foldr (>>) (return ()) $ map removeUnRegUser $ rest

-- Handle the regIds that I need to resend because of an internal server error. (Unavailable error)
handleToResend :: GCMmessage -> [RegId] -> Handler ()
handleToResend msg list = do
                    $(logInfo) ("\tHandling ToReSend: "<>pack (show list)<>"\t")
                    if list /= []
                    then do
                        Messages _ _ (AppConfig gcmcnfg _) m <- getYesod
                        gcmResult <- liftIO $ CE.catch -- I catch IO exceptions to avoid showing unsecure information.
                                    (sendGCM m gcmcnfg msg{registration_ids = Just list})
                                    (\e -> do
                                                let _ = (e :: CE.SomeException)
                                                fail "Problem communicating with GCM Server")
                        handleResult msg{registration_ids = Just list} gcmResult
                    else do
                        return ()


main :: IO ()
main = do
 runResourceT . Control.Monad.Logger.runNoLoggingT $ withSqlitePool "DevicesDateBase.db3" 10 $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    man <- liftIO $ newManager def
    liftIO $ do
                static@(Static settings) <- static "static"
                warpDebug 3000 $ Messages pool static AppConfig{
                                                     gcmAppConfig  = GCMAppConfig "key=" 5 -- Here you must complete with the correct Api Key provided by Google.
                                                 ,   mpnsAppConfig = def
                                                 } man
