-- Test Example for GCM Api.
-- This is a simple example of a Yesod server, where devices can register to receive
-- GCM messages and users can send messages through the web service.
-- Before running this app, its necessary to complete the "approot" and the "apiKey" with the proper values.

{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
             QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

import Yesod
import Yesod.Static
import Database.Persist.Sqlite
import Data.Text.Internal           (empty)
import Data.Text                    (Text,pack,unpack,append)
import qualified Data.Text          as T
import Data.Default
import Data.HashMap.Strict          (fromList)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.IO.Class       (liftIO)
import Network.PushNotify.Gcm.Types
import Network.PushNotify.Gcm.Send
import qualified Control.Exception  as CE
import Data.Aeson.Types
import Data.Monoid                  ((<>))

-- Data Base:

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Devices
    user Text
    password Text
    regId Text
    UniqueUser user
    UniqueRegId regId
    deriving Show
|]


-- Yesod App:

staticFiles "static"

data Messages = Messages {
                            connectionPool :: ConnectionPool -- Connection to the Database.
                         ,  getStatic :: Static -- ^ Reference point of the static data.
                         ,  gcmAppConfig :: GCMAppConfig -- GCM configuration.
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
        Messages pool _ _ <- getYesod
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
-- Receives a user name, password and a regId provided by a GCM Server.
postRegisterR :: Handler ()
postRegisterR = do
    regId <- runInputPost $ ireq textField "regId"
    usr <- runInputPost $ ireq textField "user"
    pass <- runInputPost $ ireq textField "password"
    device <- runDB $ getBy $ UniqueUser usr
    $(logInfo) ("\nIntent for a new user!:\n-User: "<> usr <>"\n-Password: "<> pass <>"\n-Red ID: "<> regId <>"\n")
    case device of
        Nothing -> do
                        dev  <-  runDB $ getBy $ UniqueRegId regId
                        case dev of
                            Just x  ->  do
                                            runDB $ update (entityKey (x)) [DevicesUser =. usr , DevicesPassword =. pass ]
                                            sendResponse $ RepJson emptyContent
                            Nothing ->  do
                                            runDB $ insert $ Devices usr pass regId
                                            sendResponse $ RepJson emptyContent
        Just a	-> case devicesPassword (entityVal (a)) == pass of
                        True  -> do -- regId has changed.
                                    runDB $ update (entityKey (a)) [DevicesRegId =. regId ]
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
    msg <- runInputPost $ ireq textField "message" 
    dest <- runInputPost $ ireq textField "destination"
    list <- case dest of
                    "EveryOne"  ->  runDB $ selectList [] [Desc DevicesRegId]
                    usr         ->  do
                                        res <- runDB $ getBy $ UniqueUser usr
                                        case res of
                                            Just a  -> return [a]
                                            Nothing -> return []
    Messages _ _ gcmappConfig <- getYesod
    $(logInfo) ("\tA new message: \""<>msg<>"\"\t")
    let regIdsList = map (\a -> devicesRegId(entityVal a)) list
    if regIdsList /= []
    then do 
            let gcmMessage = def{registration_ids = Just regIdsList , data_object = Just (fromList [(pack "Message" .= msg)]) }
            gcmResult <- liftIO $ CE.catch -- I catch IO exceptions to avoid showing unsecure information.
                        (sendGCM gcmappConfig gcmMessage 5)
                        (\e -> do
                                    let _ = (e :: CE.SomeException)
                                    fail "Problem communicating with GCM Server")
            handleResult gcmMessage gcmResult
    else do
            return ()
    his <- lookupSession "history"
    case his of
        Just m  -> setSession "history" (m <> "\n" <> msg)
        Nothing -> setSession "history" msg
    redirect RootR

-- Handle the result of the communication with the GCM Server.
handleResult :: GCMmessage -> GCMresult -> GHandler Messages Messages ()
handleResult msg res = do
                        handleNewRegIDs $ newRegids res
                        handleUnRegistered $ errorUnRegistered res
                        handleToResend msg $ errorToReSend res
                        handleRest $ errorRest res
                        
-- Handle the regIds that have changed, I neew to actualize the DB.
handleNewRegIDs :: [(RegId,RegId)] -> GHandler Messages Messages ()
handleNewRegIDs newRegIDs = do
                        $(logInfo) ("\tHandling NewRegIds: "<>pack (show newRegIDs)<>"\t")
                        let replaceOldRegId (old,new) = do
                                            dev  <-  runDB $ getBy $ UniqueRegId old
                                            case dev of
                                                Just x  ->  runDB $ update (entityKey (x)) [DevicesRegId =. new ]
                                                Nothing ->  return ()
                        foldr (>>) (return ()) $ map replaceOldRegId $ newRegIDs

-- Handle the regIds that have been unregistered, I neew to remove them from the DB. (Unregistered error)
handleUnRegistered :: [RegId] -> GHandler Messages Messages ()
handleUnRegistered unRegIDs = do
                        $(logInfo) ("\tHandling UnRegistered: "<>pack (show unRegIDs)<>"\t")
                        let removeUnRegUser regid = runDB $ deleteBy $ UniqueRegId regid
                        foldr (>>) (return ()) $ map removeUnRegUser $ unRegIDs

-- I decide to unregister all regId with error different to Unregistered or Unavailable.
-- Because these are non-recoverable error.
handleRest :: [(RegId,Text)] -> GHandler Messages Messages ()
handleRest rest = do
                    $(logInfo) ("\tHandling RestError: "<>pack (show rest)<>"\t")
                    let removeUnRegUser (regid,_) = runDB $ deleteBy $ UniqueRegId regid
                    foldr (>>) (return ()) $ map removeUnRegUser $ rest

-- Handle the regIds that I need to resend because of an internal server error. (Unavailable error)
handleToResend :: GCMmessage -> [RegId] -> GHandler Messages Messages ()
handleToResend msg list = do
                    $(logInfo) ("\tHandling ToReSend: "<>pack (show list)<>"\t")
                    if list /= []
                    then do
                        Messages _ _ gcmappConfig <- getYesod
                        gcmResult <- liftIO $ CE.catch -- I catch IO exceptions to avoid showing unsecure information.
                                    (sendGCM gcmappConfig msg{registration_ids = Just list} 5)
                                    (\e -> do
                                                let _ = (e :: CE.SomeException)
                                                fail "Problem communicating with GCM Server")
                        handleResult msg{registration_ids = Just list} gcmResult
                    else do
                        return ()


main :: IO ()
main = do
 runResourceT $ withSqlitePool "DevicesDateBase.db3" 10 $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    liftIO $ do
                static@(Static settings) <- static "static"
                warpDebug 3000 $ Messages pool static GCMAppConfig{apiKey = "key="} -- Here you must complete with the correct Api Key provided by Google.
