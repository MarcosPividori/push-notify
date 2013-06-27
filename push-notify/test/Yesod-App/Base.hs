
{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
             QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

import Yesod
import Database.Persist.Sqlite
import Data.Text.Internal           (empty)
import Data.Text                    (Text,pack,unpack)
import Data.Default
import Data.HashMap.Strict          (fromList)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.IO.Class       (liftIO)
import Network.PushNotify.Gcm.Types
import Network.PushNotify.Gcm.Send
import qualified Control.Exception  as CE

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Devices
    user Text
    password Text
    regId Text
    UniqueUser user
    UniqueRegId regId
    deriving Show
|]


data Messages = Messages {
                            connectionPool :: ConnectionPool -- Connection to the Database.
                         ,  gcmAppConfig :: GCMAppConfig -- GCM configuration.
                         }


mkYesod "Messages" [parseRoutes|
/ RootR GET
/register RegisterR POST
/fromdevices FromdevicesR POST
/fromweb FromWebR POST
|]


instance Yesod Messages where
    approot = ApprootStatic "http://192.168.0.52:3000" -- Here you must complete with the correct route.

instance YesodPersist Messages where
    type YesodPersistBackend Messages = SqlPersist
    runDB action = do
        Messages pool _ <- getYesod
        runSqlPool action pool

instance RenderMessage Messages FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Handlers:

getRootR :: Handler RepHtml
getRootR = defaultLayout $ do
                                setTitle "Just a test example."
                                toWidget [hamlet|
<div id="main">
                <li><a>
                        <form method=post action=@{FromWebR}>
                            <input type="text" name="message" />
                            <input type=submit style="font-weight:bold;" value="Send">
|]


-- 'postRegister' allows a mobile device register. (POST messages to '/register')
-- Receives a user name, and regId provided by a GCM Server.
postRegisterR :: Handler ()
postRegisterR = do
    regId <- runInputPost $ ireq textField "regId"
    usr <- runInputPost $ ireq textField "user"
    pass <- runInputPost $ ireq textField "password"
    device <- runDB $ getBy $ UniqueUser usr
    liftIO (putStr ("\nREGISTRO:\n-USUARIO: "++(unpack usr)++"\n-PASSWORD: "++ (unpack pass)++"\n-REG ID: "++ (unpack regId)++"\n"))
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
                        True  -> do
                                    runDB $ update (entityKey (a)) [DevicesRegId =. regId ]
                                    sendResponse $ RepJson emptyContent
                        False -> invalidArgs []


-- 'postFromDevicesR' receives the messages sent by registered devices. (POST messages to '/fromdevices')
--  Authenticates devices through its user name and password.
postFromdevicesR :: Handler ()
postFromdevicesR = do
    msg <- runInputPost $ ireq textField "message"
    usr <- runInputPost $ ireq textField "user"
    pass <- runInputPost $ ireq textField "password"
    device <- runDB $ getBy $ UniqueUser usr
    case device of
        Nothing -> permissionDenied empty
        Just a	-> case devicesPassword (entityVal (a)) == pass of
                       True  -> permissionDenied empty -- to be changed ...
                       False -> permissionDenied empty


-- 'postFromWebR' receives messages from the website user. (POST messages to '/fromweb')
postFromWebR :: Handler ()
postFromWebR = do
    msg <- runInputPost $ ireq textField "message"
    Messages _ gcmappConfig <- getYesod
    list <- runDB $ selectList [] [Desc DevicesRegId]
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
    redirect RootR

handleResult :: GCMmessage -> GCMresult -> GHandler Messages Messages ()
handleResult msg res = do
                        handleNewRegIDs $ newRegids res
                        handleUnRegistered $ errorUnRegistered res
                        handleToResend msg $ errorToReSend res
                        handleRest $ errorRest res
                        

handleNewRegIDs :: [(RegId,RegId)] -> GHandler Messages Messages ()
handleNewRegIDs newRegIDs = do
                        liftIO (putStr ("\nHANDLING NEWREGIDS: "++(show newRegIDs)++"\n"))
                        let replaceOldRegId (old,new) = do
                                            dev  <-  runDB $ getBy $ UniqueRegId old
                                            case dev of
                                                Just x  ->  runDB $ update (entityKey (x)) [DevicesRegId =. new ]
                                                Nothing ->  return ()
                        foldr (>>) (return ()) $ map replaceOldRegId $ newRegIDs

handleUnRegistered :: [RegId] -> GHandler Messages Messages ()
handleUnRegistered unRegIDs = do
                        liftIO (putStr ("\nHANDLING UNREGISTERED: "++(show unRegIDs)++"\n"))
                        let removeUnRegUser regid = runDB $ deleteBy $ UniqueRegId regid
                        foldr (>>) (return ()) $ map removeUnRegUser $ unRegIDs

-- I decide to unregister all regId with error different to Unregistered or Unavailable.
-- Because these are non-recoverable error.
handleRest :: [(RegId,Text)] -> GHandler Messages Messages ()
handleRest rest = do
                    liftIO (putStr ("\nHANDLING RESTERROR: "++(show rest)++"\n"))
                    let removeUnRegUser (regid,_) = runDB $ deleteBy $ UniqueRegId regid
                    foldr (>>) (return ()) $ map removeUnRegUser $ rest

handleToResend :: GCMmessage -> [RegId] -> GHandler Messages Messages ()
handleToResend msg list = do
                    liftIO (putStr ("\nHANDLING TORESEND: "++(show list)++"\n"))
                    if list /= []
                    then do
                        Messages _ gcmappConfig <- getYesod
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
 runResourceT $ withSqlitePool "DevicesDateBase.db3" openConnectionCount $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    liftIO $ warpDebug 3000 $ Messages pool GCMAppConfig{apiKey = "key="}

openConnectionCount :: Int
openConnectionCount = 10
