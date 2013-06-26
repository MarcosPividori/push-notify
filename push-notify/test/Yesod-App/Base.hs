
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
    pass <- runInputPost $ ireq textField "pass"
    persona <- runDB $ getBy $ UniqueUser usr
    liftIO (putStr ("\nREGISTRO:\n-USUARIO: "++(unpack usr)++"\n-PASSWORD: "++ (unpack pass)++"\n-REG ID: "++ (unpack regId)++"\n"))
    case persona of
        Nothing -> do
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
    persona <- runDB $ getBy $ UniqueUser usr
    case persona of
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
    gcmResult <- liftIO $ CE.catch
             (sendGCM gcmappConfig def{registration_ids = Just regIdsList , data_object = Just (fromList [(pack "Message" .= msg)]) } 5)
             (\e -> do
                        let _ = (e :: CE.SomeException)
                        fail "Problem communicating with GCM Server")
    handleResult gcmResult
    redirect RootR

handleResult :: GCMresult -> Handler ()
handleResult msg = return () -- to be continued ...                      

main :: IO ()
main = do
 runResourceT $ withSqlitePool "DevicesDateBase.db3" openConnectionCount $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    liftIO $ warpDebug 3000 $ Messages pool GCMAppConfig{apiKey = "key="}

openConnectionCount :: Int
openConnectionCount = 10
