-- Test Example for Push Notifications.
-- This is a simple example of a Yesod server, where devices can register
-- and play the multiplayer "Connect 4" game, and send/receive messages.

{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses #-}

module Main where

import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail
import Database.Persist.Sqlite
import Data.Aeson
import Data.Default
import Data.IORef
import Data.List                        ((\\))
import Data.Text                        (Text,pack,unpack,empty)
import Data.Time.Clock.POSIX
import qualified Data.Array             as DA
import Control.Concurrent
import Control.Concurrent.STM           (atomically)
import Control.Concurrent.STM.TChan     (TChan, newTChan, tryReadTChan, readTChan)
import Control.Monad.Logger
import Control.Monad.Trans.Resource     (runResourceT)
import System.Environment               (getEnv)
import System.Timeout
import Network.PushNotify.Gcm
import Network.PushNotify.General
import Network.HTTP.Conduit
import Connect4
import CommWebUsers
import CommDevices
import DataBase
import Extra
import Handlers
import Import

-- Yesod App:

staticFiles "static"

data Messages = Messages {
                            connectionPool :: ConnectionPool -- Connection to the Database.
                         ,  getStatic      :: Static         -- Reference point of the static data.
                         ,  pushManager    :: PushManager
                         ,  httpManager    :: Manager
                         ,  onlineUsers    :: IORef WebUsers
                         }

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

mkYesod "Messages" [parseRoutes|
/ RootR GET
/fromweb FromWebR POST
/receive ReceiveR GET
/fromdevices PushManagerR PushManager pushManager
/getusers GetUsersR GET
/static StaticR Static getStatic
/auth AuthR Auth getAuth
|]

-- Instances:

instance Yesod Messages where
    approot = ApprootStatic "http://192.168.0.52:3000"
    defaultLayout widget = do
        pc <- widgetToPageContent $ do
            widget
        base <- widgetToPageContent ($(widgetFile "base"))
        giveUrlRenderer [hamlet|
$doctype 5
<html>
    <head>
        ^{pageHead base}
        ^{pageHead pc}
    <body>
        <div id="main">
            ^{pageBody base}
            ^{pageBody pc}
        <div id="footer">
           <div id="source">
             Source code available on <a href="https://github.com/MarcosPividori/Connect4-GSoCExample"><b>GitHub</b></a>
           <div id="deployment">
             Deployed on <a href="https://www.fpcomplete.com/"><b>FPComplete</b></a>
|]

instance YesodAuth Messages where
    type AuthId Messages = Text
    getAuthId = return . Just . credsIdent
    loginDest _ = RootR
    logoutDest _ = RootR
    authPlugins _ =
        [ authBrowserId def
        , authGoogleEmail
        ]
    authHttpManager = httpManager
    maybeAuthId = lookupSession "_ID"

instance YesodPersist Messages where
    type YesodPersistBackend Messages = SqlPersistT
    runDB action = do
        Messages pool _ _ _ _ <- getYesod
        runSqlPool action pool

instance RenderMessage Messages FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Handlers:

-- This function actualizes the time of the last communication with a web user.
actualize :: Text -> Handler WebUsers
actualize user1 = do
    Messages _ _ _ _ webUsers <- getYesod
    ctime <- liftIO $ getPOSIXTime
    liftIO $ atomicModifyIORef webUsers (\s -> 
        case getClient user1 s of
          Just (c,_) -> let s' = addClient user1 c ctime s in (s',s')
          _          -> (s,s)  )

-- 'postFromWebR' receives messages from the website user. (POST messages to '/fromweb')
postFromWebR :: Handler ()
postFromWebR = do
    maid <- maybeAuthId
    case maid of
      Nothing    -> redirect $ AuthR LoginR
      Just user1 -> do
          Messages pool _ man _ _ <- getYesod
          list <- actualize user1
          case getClient user1 list of
            Nothing      -> redirect $ AuthR LoginR
            Just (id1,_) -> do
                m <- runInputGet $ iopt textField "message"
                $(logInfo) . pack $ "New Message : " ++ show m
                case m >>= readMaybe . unpack of
                  Just msg -> do
                                $(logInfo) . pack $ "New parsed Message : " ++ show msg
                                liftIO $ handleMessage pool list man (Web id1) user1 msg
                  _ -> return ()
                redirect RootR

-- 'getReceiveR' wait for an event. (GET messages to '/receive')
getReceiveR :: Handler ()
getReceiveR = do
    maid <- maybeAuthId
    case maid of
      Nothing    -> return ()
      Just user1 -> do
          Messages _ _ _ _ webRef <- getYesod
          webUsers <- liftIO $ readIORef webRef
          case getClient user1 webUsers of
            Nothing -> sendResponse $ RepJson $ toContent $ object [] 
            Just (ch,_) -> do
              actualize user1
              res <- liftIO $ timeout 10000000 $ (atomically $ readTChan ch)
              $(logInfo) $ pack $ "Sending MESSAGE TO WEBUSER" ++ show res
              case res of
                Nothing -> sendResponse $ RepJson $ toContent $ object []
                Just r  -> sendResponse $ RepJson $ toContent $ setMessageValue r

-- 'getRootR' provides the main page.
getRootR :: Handler Html
getRootR = do
    maid <- maybeAuthId
    case maid of
      Nothing -> redirect $ AuthR LoginR
      Just user1 -> do
          Messages _ _ _ _ webRef <- getYesod
          webUsers <- liftIO $ readIORef webRef
          case getClient user1 webUsers of
            Nothing     -> do
              ctime <- liftIO $ getPOSIXTime
              chan <- liftIO $ atomically $ newTChan
              liftIO $ atomicModifyIORef webRef (\s ->
                let s' = addClient user1 chan ctime s
                in (s', ()) )
            Just (ch,_) -> liftIO $ cleanTChan ch
          g1 <- runDB $ getBy $ UniqueUser1 user1
          g2 <- runDB $ getBy $ UniqueUser2 user1
          let (game,numUser) = case g1 of
                                 Nothing -> (g2,-1)
                                 _       -> (g1,1)
          case game of
            Just g  -> do
                let list = concat $ map (\i -> map (\j -> (DA.!) (gamesMatrix(entityVal g)) (i,j)) [0..6]) [0..5]
                    user2 = case gamesUser1 (entityVal g) == user1 of
                              True  -> gamesUser2 (entityVal g)
                              False -> gamesUser1 (entityVal g)
                    turn = gamesTurn (entityVal g)
                    jsonData = object [ "grid"    .= array list , "numUser" .= show numUser , "user2" .= user2
                                      , "playing" .= True       , "turn"    .= turn         , "user1" .= user1 ]
                defaultLayout $(widgetFile "Home")
            Nothing -> do
                let jsonData = object ["playing" .= False , "user1" .= user1 ]
                defaultLayout $(widgetFile "Home")
     where
         cleanTChan ch = do
                           r <- atomically $ tryReadTChan ch
                           case r of
                             Just _  -> cleanTChan ch
                             Nothing -> return ()

getFreelist :: Handler [Text]
getFreelist = do
    connect4list <- (runDB $ selectList [] [Desc DevicesUser])
                    >>= return . map (\a -> devicesUser(entityVal a))
    Messages _ _ _ _ refUsers <- getYesod
    webList <- liftIO $ readIORef refUsers
                        >>= return . getClients
    playinglist1 <- runDB (selectList [] [Desc GamesUser1])
                    >>= return . map (\a -> gamesUser1(entityVal a))
    playinglist2 <- runDB (selectList [] [Desc GamesUser2])
                    >>= return . map (\a -> gamesUser2(entityVal a))
    let mCon4 = ((connect4list ++ webList) \\ playinglist1) \\ playinglist2
    return mCon4

-- 'getGetUsersR' provides the list of users that are available to play ("con4").
getGetUsersR :: Handler RepJson
getGetUsersR = do
    maid <- maybeAuthId
    case maid of
      Just user1 -> actualize user1 >> return ()
      Nothing -> return () 
    con4List <- getFreelist
    sendResponse $ toTypedContent $ object ["con4" .= array con4List ]

main :: IO ()
main = runResourceT . runNoLoggingT $ withSqlitePool "DevicesDateBase.db3" 10 $ \pool -> do
     runSqlPool (runMigration migrateAll) pool
     liftIO $ do
      ref <- newIORef Nothing
      onlineUsers <- newIORef newWebUsersState
      man <- startPushService $ PushServiceConfig{
            pushConfig           = def{ gcmConfig  = Just $ Http $ def{apiKey = "api key for connect 4 app"} }
        ,   newMessageCallback   = handleNewMessage pool onlineUsers ref
        ,   newDeviceCallback    = handleNewDevice pool
        ,   unRegisteredCallback = handleUnregistered pool
        ,   newIdCallback        = handleNewId pool
        }
      m <- newManager def
      forkIO $ checker onlineUsers man pool
      writeIORef ref $ Just man
      static@(Static settings) <- static "static"
      warp 3000 $ Messages pool static man m onlineUsers

limit :: POSIXTime
limit = 240

-- 'checker' checks every 30 seconds the webUsers list and removes inactive users.
checker :: IORef WebUsers -> PushManager -> ConnectionPool -> IO ()
checker onlineUsers man pool = do
                        ctime <- getPOSIXTime
                        (r,c,s) <- atomicModifyIORef onlineUsers (\s ->
                            let rem = filterClients (\(c,t) -> (ctime - t) > limit) s
                                names = getClients $ rem
                                chans = getClientsElems $ rem
                                s' = filterClients (\(c,t) -> (ctime - t) <= limit) s
                            in (s', (names,chans,s)) )
                        putStrLn $ "Remove: " ++ show r
                        mapM_ (\(us,(ch,_)) -> handleMessage pool s man (Web ch) us Cancel) $ zip r c
                        threadDelay 30000000
                        checker onlineUsers man pool
