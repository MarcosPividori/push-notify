-- Test Example for Push Notifications.
-- This is a simple example of a Yesod server, where devices can register
-- and play the multiplayer "Connect 4" game.

{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses #-}

module Main where

import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail
import Database.Persist.Sqlite
import Data.Default
import Data.IORef
import Data.List                      ((\\))
import Data.Text                      (Text,pack,unpack,empty)
import Control.Monad.Logger
import Control.Monad.Trans.Resource   (runResourceT)
import Network.PushNotify.Gcm
import Network.PushNotify.General
import Network.PushNotify.General.Types
import DataBase
import Handlers
import Import
import Connect4
import qualified Data.Array  as DA
import Extra

-- Yesod App:

staticFiles "static"

data Messages = Messages {
                            connectionPool :: ConnectionPool -- Connection to the Database.
                         ,  getStatic      :: Static         -- Reference point of the static data.
                         ,  manager        :: PushManager
                         ,  pushAppSub     :: PushAppSub
                         }

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing
              
instance PathPiece MsgFromDevice where
    fromPathPiece t = readMaybe $ unpack t
    toPathPiece m   = pack $ show m

mkYesod "Messages" [parseRoutes|
/ RootR GET
/fromweb/#MsgFromDevice FromWebR GET
/fromdevices PushAppSubR PushAppSub pushAppSub
/getusers GetUsersR GET
/static StaticR Static getStatic
/auth AuthR Auth getAuth
|]

-- Instances:

instance Yesod Messages where
    approot = ApprootStatic "" -- Here you must complete with the correct route.

instance YesodAuth Messages where
    type AuthId Messages = Text
    getAuthId = return . Just . credsIdent
    loginDest _ = RootR
    logoutDest _ = RootR
    authPlugins _ =
        [ authBrowserId def
        , authGoogleEmail
        ]
    authHttpManager = (\(Just m) -> m) . httpManager . manager
    maybeAuthId = lookupSession "_ID"

instance YesodPersist Messages where
    type YesodPersistBackend Messages = SqlPersistT
    runDB action = do
        Messages pool _ _ _ <- getYesod
        runSqlPool action pool

instance RenderMessage Messages FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Handlers:
              
getFromWebR :: MsgFromDevice -> Handler Html
getFromWebR msg = do
                    maid <- maybeAuthId
                    case maid of
                     Nothing    -> return ()
                     Just user1 -> do
                                     res <- runDB $ getBy $ UniqueUser user1
                                     case res of
                                       Nothing -> return ()
                                       Just a  -> do
                                                    Messages pool _ man _ <- getYesod
                                                    liftIO $ handleMessage pool man (devicesIdentifier (entityVal a)) user1 msg
                    redirect RootR

getRootR :: Handler Html
getRootR = do
                maid <- maybeAuthId
                case maid of
                  Just user1 -> do
                              reg <- runDB $ getBy $ UniqueUser user1
                              case reg of
                                Nothing -> (runDB $ insert $ Devices user1 "nothing" $ Web user1) >> return ()
                                Just _  -> return ()
                              g1 <- runDB $ getBy $ UniqueUser1 user1
                              g2 <- runDB $ getBy $ UniqueUser2 user1
                              let g = case g1 of
                                        Nothing -> g2
                                        _       -> g1
                              case g of
                                Just g -> do
                                   let rows = map (\i -> (map (\j -> (j,(DA.!) (gamesMatrix(entityVal g)) (i,j))) [0..6])) [0..5]
                                       minusone = (-1)
                                       user2 = case gamesUser1 (entityVal g) == user1 of
                                              True  -> gamesUser2 (entityVal g)
                                              False -> gamesUser1 (entityVal g)
                                       turn = gamesTurn (entityVal g)
                                   defaultLayout $(widgetFile "Home")
                                _      -> do
                                   list <- getFreelist
                                   let freeList = list \\ [user1]
                                   defaultLayout $(widgetFile "SelectUser")                                            
                  _ -> redirect $ AuthR LoginR

getFreelist :: Handler [Text]
getFreelist = do
                  list  <- (runDB $ selectList [] [Desc DevicesUser]) >>= return . map (\a -> devicesUser(entityVal a))
                  list2 <- (runDB $ selectList [] [Desc GamesUser1])  >>= return . map (\a -> gamesUser1(entityVal a))
                  list3 <- (runDB $ selectList [] [Desc GamesUser2])  >>= return . map (\a -> gamesUser2(entityVal a))
                  return $ (list \\ list2) \\ list3

getGetUsersR :: Handler RepJson
getGetUsersR = do
                  freeList <- getFreelist
                  sendResponse $ toTypedContent $ object ["users" .= array freeList ]

main :: IO ()
main = do
  runResourceT . runNoLoggingT $ withSqlitePool "DevicesDateBase.db3" 10 $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    liftIO $ do
      ref <- newIORef Nothing
      (man,pSub) <- startPushService $ PushServiceConfig{
            pushConfig           = def{
                                       gcmAppConfig  = Just $ GCMAppConfig 
                                                              "" 
                                                              ""  
                                                              5 
                                   ,   mpnsAppConfig = Just def
                                   }
        ,   newMessageCallback   = handleNewMessage pool ref
        ,   newDeviceCallback    = handleNewDevice pool
        ,   unRegisteredCallback = handleUnregistered pool
        ,   newIdCallback        = handleNewId pool
        }
      writeIORef ref $ Just man
      static@(Static settings) <- static "static"
      warp 3000 $ Messages pool static man pSub

