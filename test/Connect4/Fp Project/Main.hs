-- Test Example for Push Notifications.
-- This is a simple example of a Yesod server, where devices can register
-- and play the multiplayer "Connect 4" game.

{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses #-}

module Main where

import Yesod
import Yesod.Static
import Database.Persist.Sqlite
import Data.Default
import Data.IORef
import Data.List                      ((\\))
import Data.Text                      (Text,pack,unpack,empty)
import Control.Monad.Logger
import Control.Monad.Trans.Resource   (runResourceT)
import PushNotify.Gcm
import PushNotify.General
import DataBase
import Handlers

-- Yesod App:

staticFiles "static"

data Messages = Messages {
                            connectionPool :: ConnectionPool -- Connection to the Database.
                         ,  getStatic      :: Static         -- Reference point of the static data.
                         ,  manager        :: PushManager
                         ,  pushAppSub     :: PushAppSub
                         }

mkYesod "Messages" [parseRoutes|
/fromdevices PushAppSubR PushAppSub pushAppSub
/getusers GetUsersR GET
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

getGetUsersR :: Handler RepJson
getGetUsersR = do
                  list  <- (runDB $ selectList [] [Desc DevicesUser]) >>= return . map (\a -> devicesUser(entityVal a))
                  list2 <- (runDB $ selectList [] [Desc GamesUser1])  >>= return . map (\a -> gamesUser1(entityVal a))
                  list3 <- (runDB $ selectList [] [Desc GamesUser2])  >>= return . map (\a -> gamesUser2(entityVal a))
                  let freeList = (list \\ list2) \\ list3
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
