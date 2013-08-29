-- Test Example for Push Notifications.
-- This is a simple example of a Yesod server, where devices can register
-- and play the multiplayer Connect 4 game.

{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell, FlexibleInstances,
             QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

import Yesod
import Yesod.Static
import Database.Persist
import Database.Persist.Sqlite
import Data.Aeson.Types
import Data.Conduit.Pool
import Data.Default
import Data.Functor
import Data.IORef
import Data.Monoid                    ((<>))
import Data.Text.Internal             (empty)
import Data.Text                      (Text,pack,unpack,append,isPrefixOf)
import qualified Control.Exception    as CE
import qualified Data.HashMap.Strict  as HM
import qualified Data.Text            as T
import qualified Data.Map             as M
import qualified Data.Vector          as DV
import Text.XML
import Text.Hamlet.XML
import Control.Applicative
import Control.Monad                  (mzero)
import Control.Monad.IO.Class         (liftIO)
import Control.Monad.Logger
import Control.Monad.Trans.Resource   (runResourceT)
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Network.PushNotify.Gcm
import Network.PushNotify.Apns
import Network.PushNotify.Mpns
import Network.PushNotify.General
import Extra

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
                  list  <- runDB $ selectList [] [Desc DevicesUser]
                  users <- return $ map (\a -> devicesUser(entityVal a)) list
                  sendResponse $ toTypedContent $ object ["users" .= array users ]

main :: IO ()
main = do
  runResourceT . runNoLoggingT $ withSqlitePool "DevicesDateBase.db3" 10 $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    liftIO $ do
      ref <- newIORef Nothing
      (man,pSub) <- startPushService $ PushServiceConfig{
            pushConfig           = def{
                                       gcmAppConfig  = Just $ GCMAppConfig "" "" 5 -- Here you must complete with the correct Api Key and SenderId.
--                                 ,   apnsAppConfig = Just def{certificate  = "" , privateKey   = "" }
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
      
      where
       pars :: Value -> Parser (Text,Text)
       pars (Object v) = (,) <$>
                         v .: "user" <*>
                         v .: "password"
       pars _          = mzero

       parsMsg :: Value -> Parser Text
       parsMsg (Object v) = v .: "message"
       parsMsg _          = mzero
       
       runDBAct p a = runResourceT . runNoLoggingT $ runSqlPool a p
              
       handleNewDevice pool d v = do
          res  <- return $ parseMaybe pars v
          case res of
            Nothing         -> return $ ErrorReg "No User or Password"
            Just (usr,pass) -> do
              putStr ("\nIntent for a new user!:\n-User: " ++ show usr ++ "\n-Password: "
                      ++ show pass ++ "\n-Identifier: " ++ show d ++ "\n")
              device <- runDBAct pool $ getBy $ UniqueUser usr
              case device of
                Nothing -> do
                             dev <- runDBAct pool $ getBy $ UniqueDevice d
                             case dev of
                               Just x  -> do 
                                            runDBAct pool $ update (entityKey (x)) [DevicesUser =. usr , DevicesPassword =. pass]
                                            return SuccessfulReg
                               Nothing -> do
                                            runDBAct pool $ insert $ Devices usr pass d
                                            return SuccessfulReg
                Just a	-> case devicesPassword (entityVal (a)) == pass of
                             True  -> do
                                        runDBAct pool $ update (entityKey (a)) [DevicesIdentifier =. d]
                                        return SuccessfulReg
                             False -> return $ ErrorReg "Invalid Username or Password"

       handleNewMessage pool ref d v = do
          Just man <- readIORef ref
          res  <- return $ parseMaybe pars v
          case res of
            Nothing         -> return ()
            Just (usr,pass) -> do
              device <- runDBAct pool $ getBy $ UniqueUser usr
              case device of
                Nothing -> return ()
                Just a	-> case devicesPassword (entityVal (a)) == pass of
                             False -> return ()
                             True  -> do
                                        m  <- return $ parseMaybe parsMsg v
                                        case m of
                                          Nothing  -> return ()
                                          Just msg -> do
                                                        let message = def {gcmNotif  = Just $ def {
                                                                      data_object = Just (HM.fromList [(pack "Message" .= msg)]) }}
                                                        --sendPush man message [d]
                                                        putStr ("\nNew message from device!:\n-User: " ++ show usr
                                                                ++ "\n-Msg: " ++ show msg ++ "\n")
       
       handleNewId pool (old,new) = do
                                      dev  <- runDBAct pool $ getBy $ UniqueDevice old
                                      case dev of
                                          Just x  ->  runDBAct pool $ update (entityKey (x)) [DevicesIdentifier =. new ]
                                          Nothing ->  return ()

       handleUnregistered pool d = runDBAct pool $ deleteBy $ UniqueDevice d
