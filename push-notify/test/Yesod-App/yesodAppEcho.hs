-- Echo Test Example for Push Notifications.
-- This is a simple example of a Yesod server, where devices can register to receive
-- messages and every time they send a message to the server, they receive an echo response.

{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell, FlexibleInstances,
             QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

import Yesod
import Database.Persist.Sqlite
import Database.Persist
import Data.Aeson.Types
import Data.Conduit.Pool
import Data.Default
import Data.Functor
import Data.IORef
import Data.Text                      (Text,pack)
import qualified Data.Map             as M
import qualified Data.HashMap.Strict  as HM
import Text.Hamlet.XML
import Text.XML
import Control.Applicative
import Control.Monad                  (mzero)
import Control.Monad.Trans.Resource   (runResourceT)
import Control.Monad.IO.Class         (liftIO)
import Control.Monad.Logger
import Network.PushNotify.Gcm.Types
import Network.PushNotify.Apns.Types
import Network.PushNotify.Mpns.Types
import Network.PushNotify.General
import Network.PushNotify.PushService
import Extra

-- Data Base:

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Devices
    identifier Device
    UniqueDevice identifier
    deriving Show
|]

-- Yesod App:

data Echo = Echo {
                     connectionPool :: ConnectionPool
                  ,  manager        :: PushManager
                  ,  pushAppSub     :: PushAppSub     -- Yesod Subsite.
                  }

mkYesod "Echo" [parseRoutes|
/ PushAppSubR PushAppSub pushAppSub
|]

-- Instances:

instance Yesod Echo

instance YesodPersist Echo where
    type YesodPersistBackend Echo = SqlPersistT
    runDB action = do
        Echo pool _ _ <- getYesod
        runSqlPool action pool

instance RenderMessage Echo FormMessage where
    renderMessage _ _ = defaultFormMessage

main :: IO ()
main = do
  runResourceT . runNoLoggingT $ withSqlitePool "DevicesDateBase.db3" 10 $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    liftIO $ do
      ref <- newIORef Nothing
      (man,pSub) <- startPushService $ PushServiceConfig{
            pushConfig           = def{
                                       gcmAppConfig  = Just $ GCMAppConfig "key=" 5 -- Here you must complete with the correct Api Key.
--                                 ,   apnsAppConfig = Just apnsMan
                                   ,   mpnsAppConfig = Just def
                                   }
        ,   newMessageCallback   = handleNewMessage pool ref
        ,   newDeviceCallback    = handleNewDevice pool
        ,   unRegisteredCallback = handleUnregistered pool
        ,   newIdCallback        = handleNewId pool
        }
      writeIORef ref $ Just man
      warp 3000 $ Echo pool man pSub
      
      where
       parsMsg :: Value -> Parser Text
       parsMsg (Object v) = v .: "message"
       parsMsg _          = mzero
       
       runDBAct p a = runResourceT . runNoLoggingT $ runSqlPool a p
              
       handleNewDevice pool d v = do
          device <- runDBAct pool $ getBy $ UniqueDevice d
          case device of
            Nothing -> do
                         runDBAct pool $ insert $ Devices d
                         return SuccessfulReg
            Just a	-> return $ ErrorReg "User has registered before."

       handleNewMessage pool ref d v = do
          Just man <- readIORef ref
          device <- runDBAct pool $ getBy $ UniqueDevice d
          case device of
            Nothing -> return ()
            Just a	-> do
                         m <- return $ parseMaybe parsMsg v
                         case m of
                           Nothing  -> return ()
                           Just msg -> do
                                         let message = def {
                                                             gcmNotif  = Just $ def {data_object = Just (HM.fromList 
                                                                         [(pack "Message" .= msg)]) }
                                                           , mpnsNotif = Just $ def {target = Toast , restXML = 
                                                                         Document (Prologue [] Nothing []) (xmlMessage msg) []}}
                                         sendPush man message [d]
                                         return ()

       handleNewId pool (old,new) = do
                                      dev  <- runDBAct pool $ getBy $ UniqueDevice old
                                      case dev of
                                        Just x  ->  runDBAct pool $ update (entityKey (x)) [DevicesIdentifier =. new ]
                                        Nothing ->  return ()

       handleUnregistered pool d = runDBAct pool $ deleteBy $ UniqueDevice d
       
       xmlMessage msg = Element (Name "Notification" (Just "WPNotification") (Just "wp")) (M.singleton "xmlns:wp" "WPNotification") [xml|
<wp:Toast>
    <wp:Text1>New message: 
    <wp:Text2>#{msg}
    <wp:Param>?msg=#{msg}
|]
