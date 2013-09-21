{-# LANGUAGE OverloadedStrings #-}

-- This module defines the main callback functions to communicate with mobile devices using the general library.
module CommDevices
    ( handleNewId
    , handleUnregistered
    , handleNewMessage
    , handleNewDevice
    ) where

import Database.Persist.Sqlite
import Data.Aeson.Types
import Data.Conduit.Pool
import Data.Default
import Data.IORef
import Data.Maybe
import Data.Text                      (Text,pack,unpack,empty)
import qualified Data.HashMap.Strict  as HM
import Control.Applicative
import Control.Monad                  (mzero)
import Control.Monad.Logger
import Control.Monad.Trans.Resource   (runResourceT)
import Network.PushNotify.Gcm
import Network.PushNotify.General
import Connect4
import DataBase
import Extra
import Handlers


pars :: Value -> Parser (Text,Text)
pars (Object v) = (,) <$>
                  v .:  "user" <*>
                  v .:  "password"
pars _          = mzero

runDBAct p a = runResourceT . runNoLoggingT $ runSqlPool a p

handleNewDevice :: Pool Connection -> Device -> Value -> IO RegisterResult
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
           Just a  -> case devicesPassword (entityVal (a)) == pass of
                        True  -> do
                            runDBAct pool $ update (entityKey (a)) [DevicesIdentifier =. d]
                            return SuccessfulReg
                        False -> return $ ErrorReg "Invalid Username or Password"

handleNewId :: Pool Connection -> (Device, Device) -> IO ()
handleNewId pool (old,new) = do
    dev  <- runDBAct pool $ getBy $ UniqueDevice old
    case dev of
      Just x  ->  runDBAct pool $ update (entityKey (x)) [DevicesIdentifier =. new ]
      Nothing ->  return ()

handleUnregistered :: Pool Connection -> Device -> IO ()
handleUnregistered pool d = runDBAct pool $ deleteBy $ UniqueDevice d

handleNewMessage :: Pool Connection -> IORef WebUsers -> IORef (Maybe PushManager) -> Device -> Value -> IO ()
handleNewMessage pool webRef ref d v = do
   Just man <- readIORef ref
   res    <- return $ parseMaybe pars v
   isUser <- authenticate res
   case isUser of 
     Nothing  -> return ()
     Just usr -> do--Authenticated!
         m  <- return $ parsMessage v
         case m of 
           Nothing -> return ()
           Just m2 -> do
               webUsers <- readIORef webRef
               handleMessage pool webUsers man (Dev d) usr m2
   where
       authenticate Nothing           = return Nothing
       authenticate (Just (usr,pass)) = do
           device <- runDBAct pool $ getBy $ UniqueUser usr
           case device of
             Nothing -> return Nothing
             Just a	 -> case devicesPassword (entityVal (a)) == pass of
                          False -> return Nothing
                          True  -> return $ Just usr
