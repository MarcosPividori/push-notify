{-# LANGUAGE OverloadedStrings #-}

module Handlers
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
import Data.Text                      (Text,pack,unpack,empty)
import qualified Data.HashMap.Strict  as HM
import Control.Applicative
import Control.Monad                  (mzero)
import Control.Monad.Logger
import Control.Monad.Trans.Resource   (runResourceT)
import PushNotify.Gcm
import PushNotify.General
import Connect4
import DataBase

data MsgFromDevice = Cancel | Movement Int | NewGame Text | Winner Text

pars :: Value -> Parser (Text,Text)
pars (Object v) = (,) <$>
                  v .: "user" <*>
                  v .: "password"
pars _          = mzero

parsMsg :: Value -> Parser MsgFromDevice
parsMsg (Object v) = setMsg <$>
                       v .:? "Cancel"   <*>
                       v .:? "Movement" <*>
                       v .:? "NewGame"
parsMsg _          = mzero

setMsg :: Maybe Text -> Maybe Text -> Maybe Text -> MsgFromDevice
setMsg (Just _) _ _ = Cancel
setMsg _ (Just a) _ = Movement ((read $ unpack a) :: Int)
setMsg _ _ (Just a) = NewGame a
       
setMessage :: MsgFromDevice -> PushNotification
setMessage m = let message = case m of
                               Cancel       -> [(pack "Cancel" .= pack "")]
                               Movement mov -> [(pack "Movement" .= (pack $ show mov) )]
                               NewGame usr  -> [(pack "NewGame" .= usr)]
                               Winner  usr  -> [(pack "Winner" .= usr)]
               in def { gcmNotif  = Just $ def { data_object = Just (HM.fromList message) } }

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
         Just a	-> case devicesPassword (entityVal (a)) == pass of
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

handleNewMessage :: Pool Connection -> IORef (Maybe PushManager) -> Device -> Value -> IO ()
handleNewMessage pool ref d v = do
   Just man <- readIORef ref
   res      <- return $ parseMaybe pars v
   isUser   <- authenticate res
   case isUser of 
     Nothing  -> return ()
     Just usr -> do--Authenticated!
                   m  <- return $ parseMaybe parsMsg v
                   case m of 
                     Nothing -> return ()
                     Just m2 -> case m2 of
                      Cancel       -> do--Cancel
                                        user2 <- getOpponentId usr
                                        sendPush man (setMessage Cancel) user2
                                        runDBAct pool $ deleteBy $ UniqueUser1 usr
                                        runDBAct pool $ deleteBy $ UniqueUser2 usr

                      Movement mov -> do--New Movement
                                        user2 <- getOpponentId usr
                                        game  <- getGame usr
                                        case (user2,game) of
                                          ([],_)      -> sendPush man (setMessage Cancel) [d] >> return ()
                                          (_,Nothing) -> sendPush man (setMessage Cancel) [d] >> return ()
                                          (_,Just g)  -> do                                                              
                                                  sendPush man (setMessage $ Movement mov) user2
                                                  let newBoard = if gamesUser1 (entityVal g) == usr
                                                                  then newMovement mov 1 (gamesMatrix(entityVal g))
                                                                  else newMovement mov (-1) (gamesMatrix(entityVal g))
                                                  case checkWinner newBoard of
                                                    Nothing -> runDBAct pool $ update (entityKey (g)) [GamesMatrix =. newBoard]
                                                    Just x  -> do
                                                                 let msg = if x == 1 
                                                                             then Winner $ gamesUser1 (entityVal g)
                                                                             else Winner $ gamesUser2 (entityVal g)
                                                                 sendPush man (setMessage $ msg) (d:user2)
                                                                 runDBAct pool $ deleteBy $ UniqueUser1 usr
                                                                 runDBAct pool $ deleteBy $ UniqueUser2 usr
                                                  return ()

                      NewGame usr2 -> do--New Game
                                        res <- runDBAct pool $ getBy $ UniqueUser usr2
                                        case res of
                                          Nothing -> do
                                                       sendPush man (setMessage Cancel) [d]
                                                       return ()
                                          Just a  -> do
                                                       res <- isPlaying usr2
                                                       case res of
                                                         True  -> sendPush man (setMessage Cancel) [d] >> return ()
                                                         False -> do
                                                                    sendPush man (setMessage $ NewGame usr) 
                                                                                 [devicesIdentifier (entityVal a)]
                                                                    runDBAct pool $ insert $ Games usr usr2 emptyBoard
                                                                    return ()
                      _ -> return ()
   where
        getGame usr       = do
                              game1 <- runDBAct pool $ getBy $ UniqueUser1 usr
                              case game1 of
                                Just _ -> return game1
                                _      -> runDBAct pool $ getBy $ UniqueUser2 usr
        getOpponentId usr = do
                              game1 <- runDBAct pool $ getBy $ UniqueUser1 usr
                              game2 <- runDBAct pool $ getBy $ UniqueUser2 usr
                              case (game1,game2) of
                                (Just g , _) -> do
                                                  res <- runDBAct pool $ getBy $ UniqueUser (gamesUser2 (entityVal g))
                                                  case res of
                                                    Just a -> return [devicesIdentifier (entityVal a)]
                                                    _      -> return []
                                (_ , Just g) -> do
                                                  res <- runDBAct pool $ getBy $ UniqueUser (gamesUser1 (entityVal g))
                                                  case res of
                                                    Just a -> return [devicesIdentifier (entityVal a)]
                                                    _      -> return []
                                _            -> return []
        isPlaying usr     = do
                              game1 <- runDBAct pool $ getBy $ UniqueUser1 usr
                              game2 <- runDBAct pool $ getBy $ UniqueUser2 usr
                              case (game1,game2) of
                                (Nothing,Nothing) -> return False
                                _                 -> return True

        authenticate Nothing           = return Nothing
        authenticate (Just (usr,pass)) = do
                                           device <- runDBAct pool $ getBy $ UniqueUser usr
                                           case device of
                                             Nothing -> return Nothing
                                             Just a	 -> case devicesPassword (entityVal (a)) == pass of
                                                           False -> return Nothing
                                                           True  -> return $ Just usr

