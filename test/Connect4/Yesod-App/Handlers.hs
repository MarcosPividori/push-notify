{-# LANGUAGE OverloadedStrings #-}

module Handlers
    ( handleNewId
    , handleUnregistered
    , handleNewMessage
    , handleNewDevice
    , MsgFromDevice(..)
    , handleMessage
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
import Network.PushNotify.Gcm
import Network.PushNotify.General
import Connect4
import DataBase
import Extra

data MsgFromDevice = Cancel | Movement Int | NewGame Text | Winner Text deriving (Show,Read,Eq)

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
                      dev <- runDBAct pool $ getBy $ UniqueDevice $ Dev d
                      case dev of
                        Just x  -> do 
                                     runDBAct pool $ update (entityKey (x)) [DevicesUser =. usr , DevicesPassword =. pass]
                                     return SuccessfulReg
                        Nothing -> do
                                     runDBAct pool $ insert $ Devices usr pass $ Dev d
                                     return SuccessfulReg
         Just a	-> case devicesPassword (entityVal (a)) == pass of
                      True  -> do
                                 runDBAct pool $ update (entityKey (a)) [DevicesIdentifier =. Dev d]
                                 return SuccessfulReg
                      False -> return $ ErrorReg "Invalid Username or Password"

handleNewId :: Pool Connection -> (Device, Device) -> IO ()
handleNewId pool (old,new) = do
                               dev  <- runDBAct pool $ getBy $ UniqueDevice $ Dev old
                               case dev of
                                   Just x  ->  runDBAct pool $ update (entityKey (x)) [DevicesIdentifier =. Dev new ]
                                   Nothing ->  return ()

handleUnregistered :: Pool Connection -> Device -> IO ()
handleUnregistered pool d = runDBAct pool $ deleteBy $ UniqueDevice $ Dev d

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
                     Just m2 -> handleMessage pool man (Dev d) usr m2
   where
       authenticate Nothing           = return Nothing
       authenticate (Just (usr,pass)) = do
                                           device <- runDBAct pool $ getBy $ UniqueUser usr
                                           case device of
                                             Nothing -> return Nothing
                                             Just a	 -> case devicesPassword (entityVal (a)) == pass of
                                                           False -> return Nothing
                                                           True  -> return $ Just usr
                     
handleMessage :: Pool Connection -> PushManager -> Identifier -> Text -> MsgFromDevice -> IO ()
handleMessage pool man id1 user1 msg = do
                 case msg of
                      Cancel       -> do--Cancel
                                        mId2 <- getOpponentId user1
                                        case mId2 of
                                          Just (id2,_) -> sendMessage (setMessage Cancel) id2
                                          _            -> return ()
                                        deleteGame user1

                      Movement mov -> do--New Movement
                                        mId2 <- getOpponentId user1
                                        game <- getGame user1
                                        case (mId2,game) of
                                          (Nothing,_)       -> sendMessage (setMessage Cancel) id1
                                          (_,Nothing)       -> sendMessage (setMessage Cancel) id1
                                          (Just (id2,user2),Just g) -> do
                                              if gamesTurn (entityVal g) /= user1
                                                then return ()
                                                else do                                                    
                                                  sendMessage (setMessage $ Movement mov) id2
                                                  let newBoard = if gamesUser1 (entityVal g) == user1
                                                                  then newMovement mov 1 (gamesMatrix(entityVal g))
                                                                  else newMovement mov (-1) (gamesMatrix(entityVal g))
                                                  case checkWinner newBoard of
                                                    Nothing -> runDBAct pool $ update (entityKey (g)) [GamesMatrix =. newBoard, GamesTurn =. user2]
                                                    Just x  -> do
                                                                 let msg = if x == 1 
                                                                             then Winner $ gamesUser1 (entityVal g)
                                                                             else Winner $ gamesUser2 (entityVal g)
                                                                 sendMessage (setMessage $ msg) id1
                                                                 sendMessage (setMessage $ msg) id2
                                                                 deleteGame user1
                                                  return ()

                      NewGame user2 -> do--New Game
                                        res1 <- runDBAct pool $ getBy $ UniqueUser user2
                                        res2 <- isPlaying user2
                                        case (res1,res2) of
                                          (Just a,False) -> do
                                                       sendMessage (setMessage $ NewGame user1) $ devicesIdentifier (entityVal a)
                                                       runDBAct pool $ insert $ Games user1 user2 user2 emptyBoard
                                                       return ()
                                          (_,_) -> sendMessage (setMessage Cancel) id1
                      _ -> return ()
    where
        sendMessage msg id = case id of
                             Web _ -> return ()
                             Dev d -> sendPush man msg [d] >> return ()
        
        deleteGame usr    = do
                              runDBAct pool $ deleteBy $ UniqueUser1 usr
                              runDBAct pool $ deleteBy $ UniqueUser2 usr
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
                                                    Just a -> return $ Just (devicesIdentifier (entityVal a),devicesUser (entityVal a))
                                                    _      -> return Nothing
                                (_ , Just g) -> do
                                                  res <- runDBAct pool $ getBy $ UniqueUser (gamesUser1 (entityVal g))
                                                  case res of
                                                    Just a -> return $ Just (devicesIdentifier (entityVal a),devicesUser (entityVal a))
                                                    _      -> return Nothing
                                _            -> return Nothing
        isPlaying usr     = do
                              game1 <- runDBAct pool $ getBy $ UniqueUser1 usr
                              game2 <- runDBAct pool $ getBy $ UniqueUser2 usr
                              case (game1,game2) of
                                (Nothing,Nothing) -> return False
                                _                 -> return True

