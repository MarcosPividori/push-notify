-- GSoC 2013 - Communicating with mobile devices.

{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
             QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

-- | This Module define the main function to send Push Notifications through CCS.
module Send
    ( startCCS
    , closeCCS
    , sendCCS
    ) where

import Constants
import Types
import Network.PushNotify.Gcm.Types

import Data.XML.Types
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TChan
import Control.Monad.STM
import Data.Default
import Data.IORef
import Data.Monoid                    ((<>))
import Data.Text
import Data.Int
import qualified Control.Exception      as CE
import qualified Data.HashMap.Strict    as HM
import Network
import Network.Xmpp
import Network.BSD
import Network.Socket

--import Data.Attoparsec.ByteString
import Data.Aeson.Parser
--import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Aeson

-- 'connectCCS' starts a secure connection with CCS servers.
connectCCS :: GCMAppConfig -> IO Session
connectCCS config = do
    he    <- getHostByName $ unpack cCCS_URL
    pro   <- (getProtocolNumber "tcp")
    soc   <- socket AF_INET Stream pro

    result <- session
                  "gcm.googleapis.com"
                  def{sessionStreamConfiguration = def{ socketDetails = Just (soc , SockAddrInet (fromIntegral 5235) (hostAddress he) ) } }
                  (Just (( [plain (senderID config <> "@gcm.googleapis.com") Nothing (apiKey config) ]) , Nothing))
    case result of
        Right s -> return s
        Left e  -> error $ "XmppFailure: " ++ (show e)


-- | 'startCCS' starts the CCS service.
startCCS :: GCMAppConfig -> IO CCSManager
startCCS config = do
        c       <- newTChanIO
        ref     <- newIORef $ Just ()
        tID     <- forkIO $ CE.catch (ccsWorker config c) (\e -> let _ = (e :: CE.SomeException) 
                                                                  in atomicModifyIORef ref (\_ -> (Nothing,())))
        return $ CCSManager ref c tID 0

ccsWorker :: GCMAppConfig -> TChan (MVar Text , GCMmessage) -> IO ()
ccsWorker config requestChan = do
        sess       <- connectCCS config
        cont       <- newMVar 1000
        hmap       <- newMVar HM.empty
        lock       <- newEmptyMVar
        s          <- async (catch $ sender 1 cont lock hmap requestChan sess)
        r          <- async (catch $ receiver cont lock hmap sess)
        res        <- waitEither s r

        case res of
            Left  _ -> cancel r
            Right v -> cancel s

        ccsWorker config requestChan -- restarts.

        where
            catch :: IO Int -> IO Int
            catch m = CE.catch m (\e -> do
                            let _ = (e :: CE.SomeException)
                            return 0)
            
            sender  :: Int32
                    -> MVar Int
                    -> MVar ()
                    -> MVar (HM.HashMap Int32 (MVar Text))
                    -> TChan (MVar Text , GCMmessage)
                    -> Session
                    -> IO Int
            sender n cont lock hmap requestChan sess = do -- this function reads the channel and sends the messages.
                    
                    c <- readMVar cont
                    putMVar cont (c-1)
                    if (c-1) == 0
                      then takeMVar lock -- blocks
                      else return ()

                    (var,msg)   <- atomically $ readTChan requestChan

                    let value   = fromGCMtoCCS (Prelude.head $ registration_ids msg) n msg
                        message = Message{
                                    messageID      = Nothing
                                  , messageFrom    = Nothing
                                  , messageTo      = Nothing
                                  , messageLangTag = Nothing
                                  , messageType    = Normal
                                  , messagePayload = [Element (Name "gcm" (Just "google:mobile:data") Nothing) [] [(NodeContent $ ContentText $ pack $ show value)] ]
                                  }
                    
                    sendMessage message sess
                    
                    hashMap <- takeMVar hmap
                    let newMap = HM.insert n var hashMap
                    putMVar hmap newMap

                    sender (n+1) cont lock hmap requestChan sess

            receiver :: MVar Int
                     -> MVar()
                     -> MVar (HM.HashMap Int32 (MVar Text))
                     -> Session
                     -> IO Int
            receiver cont lock hmap sess = do
                         msg <- getMessage sess

                         --  si es ACK/NACK -> busco en el hashmap la MVar y le respondo con la respuesta.
                         let [Element _ _ ([NodeContent (ContentText p)])] = messagePayload msg
                             value = case maybeResult $ parse json $ show p of
                                         Nothing -> empty
                                         Just t  -> t
                         cMessageType
                         c <- takeMVar cont
                         if c == 0
                          then putMVar lock () -- unblock the sender thread
                          else return ()
                         putMVar cont (c+1)
                         
                         let id = undefined
                         
                         hashMap <- takeMVar hmap
                         case HM.lookup id hashMap of
                           Just m  -> putMVar m "Resultado"
                           Nothing -> return ()
                         let newMap = HM.delete id hashMap
                         putMVar hmap newMap

                         --  si es MSG      -> Respondo Ack al servidor y disparo la callback function.

                         receiver cont lock hmap sess

-- | 'closeCCS' stops the CCS service.
closeCCS :: CCSManager -> IO ()
closeCCS m = do
                atomicModifyIORef (mState m) (\_ -> (Nothing,()))
                killThread $ mWorkerID m

-- | 'sendCCS' sends the message through a CCS Server.
sendCCS :: CCSManager -> GCMmessage -> IO Text
sendCCS man msg = do
    s <- readIORef $ mState man
    case s of
      Nothing -> fail "CCS Service closed."
      Just () -> do
          let requestChan = mCcsChannel man
          var <- newEmptyMVar
          atomically $ writeTChan requestChan (var,msg)
          takeMVar var

