-- GSoC 2013 - Communicating with mobile devices.

{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
             QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

-- | This Module define the main function to send Push Notifications through Cloud Connection Server (GCM).
module Send
    ( startCCS
    , closeCCS
    , sendCCS
    ) where

import Constants
import Types
import Network.PushNotify.Gcm.Types

import Data.XML.Types
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Parser
import Data.Default
import Data.Functor
import Data.IORef
import Data.Int
import Data.Text
import Data.Text.Encoding
import Data.Monoid                          ((<>))
import qualified Data.Attoparsec.ByteString as AB
import qualified Control.Exception          as CE
import qualified Data.HashMap.Strict        as HM
import Network
import Network.Xmpp
import System.Log.Logger

-- 'connectCCS' starts a secure connection with CCS servers.
connectCCS :: GCMAppConfig -> IO Session
connectCCS config = do
    updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG
    result <- session
                (unpack cCCS_URL)
                (Just ( \_ -> [plain (senderID config <> "@" <> cCCS_URL) Nothing (apiKey config) ] , Nothing))
                def{ sessionStreamConfiguration = def{ 
                         connectionDetails = UseHost (unpack cCCS_URL) (PortNumber (fromIntegral cCCS_PORT))}
                   }
    case result of
        Right s -> return s
        Left e  -> error $ "XmppFailure: " ++ (show e)


-- | 'startCCS' starts the CCS service.
startCCS :: GCMAppConfig -> (RegId -> Value -> IO ()) -> IO CCSManager
startCCS config newMessageCallbackFunction = do
        c      <- newChan
        ref    <- newIORef $ Just ()
        tID    <- forkIO $ CE.catch (ccsWorker config c newMessageCallbackFunction) (\e -> let _ = (e :: CE.SomeException) 
                                                                  in atomicModifyIORef ref (\_ -> (Nothing,())))
        return $ CCSManager ref c tID 0


ccsWorker :: GCMAppConfig -> Chan (MVar GCMresult , GCMmessage) -> (RegId -> Value -> IO ()) -> IO ()
ccsWorker config requestChan callBackF = do
        sess   <- connectCCS config
        cont   <- newMVar 1000
        hmap   <- newMVar HM.empty
        lock   <- newEmptyMVar
        s      <- async (sender 1 cont lock hmap requestChan sess)
        r      <- async (receiver cont lock hmap sess)
        res    <- waitEitherCatchCancel s r
        newMap <- takeMVar hmap
        _      <- return $ HM.foldr (\m r -> r >> (tryPutMVar (fst m) def{failure = Just 1 , errorToReSend = [snd m]})) (return False) newMap
        ccsWorker config requestChan callBackF -- restarts.

        where
            buildMessage :: Value -> Message
            buildMessage value = Message{
                                    messageID      = Nothing
                                  , messageFrom    = Nothing
                                  , messageTo      = Nothing
                                  , messageLangTag = Nothing
                                  , messageType    = Normal
                                  , messagePayload = [ Element (Name "gcm" (Just "google:mobile:data") Nothing) [] 
                                                       [(NodeContent $ ContentText $ pack $ show value)]
                                                     ]
                                  }
            
            buildAck :: Text -> Text -> Message
            buildAck regId id = buildMessage $ object [cTo .= regId , cMessageId .= id , cMessageType .= cAck]

            sender  :: Int32
                    -> MVar Int
                    -> MVar ()
                    -> MVar (HM.HashMap Text (MVar GCMresult,RegId))
                    -> Chan (MVar GCMresult , GCMmessage)
                    -> Session
                    -> IO Int
            sender n cont lock hmap requestChan sess = do -- this function reads the channel and sends the messages.

                    c <- takeMVar cont
                    putMVar cont (c-1)
                    if (c-1) == 0
                      then takeMVar lock -- blocks
                      else return ()

                    (var,msg)   <- readChan requestChan

                    let id = pack $ show n
                        value   = fromGCMtoCCS (Prelude.head $ registration_ids msg) id msg

                    hashMap <- takeMVar hmap
                    let newMap = HM.insert id (var,(Prelude.head $ registration_ids msg)) hashMap
                    putMVar hmap newMap
                    
                    sendMessage (buildMessage value) sess

                    sender (n+1) cont lock hmap requestChan sess


            controlPars :: Value -> Parser (Text,Text,Text,Maybe Text)
            controlPars (Object v) = (,,,) <$>
                              v .: cMessageId <*>
                              v .: cFrom <*>
                              v .: cMessageType <*>
                              v .:? cError
            controlPars _          = mzero
            
            msgPars :: Value -> Parser (Value,Text,Text)
            msgPars (Object v) = (,,) <$>
                              v .: cData <*>
                              v .: cMessageId <*>
                              v .: cFrom
            msgPars _          = mzero


            receiver :: MVar Int
                     -> MVar ()
                     -> MVar (HM.HashMap Text (MVar GCMresult,RegId))
                     -> Session
                     -> IO Int
            receiver cont lock hmap sess = do
                    msg <- getMessage sess

                    let [Element _ _ ([NodeContent (ContentText p)])] = messagePayload msg
                        value = case AB.maybeResult $ AB.parse json $ encodeUtf8 p of
                                    Nothing -> object []
                                    Just v  -> v

                    case parseMaybe controlPars value of
                        Nothing         -> case parseMaybe msgPars value of
                                              Nothing       -> return () -- no expected msg
                                              Just (v,id,f) -> do -- it is a message from device so I send the Ack response to CCS server
                                                                  -- and start the callback function.
                                                                 sendMessage (buildAck f id) sess
                                                                 forkIO $ callBackF f v
                                                                 return ()
                        Just (id,f,t,e) -> do -- it is an ACK/NACK message so I look for the MVar of this message 
                                              -- in the hashmap and I put the response in the MVar.
                                              c <- takeMVar cont
                                              if c == 0
                                                then putMVar lock () -- unblock the sender thread
                                                else return ()
                                              putMVar cont (c+1)

                                              hashMap <- readMVar hmap
                                              case HM.lookup id hashMap of
                                                Just (var,regId) -> do
                                                  let result = case t of
                                                        cAck -> def{success = Just 1}
                                                        _    -> (case e of
                                                          Just cBadRegistration     -> def{errorRest = [(regId,cBadRegistration)]}
                                                          Just cDeviceUnregistered  -> def{errorUnRegistered = [regId]}
                                                          Just cInternalServerError -> def{errorRest = [(regId,cInternalServerError)]}
                                                          Just cServiceUnAvailable  -> def{errorToReSend = [regId]}
                                                          _                         -> def{errorToReSend = [regId]} -- no expected msg
                                                          ){failure = Just 1}
                                                  putMVar var result
                                                Nothing -> return ()
                                              hashMap' <- takeMVar hmap
                                              let newMap = HM.delete id hashMap'
                                              putMVar hmap newMap

                    receiver cont lock hmap sess


-- | 'closeCCS' stops the CCS service.
closeCCS :: CCSManager -> IO ()
closeCCS m = do
               atomicModifyIORef (mState m) (\_ -> (Nothing,()))
               killThread $ mWorkerID m


-- | 'sendCCS' sends the message through a CCS Server.
sendCCS :: CCSManager -> GCMmessage -> IO GCMresult
sendCCS man msg = do
    s <- readIORef $ mState man
    case s of
      Nothing -> fail "CCS Service closed."
      Just () -> do
          let requestChan = mCcsChannel man
          var <- newEmptyMVar
          writeChan requestChan (var,msg)
          takeMVar var

