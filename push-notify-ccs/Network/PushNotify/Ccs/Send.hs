-- GSoC 2013 - Communicating with mobile devices.

{-# LANGUAGE OverloadedStrings , ScopedTypeVariables #-}

-- | This Module define the main function to send Push Notifications through Cloud Connection Server (GCM).
module Network.PushNotify.Ccs.Send
    ( startCCS
    , closeCCS
    , sendCCS
    , withCCS
    , withCCS'
    ) where

import Network.PushNotify.Ccs.Constants
import Network.PushNotify.Ccs.Types
import Network.PushNotify.Gcm.Types

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent.STM.TChan
import Control.Monad.Error
import Control.Monad.STM
import Control.Monad
import Control.Retry
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Parser
import Data.Default
import Data.Functor
import Data.IORef
import Data.Int
import Data.List
import Data.Text
import Data.Text.Encoding
import Data.Monoid                          ((<>))
import Data.XML.Types
import qualified Data.Attoparsec.ByteString as AB
import qualified Control.Exception          as CE
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString            as BS
import qualified Data.Text.Encoding         as E
import Network
import Network.Xmpp
import Network.Xmpp.Internal
import Network.TLS
import Network.TLS.Extra
import Crypto.Random.API
import qualified Crypto.Random.AESCtr
import GHC.IO.Handle
import System.Log.Logger

-- 'connectCCS' starts a secure connection with CCS servers.
connectCCS :: GCMCcsConfig -> IO Session
connectCCS config = do
    let getStreamHandle = lift $ do
            hdl <- connectTo (unpack cCCS_URL) (PortNumber (fromIntegral cCCS_PORT))
            let bck = Backend { backendFlush = hFlush hdl
                              , backendClose = hClose hdl
                              , backendSend  = BS.hPut hdl
                              , backendRecv  = BS.hGet hdl
                              }
            gen <- Crypto.Random.AESCtr.makeSystem
            ctx <- contextNew
                       bck
                       (defaultParamsClient { pCiphers = ciphersuite_medium })
                       gen
            handshake ctx
            return StreamHandle { streamSend    = \bs -> CE.catch
                                                    (sendData ctx (BL.fromChunks [bs]) >> return (Right ()))
                                                    (\(_e :: CE.SomeException) -> return $ Left XmppOtherFailure)
                                , streamReceive = \bs -> CE.catch
                                                    (recvData ctx >>= return . Right )
                                                    (\(_e :: CE.SomeException) -> return $ Left XmppOtherFailure)
                                , streamFlush   = contextFlush ctx
                                , streamClose   = bye ctx }
    result <- session
                (unpack cCCS_URL)
                (Just ( \_ -> [plain (senderID config <> "@" <> cCCS_URL) Nothing (aPiKey config) ] , Nothing))
                def{ sessionStreamConfiguration = def{
                         connectionDetails = UseConnection getStreamHandle }
                   }
    case result of
        Right s -> return s
        Left e  -> fail $ "XmppFailure: " ++ (show e)

-- | 'startCCS' starts the CCS service, which means starting a worker thread which maintains a connection with CCS servers.
startCCS :: GCMCcsConfig                 -- ^ The main configuration for the GCM service.
         -> (RegId -> Value -> IO ())    -- ^ A callback function to be called each time a message arrives from a device.
         -> IO CCSManager
startCCS config newMessageCallbackFunction = do
        c      <- newTChanIO
        ref    <- newIORef $ Just ()
        tID    <- forkIO $ CE.catch (ccsWorker config c newMessageCallbackFunction)
                                    (\(e :: CE.SomeException) -> atomicModifyIORef ref (\_ -> (Nothing,())) )
        return $ CCSManager ref c tID

-- Main worker thread.
ccsWorker :: GCMCcsConfig -> TChan (Chan GCMresult , MVar (Chan ()), GCMmessage) -> (RegId -> Value -> IO ()) -> IO ()
ccsWorker config requestChan callBackF = do
        sess      <- recoverAll (ccsRetrySettings config) $ connectCCS config
        cont      <- newIORef 1000
        hmap      <- newIORef HM.empty
        lock      <- newEmptyMVar
        locki     <- newMVar ()
        errorChan <- newChan -- new Error Channel.
        s         <- async (sender 1 cont lock locki hmap requestChan errorChan sess)
        r         <- async (receiver cont lock hmap sess)
        res       <- waitEither s r
        case res of
            Left  _ -> do
                         cancel r
                         writeChan errorChan ()
            Right v -> do
                         takeMVar locki
                         cancel s
                         writeChan errorChan ()
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
                                                       [(NodeContent $ ContentText $ E.decodeUtf8 $
                                                                       BS.concat . BL.toChunks $ encode value)]
                                                     ]
                                  }

            buildAck :: Text -> Text -> Message
            buildAck regId id = buildMessage $ object [cTo .= regId , cMessageId .= id , cMessageType .= cAck]

            sender  :: Int32
                    -> IORef Int
                    -> MVar ()
                    -> MVar ()
                    -> IORef (HM.HashMap Text (Chan GCMresult,RegId))
                    -> TChan (Chan GCMresult , MVar (Chan ()), GCMmessage)
                    -> Chan ()
                    -> Session
                    -> IO Int
            sender n cont lock locki hmap requestChan errorChan sess = do -- this function reads the channel and sends the messages.

                    atomically $ peekTChan requestChan
                    -- Now there is at least one element in the channel, so the next readTChan won't block.
                    takeMVar locki

                    (chanRes,varErr,msg) <- atomically $ readTChan requestChan
                    echan <- dupChan errorChan
                    putMVar varErr echan -- Here, notifies that it is attending this request,
                                         -- and provides a duplicated error channel.
                    putMVar locki ()

                    m <- loopSend (HS.toList $ registration_ids msg) msg chanRes sess hmap cont lock n

                    sender m cont lock locki hmap requestChan errorChan sess

            loopSend []     _   _       _    _    _    _    n = return n
            loopSend (x:xs) msg chanRes sess hmap cont lock n = do
                    checkCounter cont lock

                    let id     = pack $ show n
                        value  = fromGCMtoCCS x id msg

                    atomicModifyIORef hmap (\hashMap -> (HM.insert id (chanRes,x) hashMap,()))

                    sendMessage (buildMessage value) sess

                    loopSend xs msg chanRes sess hmap cont lock (n+1)

            checkCounter :: IORef Int -> MVar () -> IO ()
            checkCounter cont lock = do
                    newC <- atomicModifyIORef cont (\c -> (c-1,c-1))
                    if newC == 0
                      then do
                             race (threadDelay 5000000 >> fail "Timeout") (takeMVar lock) -- blocks
                             return ()
                      else return ()

            controlPars :: Value -> Parser (Text,Text,Text,Maybe Text)
            controlPars (Object v) = (,,,) <$>
                         v .: cMessageId   <*>
                         v .: cFrom        <*>
                         v .: cMessageType <*>
                         v .:? cError
            controlPars _          = mzero

            msgPars :: Value -> Parser (Value,Text,Text)
            msgPars (Object v) = (,,) <$>
                      v .: cData      <*>
                      v .: cMessageId <*>
                      v .: cFrom
            msgPars _          = mzero

            receiver :: IORef Int
                     -> MVar ()
                     -> IORef (HM.HashMap Text (Chan GCMresult,RegId))
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
                                             Nothing       -> return () -- No expected msg.
                                             Just (v,id,f) -> do -- This is a message from device so I send the Ack response to CCS server
                                                                 -- and start the callback function.
                                                                sendMessage (buildAck f id) sess
                                                                forkIO $ callBackF f v
                                                                return ()
                        Just (id,f,t,e) -> do -- This is an ACK/NACK message so I look for the entry of this message
                                              -- in the hashmap and I put the response in the proper channel.

                                             oldC <- atomicModifyIORef cont (\c -> (c+1,c))

                                             if oldC == 0
                                               then putMVar lock () -- unblock the sender thread
                                               else return ()

                                             hashMap <- readIORef hmap

                                             case HM.lookup id hashMap of
                                               Just (chanRes,regId) -> do
                                                 let result = getRes t e regId
                                                 writeChan chanRes result
                                               Nothing -> return ()

                                             atomicModifyIORef hmap (\hashMap -> (HM.delete id hashMap,()))

                    receiver cont lock hmap sess
                     where
                      getRes t e regId
                       | t == cAck                      = def{success = Just 1}
                       | e == Just cBadRegistration     = def{failure = Just 1 , errorRest         = HM.singleton regId cBadRegistration}
                       | e == Just cDeviceUnregistered  = def{failure = Just 1 , errorUnRegistered = HS.singleton regId }
                       | e == Just cInternalServerError = def{failure = Just 1 , errorRest         = HM.singleton regId cInternalServerError }
                       | e == Just cServiceUnAvailable  = def{failure = Just 1 , errorToReSend     = HS.singleton regId }
                       | otherwise                      = def{failure = Just 1 , errorToReSend     = HS.singleton regId } -- no expected msg


-- | 'closeCCS' stops the CCS service.
--
-- This means stopping the worker thread which maintains a connection with CCS servers.
closeCCS :: CCSManager -> IO ()
closeCCS m = do
               atomicModifyIORef (mState m) (\_ -> (Nothing,()))
               killThread $ mWorkerID m

-- | 'sendCCS' sends messages to a CCS Server.
--
-- Every time you call this function, it will put the notification in a channel waiting to be proceesed by the worker thread.
--
-- It will block until the worker thread receives a response from CCS server.
sendCCS :: CCSManager -> GCMmessage -> IO GCMresult
sendCCS man msg = do
    s <- readIORef $ mState man
    case s of
      Nothing -> fail "CCS Service closed."
      Just () -> do
          let requestChan = mCcsChannel man
          chanRes   <- newChan
          varErr    <- newEmptyMVar
          atomically $ writeTChan requestChan (chanRes,varErr,msg)
          errorChan <- takeMVar varErr
          v         <- race (readChan errorChan) (loopResponse chanRes)
          case v of
              Left _  -> return def{ failure = Just (HS.size $ registration_ids msg)
                                   , errorToReSend = (registration_ids msg)} -- Error while sending.
              Right r -> return r -- Successful.
           where
             loopResponse chan = Data.List.foldr (\_ m -> do
                                                           r   <- readChan chan
                                                           res <- m
                                                           return $ r <> res)
                                                (return def)
                                                (HS.toList $ registration_ids msg)

-- | 'withCCS' creates a new manager, uses it in the provided function, and then releases it.
--
-- (The second argument is a callback function to be called each time a message arrives from a device).
withCCS :: GCMCcsConfig -> (RegId -> Value -> IO ()) -> (CCSManager -> IO a) -> IO a
withCCS confg callback fun = CE.bracket (startCCS confg callback) closeCCS fun

-- | 'withCCS'' creates a new manager, uses it in the provided function, and then releases it 
-- (ignores messages that arrive from a device).
withCCS' :: GCMCcsConfig -> (CCSManager -> IO a) -> IO a
withCCS' confg fun = withCCS confg (\_ _ -> return ()) fun
