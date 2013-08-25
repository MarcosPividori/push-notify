-- GSoC 2013 - Communicating with mobile devices.

{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
             QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

-- | This Module define the main function to send Push Notifications through Apple Push Notification Service,
-- and to communicate with the Feedback Service.
module Network.PushNotify.Apns.Send
    ( sendAPNS
    , startAPNS
    , closeAPNS
    , feedBackAPNS
    ) where

import Network.PushNotify.Apns.Types
import Network.PushNotify.Apns.Constants

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TChan
import Control.Monad.STM
import Data.Certificate.X509            (X509)
import Data.Convertible                 (convert)
import Data.Default
import Data.Int
import Data.IORef
import Data.Serialize
import Data.Text.Encoding               (encodeUtf8,decodeUtf8)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.Aeson.Encode      as AE
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as LB
import qualified Data.ByteString.Base16 as B16
import qualified Control.Exception      as CE
import qualified Crypto.Random.AESCtr   as RNG
import Network
import Network.Socket.Internal          (PortNumber(PortNum))
import Network.TLS
import Network.TLS.Extra                (fileReadCertificate,fileReadPrivateKey,ciphersuite_all)
import System.Timeout


connParams :: X509 -> PrivateKey -> Params
connParams cert privateKey = defaultParamsClient{ 
                     pConnectVersion    = TLS11
                   , pAllowedVersions   = [TLS10,TLS11,TLS12]
                   , pCiphers           = ciphersuite_all
                   , pCertificates      = [(cert , Just privateKey)]
                   , onCertificatesRecv = const $ return CertificateUsageAccept
                   , roleParams         = Client $ ClientParams{
                            clientWantSessionResume    = Nothing
                        ,   clientUseMaxFragmentLength = Nothing
                        ,   clientUseServerName        = Nothing
                        ,   onCertificateRequest       = \x -> return [(cert , Just privateKey)]
                        }
                   }

-- 'connectAPNS' starts a secure connection with APNS servers.
connectAPNS :: APNSAppConfig -> IO Context
connectAPNS config = do
        cert    <- fileReadCertificate $ certificate config
        key     <- fileReadPrivateKey $ privateKey config
        handle  <- case environment config of
                    Development -> connectTo cDEVELOPMENT_URL
                                           $ PortNumber $ fromInteger cDEVELOPMENT_PORT
                    Production  -> connectTo cPRODUCTION_URL
                                           $ PortNumber $ fromInteger cPRODUCTION_PORT
        rng     <- RNG.makeSystem
        ctx     <- contextNewOnHandle handle (connParams cert key) rng
        handshake ctx
        return ctx

-- | 'startAPNS' starts the APNS service.
startAPNS :: APNSAppConfig -> IO APNSManager
startAPNS config = do
        c       <- newTChanIO
        ref     <- newIORef $ Just ()
        tID     <- forkIO $ CE.catch (apnsWorker config c) (\e -> let _ = (e :: CE.SomeException) 
                                                                  in atomicModifyIORef ref (\_ -> (Nothing,())))
        return $ APNSManager ref c tID $ timeoutLimit config

-- | 'closeAPNS' stops the APNS service.
closeAPNS :: APNSManager -> IO ()
closeAPNS m = do
                atomicModifyIORef (mState m) (\_ -> (Nothing,()))
                killThread $ mWorkerID m

-- | 'sendAPNS' sends the message through a APNS Server.
sendAPNS :: APNSManager -> APNSmessage -> IO APNSresult
sendAPNS m msg = do
    s <- readIORef $ mState m
    case s of
      Nothing -> fail "APNS Service closed."
      Just () -> do     
        let requestChan = mApnsChannel m
        var1 <- newEmptyMVar

        atomically $ writeTChan requestChan (var1,msg)
        Just (errorChan,startNum) <- takeMVar var1 -- waits until the request is attended by the worker thread.
        -- errorChan -> is the channel where I will receive an error message if the sending fails.
        -- startNum  -> My messages will be identified from startNum to (startNum + num of messages-1)
        
        v    <- race  (readChan errorChan) (takeMVar var1 >> (threadDelay $ mTimeoutLimit m))

        let (success,fail)    = case v of
                Left s  -> if s >= startNum -- an error response, s identifies the last notification that was successfully sent.
                        then splitAt (s+1-startNum) $ deviceTokens msg -- An error occurred.
                        else ([],deviceTokens msg) -- An old error occurred, so nothing was sent.
                Right _ -> (deviceTokens msg,[]) -- Successful.
        return $ APNSresult success fail

apnsWorker :: APNSAppConfig -> TChan ( MVar (Maybe (Chan Int,Int)) , APNSmessage) -> IO ()
apnsWorker config requestChan = do
        ctx        <- connectAPNS config
        errorChan  <- newChan -- new Error Channel.
        lock       <- newMVar ()
        
        s          <- async (catch $ sender 1 lock requestChan errorChan ctx)
        r          <- async (catch $ receiver ctx)
        res        <- waitEither s r

        case res of
            Left  _ -> do
                            cancel r
                            writeChan errorChan 0
            Right v -> do
                            takeMVar lock
                            cancel s
                            writeChan errorChan v -- v is an int representing: 
                                        -- 0 -> internal worker error.
                                        -- n -> the identifier received in an error msg.
                                        --      This represent, the last message that was sent successfully.
        CE.catch (contextClose ctx) (\e -> let _ = (e :: CE.SomeException) in return ())
        apnsWorker config requestChan -- restarts.

        where
            catch :: IO Int -> IO Int
            catch m = CE.catch m (\e -> do
                            let _ = (e :: CE.SomeException)
                            return 0)
            
            sender  :: Int32
                    -> MVar ()
                    -> TChan (MVar (Maybe (Chan Int,Int)) , APNSmessage)
                    -> Chan Int
                    -> Context
                    -> IO Int
            sender n lock requestChan errorChan c = do -- this function reads the channel and sends the messages.
                                
                    atomically $ peekTChan requestChan
                    -- Now there is at least one element in the channel, so the next readTChan won't block.
                    takeMVar lock

                    (var,msg)   <- atomically $ readTChan requestChan

                    let len = convert $ length $ deviceTokens msg     -- len is the number of messages it will send.
                        num = if (n + len :: Int32) < 0 then 1 else n -- to avoid overflow.
                    echan       <- dupChan errorChan
                    putMVar var $ Just (echan,convert num) -- Here, notifies that it is attending this request, 
                                                           -- and provides a duplicated error channel.
                    putMVar lock ()

                    ctime       <- getPOSIXTime
                    loop var c num (createPut msg ctime) $ deviceTokens msg -- sends the messages.
                    sender (num+len) lock requestChan errorChan c

            receiver :: Context -> IO Int
            receiver c = do
                    dat <- recvData c
                    case runGet (getWord16be >> getWord32be) dat of -- COMMAND and STATUS | ID |
                        Right ident -> return (convert ident)
                        Left _      -> return 0

            loop :: MVar (Maybe (Chan Int,Int)) 
                -> Context 
                -> Int32 -- This number is the identifier of this message, so if the sending fails,
                         -- I will receive this identifier in an error message.
                -> (DeviceToken -> Int32 -> Put)
                -> [DeviceToken]
                -> IO Bool
            loop var _   _   _    []     = tryPutMVar var Nothing
            loop var ctx num cput (x:xs) = do
                    sendData ctx $ LB.fromChunks [ (runPut $ cput x num) ]
                    loop var ctx (num+1) cput xs


-- 'createPut' builds the binary block to be sent.
createPut :: APNSmessage -> NominalDiffTime -> DeviceToken -> Int32 -> Put
createPut msg ctime dst identifier = do
   let
       -- We convert the text to binary, and then decode the hexadecimal representation.
       btoken     = fst $ B16.decode $ encodeUtf8 dst 
       bpayload   = AE.encode msg
       expiryTime = case expiry msg of
                      Nothing ->  round (ctime + posixDayLength) -- One day for default.
                      Just t  ->  round (utcTimeToPOSIXSeconds t)
   if (LB.length bpayload > 256)
      then fail "Too long payload"
      else do -- COMMAND|ID|EXPIRY|TOKENLEN|TOKEN|PAYLOADLEN|PAYLOAD|
            putWord8 1
            putWord32be $ convert identifier
            putWord32be expiryTime
            putWord16be $ convert $ B.length btoken
            putByteString btoken
            putWord16be $ convert $ LB.length bpayload
            putLazyByteString bpayload



-- 'connectFeedBackAPNS' starts a secure connection with Feedback service.
connectFeedBackAPNS :: APNSAppConfig -> IO Context
connectFeedBackAPNS config = do
        cert    <- fileReadCertificate $ certificate config
        key     <- fileReadPrivateKey $ privateKey config
        handle  <- case environment config of
                    Development -> connectTo cDEVELOPMENT_FEEDBACK_URL
                                           $ PortNumber $ fromInteger cDEVELOPMENT_FEEDBACK_PORT
                    Production  -> connectTo cPRODUCTION_FEEDBACK_URL
                                           $ PortNumber $ fromInteger cPRODUCTION_FEEDBACK_PORT
        rng     <- RNG.makeSystem
        ctx     <- contextNewOnHandle handle (connParams cert key) rng
        handshake ctx
        return ctx

-- | 'feedBackAPNS' connects with the Feedback service.
feedBackAPNS :: APNSAppConfig -> IO APNSFeedBackresult
feedBackAPNS config = do
        ctx     <- connectFeedBackAPNS config
        var     <- newEmptyMVar

        tID     <- forkIO $ loopReceive var ctx -- To receive.

        res     <- waitAndCheck var []
        killThread tID
        bye ctx
        contextClose ctx
        return res

        where
            getData :: Get (DeviceToken,UTCTime)
            getData = do -- TIMESTAMP|TOKENLEN|TOKEN|
                        time    <- getWord32be
                        length  <- getWord16be
                        dtoken  <- getBytes $ convert length
                        return (    decodeUtf8 $ B16.encode dtoken
                               ,    posixSecondsToUTCTime $ fromInteger $ convert time )

            loopReceive :: MVar (DeviceToken,UTCTime) -> Context -> IO ()
            loopReceive var ctx = do
                        dat <- recvData ctx
                        case runGet getData dat of
                            Right tuple -> do
                                                putMVar var tuple
                                                loopReceive var ctx
                            Left _      -> return ()

            waitAndCheck :: MVar (DeviceToken,UTCTime) -> [(DeviceToken,UTCTime)] -> IO APNSFeedBackresult
            waitAndCheck var list = do
                        v <- timeout (timeoutLimit config) $ takeMVar var
                        case v of
                            Nothing -> return $ APNSFeedBackresult list
                            Just t  -> waitAndCheck var (t:list)
