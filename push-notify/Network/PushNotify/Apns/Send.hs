-- GSoC 2013 - Communicating with mobile devices.

{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
             QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

-- | This Module define the main function to send Push Notifications through Apple Push Notification Service,
-- and to communicate with the Feedback Service.
module Network.PushNotify.Apns.Send (sendAPNS,feedBackAPNS) where

import Network.PushNotify.Apns.Types
import Network.PushNotify.Apns.Constants

import Control.Concurrent
import Data.Certificate.X509            (X509)
import Data.Convertible                 (convert)
import Data.Default
import Data.Serialize
import Data.Text.Encoding               (encodeUtf8,decodeUtf8)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.Aeson.Encode      as AE
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as LB
import qualified Data.ByteString.Base16 as B16
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

-- | 'sendAPNS' sends the message through a APNS Server.
sendAPNS :: APNSAppConfig -> APNSmessage -> IO APNSresult
sendAPNS config msg = do
        ctime   <- getPOSIXTime
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
        var     <- newEmptyMVar

        -- To receive error response.
        tID1    <- forkIO $ do
                    dat <- recvData ctx
                    case runGet (getWord16be >> getWord32be) dat of -- | COMMAND and STATUS | ID |
                        Right ident -> putMVar var $ Just (convert ident)
                        Left _      -> putMVar var $ Just 0

        -- To send the notifications.
        tID2    <- forkIO $ loop var ctx 1 (createPut msg ctime) $ deviceTokens msg

        -- Wait for an error response or the end of the sending.
        v       <- takeMVar var
        killThread tID1
        killThread tID2
        bye ctx
        contextClose ctx
        case v of
            Just s  -> return $ APNSresult $ drop s $ deviceTokens msg -- An error occurred.
            Nothing -> return $ APNSresult [] -- Successful.

        where
            loop :: MVar (Maybe Int) 
                -> Context 
                -> Int 
                -> (DeviceToken -> Int -> Put)
                -> [DeviceToken]
                -> IO ()
            loop var _   _   _    []     = do
                                            threadDelay $ timeoutTime config
                                            putMVar var Nothing
            loop var ctx num cput (x:xs) = do
                                            sendData ctx $ LB.fromChunks [ (runPut $ cput x num) ]
                                            loop var ctx (num+1) cput xs

-- 'createPut' builds the binary block to be sent.
createPut :: APNSmessage -> NominalDiffTime -> DeviceToken -> Int -> Put
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
      else do -- |COMMAND|ID|EXPIRY|TOKENLEN|TOKEN|PAYLOADLEN|PAYLOAD|
            putWord8 1
            putWord32be $ convert identifier
            putWord32be expiryTime
            putWord16be $ convert $ B.length btoken
            putByteString btoken
            putWord16be $ convert $ LB.length bpayload
            putLazyByteString bpayload


-- | 'feedBackAPNS' connects with the Feedback service.
feedBackAPNS :: APNSAppConfig -> IO APNSFeedBackresult
feedBackAPNS config = do
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
        var     <- newEmptyMVar

        -- To receive.
        tID     <- forkIO $ loopReceive var ctx

        res     <- waitAndCheck var []
        killThread tID
        bye ctx
        contextClose ctx
        return res

        where
            getData :: Get (DeviceToken,UTCTime)
            getData = do -- |TIMESTAMP|TOKENLEN|TOKEN|
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
                        v <- timeout (timeoutTime config) $ takeMVar var
                        case v of
                            Nothing -> return $ APNSFeedBackresult list
                            Just t  -> waitAndCheck var (t:list)
