-- GSoC 2013 - Communicating with mobile devices.

{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
             QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

-- | This Module define the main function to send Push Notifications through Apple Push Notification Service.
module Network.PushNotify.Apns.Send (sendAPNS) where

import Network.PushNotify.Apns.Types
import Network.PushNotify.Apns.Constants

import Data.Convertible             (convert)
import Data.Default
import Data.Serialize
import Data.Text.Encoding           (encodeUtf8)
import Data.Text                    (unpack)
import Data.Time.Clock.POSIX
import Data.Time.Clock
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Aeson.Encode as AE
import Network.Socket.Internal      (PortNumber(PortNum))
import Network.TLS.Extra            (fileReadCertificate,fileReadPrivateKey,ciphersuite_all)
import Network.TLS
import Network
import qualified Crypto.Random.AESCtr as RNG
import Data.Certificate.X509        (X509)
import Control.Concurrent

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
        let env = environment config
        ctime   <- getPOSIXTime
        cert    <- fileReadCertificate $ certificate config
        key     <- fileReadPrivateKey $ privateKey config
        handle  <- case env of
                    Development -> connectTo cDEVELOPMENT_URL $ PortNumber $ fromInteger cDEVELOPMENT_PORT
                    Production  -> connectTo cPRODUCTION_URL $ PortNumber $ fromInteger cPRODUCTION_PORT
        rng     <- RNG.makeSystem
        ctx     <- contextNewOnHandle handle (connParams cert key) rng
        handshake ctx
        var     <- newEmptyMVar

        forkIO $ do -- To receive error response.
                    dat <- recvData ctx
                    case runGet (getWord16be >> getWord32be) dat of
                        Right ident -> putMVar var $ Just (convert ident)
                        Left _      -> return ()

        forkIO $ loop var ctx 1 (createPut def ctime) $ deviceTokens msg -- To send the notifications.

        v <- takeMVar var
        bye ctx
        case v of
            Just s  -> return $ APNSresult $ drop s $ deviceTokens msg
            Nothing -> return $ APNSresult []

loop :: MVar (Maybe Int) 
     -> Context 
     -> Int 
     -> (DeviceToken -> Int -> Put)
     -> [DeviceToken]
     -> IO ()
loop var ctx _ cput []       = do
                                    threadDelay (10000)
                                    putMVar var Nothing
loop var ctx num cput (x:xs) = do
                                    sendData ctx $ LB.fromChunks [ (runPut $ cput x num) ]
                                    loop var ctx (num+1) cput xs


createPut :: APNSmessage -> NominalDiffTime -> DeviceToken -> Int -> Put
createPut msg ctime dst identifier = do
   let
       btoken     = encodeUtf8 dst -- I have to check if encodeUtf8 is the appropiate function.
       bpayload   = AE.encode msg
       expiryTime = case expiry msg of
                      Nothing ->  round (ctime + posixDayLength) -- One day for default
                      Just t  ->  round (utcTimeToPOSIXSeconds t)
   if (LB.length bpayload > 256)
      then fail "Too long payload"
      else do
            putWord8 1
            putWord32be $ convert identifier
            putWord32be expiryTime
            putWord16be $ convert $ B.length btoken
            putByteString btoken
            putWord16be $ convert $ LB.length bpayload
            putLazyByteString bpayload
