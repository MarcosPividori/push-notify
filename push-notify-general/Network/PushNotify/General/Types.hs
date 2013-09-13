{-# LANGUAGE OverloadedStrings #-}
-- | This Module defines the main data types for the Push Service.
module Network.PushNotify.General.Types
    ( -- * Push Settings     
      Device(..)
    , PushServiceConfig(..)
    , RegisterResult(..)
    , GCMConfig(..)
    , PushConfig(..)
      -- * Push Manager
    , PushManager(..)
      -- * Push Message
    , PushNotification(..)
    , generalNotif
      -- * Push Result
    , PushResult(..)
    , IsPushResult(..)
    ) where

import Network.PushNotify.Gcm
import Network.PushNotify.Apns
import Network.PushNotify.Mpns
import Network.PushNotify.Ccs
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Control.Exception

import qualified Data.Map               as M
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString        as BS
import qualified Data.Text.Encoding     as E
import Data.Aeson
import Data.Default
import Data.List
import Data.Monoid
import Data.Text     (Text,pack)
import Text.XML

-- | Unique identifier of an app/device.
data Device = GCM  RegId        -- ^ An Android app.
            | APNS DeviceToken  -- ^ An iOS app.
            | MPNS DeviceURI    -- ^ A WPhone app.
            deriving (Show, Read, Eq)

-- | General notification to be sent.
data PushNotification = PushNotification {
        apnsNotif :: Maybe APNSmessage
    ,   gcmNotif  :: Maybe GCMmessage
    ,   mpnsNotif :: Maybe MPNSmessage
    } deriving Show

instance Default PushNotification where
    def = PushNotification Nothing Nothing Nothing

-- | 'generalNotif' Builds a general notification from JSON data.
--
-- If data length exceeds 256 bytes (max payload limit for APNS) it will fails.
--
-- For MPNS, data will be XML-labeled as \"jsonData\".
generalNotif :: Object -> IO PushNotification
generalNotif dat = do 
    let msg = PushNotification {
            apnsNotif = Just def{ rest        = Just dat}
        ,   gcmNotif  = Just def{ data_object = Just dat}
        ,   mpnsNotif = Just def{ restXML     = Document (Prologue [] Nothing [])
                                                         (Element (Name "jsonData" Nothing Nothing)
                                                                 M.empty
                                                                 [NodeContent $ E.decodeUtf8 $
                                                                    BS.concat . BL.toChunks $ encode dat])
                                                         []
                                }
        }
    if ((BL.length . encode . apnsNotif) msg > 256)
      then fail "Too long payload"
      else return msg

-- | Settings for GCM service.
data GCMConfig = Http GCMHttpConfig | Ccs GCMCcsConfig

-- | Main settings for the different Push Services. @Nothing@ means the service won't be used.
data PushConfig = PushConfig{
        gcmConfig  :: Maybe GCMConfig
    ,   apnsConfig :: Maybe APNSConfig
    ,   mpnsConfig :: Maybe MPNSConfig
    }

instance Default PushConfig where
    def = PushConfig Nothing Nothing Nothing

-- | 'RegisterResult' represents the result of a device attempting to register
data RegisterResult = SuccessfulReg | ErrorReg Text

-- | Main settings for the Push Service.
data PushServiceConfig = PushServiceConfig {
        pushConfig           :: PushConfig                   -- ^ Main configuration.
    ,   newMessageCallback   :: Device -> Value -> IO ()     -- ^ The callback function to be called when receiving messages from devices
                                                             -- (This means through the CCS connection or as POST requests
                                                             -- to the Yesod subsite).
    ,   newDeviceCallback    :: Device -> Value -> IO RegisterResult -- ^ The callback function to be called when
                                                                     -- a new device try to register on server.
    ,   unRegisteredCallback :: Device -> IO ()              -- ^ The callback function to be called when a device unregisters.
    ,   newIdCallback        :: (Device,Device) -> IO ()     -- ^ The callback function to be called when a device's identifier changes.
    }

instance Default PushServiceConfig where
    def = PushServiceConfig {
        pushConfig           = def
    ,   newMessageCallback   = \_ _ -> return ()
    ,   newDeviceCallback    = \_ _ -> return SuccessfulReg
    ,   unRegisteredCallback = \_ -> return ()
    ,   newIdCallback        = \_ -> return ()
    }

-- | Main manager for the Push Service.
--
-- This 'PushManager' will be used to send notifications and also can be added as a subsite to a Yesod app
-- in order to receive registrations and messages from devices as HTTP POST requests.
data PushManager = PushManager {
        httpManager    :: Maybe Manager       -- ^ Conduit manager for sending push notifications though POST requests.
    ,   apnsManager    :: Maybe APNSManager   -- ^ Apns manager for sending push notifications though APNS servers.
    ,   ccsManager     :: Maybe CCSManager    -- ^ Ccs manager for communicating with GCM through Cloud Connection Server.
    ,   serviceConfig  :: PushServiceConfig   -- ^ Main configuration.
    }

-- | PushResult represents a general result after communicating with a Push Server.
data PushResult = PushResult {
        successful   :: [Device] -- ^ Notifications that were successfully sent.
    ,   failed       :: [(Device,Either Text SomeException)] -- ^ Notifications that were not successfully sent.
    ,   toResend     :: [Device] -- ^ Failed notifications that you need to resend,
                                 -- because servers were not available or there was a problem with the connection.
    ,   unRegistered :: [Device] -- ^ List of unregistered devices.
    ,   newIds       :: [(Device,Device)] -- ^ List of devices which have changed their identifiers.
    } deriving Show

instance Default PushResult where
    def = PushResult [] [] [] [] []

instance Monoid PushResult where
    mempty = def
    mappend (PushResult a1 b1 c1 d1 e1) (PushResult a2 b2 c2 d2 e2) = PushResult (a1 ++ a2) (b1 ++ b2) (c1 ++ c2) (d1 ++ d2) (e1 ++ e2)


-- | This class represent the translation from a specific service's result into a general Push result.
class IsPushResult a where
    toPushResult :: a -> PushResult


instance IsPushResult GCMresult where
    toPushResult r = def {
        successful   = map (GCM . fst) $ messagesIds r
    ,   failed       = map (\(x,y) -> (GCM x,Left y))                   (errorRest         r)
                    ++ map (\x     -> (GCM x,Left "UnregisteredError")) (errorUnRegistered r)
                    ++ map (\x     -> (GCM x,Left "InternalError"))     (errorToReSend     r)
    ,   toResend     = map GCM $ errorToReSend r
    ,   unRegistered = map GCM $ errorUnRegistered r ++ (map fst $ errorRest r)
    -- I decide to unregister all regId with error different to Unregistered or Unavailable. (errorRest)
    -- Because these are non-recoverable error.
    ,   newIds       = map (\(x,y) -> (GCM x,GCM y)) $ newRegids r
    }


instance IsPushResult APNSresult where
    toPushResult r = def {
        successful   = map APNS $ successfulTokens r
    ,   failed       = map (\x -> (APNS x , Left "CommunicatingError")) $ toReSendTokens r
    ,   toResend     = map APNS $ toReSendTokens r
    }

instance IsPushResult APNSFeedBackresult where
    toPushResult r = def {
        unRegistered = map (APNS . fst) $ unRegisteredTokens r
    }


instance IsPushResult MPNSresult where
    toPushResult r = let (successList,failureList) = partition ((== Just Received) . notificationStatus . snd ) $ successfullResults r
                     in def {
        successful   = map (MPNS . fst) successList
    ,   failed       = map (\(x,y) -> (MPNS x , Right y)) (errorException r)
                    ++ map (\(x,y) -> (MPNS x , Left $ pack $ show $ notificationStatus y)) failureList
    ,   toResend     = map (MPNS . fst) . filter (error500 . snd) $ errorException r
    ,   unRegistered = map (MPNS . fst) . filter ((== Just Expired) . subscriptionStatus . snd ) $ successfullResults r
    } where
        error500 :: SomeException -> Bool
        error500 e = case (fromException e) :: Maybe HttpException of
                         Just (StatusCodeException status _ _) -> (statusCode status) >= 500
                         _                                     -> False
