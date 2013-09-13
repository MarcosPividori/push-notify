-- GSoC 2013 - Communicating with mobile devices.

{-# LANGUAGE FlexibleContexts #-}

-- | This Module define the main data types for sending Push Notifications through Apple Push Notification Service.

module Network.PushNotify.Apns.Types
    ( -- * APNS Settings
      APNSConfig(..)
    , APNSManager(..)
    , DeviceToken
    , Env(..)
      -- * APNS Messages
    , APNSmessage(..)
    , AlertDictionary(..)
      -- * APNS Results
    , APNSresult(..)
    , APNSFeedBackresult(..)
    ) where

import Network.PushNotify.Apns.Constants
import Network.TLS              (PrivateKey)
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad.Writer
import Data.Aeson.Types
import Data.Certificate.X509    (X509)
import Data.Default
import Data.HashMap.Strict      (insert,HashMap)
import Data.IORef
import Data.Text
import Data.Time.Clock

-- | 'Env' represents the three possible working environments. This determines the url and port to connect to.
data Env = Development -- ^ Development environment (by Apple).
         | Production  -- ^ Production environment (by Apple).
         | Local       -- ^ Local environment, just to test the service in the \"localhost\".
         deriving Show

-- | 'APNSConfig' represents the main necessary information for sending notifications through APNS.
data APNSConfig = APNSConfig
    {   apnsCertificate :: X509       -- ^ Certificate provided by Apple.
    ,   apnsPrivateKey  :: PrivateKey -- ^ Private key provided by Apple.
    ,   environment     :: Env        -- ^ One of the possible environments.
    ,   timeoutLimit    :: Int        -- ^ The time to wait for a server response. (microseconds)
    }

instance Default APNSConfig where
    def = APNSConfig {
        apnsCertificate = undefined
    ,   apnsPrivateKey  = undefined
    ,   environment     = Development
    ,   timeoutLimit    = 200000
    }

data APNSManager = APNSManager
    {   mState        :: IORef (Maybe ())
    ,   mApnsChannel  :: TChan ( MVar (Maybe (Chan Int,Int)) , APNSmessage)
    ,   mWorkerID     :: ThreadId
    ,   mTimeoutLimit :: Int
    }

-- | Binary token stored in hexadecimal representation as text.
type DeviceToken = Text


-- | 'APNSmessage' represents a message to be sent through APNS.
data APNSmessage = APNSmessage
    {   deviceTokens :: [DeviceToken] -- ^ Destination.
    ,   expiry       :: Maybe UTCTime -- ^ Identifies when the notification is no longer valid and can be discarded. 
    ,   alert        :: Either Text AlertDictionary -- ^ For the system to displays a standard alert.
    ,   badge        :: Maybe Int     -- ^ Number to display as the badge of the application icon.
    ,   sound        :: Text          -- ^ The name of a sound file in the application bundle.
    ,   rest         :: Maybe Object  -- ^ Extra information.
    } deriving Show

instance Default APNSmessage where
    def = APNSmessage {
        deviceTokens = []
    ,   expiry       = Nothing
    ,   alert        = Left empty
    ,   badge        = Nothing
    ,   sound        = empty
    ,   rest         = Nothing
    }

-- | 'AlertDictionary' represents the possible dictionary in the 'alert' label.
data AlertDictionary = AlertDictionary
    {   body           :: Text
    ,   action_loc_key :: Text
    ,   loc_key        :: Text
    ,   loc_args       :: [Text]
    ,   launch_image   :: Text
    } deriving Show

instance Default AlertDictionary where
    def = AlertDictionary{
        body           = empty
    ,   action_loc_key = empty
    ,   loc_key        = empty
    ,   loc_args       = []
    ,   launch_image   = empty
    }

-- | 'APNSresult' represents information about messages after a communication with APNS Servers.
data APNSresult = APNSresult
    {   successfulTokens :: [DeviceToken]
    ,   toReSendTokens   :: [DeviceToken] -- ^ Failed tokens that you need to resend the message to,
                                          -- because there was a problem.
    } deriving Show

instance Default APNSresult where
    def = APNSresult [] []

-- | 'APNSFeedBackresult' represents information after connecting with the Feedback service.
data APNSFeedBackresult = APNSFeedBackresult
    {   unRegisteredTokens :: [(DeviceToken,UTCTime)] -- ^ Devices tokens and time indicating when APNS determined
                                                      -- that the application no longer exists on the device.
    } deriving Show

instance Default APNSFeedBackresult where
    def = APNSFeedBackresult []


ifNotDef :: (ToJSON a,MonadWriter [Pair] m,Eq a,Default b)
            => Text
            -> (b -> a)
            -> b
            -> m ()
ifNotDef label f msg = if f def /= f msg
                        then tell [(label .= (f msg))]
                        else tell []

instance ToJSON APNSmessage where
    toJSON msg = case rest msg of
                     Nothing    -> object [(cAPPS .= toJSONapps msg)]
                     Just (map) -> Object $ insert cAPPS (toJSONapps msg) map

toJSONapps msg = object $ execWriter $ do
                                        case alert msg of
                                            Left xs  -> if xs == empty
                                                            then tell []
                                                            else tell [(cALERT .= xs)]
                                            Right m  -> tell [(cALERT .= (toJSON m))]
                                        ifNotDef cBADGE badge msg
                                        ifNotDef cSOUND sound msg

instance ToJSON AlertDictionary where
    toJSON msg = object $ execWriter $ do
                                        ifNotDef cBODY body msg
                                        ifNotDef cACTION_LOC_KEY action_loc_key msg
                                        ifNotDef cLOC_KEY loc_key msg
                                        if loc_key def /= loc_key msg
                                            then ifNotDef cLOC_ARGS loc_args msg
                                            else tell []
                                        ifNotDef cLAUNCH_IMAGE launch_image msg
