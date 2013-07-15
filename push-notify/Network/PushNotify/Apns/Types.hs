-- GSoC 2013 - Communicating with mobile devices.

{-# LANGUAGE FlexibleContexts #-}

-- | This Module define the main data types for sending Push Notifications through Apple Push Notification Service.

module Network.PushNotify.Apns.Types
    ( APNSAppConfig(..)
    , APNSManager(..)
    , APNSmessage(..)
    , AlertDictionary(..)
    , APNSresult(..)
    , APNSFeedBackresult(..)
    , DeviceToken
    , Env(..)
    ) where

import Network.PushNotify.Apns.Constants
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Data.Default
import Data.Aeson.Types
import Data.Text
import Control.Monad.Writer
import Data.HashMap.Strict (insert,HashMap)
import Data.Time.Clock

-- | 'Env' represents the two possible environments.
data Env = Development | Production deriving Show

-- | 'APNSAppConfig' represents the main necessary information for sending notifications through APNS.
data APNSAppConfig = APNSAppConfig
    {   certificate  :: String -- ^ certificate provided by Apple.
    ,   privateKey   :: String -- ^ private key provided by Apple.
    ,   environment  :: Env -- ^ One of the two possible environments.
    ,   timeoutLimit :: Int -- ^ The time to wait for a server response. (microseconds)
    }

instance Default APNSAppConfig where
    def = APNSAppConfig {
        certificate  = ""
    ,   privateKey   = ""
    ,   environment  = Development
    ,   timeoutLimit = 1000000
    }

data APNSManager = APNSManager
    {   mApnsChannel  :: TChan ( MVar (Maybe (Chan Int,Int)) , APNSmessage)
    ,   mWorkerID     :: ThreadId
    ,   mTimeoutLimit :: Int
    }

type DeviceToken = Text -- Binary token stored in hexadecimal representation as text.


-- | 'APNSmessage' represents a message to be sent through APNS.
data APNSmessage = APNSmessage
    {   deviceTokens :: [DeviceToken] -- ^ Destination.
    ,   expiry       :: Maybe UTCTime -- ^ Identifies when the notification is no longer valid and can be discarded. 
    ,   alert        :: Either Text AlertDictionary -- ^ For the system to displays a standard alert.
    ,   badge        :: Maybe Int -- ^ Number to display as the badge of the application icon.
    ,   sound        :: Text -- ^ The name of a sound file in the application bundle. 
    ,   rest         :: Maybe Object -- ^ Extra information.
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
    {   toReSendTokens :: [DeviceToken]
    } deriving Show

instance Default APNSresult where
    def = APNSresult []

-- | 'APNSFeedBackresult' represents information after connecting with the Feedback service.
data APNSFeedBackresult = APNSFeedBackresult
    {   unRegistered :: [(DeviceToken,UTCTime)]
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
