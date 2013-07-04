-- GSoC 2013 - Communicating with mobile devices.

{-# LANGUAGE FlexibleContexts #-}

-- | This Module define the main data types for sending Push Notifications through Apple Push Notification Service.

module Types
    ( APNSAppConfig(..)
    , APNSmessage(..)
    , AlertDictionary(..)
    , DeviceToken
    , Env(..)
    ) where

import Constants
import Data.Default
import Data.Aeson.Types
import Data.Text
import Control.Monad.Writer
import Data.HashMap.Strict (insert,HashMap)
import Data.Time.Clock

data Env = Development | Production deriving Show

-- | 'APNSAppConfig' represents the main necessary information for sending notifications through APNS.
data APNSAppConfig = APNSAppConfig
    {   certificate :: String
    ,   privateKey  :: String
    ,   environment :: Env     
    }   deriving Show


type DeviceToken = Text


-- | 'APNSmessage' represents a message to be sent through APNS.
data APNSmessage = APNSmessage
    {   deviceToken :: DeviceToken
    ,   expiry :: Maybe UTCTime
    ,   alert :: Either Text AlertDictionary
    ,   badge :: Maybe Int
    ,   sound :: Text
    ,   rest :: Maybe Object
    } deriving Show

instance Default APNSmessage where
    def = APNSmessage {
        deviceToken = empty
    ,   expiry = Nothing
    ,   alert = Left empty
    ,   badge = Nothing
    ,   sound = empty
    ,   rest = Nothing
    }

data AlertDictionary = AlertDictionary
    {   body :: Text
    ,   action_loc_key :: Text
    ,   loc_key :: Text
    ,   loc_args :: [Text]
    ,   launch_image :: Text
    } deriving Show

instance Default AlertDictionary where
    def = AlertDictionary{
        body = empty
    ,   action_loc_key = empty
    ,   loc_key = empty
    ,   loc_args = []
    ,   launch_image = empty
    }

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
                     Nothing           -> object [(cAPPS .= toJSONapps msg)]
                     Just (map) -> Object $ insert cAPPS (toJSONapps msg) map    

toJSONapps msg = object $ execWriter $ do
                                        case alert msg of
                                            Left  xs -> if xs == empty
                                                            then tell []
                                                            else tell [(cALERT .= xs)]
                                            Right m     -> tell [(cALERT .= (toJSON m))]
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
