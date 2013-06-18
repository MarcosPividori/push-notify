-- GSoC 2013 - Communicating with mobile devices.

{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
             QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

-- | This Module define the main data types for sending Push Notifications through Google Cloud Messaging.
module Types (GCMmessage(..),GCMresult(..),MRes(..),RegId,Notif_key,Notif_key_name) where

import Data.Default
import Data.Aeson.Types

type RegId = String
type Notif_key = String
type Notif_key_name = String

-- | 'GCMmessage' represents a message to be sent through GCM.
data GCMmessage = GCMmessage
    {   registration_ids :: Maybe [RegId] 
    ,   notification_key :: Maybe Notif_key
    ,   notification_key_name :: Maybe Notif_key_name
    ,   collapse_key :: String
    ,   data_object :: Maybe Object
    ,   delay_while_idle :: Bool
    ,   time_to_live :: Maybe Int
    ,   restricted_package_name :: String
    ,   dry_run :: Bool
    } deriving Show

instance Default GCMmessage where
    def = GCMmessage {
        registration_ids = Nothing
    ,   notification_key = Nothing
    ,   notification_key_name = Nothing
    ,   collapse_key = []
    ,   data_object = Nothing
    ,   delay_while_idle = False
    ,   time_to_live = Nothing
    ,   restricted_package_name = []
    ,   dry_run = False
    }

data MRes = GCMError String 
          | GCMOk String 
            deriving Show

-- | 'GCMresult' represents information about messages after a communication with GCM Servers.
data GCMresult = GCMresult
    {   multicast_id :: Maybe Integer
    ,   success :: Maybe Int
    ,   failure :: Maybe Int
    ,   canonical_ids :: Maybe Int
    ,   results :: [MRes]
    ,   newRegids :: [(RegId,RegId)] -- ^ regIds that need to be replaced.
    ,   unRegistered :: [RegId] -- ^ regIds that need to be removed.
    ,   toReSend :: [RegId] -- ^ regIds that I need to resend the message to.
    } deriving Show

instance Default GCMresult where
    def = GCMresult {
        multicast_id = Nothing
    ,   success = Nothing
    ,   failure = Nothing
    ,   canonical_ids = Nothing
    ,   results = []
    ,   newRegids = []
    ,   unRegistered = []
    ,   toReSend = []
    }
