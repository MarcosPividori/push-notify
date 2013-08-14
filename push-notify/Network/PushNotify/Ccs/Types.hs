-- GSoC 2013 - Communicating with mobile devices.
{-# LANGUAGE FlexibleContexts #-}
-- | This Module define the main data types for sending Push Notifications through Google Cloud Messaging.
module Types
    ( CCSManager(..)
    , fromGCMtoCCS
    ) where


import Network.PushNotify.Gcm.Constants
import Network.PushNotify.Gcm.Types

import Constants

import Control.Concurrent
import Control.Concurrent.STM.TChan
import Data.IORef
import Data.Int
import Data.Default
import Data.Aeson.Types
import Data.Text
import qualified Data.HashMap.Strict    as HM

data CCSManager = CCSManager
    {   mState        :: IORef (Maybe ())
    ,   mCcsChannel   :: TChan ( MVar Text , GCMmessage)
    ,   mWorkerID     :: ThreadId
    ,   mTimeoutLimit :: Int
    }

fromGCMtoCCS :: RegId -> Int32 -> GCMmessage -> Value
fromGCMtoCCS regId identif msg =
        let Object hmap = toJSON msg
            nmap        = HM.delete cREGISTRATION_IDS hmap
            nmap'       = HM.insert cTo (String regId) nmap
            nmap''      = HM.insert cMessageId (String (pack $ show identif)) nmap'
        in Object nmap''
