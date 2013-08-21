-- GSoC 2013 - Communicating with mobile devices.
{-# LANGUAGE FlexibleContexts #-}
-- | This Module define the main data types for sending Push Notifications through Cloud Connection Server (GCM).
module Network.PushNotify.Ccs.Types
    ( CCSManager(..)
    , fromGCMtoCCS
    ) where

import Network.PushNotify.Gcm.Constants
import Network.PushNotify.Gcm.Types

import Network.PushNotify.Ccs.Constants

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.STM.TChan
import Data.IORef
import Data.Default
import Data.Aeson.Types
import Data.Text
import qualified Data.HashMap.Strict    as HM

data CCSManager = CCSManager
    {   mState      :: IORef (Maybe ())
    ,   mCcsChannel :: TChan (MVar GCMresult , MVar (Chan ()), GCMmessage)
    ,   mWorkerID   :: ThreadId
    }

fromGCMtoCCS :: RegId -> Text -> GCMmessage -> Value
fromGCMtoCCS regId identif msg =
        let Object hmap = toJSON msg
            nmap        = HM.delete cREGISTRATION_IDS hmap
            nmap'       = HM.insert cTo (String regId) nmap
            nmap''      = HM.insert cMessageId (String identif) nmap'
        in Object nmap''
