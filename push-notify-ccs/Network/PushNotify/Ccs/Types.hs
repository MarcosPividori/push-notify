-- GSoC 2013 - Communicating with mobile devices.
{-# LANGUAGE FlexibleContexts #-}
-- | This Module define the main data types for sending Push Notifications through Cloud Connection Server (GCM).
module Network.PushNotify.Ccs.Types
    ( CCSManager(..)
    , fromGCMtoCCS
    ) where

import Network.PushNotify.Gcm
import Network.PushNotify.Ccs.Constants

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.STM.TChan
import Data.IORef
import Data.Default
import Data.Aeson.Types
import Data.Text
import qualified Data.HashMap.Strict    as HM

-- | Manager of a CCS Connection.
data CCSManager = CCSManager
    {   mState      :: IORef (Maybe ())
    -- ^ @Nothing@ indicates that the manager is closed.
    ,   mCcsChannel :: TChan (MVar GCMresult , MVar (Chan ()), GCMmessage)
    -- ^ Channel to communicate with the worker thread.
    ,   mWorkerID   :: ThreadId
    -- ^ Worker thread ID.
    }

-- 'fromGCMtoCCS' converts a Gcm message to a proper CCS message.
fromGCMtoCCS :: RegId -> Text -> GCMmessage -> Value
fromGCMtoCCS regId identif msg =
        let Object hmap = toJSON msg
            nmap        = HM.delete cREGISTRATION_IDS hmap
            nmap'       = HM.insert cTo (String regId) nmap
            nmap''      = HM.insert cMessageId (String identif) nmap'
        in Object nmap''
