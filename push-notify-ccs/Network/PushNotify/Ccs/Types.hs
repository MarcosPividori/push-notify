-- GSoC 2013 - Communicating with mobile devices.

{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

-- | This Module define the main data types for sending Push Notifications through Cloud Connection Server (GCM).
module Network.PushNotify.Ccs.Types
    ( CCSManager(..)
    , GCMCcsConfig(..)
    , fromGCMtoCCS
    ) where

import Network.PushNotify.Gcm
import Network.PushNotify.Ccs.Constants

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.STM.TChan
import Control.Retry
import qualified Data.HashMap.Strict    as HM
import Data.Aeson.Types
import Data.Default
import Data.IORef
import Data.Text

-- | Manager of a CCS Connection.
data CCSManager = CCSManager
    {   mState      :: IORef (Maybe ())
    -- ^ @Nothing@ indicates that the manager is closed.
    ,   mCcsChannel :: TChan (Chan GCMresult , MVar (Chan ()), GCMmessage)
    -- ^ Channel to communicate with the worker thread.
    ,   mWorkerID   :: ThreadId
    -- ^ Worker thread ID.
    }

-- | 'GCMCcsConfig' represents the main necessary information for sending notifications through CCS.
data GCMCcsConfig = GCMCcsConfig
    {   aPiKey           :: Text          -- ^ Api key provided by Google.
    ,   senderID         :: Text          -- ^ Sender ID provided by Google.
    ,   ccsRetrySettings :: RetrySettings -- ^ How to retry to connect to CCS servers.
    }

instance Default GCMCcsConfig where
    def = GCMCcsConfig{
            aPiKey   = ""
        ,   senderID = ""
        ,   ccsRetrySettings = RetrySettings {
                                backoff     = True
                            ,   baseDelay   = 200
                            ,   numRetries  = limitedRetries 2
                            }
        }

-- 'fromGCMtoCCS' converts a Gcm message to a proper CCS message.
fromGCMtoCCS :: RegId -> Text -> GCMmessage -> Value
fromGCMtoCCS regId identif msg =
        let Object hmap = toJSON msg
            nmap        = HM.delete cREGISTRATION_IDS hmap
            nmap'       = HM.insert cTo (String regId) nmap
            nmap''      = HM.insert cMessageId (String identif) nmap'
        in Object nmap''
