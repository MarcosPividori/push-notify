-- GSoC 2013 - Communicating with mobile devices.

{-# LANGUAGE FlexibleContexts , OverloadedStrings #-}

-- | This Module define the main data types for sending Push Notifications through Google Cloud Messaging.
module Network.PushNotify.Gcm.Types
    ( -- * GCM Settings
      GCMHttpConfig(..)
    , RegId
      -- * GCM Messages
    , GCMmessage(..)
      -- * GCM Result
    , GCMresult(..)
    ) where


import Network.PushNotify.Gcm.Constants
import Control.Monad.Writer
import qualified Data.HashMap.Strict    as HM
import qualified Data.HashSet           as HS
import Data.Aeson.Types
import Data.Default
import Data.Monoid
import Data.Text


-- | 'GCMHttpConfig' represents the main necessary information for sending notifications through GCM.
data GCMHttpConfig = GCMHttpConfig
    {   apiKey   :: Text -- ^ Api key provided by Google.
    ,   numRet   :: Int  -- ^ Number of attemps to send the message to the server.
    }   deriving Show
    
instance Default GCMHttpConfig where
    def = GCMHttpConfig "" 5

-- | 'RegId' is an unique identifier of an app/device, provided by GCM.
type RegId = Text

-- | 'GCMmessage' represents a message to be sent through GCM. In general cases, you can use the 'Default' value and only specify 'registration_ids' and 'data_object'.
--
--On the other hand, if you want to use the rest of specific aspects, you can find more information on GCM website.
data GCMmessage = GCMmessage
    {   registration_ids        :: HS.HashSet RegId -- ^ Destination.
    ,   collapse_key            :: Maybe Text
    ,   data_object             :: Maybe Object     -- ^ Main JSON data to be sent.
    ,   delay_while_idle        :: Bool
    ,   time_to_live            :: Maybe Int
    ,   restricted_package_name :: Maybe Text
    ,   dry_run                 :: Bool
    } deriving Show

instance Default GCMmessage where
    def = GCMmessage {
        registration_ids        = HS.empty
    ,   collapse_key            = Nothing
    ,   data_object             = Nothing
    ,   delay_while_idle        = False
    ,   time_to_live            = Nothing
    ,   restricted_package_name = Nothing
    ,   dry_run                 = False
    }


-- | 'GCMresult' represents information about messages after a communication with GCM Servers.
data GCMresult = GCMresult
    {   multicast_id      :: Maybe Integer   -- ^ Unique ID (number) identifying the multicast message.
    ,   success           :: Maybe Int       -- ^ Number of messages that were processed without an error.
    ,   failure           :: Maybe Int       -- ^ Number of messages that could not be processed.
    ,   canonical_ids     :: Maybe Int       -- ^ Number of results that contain a canonical registration ID.
    ,   newRegids         :: HM.HashMap RegId RegId -- ^ RegIds that need to be replaced.
    ,   messagesIds       :: HM.HashMap RegId Text  -- ^ Successful RegIds, and its \"message_id\".
    ,   errorUnRegistered :: HS.HashSet RegId       -- ^ Failed regIds that need to be removed.
    ,   errorToReSend     :: HS.HashSet RegId       -- ^ Failed regIds that is necessary to resend the message to,
                                                    -- because there was an internal problem in GCM servers.
    ,   errorRest         :: HM.HashMap RegId Text  -- ^ Failed regIds with the rest of the possible errors
                                                    -- (probably non-recoverable errors).
    } deriving Show

instance Default GCMresult where
    def = GCMresult {
        multicast_id      = Nothing
    ,   success           = Nothing
    ,   failure           = Nothing
    ,   canonical_ids     = Nothing
    ,   newRegids         = HM.empty
    ,   messagesIds       = HM.empty
    ,   errorUnRegistered = HS.empty
    ,   errorToReSend     = HS.empty
    ,   errorRest         = HM.empty
    }

instance Monoid GCMresult where
    mempty = def
    mappend (GCMresult _ x1 y1 z1 a1 b1 c1 d1 e1) (GCMresult _ x2 y2 z2 a2 b2 c2 d2 e2) =
                        GCMresult Nothing (add x1 x2) (add y1 y2) (add z1 z2) (HM.union a1 a2) 
                                  (HM.union b1 b2) (HS.union c1 c2) (HS.union d1 d2) (HM.union e1 e2)
                        where
                              add x Nothing = x
                              add (Just n) (Just m) = Just (n+m)
                              add (Nothing) (Just m) = Just m

ifNotDef :: (ToJSON a,MonadWriter [Pair] m,Eq a)
            => Text
            -> (GCMmessage -> a)
            -> GCMmessage
            -> m ()
ifNotDef label f msg = if f def /= f msg
                        then tell [(label .= (f msg))]
                        else tell []

instance ToJSON GCMmessage where
    toJSON msg = object $ execWriter $ do
                                         ifNotDef cREGISTRATION_IDS (HS.toList . registration_ids) msg
                                         ifNotDef cTIME_TO_LIVE time_to_live msg
                                         ifNotDef cDATA data_object msg
                                         ifNotDef cCOLLAPSE_KEY collapse_key msg
                                         ifNotDef cRESTRICTED_PACKAGE_NAME restricted_package_name msg
                                         ifNotDef cDELAY_WHILE_IDLE delay_while_idle msg
                                         ifNotDef cDRY_RUN dry_run msg
