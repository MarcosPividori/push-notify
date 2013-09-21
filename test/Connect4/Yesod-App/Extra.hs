{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell, TypeSynonymInstances, FlexibleInstances,
               QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

-- This module defines some common datatypes.
module Extra where

import Network.PushNotify.General
import Database.Persist.TH
import Connect4
import Data.Text
import Data.Time.Clock.POSIX
import Control.Concurrent.STM.TChan            (TChan)
import qualified Data.HashMap.Strict as HM

data Identifier = Dev Device | Web (TChan MsgFromPlayer)
type WebUsers   = HM.HashMap Text (TChan MsgFromPlayer,POSIXTime)
data MsgFromPlayer = Cancel | Movement Int | NewGame Text | Winner Text deriving (Show,Read,Eq)

derivePersistField "Device"
derivePersistField "Board"
