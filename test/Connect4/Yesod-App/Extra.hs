{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell, TypeSynonymInstances, FlexibleInstances,
               QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

-- This module defines some common datatypes.
module Extra where

import Network.PushNotify.General
import Database.Persist.TH
import Connect4
import Data.Text
import Data.Time.Clock.POSIX
import Control.Concurrent.Chan (Chan)
import qualified Data.HashMap.Strict as HM

data Identifier = Dev Device | Web (Chan MsgFromDevice) deriving Eq
type WebUsers   = HM.HashMap Text (Chan MsgFromDevice,POSIXTime)
data MsgFromDevice = Cancel | Movement Int | NewGame Text | Winner Text | NewMessage Text Text | Offline deriving (Show,Read,Eq)

derivePersistField "Device"
derivePersistField "Board"
