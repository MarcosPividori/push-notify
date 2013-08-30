{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell, TypeSynonymInstances, FlexibleInstances,
               QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}
               
module Extra where

import Network.PushNotify.General
import Database.Persist.TH
import Connect4

derivePersistField "Device"
derivePersistField "Board"
