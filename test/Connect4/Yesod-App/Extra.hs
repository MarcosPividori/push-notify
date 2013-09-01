{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell, TypeSynonymInstances, FlexibleInstances,
               QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}
               
module Extra where

import Network.PushNotify.General
import Database.Persist.TH
import Connect4
import Data.Text

data Identifier = Dev Device | Web Text deriving (Show,Read)

derivePersistField "Identifier"
derivePersistField "Board"
