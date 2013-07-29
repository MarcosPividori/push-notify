{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
               QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}
               
module Extra where

import Network.PushNotify.General
import Database.Persist.TH

derivePersistField "Device"
