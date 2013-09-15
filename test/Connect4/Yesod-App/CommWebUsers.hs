{-# LANGUAGE OverloadedStrings #-}

-- This module defines the main functions to handle the list of Web users.
module CommWebUsers where

import Database.Persist.Sqlite
import Data.Conduit.Pool
import Data.Aeson
import Data.Text.Encoding
import Data.IORef
import Data.Char                            (isPunctuation, isSpace)
import Data.Monoid                          (mappend)
import Data.Text                            (Text)
import Control.Exception                    (fromException)
import Control.Monad                        (forM_)
import Control.Monad.IO.Class               (liftIO)
import Control.Concurrent.Chan              (Chan, writeChan)
import Network.Wai.EventSource              (ServerEvent (..))
import Blaze.ByteString.Builder.Char.Utf8   (fromText)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import Control.Concurrent.Chan              (Chan)
import qualified Data.HashMap.Strict        as HM
import qualified Data.Attoparsec.ByteString as AB
import Network.PushNotify.General
import Data.Time.Clock.POSIX
import Handlers
import Extra

newWebUsersState :: WebUsers
newWebUsersState = HM.empty

numClients :: WebUsers -> Int
numClients = HM.size

clientExists :: Text -> WebUsers -> Bool
clientExists = HM.member

getClient :: Text -> WebUsers -> Maybe (Chan ServerEvent,POSIXTime)
getClient = HM.lookup

addClient :: Text -> Chan ServerEvent -> POSIXTime -> WebUsers -> WebUsers
addClient t a1 a2 = HM.insert t (a1,a2)

getClients :: WebUsers -> [Text]
getClients = HM.keys

getClientsElems :: WebUsers -> [(Chan ServerEvent,POSIXTime)]
getClientsElems = HM.elems

filterClients :: ((Chan ServerEvent,POSIXTime) -> Bool) -> WebUsers -> WebUsers
filterClients = HM.filter

removeClient :: Text -> WebUsers -> WebUsers
removeClient = HM.delete
