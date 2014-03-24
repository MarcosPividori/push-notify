{-# LANGUAGE OverloadedStrings #-}

import Network.PushNotify.Mpns
import Text.XML
import Network
import Network.HTTP.Conduit
import qualified Data.HashSet as HS

main :: IO ()
main = send def

send :: MPNSmessage -> IO ()
send msg = withSocketsDo $ do
           m    <- newManagerMPNS def
           res  <- sendMPNS m def msg{
                     deviceURIs = HS.singleton "DeviceUri" -- here you complete with the device URI.
                   , restXML    = parseText_ def "<?xml version=\"1.0\" encoding=\"utf-8\"?> <root> <value1> Hello World!! </value1> </root>"
                   }
           print res
           return ()
