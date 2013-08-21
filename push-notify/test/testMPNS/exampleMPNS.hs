{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
               QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

import Network.PushNotify.Mpns.Send
import Network.PushNotify.Mpns.Types
import Text.XML
import Network
import Data.Map (empty,singleton)
import Network.HTTP.Conduit

main :: IO ()
main = send def

send :: MPNSmessage -> IO ()
send msg = withSocketsDo $ do
           m    <- newManager def
           res  <- sendMPNS m def msg{
                            deviceURIs  = ["DeviceUri"] -- here you complete with the device URI.
                        ,   restXML     = parseText_ def "<?xml version=\"1.0\" encoding=\"utf-8\"?> <root> <value1> Hello World!! </value1> </root> "
                        }
           print res
           return ()
