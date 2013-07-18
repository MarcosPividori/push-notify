{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
               QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

import Network.PushNotify.Mpns.Send
import Network.PushNotify.Mpns.Types
import Text.XML
import Text.Hamlet.XML
import Network
import Data.Map (singleton)

send :: MPNSmessage -> IO ()
send msg = withSocketsDo $ do
           res <- sendMPNS def msg{ deviceURIs = [""] }
           print res
           return ()
       
msgToast :: MPNSmessage
msgToast =  MPNSmessage {
            target            = Toast
        ,   rest              = Document (Prologue [] Nothing []) rootToast []
        }

rootToast = Element "wp:Notification" (singleton "xmlns:wp" "\"WPNotification\"")  [xml|
<wp:Toast>
    <wp:Text1> Text1
    <wp:Text2> Text2
    <wp:Param>/Page2.xaml?NavigatedFrom=Toast Notification
|]

msgTile :: MPNSmessage
msgTile =  MPNSmessage {
            target            = Tile
        ,   rest              = Document (Prologue [] Nothing []) rootTile [] 
        }

rootTile = Element "wp:Notification" (singleton "xmlns:wp" "\"WPNotification\"")  [xml|
<wp:Tile>
    <wp:BackgroundImage> ic_launcher-web.png
    <wp:Count> 1
    <wp:Title> TITLE 1
    <wp:BackBackgroundImage> blue.jpg
    <wp:BackTitle> TITLE2
    <wp:BackContent> Contenido
|]

msgRaw :: MPNSmessage
msgRaw = def {
            target            = Raw
        ,   rest              = Document (Prologue [] Nothing []) rootRaw []
        }

rootRaw = Element "wp:Notification" (singleton "xmlns:wp" "\"WPNotification\"")  [xml|
<root>
    <value1> hola
|]
