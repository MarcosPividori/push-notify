{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
               QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

import Send
import Types
import Text.XML
import Text.Hamlet.XML
import Network
import Data.Map (empty,singleton)
import Data.Text (pack,unpack,Text)

uri :: Text
uri = "http://sn1.notify.live.net/throttledthirdparty/01.00/AQE7ohp4FBGXTKtYz_yPmDc6AgAAAAADAQAAAAQUZm52OkJCMjg1QTg1QkZDMkUxREQFBlVTU0MwMQ"

send :: MPNSmessage -> IO ()
send msg = withSocketsDo $ do
           res <- sendMPNS def msg{ deviceURIs = [uri] }
           print res
           return ()

sendText :: String -> IO ()
sendText msg = withSocketsDo $ do
           let mensaje = (msgToastMsg msg){ deviceURIs = [uri] }
           putStrLn $ show $ rest mensaje
           putStrLn $ show $ renderText def (rest mensaje)
           res <- sendMPNS def mensaje
           print res
           return ()
       
msgToast :: MPNSmessage
msgToast = def {
            target            = Toast
        ,   rest              = Document (Prologue [] Nothing []) rootToast []
        }

rootToast = Element "wp:Notification" (singleton "xmlns:wp" "WPNotification")  [xml|
<wp:Toast>
   <wp:Text1> Text1
   <wp:Text2> Text2
|]
--    <wp:Param>/Page2.xaml?NavigatedFrom=Toast Notification
-- |]

msgToastMsg :: String -> MPNSmessage
msgToastMsg msg = def {
            target            = Toast
        ,   rest              = Document (Prologue [] Nothing []) (rootToastMsg msg) []
-- parseText_ def "<?xml version=\"1.0\" encoding=\"utf-8\"?> <wp:Notification xmlns:wp=\"WPNotification\"> <wp:Toast> <wp:Text1>text1 </wp:Text1> text2 <wp:Text2> </wp:Text2> <wp:Param>?msg=hola</wp:Param> </wp:Toast> </wp:Notification>" -- 
        }

rootToastMsg msg = Element "wp:Notification" (singleton "xmlns:wp" "WPNotification") [xml|
    <wp:Toast>
        <wp:Text1> Text1
        <wp:Text2> Text2
        <wp:Param> #{pack msg}
|]

msgTile :: MPNSmessage
msgTile = def {
            target            = Tile
        ,   rest              = Document (Prologue [] Nothing []) rootTile [] 
        }

rootTile = Element "wp:Notification" (singleton "xmlns:wp" "WPNotification")  [xml|
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

rootRaw = Element "root" empty [xml|
<value1> hola
|]
