## API for communicating through MPNS

I developed an API for communicating through MPNS with Windows Phone mobile devices ([1](https://github.com/MarcosPividori/GSoC-Communicating-with-mobile-devices/tree/master/push-notify/Network/PushNotify/Mpns)). For this, I designed a way of building the messages, sending them and handling the result.

To communicate with MPNS servers, I need to send POST requests, so I decided to use Conduit, as I have done with GCM. The MPNS Api looks like this:

MPNS let you send notification in an authenticated or unauthenticated mode, so you can configure this with  MPNSAppConfig:

data **MPNSAppConfig** = **MPNSAppConfig**{
        numRet       :: Int      -- def = 5
    ,   useSecure    :: Bool     -- def = False
    ,   certificate  :: String   -- def = ""
    ,   privateKey   :: String   -- def = ""
    }

To build messages, I developed a data type "MPNSmessage"

data **MPNSmessage** = **MPNSmessage**{
        deviceURIs          :: [DeviceURI]  -- destinations
    ,   batching_interval   :: MPNSInterval -- Immediate | Sec450 | Sec900
    ,   target              :: MPNSType     -- Toast     | Raw    | Tile
    ,   restXML             :: Document     -- the XML data to be sent
    }

For sending notifications:

**sendMPNS** :: Manager -> MPNSAppConfig -> MPNSmessage -> IO MPNSresult

Last, for handling the result:

data **MPNSresult** = **MPNSresult**{
        sucessfullResults :: [(DeviceURI,MPNSinfo)]
    ,   errorException    :: [(DeviceURI,CE.SomeException)]
    }

The code is available on GitHub:
 - The MPNS Api : [1](https://github.com/MarcosPividori/GSoC-Communicating-with-mobile-devices/tree/master/push-notify/Network/PushNotify/Mpns)
 - Test example for MPNS: [2](https://github.com/MarcosPividori/GSoC-Communicating-with-mobile-devices/tree/master/push-notify/test)

For more information about:
 - The MPNS Service: [3](http://msdn.microsoft.com/en-us/library/windowsphone/develop/ff402558(v=vs.105).aspx)
 - The communication with MPNS servers:  [4](http://msdn.microsoft.com/en-us/library/windowsphone/develop/hh202945(v=vs.105).aspx)
