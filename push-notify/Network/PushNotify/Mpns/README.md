## API for communicating through MPNS
I developed an API for communicating through MPNS with Windows Phone mobile devices ([1]). For this, I designed a way of building the messages, sending them and handling the result.
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
 - The MPNS Api : [1]
 - Test example for MPNS: [2]

For more information about:
 - The MPNS Service: [3]
 - The communication with MPNS servers:  [4]
