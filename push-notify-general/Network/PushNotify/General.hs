module Network.PushNotify.General
    (
    -- * The purpose of this library
    -- $purpose
    
    -- * How to use this library
    -- $explanation
    
    -- * Push Service
      startPushService
    , closePushService
    , sendPush
    -- * Push Settings
    , Device(..)
    , PushServiceConfig(..)
    , RegisterResult(..)
    , PushAppConfig(..)
    , PushManager
    -- * Push Message
    , PushNotification(..)
    -- * Push Result
    , PushResult(..)
    -- * Yesod Push Subsite
    , PushAppSub
    ) where

import Network.PushNotify.General.Send
import Network.PushNotify.General.Types
import Network.PushNotify.General.YesodPushApp

-- $purpose
-- This library defines a general API for communicating with iOS,
-- Android and WPhone powered devices, sending/receiving Push Notifications.
--
-- The main idea is to hide as much as possible the differences between the different services.
--
-- Push Notification services in general, are very similar. They all are asynchronous, best-effort services
-- that offer third-party developers a channel to send data to apps from a cloud service in a power-efficient manner.
-- The main differences between the services, refer to details as: the maxim payload length, the quality of service,
-- queueing the messages or not, and the time limit for this, the way the messages are handled in the devices, etc.
--
-- The registration of devices is very similar too. The device/app gets a unique ID provided by the different services, and sends this identifier
-- to your 3rd party server. So then, when you want to send messages to these devices, you have to send them to the Push Servers, and
-- they will deliver the messages to the proper devices.
--
-- So 'Device' will be our general identifier.
--
-- Everytime a device sends a message to our system it should provide the pair
-- {\"regId\" : identifier , \"system\" : system } specifing the ID and the system running on the device, so we can build 
-- the 'Device' identifier.
--
-- Now we support APNS, MPNS and GCM. So the possible combinations are:
--
-- { \"regId\" : 'Network.PushNotify.Gcm.RegId' , \"system\" : \"ANDROID\" }
--
-- { \"regId\" : 'Network.PushNotify.Mpns.DeviceURI' , \"system\" : \"WPHONE\" }
--
-- { \"regId\" : 'Network.PushNotify.Apns.DeviceToken' , \"system\" : \"IOS\" } (token stored in hexadecimal representation)
-- 
-- 
-- We will provide a Yesod subsite which will consist of 2 routes for receiving JSON data from devices:
--
--  * /register -> this route will receive the registration from devices. So devices have to send the pairs
--      {\"regId\" : identifier , \"system\" : system }  and any extra information. When a new intent for a registration arrives,
--      the callback function will be called with the identifier and JSON data as arguments so you can ask for more information
--      as username and password, etc. Depending on the callback function result, the response to the POST request
--      will be set, so device can know if it has successfully registered on the server.
--
--  *  /messages -> this route will receive the POST messages from devices. Again, devices have to send the pairs
--      {\"regId\" : identifier , \"system\" : system } and any other extra information.
--      Everytime a new messages arrives to the Yesod subsite, the callback function will be called with the identifier
--      and JSON data as arguments.
--      This abstraction lets us use the same callback function to handle the messages that arrive from CCS too (XMPP connection for GCM).

-- $explanation
-- * First you establish the main configuration for the push service: 'PushServiceConfig' . This means, set the configurations for the
-- different Push services you want to use, and also set the callbacks functions. These functions could be used to actualize a DB,
-- do some processing in background, etc.
--
-- * Then you start the service with the function: 'startPushService' and you get the Manager and YesodSubsite.
--     So, you can add this subsite to your Yesod app or just ignore it if you don't want to receive messages from devices.
--
-- * When you want to send notifications: 
--
--      (1) You have to specify the 'PushNotification' , setting the parameters. Here you have to do it
--           for each kind of notification, because they contain different structures.
-- 
--      2. Then you can send these with the function 'sendPush' and the 'PushManager' as arguments. Also, you can get useful information
--           from the 'PushResult' value after a communication with servers.
--
-- * At the end, you stop the service with: 'closePushService'.
--
--
-- When your Apps on devices need to send an upstream message they can do it as a HTTP POST request to your server
-- (using the route of the Yesod subsite: something like \".../messages\" ) or in case of Android devices, they can send it through CCS.
-- (For this you have to set the proper settings when starting the Push Service).
--
-- You can see many test examples on the GitHub repository: <https://github.com/MarcosPividori/GSoC-Communicating-with-mobile-devices>
