## API for communicating through GCM

At beginning, I started with an API for communicating through GCM with mobile devices running Android. I developed a way of building the messages, sending them and handling the result.
To build the messages, I developed a data type: GCMmessage, where you can customize some parameters for the servers providing the GCM service.
To send the messages you need to establish some server configurations through the data type GCMAppConfig (as the correct Api key provided by Google). Passing this configuration, a GCMmessage and a number of retries as arguments to the function sendGCM, the notifications will be sent to the GCM Servers.
The result is shown in a new data type: GCMresult, which provides some useful information, as selecting the devices that have changed their regIds, or have been unregistered, and so on. The programmer can take his own desitions on how to handle this result in his server.

To show a simple usage of this API for GCM (Network.PushNotify.Gcm), I developed a simple test example of a Yesod server, where devices can register to receive GCM messages and users can send messages through the web service. This is composed of:

+ a Yesod app for registering the devices and sending push notifications.

+ an Android app for registering on GCM and receiving the notifications.

It is available on: [1](https://github.com/MarcosPividori/GSoC-Communicating-with-mobile-devices/tree/master/push-notify/test)
