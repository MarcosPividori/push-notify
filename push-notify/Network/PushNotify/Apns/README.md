## API for communicating through APNS
I developed an API for communicating through APNS with iOS mobile devices.
For this, I designed a way of building the messages, sending them and handling the result. 
One important thing that I had to take into account, was how to maintain the connection. For APNS, I needed to communicate through a streaming SSL connection, so I couldn't use conduit as I 've done with GCM. So, to face this problem, I developed an APNSManager.
The API looks like this:

 **startAPNS**     :: APNSAppConfig -> IO APNSManager  
 **closeAPNS**     :: APNSManager   -> IO ()  
 **sendAPNS**      :: APNSManager   -> APNSmessage -> IO APNSresult  
 **feedBackAPNS**  :: APNSAppConfig -> IO APNSFeedBackresult  

The main idea is:

- First of all, the sevice is started with: 'startAPNS'.
  This function:
    * Creates a channel to put the notifications to be sent.
    * Starts a worker thread, which maintains the connection with APNS servers, and sends the data it receives through the cannel.
    * Returns an APNSManager.

- Then, I can use 'sendAPNS' to send messages. Every time I call it:
    * It puts the message on the channel.
    * Wait until the time the message is taken by the worker thread.
    * When the message is taken from the channel, the worker thread will send me a duplicate copy of the error channel. So, I will wait for the end of the sending, or an error msg through the error channel.
    * I return the appropiate result depending on a successful or not operation.

- I stop the service with 'closeAPNS'. It kills the worker thread.

The worker identifies each message it sends with a number, which is sent to APNS. If an error ocurred, this number is returned in an error messages by the APNS servers, representing the number of the last message that was successfully sent.
Then, the worker continually sends messages until it receives an error message. In this case, it resends the error message to the error channel for all the threads waiting for a response, and then it restarts itself, because after an error, APNS servers close the connection.
The time we wait for an error response, after having sent all the messages, can be stablished with the timeoutLimit parameter, when creating a Manager.

As to use this service, Apple requires you to be an enrolled iOS Developer or something similar, I decided to test the API with a local server. This server was taken from: [3](http://bravenewmethod.wordpress.com/2013/04/20/test-server-for-apple-push-notification-and-feedback-integration/) . It simulates the APNS servers and I modified it to test different common scenes.

The code is available on GitHub:
 - The APNS Api : [1](https://github.com/MarcosPividori/GSoC-Communicating-with-mobile-devices/tree/master/push-notify/Network/PushNotify/Apns)
 - Test example for APNS: [2](https://github.com/MarcosPividori/GSoC-Communicating-with-mobile-devices/tree/master/push-notify/test/testAPNS)

For more information about:
 - The APNS Service: [4](http://developer.apple.com/library/ios/#documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/Chapters/ApplePushService.html#//apple_ref/doc/uid/TP40008194-CH100-SW9)
 - The communication between providers and APNS servers:  [5](http://developer.apple.com/library/ios/#documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/Chapters/CommunicatingWIthAPS.html#//apple_ref/doc/uid/TP40008194-CH101-SW1)
 - This project, you can go to the blog: [6](http://gsoc2013cwithmobiledevices.blogspot.com.ar/)
