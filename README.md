## Communicating with mobile devices ##

Student: Marcos Pividori

Mentor: Michael Snoyman

Organization: Haskell.org

### About GsoC: ###

Google Summer of Code is a global program that offers post-secondary student developers ages 18 and older stipends to write code for various open source software projects.
Through Google Summer of Code, accepted student applicants are paired with a mentor or mentors from the participating projects, thus gaining exposure to real-world software development scenarios and the opportunity for employment in areas related to their academic pursuits. In turn, the participating projects are able to more easily identify and bring in new developers. Best of all, more source code is created and released for the use and benefit of all.

### Result of the project: ###

#### 3 Libraries for communicating through Push Notifications: ####

After reading many documentation, thinking about a good abstraction, learning about mobile apps and networks connections, and really valuable recommendations from my mentor, (I really want to thank Michael Snoyman for his availability and kindness) I succeeded in developing 3 libraries to make it easy to send push notifications to mobile devices. They are available on Hackage:

* **push-notify-0.1.0.0** : This library offers a simple API for sending notifications through APNS, GCM and MPNS. [1]

* **push-notify-ccs-0.1.0.0** : This library offers an API for sending/receiving notifications through CCS (XMPP - Google Cloud Messaging). [2]

* **push-notify-general-0.1.0.0** : This library offers a general API for sending/receiving notifications, and handling the registration of devices on the server. It provides a general abstraction which can be used to communicate through different services as APNS, GCM, MPNS, hiding as much as possible, the differences between them. [3]

Now, it is easy to send/receive information with mobile devices, so you can easily develop server applications which interact with them.
You can find a lot of documentation on the blog [6] and reading Haddock files.

#### Push Notify test examples: ####

To show a simple usage of push notifications I developed 2 test examples.
I really want to thank FPComplete for giving me the possibility of hosting the Yesod app.
The tests are available online on [5] and the code is available on GitHub [8].

##### BackAndForth Messaging test: #####
In this example, I show how to use the push-general library to handle the registration/reception and sending of messages. From a Yesod app, you can send notifications to devices registered and receive messages from them.
When the server needs to send a message, uses the general api for sending a notification and handling the result in order to correctly actualize the DB. This means removing the devices that have been unregistered and changing the regId of the devices that have changed.

###### To try this example: ######
+ On an Android device, you follow these steps: [7].
+ On a WindowsPhone device, you can download the app from Microsoft Store: [9]
+ Once you registered on Server, you can start sending/receiving notifications through the website: [5].

###### Some ScreenShots: ######

<img src="http://github.com/MarcosPividori/GSoC-Communicating-with-mobile-devices/raw/master/test/BackAndForth%20Messaging/Img2.png" width="600px"/>

<img src="http://github.com/MarcosPividori/GSoC-Communicating-with-mobile-devices/raw/master/test/BackAndForth%20Messaging/Img3.jpg" width="600px"/>


##### Connect 4 test: #####

I have been thinking of an app which lets me show how useful push notifications are. The advantage of this notifications is that you can receive information from server when it is available without needing to continually poll. So, I started to think about a multiplayers game. Every time a player does something, you let the rest know about the new movement. So, I ran into the board game: "Connect 4".
Both, authenticated web players and users of the android app, can play one against each other.
For actualizing the website, I use long polling, while for the Android app, push notifications.

###### To try this example: ######
  
  To try the example:
  
  + On an Android device, you follow these steps: [7].

  + Once you registered on Server, you can start playing against web or Android users. [5]


###### Some ScreenShots: ######

<img src="http://github.com/MarcosPividori/GSoC-Communicating-with-mobile-devices/raw/master/test/Connect4/img1.jpg" width="600px"/>


For more information you can visit the blog: [6]

Every feedback is welcome!

[1]: http://hackage.haskell.org/package/push-notify-0.1.0.0
[2]: http://hackage.haskell.org/package/push-notify-ccs-0.1.0.0
[3]: http://hackage.haskell.org/package/push-notify-general
[4]: https://github.com/MarcosPividori/GSoC-Communicating-with-mobile-devices
[5]: http://gsoc.marcospividori.com.ar
[6]: http://gsoc2013cwithmobiledevices.blogspot.com.ar
[7]: http://gsoc.marcospividori.com.ar/apps
[8]: https://github.com/MarcosPividori/GSoC-Communicating-with-mobile-devices/tree/master/test
[9]: http://www.windowsphone.com/es-ar/store/app/gsoc_example/1e4813bf-c72d-4c18-87f8-65a418433b50


----------------------------


#### Initial information about the project: ####
##### Abstract #####

The aim of this project is to develop a server-side library in Haskell for sending push notifications to devices running different OS, such as Android, iOS, Windows Phone, BlackBerry, and so on.
The fact is that every company is developing Push Notification services, and these are very similar. Then, I want to find the fundamental concepts to construct a library which enable to configure the options for the different services and send messages easily.
When I say they are very similar, I refer to the fact that they all are asynchronous, best-effort services that offers third-party developers a channel to send data to apps from a cloud service in a power-efficient manner. The most popular are:
   - Google Cloud Messaging (Android)
   - Apple Push Notification Service (iPhone / iPad)
   - Microsoft Push Notification Service (Windows Phone)
   - BlackBerry Push Service (BlackBerry)
   - Windows Push Notification Services (Windows 8)
   - etc.
Once we have this libraries, I will investigate the possibility of mainting a "back and forth" communication between a server and mobile devices and I will develop a library to handle this.

##### Motivation and expected benefits #####

I think this idea would be very useful because it will allow all Haskell developers to open to a new world of mobile devices and to build useful programs/services that interact with them.
Pushing data to smartphones provides users with instant access to desired updates as they happen, such as news and weather, sports scores, stock prices and other time-sensitive content. The push services provide an efficient way to quickly push timely information updates to many smartphones at once, in a centrally managed and controlled manner.
Generally, you can also be very selective in who you send information to, including individual customers or many customers (multicast).
This services minimizes the impact on the smartphones battery life. Instead of actively checking for new data, the applications can remain closed. Once the data is delivered, the application can be launched in the background to process it as needed.
This processes offer an alternative to other less efficient methods, such as polling, where a device regularly polls an application server to see if new content is available.
The main differences between the services, refer to details as: the maxim payload length, the quality of service, queueing the messages or not, and the time limit for this, the way the messages are handled in the devices, etc.
As all the libraries to access to these services are developed in Java, Python and so on, I thought that it would be a good idea to offer an option to Haskell programmers. Taking advantage of the similarity of these services, I could develop a very adaptable library which fits the necessities for each one and at the same time offer an abstraction to the user.

##### Technical Considerations #####

In the developing of the APIs for the communication through Push Notifications, I will aim to develop a good abstraction and find the properties in common between the differents services in order to develope an customizable tool but at the same time with a common structure.
I want to let the user build messages and send these in a simple way following each protocol. Also, I will abstract the process of registering the devices in the server and let the user manage the different registrations behind a similar abstraccion.
