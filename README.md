#### Libraries for communicating through Push Notifications: ####

3 libraries to make it easy to send push notifications to mobile devices. They are available on Hackage:

* **push-notify** : This library offers a simple API for sending notifications through APNS, GCM and MPNS. [1]

* **push-notify-ccs** : This library offers an API for sending/receiving notifications through CCS (XMPP - Google Cloud Messaging). [2]

* **push-notify-general** : This library offers a general API for sending/receiving notifications, and handling the registration of devices on the server. It provides a general abstraction which can be used to communicate through different services as APNS, GCM, MPNS, hiding as much as possible, the differences between them. [3]

More documentation and test examples on the blog [5]. Also, look at Haddock documentation.

[1]: http://hackage.haskell.org/package/push-notify-0.1.0.0
[2]: http://hackage.haskell.org/package/push-notify-ccs-0.1.0.0
[3]: http://hackage.haskell.org/package/push-notify-general
[5]: http://gsoc2013cwithmobiledevices.blogspot.com.ar
