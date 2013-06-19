## Communicating with mobile devices

Student: Marcos Pividori

Mentor: Michael Snoyman

Organization: Haskell.org

**About GsoC:**

Google Summer of Code is a global program that offers post-secondary student developers ages 18 and older stipends to write code for various open source software projects.
Through Google Summer of Code, accepted student applicants are paired with a mentor or mentors from the participating projects, thus gaining exposure to real-world software development scenarios and the opportunity for employment in areas related to their academic pursuits. In turn, the participating projects are able to more easily identify and bring in new developers. Best of all, more source code is created and released for the use and benefit of all.

**Abstract**

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

**Motivation and expected benefits**

I think this idea would be very useful because it will allow all Haskell developers to open to a new world of mobile devices and to build useful programs/services that interact with them.
Pushing data to smartphones provides users with instant access to desired updates as they happen, such as news and weather, sports scores, stock prices and other time-sensitive content. The push services provide an efficient way to quickly push timely information updates to many smartphones at once, in a centrally managed and controlled manner.
Generally, you can also be very selective in who you send information to, including individual customers or many customers (multicast).
This services minimizes the impact on the smartphones battery life. Instead of actively checking for new data, the applications can remain closed. Once the data is delivered, the application can be launched in the background to process it as needed.
This processes offer an alternative to other less efficient methods, such as polling, where a device regularly polls an application server to see if new content is available.
The main differences between the services, refer to details as: the maxim payload length, the quality of service, queueing the messages or not, and the time limit for this, the way the messages are handled in the devices, etc.
As all the libraries to access to these services are developed in Java, Python and so on, I thought that it would be a good idea to offer an option to Haskell programmers. Taking advantage of the similarity of these services, I could develop a very adaptable library which fits the necessities for each one and at the same time offer an abstraction to the user.

**Technical Considerations**

In the developing of the APIs for the communication through Push Notifications, I will aim to develop a good abstraction and find the properties in common between the differents services in order to develope an customizable tool but at the same time with a common structure.
I want to let the user build messages and send these in a simple way following each protocol. Also, I will abstract the process of registering the devices in the server and let the user manage the different registrations behind a similar abstraccion.
To develop a “back and forth” comunication between a server and mobile devices, I will investigate the different possibilities of maintaining a state of the connection. It could be through the use of cookies stored by the clients or maintaining some extra information in the server which would enable it to identify the different connections and provide the appropiate services.
