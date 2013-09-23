## Connect 4 - Test example for Push Notifications:

I have been thinking of an app which lets me show how useful push notifications are.
The advantage of this notifications is that you can receive information from server when 
it is available without needing to continually poll. So, I started to think about a multiplayers 
game. Every time a player does something, you let the rest know about the new movement. So, I ran 
into the board game: "Connect 4".
Both, authenticated web players and users of the android app, can play one against each other.
For actualizing the website, I use long pulling, while for the Android app, push notifications.


**To try the example:**
  
  To try the example:
  
  + On an Android device, you follow these steps: [1](http://gsoc.marcospividori.com.ar/apps).

  + Once you registered on Server, you can start playing against web or Android users. [2](http://gsoc.marcospividori.com.ar)


**Some ScreenShots:**

<img src="http://github.com/MarcosPividori/GSoC-Communicating-with-mobile-devices/raw/master/test/Connect4/img1.jpg" width="800px"/>


**About the Yesod App:**

Before running the Yesod app, you need to complete the "approot" and the 
"apiKey" / "senderId" with the proper values.

The API Key and Project ID are provided by Google. You need to start a project at Google Apis
and enable the GCM service. (https://code.google.com/apis/console)

Main modules description:
 - CommDevices : defines the main callback functions to communicate with mobile devices using the general library.
 - CommWebUsers : defines the main functions to handle the set of Web users.
 - Connect4 : defines the main functions to save the state of the Connect4 game.
 - DataBase : defines the DB structure.
 - Handlers :  defines the main function to handle the messages that comes from users (web or Android users).
 - Main : defines the Yesod app.

**About the Android app:**

Its very simple. When started, it connects to the GCM Service and gets its RegID.
Then, it asks you for a username and password, and sends all this information to
your server.
Once you registered on Server, you can start playing against web or Android users.

For more information, you can visit the blog: http://gsoc2013cwithmobiledevices.blogspot.com.ar/

