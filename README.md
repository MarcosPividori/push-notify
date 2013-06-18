GSoC-Communicating-with-mobile-devices
======================================

This is a project as part of the Google Summer of Code program for this summer 2013.

Student: Marcos Pividori
Mentor: Michael Snoyman
Organization: Haskell.org

Abstract

The aim of this project is to develop a server-side library in Haskell 
for sending push notifications to devices running different OS, such as 
Android, iOS, Windows Phone, BlackBerry, and so on.
The fact is that every company is developing Push Notification services, 
and these are very similar. Then, I want to find the fundamental concepts
to construct a library which enable to configure the options for the different
services and send messages easily.
When I say they are very similar, I refer to the fact that they all are
asynchronous, best-effort services that offers third-party developers a channel
to send data to apps from a cloud service in a power-efficient manner.

The most popular are:
   - Google Cloud Messaging (Android)
   - Apple Push Notification Service (iPhone / iPad)
   - Microsoft Push Notification Service (Windows Phone)
   - BlackBerry Push Service (BlackBerry)
   - Windows Push Notification Services (Windows 8)
   - etc.

Once we have this libraries, I will investigate the possibility of mainting a 
"back and forth" communication between a server and mobile devices and I will 
develop a library to handle this.
