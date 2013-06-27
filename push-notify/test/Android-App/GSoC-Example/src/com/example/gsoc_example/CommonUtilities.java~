package com.example.gsoc_example;

import android.content.Context;
import android.content.Intent;

//This class provide some common methods and constants.
public final class CommonUtilities {
	
	//URL of the server.
	static final String SERVER_URL = "http://192.168.0.52:3000";

    //Google API project id registered.
	/** Substitute you own sender ID here. */
    static final String SENDER_ID = "";

    //Intent used for showing messages on the screen.
    static final String DISPLAY_MESSAGE_ACTION =
            "com.example.informaciondedispositivos.DISPLAY_MESSAGE";
    
    //Intent's extra which contains the message to be shown.
    static final String EXTRA_MESSAGE = "message";
    

    //This method lets show a message in the main console.
    static void displayMessage(Context context, String message) {
        Intent intent = new Intent(DISPLAY_MESSAGE_ACTION);
        intent.putExtra(EXTRA_MESSAGE, message);
        context.sendBroadcast(intent);
    }
    
}
