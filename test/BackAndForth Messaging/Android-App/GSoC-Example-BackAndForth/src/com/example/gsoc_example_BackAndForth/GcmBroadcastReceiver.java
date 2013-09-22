package com.example.gsoc_example_BackAndForth;

import java.sql.Timestamp;

import com.example.gsoc_example_BackAndForth.R;
import com.google.android.gms.gcm.GoogleCloudMessaging;

import android.app.Activity;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.media.RingtoneManager;
import android.net.Uri;
import android.support.v4.app.NotificationCompat;
import android.util.Log;

import static com.example.gsoc_example_BackAndForth.CommonUtilities.displayMessage;


//This class handles GCM messages.
public class GcmBroadcastReceiver extends BroadcastReceiver {
    
	static final String TAG = "GSoC-Example";
    public static final int NOTIFICATION_ID = 1;
    private NotificationManager mNotificationManager;
    NotificationCompat.Builder builder;
    Context ctx;
    
    @Override
    public void onReceive(Context context, Intent intent) {
        GoogleCloudMessaging gcm = GoogleCloudMessaging.getInstance(context);
        ctx = context;
        String messageType = gcm.getMessageType(intent);
        
        String regId = intent.getExtras().getString("registration_id");
        if(regId != null && !regId.equals("")) {
        	SharedPreferences prefs = context.getSharedPreferences(MainActivity.class.getSimpleName(),
					Context.MODE_PRIVATE);
        	SharedPreferences.Editor editor = prefs.edit();
            editor.putString("registration_id", regId);

            long expirationTime = System.currentTimeMillis() + 1000 * 3600 * 24 * 7;

            Log.v(TAG, "Setting registration expiry time to " +
                    new Timestamp(expirationTime));
            editor.putLong("onServerExpirationTimeMs", expirationTime);
            editor.commit();
        }        
        
        if (GoogleCloudMessaging.MESSAGE_TYPE_SEND_ERROR.equals(messageType)) {
        	displayMessage(context,"The message couldn't be sent: " + intent.getExtras().toString());
        } else if (GoogleCloudMessaging.MESSAGE_TYPE_DELETED.equals(messageType)) {
            sendNotification("Deleted messages on server: " +
                    intent.getExtras().toString());
        } else {
        	String message = intent.getStringExtra("NewMessage");
            if(message != null){
            	sendNotification("Received: " + message);
            	SharedPreferences prefs = context.getSharedPreferences(MainActivity.class.getSimpleName(),
            										Context.MODE_PRIVATE);
            	String list = prefs.getString("historial","");
            	SharedPreferences.Editor editor = prefs.edit();
        	    editor.putString("historial", list + "Received: " + message + "\n");
        	    editor.commit();
            	displayMessage(context,"Received: " + message);
        	}
        }        
        setResultCode(Activity.RESULT_OK);
    }
    
    //Shows a notification.
    private void sendNotification(String msg) {
        mNotificationManager = (NotificationManager)
                ctx.getSystemService(Context.NOTIFICATION_SERVICE);

        PendingIntent contentIntent = PendingIntent.getActivity(ctx, 0,
                new Intent(ctx, MainActivity.class), 0);
        
        Uri uri= RingtoneManager.getDefaultUri(RingtoneManager.TYPE_NOTIFICATION);
        
        NotificationCompat.Builder mBuilder =
                new NotificationCompat.Builder(ctx)
        .setSmallIcon(R.drawable.ic_launcher)
        .setContentTitle("GCM Notification")
        .setStyle(new NotificationCompat.BigTextStyle()
        .bigText(msg))
        .setContentText(msg)
        .setSound(uri)
        .setAutoCancel(true);

        mBuilder.setContentIntent(contentIntent);
        mNotificationManager.notify(NOTIFICATION_ID, mBuilder.build());
    }
}