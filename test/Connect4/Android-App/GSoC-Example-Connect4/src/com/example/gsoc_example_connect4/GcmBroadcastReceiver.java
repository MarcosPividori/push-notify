package com.example.gsoc_example_connect4;

import java.sql.Timestamp;

import com.example.gsoc_example_connect4.R;
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

import static com.example.gsoc_example_connect4.CommonUtilities.EXTRA_CANCEL;
import static com.example.gsoc_example_connect4.CommonUtilities.EXTRA_MOVEMENT;
import static com.example.gsoc_example_connect4.CommonUtilities.NEW_MESSAGE_ACTION;
import static com.example.gsoc_example_connect4.CommonUtilities.EXTRA_NEWGAME;
import static com.example.gsoc_example_connect4.CommonUtilities.EXTRA_WINNER;
import static com.example.gsoc_example_connect4.CommonUtilities.EXTRA_ERRORSENDING;


//This class handles GCM messages.
public class GcmBroadcastReceiver extends BroadcastReceiver {
    
	static final String TAG = "GSoC-Example";
    public static final int NOTIFICATION_ID = 1;
    private NotificationManager mNotificationManager;
    NotificationCompat.Builder builder;
    Context ctx;
    
    //Receives GCM messages.
    @Override
    public void onReceive(Context context, Intent intent) {
        GoogleCloudMessaging gcm = GoogleCloudMessaging.getInstance(context);
        ctx = context;
        String messageType = gcm.getMessageType(intent);
        SharedPreferences prefs = context.getSharedPreferences(MainActivity.class.getSimpleName(),
				Context.MODE_PRIVATE);
        Boolean onForeGround = prefs.getBoolean("onForeGround",false);
		
        String regId = intent.getExtras().getString("registration_id");
        if(regId != null && !regId.equals("")) {
        	SharedPreferences.Editor editor = prefs.edit();
            editor.putString("registration_id", regId);

            long expirationTime = System.currentTimeMillis() + 1000 * 3600 * 24 * 7;

            Log.v(TAG, "Setting registration expiry time to " +
                    new Timestamp(expirationTime));
            editor.putLong("onServerExpirationTimeMs", expirationTime);
            editor.commit();
        }        
        
        if (GoogleCloudMessaging.MESSAGE_TYPE_SEND_ERROR.equals(messageType)) {
        	sendNotification("The CCS message couldn't be sent: " + intent.getExtras().toString());
        	SharedPreferences.Editor editor = prefs.edit();
        	editor.remove("actualPlayer");
    		editor.remove("board");
    		editor.remove("turn");
    		editor.commit();
    		Intent newIntent = new Intent(NEW_MESSAGE_ACTION);
        	newIntent.putExtra(EXTRA_ERRORSENDING,"");
            context.sendBroadcast(newIntent);   
        } else if (GoogleCloudMessaging.MESSAGE_TYPE_DELETED.equals(messageType)) {
            ;
        } else {
        	String position,winner;
        	if( intent.getExtras().getString(EXTRA_CANCEL) != null){
        		SharedPreferences.Editor editor = prefs.edit();
        		if(prefs.getString("actualPlayer",null) != null)
        			editor.putString(EXTRA_CANCEL,prefs.getString("actualPlayer",""));
        		editor.remove("actualPlayer");
        		editor.remove("board");
        		editor.remove("turn");
        		editor.commit();
        		if(!onForeGround) sendNotification("Game cancelled!");
        	}
        	else if( (winner=intent.getExtras().getString(EXTRA_WINNER)) != null){
        		SharedPreferences.Editor editor = prefs.edit();
            	editor.remove("actualPlayer");
        		editor.remove("board");
        		editor.remove("turn");
        		editor.putString(EXTRA_WINNER,winner);
        		editor.commit();
        		if(!onForeGround) sendNotification(winner + " wins!");
        	}
        	else if( (position = intent.getExtras().getString(EXTRA_MOVEMENT)) != null){
        		Board board = new Board(prefs,false,false);
        		board.newMovement(Integer.parseInt(position),2);
        		if(!onForeGround) sendNotification("New movement!");
        	}
            else{
            	String player;
            	if( (player=intent.getExtras().getString(EXTRA_NEWGAME)) != null){
            		if (prefs.getString("actualPlayer",null) == null){
            			SharedPreferences.Editor editor = prefs.edit();
            	   		editor.putString("actualPlayer", player);
            	   		editor.putString(EXTRA_NEWGAME, "");
            	   		editor.commit();
            			new Board(prefs,true,true);
            			if(!onForeGround) sendNotification("An invitation to play!");
            		}
            	}
            }
        	
        	
        	Intent newIntent = new Intent(NEW_MESSAGE_ACTION);
        	newIntent.putExtras(intent.getExtras());
            context.sendBroadcast(newIntent);        	
        
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
        .setContentTitle("Connect 4")
        .setStyle(new NotificationCompat.BigTextStyle()
        .bigText(msg))
        .setContentText(msg)
        .setSound(uri)
        .setAutoCancel(true);

        mBuilder.setContentIntent(contentIntent);
        mNotificationManager.notify(NOTIFICATION_ID, mBuilder.build());
    }
}