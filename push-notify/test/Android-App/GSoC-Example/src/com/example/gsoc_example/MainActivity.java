package com.example.gsoc_example;

import java.io.IOException;
import java.sql.Timestamp;

import android.os.AsyncTask;
import android.os.Bundle;
import android.app.Activity;
import android.app.AlertDialog;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.SharedPreferences;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.TextView;

import com.google.android.gms.gcm.GoogleCloudMessaging;
import static com.example.gsoc_example.CommonUtilities.SENDER_ID;
import static com.example.gsoc_example.CommonUtilities.DISPLAY_MESSAGE_ACTION;

public class MainActivity extends Activity {

	public static final String EXTRA_MESSAGE = "message";
    public static final String PROPERTY_REG_ID = "registration_id";
    private static final String PROPERTY_ON_SERVER_EXPIRATION_TIME = "onServerExpirationTimeMs";
    
    // Default lifespan (7 days) of a reservation until it is considered expired.
    public static final long REGISTRATION_EXPIRY_TIME_MS = 1000 * 3600 * 24 * 7;

    // Tag used on log messages.
    static final String TAG = "GSoC-Example";

    TextView mDisplay;
    GoogleCloudMessaging gcm;
    
    SharedPreferences prefs;
    Context context;

    String regid,user,password,server_url;
    Boolean registered;
    AlertDialog dialog;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        
        registerReceiver(mHandleMessageReceiver,
                new IntentFilter(DISPLAY_MESSAGE_ACTION));
        
        prefs = getSharedPreferences(MainActivity.class.getSimpleName(),Context.MODE_PRIVATE);
        
        registered = prefs.getBoolean("registered", false);
        user = prefs.getString("user","");
        password = prefs.getString("user","");
        server_url = prefs.getString("server_url","");
        
        String historial = prefs.getString("historial","No Messages!\n");
             
        mDisplay = (TextView) findViewById(R.id.display);
        mDisplay.append(historial);
        
        context = getApplicationContext();
        regid = getRegistrationId(context);
        
        if (regid.length() == 0) {
            registerBackground();
        }else{
        	if (user.length() == 0) {//Not registered
                Intent intent = new Intent(context, Register.class);
                startActivityForResult(intent,1000);
            }
            else
            	if(!registered)
            		sendInfoToServer();        	
        }
        
        gcm = GoogleCloudMessaging.getInstance(this);
    }
    
    @Override
    protected void onDestroy() {
        unregisterReceiver(mHandleMessageReceiver);
        super.onDestroy();
    }
    
    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu, this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.activity_main, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch(item.getItemId()) {
            //Clean the screen.
            case R.id.options_clear:
                mDisplay.setText(null);
                SharedPreferences.Editor editor = prefs.edit();
        	    editor.putString("historial", "");
        	    editor.commit();
                return true;
            //Show some info about this example.
            case R.id.options_information:
                AlertDialog.Builder builder = new AlertDialog.Builder(MainActivity.this);
                builder.setMessage(R.string.info_message)
                       .setTitle(R.string.info_title)
                	   .setNeutralButton(R.string.close, new DialogInterface.OnClickListener() {
                    public void onClick(DialogInterface dialog, int id) {
                        ;
                    }
                });
                dialog = builder.create();
                dialog.show();
                return true;
            //Close the app.
            case R.id.options_exit:
                finish();
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }
    
    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
    	super.onActivityResult(requestCode, resultCode, data);
    	//Returning from the register activity.
    	if(requestCode == 1000){
    		user = data.getStringExtra("USER");
    		password = data.getStringExtra("PASSWORD");
    		server_url = data.getStringExtra("SERVER_URL");
    		SharedPreferences.Editor editor = prefs.edit();
    	    editor.putString("user", user);
    	    editor.putString("password", password);
    	    editor.putString("server_url", server_url);
    	    // Commit the edits!
    	    editor.commit();
    	    sendInfoToServer();
    	}
    }
        
    
    // Gets the current registration id for application on GCM service.
    // If result is empty, the registration has failed.
    private String getRegistrationId(Context context) {
        String registrationId = prefs.getString(PROPERTY_REG_ID, "");
        if (registrationId.length() == 0) {
            Log.v(TAG, "Registration not found.");
            return "";
        }
        // check if registration expired.
        if (isRegistrationExpired()) {
            Log.v(TAG, "registration expired.");
            return "";
        }
        return registrationId;
    }
    
    // Checks if the registration has expired.
    // To avoid the scenario where the device sends the registration to the
    // server but the server loses it, the app developer may choose to re-register
    // after REGISTRATION_EXPIRY_TIME_MS.
    // return true if the registration has expired.
    private boolean isRegistrationExpired() {
        // checks if the information is not stale
        long expirationTime = prefs.getLong(PROPERTY_ON_SERVER_EXPIRATION_TIME, -1);
        return System.currentTimeMillis() > expirationTime;
    }
    
    // Registers the application with GCM servers asynchronously.
    // Stores the registration id, and expiration time in the 
    // application's shared preferences.
    private void registerBackground() {
        new AsyncTask<Void,Void,String>() {
            @Override
            protected String doInBackground(Void... params) {
                String msg = "";
                try {
                    if (gcm == null) {
                        gcm = GoogleCloudMessaging.getInstance(context);
                    }
                    regid = gcm.register(SENDER_ID);
                    msg = "Device registered:\nRegistration id = " + regid;
                    
                    // Save the regid - no need to register again.
                    setRegistrationId(context, regid);

                    // Send RegId to the Server.
                    user = prefs.getString("user","");
                    if (user.length() == 0) {//Not registered
                        Intent intent = new Intent(context, Register.class);
                        startActivityForResult(intent,1000);
                    }
                    else
                    	sendInfoToServer();
                } catch (IOException ex) {
                    msg = "Error :" + ex.getMessage();
                }
                return msg;
            }

            @Override
            protected void onPostExecute(String msg) {
                CommonUtilities.displayMessage(context,(msg + "\n"));
            }
        }.execute(null, null, null);
    }
    
    // Send the information to the server asynchronously.
    // Save the success of failure in the variable "registered".
    private void sendInfoToServer(){
    	new AsyncTask<Void, Void, Boolean>() {

    		@Override
    		protected Boolean doInBackground(Void... parameters) {
    			return ServerUtilities.register(context,regid,user,password,server_url);
    		}
    		protected void onPostExecute(Boolean result) {
    			registered = result;
    			SharedPreferences.Editor editor = prefs.edit();
        	    editor.putBoolean("registered", registered);
        	    // Commit the edits!
        	    editor.commit();
    	    }
    	}.execute(null,null,null);
    }
    
    // Stores the registration id and expiration time in the
    // application's SharedPreferences.
    private void setRegistrationId(Context context, String regId) {
        SharedPreferences.Editor editor = prefs.edit();
        editor.putString(PROPERTY_REG_ID, regId);

        long expirationTime = System.currentTimeMillis() + REGISTRATION_EXPIRY_TIME_MS;

        Log.v(TAG, "Setting registration expiry time to " +
                new Timestamp(expirationTime));
        editor.putLong(PROPERTY_ON_SERVER_EXPIRATION_TIME, expirationTime);
        editor.commit();
    }    
    
    
    // To Receive the messages to be shown in the text view.
    private final BroadcastReceiver mHandleMessageReceiver =
            new BroadcastReceiver() {
        
        @Override
        public void onReceive(final Context context, Intent intent) {
            String newMessage = intent.getExtras().getString(EXTRA_MESSAGE);
            mDisplay.append(newMessage+"\n");
        }
    };
    
}
