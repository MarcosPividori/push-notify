package com.example.gsoc_example_connect4;

import static com.example.gsoc_example_connect4.CommonUtilities.SERVER_URL;
import static com.example.gsoc_example_connect4.CommonUtilities.SENDER_ID;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.impl.client.DefaultHttpClient;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import com.google.android.gms.gcm.GoogleCloudMessaging;

import android.content.Context;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.util.Log;

//Class for communicating with Yesod and CCS servers.
public final class ServerUtilities {

    private static final int MAX_ATTEMPTS = 5;
    private static final int BACKOFF_MILLI_SECONDS = 2000;
    private static final Random random = new Random();
    
    // Tag used on log messages
    static final String TAG = "GSoC-Example-ServerUtilities";
    
    // Obtains the list of available users from Yesod server.
    static List<String> getUsersList(String user) {
    	ArrayList<String> l = new ArrayList<String>();
    	InputStream is = null;
    	String json = "";    	
    	
    	HttpClient client = new DefaultHttpClient();
        HttpUriRequest request = new HttpGet(SERVER_URL + "/getusers");
        HttpResponse response = null;
        try {
            response = client.execute(request);
            HttpEntity httpEntity = response.getEntity();
            is = httpEntity.getContent();
        } catch (IOException e1) {
        	return null;
        }
        try {
            BufferedReader reader = new BufferedReader(new InputStreamReader(
                    is, "iso-8859-1"), 8);
            StringBuilder sb = new StringBuilder();
            String line = null;
            while ((line = reader.readLine()) != null) {
                sb.append(line + "\n");
            }
            is.close();
            json = sb.toString();
        } catch (Exception e) {
        	return null;
        }
        
        JSONObject jObj = null;
        try {
        	jObj = new JSONObject(json);
        	JSONArray jarray = jObj.getJSONArray("con4");
        	for(int i=0;i < jarray.length();i++){ 
        		if(!user.equals(jarray.getString(i)))
        			l.add(jarray.getString(i));
        	}
        } catch (JSONException e) {
        	return null;
        }
    	return l;
    }
    
    //Register this device on Yesod server.
    static boolean register(final Context context,String regId,String user,String password) {
    	
    	String serverUrl = SERVER_URL + "/fromdevices/register";
    	Log.i(TAG, "registering device (regId = " + regId + ")");
        Map<String, String> params = new HashMap<String, String>();
        params.put("regId", regId);
        params.put("user", user);
        params.put("password", password);
        params.put("isConnect4"," ");
        params.put("system", "ANDROID");
        long backoff = BACKOFF_MILLI_SECONDS + random.nextInt(1000);

        // As the server might be down, we will retry it a couple times.
        for (int i = 1; i <= MAX_ATTEMPTS; i++) {
            Log.d(TAG, "Attempt #" + i + " to register");
            try {
            	post(serverUrl, params);
                return true;
            } catch (IOException e) {
                Log.e(TAG, "Failed to register on attempt " + i, e);
                if (i == MAX_ATTEMPTS) {
                    break;
                }
                try {
                    Log.d(TAG, "Sleeping for " + backoff + " ms before retry");
                    Thread.sleep(backoff);
                } catch (InterruptedException e1) {
                	// Activity finished before we complete - exit.
                    Log.d(TAG, "Thread interrupted: abort remaining retries!");
                    Thread.currentThread().interrupt();
                    return false;
                }
                // increase backoff exponentially.
                backoff *= 2;
            }
        }
        return false;
    }
    

    // Sends a message to the server.
    static Boolean sendMsgToServer(final Context context,String regId, String user, String password , String msg1 , String msg2, AtomicInteger msgId) {
    	SharedPreferences sharedPref = PreferenceManager.getDefaultSharedPreferences(context);
    	Boolean useCCS = sharedPref.getBoolean("pref_useCCS", false);// flag to choose between CCS or HTTP Post requests.
    	Log.i(TAG, "sending msg to server");
        String serverUrl = SERVER_URL + "/fromdevices/messages";
        if(useCCS){
        	String id = Integer.toString(msgId.incrementAndGet());
        	Bundle params = new Bundle();
        	params.putString(msg1,msg2);
            params.putString("regId", regId);
            params.putString("user",user);
            params.putString("password",password);
            params.putString("system", "ANDROID");
        	GoogleCloudMessaging gcm= GoogleCloudMessaging.getInstance(context);
        	try {
        		gcm.send(SENDER_ID + "@gcm.googleapis.com", id, 0, params);
        		return true;
        	} catch (IOException ex) {
        		return false;
            }
        }
        else{
        	Map<String, String> params = new HashMap<String, String>();
            params.put(msg1,msg2);
            params.put("regId", regId);
            params.put("user",user);
            params.put("password",password);
            params.put("system", "ANDROID");
        	long backoff = BACKOFF_MILLI_SECONDS + random.nextInt(1000);
	        for (int i = 1; i <= 2; i++) {
	            try {
	            	post(serverUrl, params);
	            	return true;
	            } catch (IOException e) {
	            	if(i == MAX_ATTEMPTS){
	            		return false;
	            	}
	            	try {
	                    Log.d(TAG, "Sleeping for " + backoff + " ms before retry");
	                    Thread.sleep(backoff);
	                } catch (InterruptedException e1) {
	                    return false;
	                }
	            	// increase backoff exponentially.
	                if (backoff<200000) backoff*= 2;
	            }
	        }
	        return false;
        }
    }
    
    // Issue a POST request to Yesod server.
    private static void post(String endpoint, Map<String, String> params)
            throws IOException {
        URL url;
        try {
            url = new URL(endpoint);
        } catch (MalformedURLException e) {
            throw new IllegalArgumentException("invalid url: " + endpoint);
        }
        
        Iterator<Entry<String, String>> iterator = params.entrySet().iterator();
        
        JSONObject holder = new JSONObject();
        
        // constructs the POST body using the parameters
        while (iterator.hasNext()) {
            Entry<String, String> param = iterator.next();
            try {
            	holder.put(param.getKey(),param.getValue());
            } catch(JSONException e){
            	throw new IllegalArgumentException(e);	
            }     
        }
        
        String body = holder.toString();
        Log.v(TAG, "Posting '" + body + "' to " + url);
        byte[] bytes = body.getBytes();
        
        HttpURLConnection conn = null;
        try {
            conn = (HttpURLConnection) url.openConnection();
            conn.setDoOutput(true);
            conn.setUseCaches(false);
            conn.setFixedLengthStreamingMode(bytes.length);
            conn.setRequestMethod("POST");
            conn.setRequestProperty("Content-Type","application/json");
            
            OutputStream out = conn.getOutputStream();
            out.write(bytes);
            out.close();

            // handle the response.
            for (int i = 1; i <= MAX_ATTEMPTS; i++) {
            	try
            	{
            		int status=200;
            		status = conn.getResponseCode();
            		if (status != 200) {
            			throw new IOException("Post failed with error code " + status);
            		}
            		break;
            	}catch(java.io.EOFException e){}
            }

        } finally {
            if (conn != null) {
                conn.disconnect();
            }
        }
    }
}
