package com.example.gsoc_example;

import static com.example.gsoc_example.CommonUtilities.SERVER_URL;
import static com.example.gsoc_example.CommonUtilities.displayMessage;

import java.io.IOException;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;

import android.content.Context;
import android.util.Log;

//Clase usada para comunicarse con el servidor.
public final class ServerUtilities {

    private static final int MAX_ATTEMPTS = 5;
    private static final int BACKOFF_MILLI_SECONDS = 2000;
    private static final Random random = new Random();

    /** Tag used on log messages. */
    static final String TAG = "GSoC-Example-ServerUtilities";
    
    //Registra este dispositivo en el servidor.
    static boolean register(final Context context, final String regId,String user,String password) {
    	  	
    	Log.i(TAG, "registering device (regId = " + regId + ")");      
        String serverUrl = SERVER_URL + "/register";
        Map<String, String> params = new HashMap<String, String>();
        params.put("regId", regId);
        params.put("user", user);
        params.put("password", password);
        long backoff = BACKOFF_MILLI_SECONDS + random.nextInt(1000);

        // Cuando GCM retorna un registration id, tenemos que registrarlo en el servidor
        // Como este servidor podria estar caido, intentaremos unas cuantas veces.
        for (int i = 1; i <= MAX_ATTEMPTS; i++) {
            Log.d(TAG, "Attempt #" + i + " to register");
            try {
            	displayMessage(context, context.getString(
                        R.string.server_registering, i, MAX_ATTEMPTS));
                post(serverUrl, params);
                String message = context.getString(R.string.server_registered);
                displayMessage(context, message);
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
                    // Actividad finalizada antes de completar.
                    Log.d(TAG, "Thread interrupted: abort remaining retries!");
                    Thread.currentThread().interrupt();
                    return false;
                }
                // incrementa el backoff exponencialmente
                backoff *= 2;
            }
        }
        String message = context.getString(R.string.server_register_error,
                MAX_ATTEMPTS);
        displayMessage(context, message);
        return false;
    }

    //Se envia un POST request al servidor
    private static void post(String endpoint, Map<String, String> params)
            throws IOException {
        URL url;
        try {
            url = new URL(endpoint);
        } catch (MalformedURLException e) {
            throw new IllegalArgumentException("invalid url: " + endpoint);
        }
        StringBuilder bodyBuilder = new StringBuilder();
        Iterator<Entry<String, String>> iterator = params.entrySet().iterator();
        // construye el cuerpo del mensaje POST usando los parametros.
        while (iterator.hasNext()) {
            Entry<String, String> param = iterator.next();
            bodyBuilder.append(param.getKey()).append('=')
                    .append(param.getValue());
            if (iterator.hasNext()) {
                bodyBuilder.append('&');
            }
        }
        String body = bodyBuilder.toString();
        Log.v(TAG, "Posting '" + body + "' to " + url);
        byte[] bytes = body.getBytes();
        HttpURLConnection conn = null;
        try {
            conn = (HttpURLConnection) url.openConnection();
            conn.setDoOutput(true);
            conn.setUseCaches(false);
            conn.setFixedLengthStreamingMode(bytes.length);
            conn.setRequestMethod("POST");
            conn.setRequestProperty("Content-Type",
                    "application/x-www-form-urlencoded;charset=UTF-8");
            // post a request
            OutputStream out = conn.getOutputStream();
            
            out.write(bytes);

            out.close();
            // Maneja la respuesta
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
