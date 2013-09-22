package com.example.gsoc_example_connect4;

import java.util.List;

import android.app.Activity;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.support.v4.app.DialogFragment;
import android.widget.ArrayAdapter;

//Simple dialog to select a user to start a new game.
public class SelectUser extends DialogFragment {
	MainActivity activityHost;
	List<String> list;
	Boolean choosen=false;
	@Override
    public Dialog onCreateDialog(Bundle savedInstanceState) {        
		AlertDialog.Builder builder = new AlertDialog.Builder(getActivity());
		list = activityHost.getListUsers();
    	ArrayAdapter<String> usersList = new ArrayAdapter<String>(getActivity(), 
    	        android.R.layout.simple_list_item_1,list);
    	
    	if(list == null){
    		builder.setMessage(R.string.problem_get_users)
    			   .setNegativeButton("Cancel"   ,new DialogInterface.OnClickListener() {
    				   public void onClick(DialogInterface dialog, int which) {
    					   ;
    				   }
    			   })
    			   .setPositiveButton("Try again", new DialogInterface.OnClickListener() {
    				   public void onClick(DialogInterface dialog, int which) {
    					   activityHost.choosePlayer();
    				   }
    			   });
    	}
    	else{
    		builder.setTitle(R.string.choose_player)
    			   .setAdapter(usersList,new DialogInterface.OnClickListener() {
    				   public void onClick(DialogInterface dialog, int which) {
    					   activityHost.resultChoose(which);
    					   choosen = true;
    				   }
    			   });
    	}
        return builder.create();
    }
	
	@Override
	public void onPause(){
		super.onPause();
		if(!choosen)
			activityHost.resultChoose(-1);
	}
	
	
	@Override
	public void onDestroyView(){
		super.onDestroyView();
		if(list == null)
			activityHost.resultChoose(-1);
	}

	
	@Override
    public void onAttach(Activity activity) {
        super.onAttach(activity);
        activityHost = (MainActivity) activity;
    }
}