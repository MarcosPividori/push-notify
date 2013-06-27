package com.example.gsoc_example;

import android.app.Activity;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.KeyEvent;
import android.view.View;
import android.view.inputmethod.EditorInfo;
import android.widget.EditText;
import android.widget.TextView;
import android.content.Intent;

// Activity to register user and password.
public class Register extends Activity {
	
	private String mUser;
	private String mPassword;

	// UI references.
	private EditText mUserView;
	private EditText mPasswordView;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		setContentView(R.layout.activity_register);

		mUserView = (EditText) findViewById(R.id.user);
		mUserView.setText(mUser);

		mPasswordView = (EditText) findViewById(R.id.password);
		mPasswordView
				.setOnEditorActionListener(new TextView.OnEditorActionListener() {
					@Override
					public boolean onEditorAction(TextView textView, int id,
							KeyEvent keyEvent) {
						if (id == R.id.login || id == EditorInfo.IME_NULL) {
							attemptLogin();
							return true;
						}
						return false;
					}
				});

		findViewById(R.id.sign_in_button).setOnClickListener(
				new View.OnClickListener() {
					@Override
					public void onClick(View view) {
						attemptLogin();
					}
				});
	}

	// Intent to register the specified account, if there is any error, (invalid user, field required, etc.)
	// the errors are shown and the registration does not success.
	public void attemptLogin() {

		// Reset errors.
		mUserView.setError(null);
		mPasswordView.setError(null);

		// Save values.
		mUser = mUserView.getText().toString();
		mPassword = mPasswordView.getText().toString();

		boolean cancel = false;
		View focusView = null;

		// Check for a valid password.
		if (TextUtils.isEmpty(mPassword)) {
			mPasswordView.setError(getString(R.string.error_field_required));
			focusView = mPasswordView;
			cancel = true;
		} else if (mPassword.length() < 4) {
			mPasswordView.setError(getString(R.string.error_invalid_password));
			focusView = mPasswordView;
			cancel = true;
		}

		// Check for a valid user.
		if (TextUtils.isEmpty(mUser)) {
			mUserView.setError(getString(R.string.error_field_required));
			focusView = mUserView;
			cancel = true;
		} else if (mUser.length() < 4) {
			mUserView.setError(getString(R.string.error_invalid_user));
			focusView = mUserView;
			cancel = true;
		}

		if (cancel) {
			// There is an error, so registration does not success and focus on the error.
			focusView.requestFocus();
		} else {
			// Send information to Main Activity.
			Intent i = getIntent();
			i.putExtra("USER",mUser);
			i.putExtra("PASSWORD",mPassword);
			setResult(RESULT_OK,i);
			finish();
		}
	}

}