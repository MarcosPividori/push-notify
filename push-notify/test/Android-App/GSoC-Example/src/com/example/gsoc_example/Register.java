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

		//mUser = getIntent().getStringExtra(EXTRA_EMAIL);
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

	// Intento de registrar la cuenta especificada, si hay errores, (email invalido, campos sin completar, etc.)
	// los errores son presentados y no se completa el registro.
	public void attemptLogin() {

		// Resetear errores.
		mUserView.setError(null);
		mPasswordView.setError(null);

		// Guarda valores al intentar registrar.
		mUser = mUserView.getText().toString();
		mPassword = mPasswordView.getText().toString();

		boolean cancel = false;
		View focusView = null;

		// Chequea por una valida password.
		if (TextUtils.isEmpty(mPassword)) {
			mPasswordView.setError(getString(R.string.error_field_required));
			focusView = mPasswordView;
			cancel = true;
		} else if (mPassword.length() < 4) {
			mPasswordView.setError(getString(R.string.error_invalid_password));
			focusView = mPasswordView;
			cancel = true;
		}

		// Chequea por un valido usuario.
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
			// Hubo un error, no se completa el registro y se enfoca en el primer error descubierto.
			focusView.requestFocus();
		} else {
			// Envio de la informacion
			Intent i = getIntent();
			i.putExtra("USER",mUser);
			i.putExtra("PASSWORD",mPassword);
			setResult(RESULT_OK,i);
			finish();
		}
	}

}