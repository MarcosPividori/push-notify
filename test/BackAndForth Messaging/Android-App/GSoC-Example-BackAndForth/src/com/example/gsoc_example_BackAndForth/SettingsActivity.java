package com.example.gsoc_example_BackAndForth;

import android.os.Bundle;
import android.preference.PreferenceActivity;
import com.example.gsoc_example_BackAndForth.R;

public class SettingsActivity extends PreferenceActivity {
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        addPreferencesFromResource(R.xml.preferences);
    }
}
