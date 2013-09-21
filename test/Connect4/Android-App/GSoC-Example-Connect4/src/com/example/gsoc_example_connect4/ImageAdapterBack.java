package com.example.gsoc_example_connect4;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.GridView;
import android.widget.ImageView;

//Image adapter for the game's board.
public class ImageAdapterBack extends BaseAdapter {
    private Context mContext;
    Board main;
    int size;

    public ImageAdapterBack(Context c,Board d,int siz) {
        mContext = c;
        main = d;
        size = siz;
    }

    public int getCount() {
        return 42;
    }

    public Object getItem(int position) {
        return null;
    }

    public long getItemId(int position) {
        return 0;
    }

    public View getView(int position, View convertView, ViewGroup parent) {
        ImageView imageView;
        if (convertView == null) {
            imageView = new ImageView(mContext);
            imageView.setLayoutParams(new GridView.LayoutParams(size,size));
            imageView.setScaleType(ImageView.ScaleType.CENTER_CROP);
            imageView.setPadding(0,0,0,0);
        } else {
            imageView = (ImageView) convertView;
        }

        imageView.setImageResource(main.getPosition(position));
        return imageView;
    }
}

