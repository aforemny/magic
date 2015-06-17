package org.nomath.magic;

import org.nomath.magic.R;

import android.app.Activity;
import android.os.Bundle;
import android.view.InputEvent;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnTouchListener;
import android.view.View.OnClickListener;
import android.view.GestureDetector;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.webkit.WebChromeClient;
import android.util.Log;

public class MainActivity extends Activity
{
    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);

        final WebView view = (WebView) findViewById(R.id.webview);
        view.setWebViewClient(new WebViewClient());
        view.getSettings().setJavaScriptEnabled(true);
        view.getSettings().setDomStorageEnabled(true);
        view.getSettings().setSupportZoom(false);
        view.getSettings().setBuiltInZoomControls(false);
        view.setWebChromeClient(new WebChromeClient() {
          public void onConsoleMessage(String msg, int ln, String id) {
            Log.d("MagicLife", msg + " -- From line " + ln + " of " + id);
          }
        });

        view.setOnTouchListener(new OnTouchListener() {

          @Override
          public boolean onTouch(View v, MotionEvent e) {
            if (e.getAction() == MotionEvent.ACTION_DOWN) {

              /*
              view.loadUrl(String.format(
                "javascript:callback('{type:\"click\",x:%d,y:%d}')",
                Math.round(e.getX()),
                Math.round(e.getY())
              ));
              */

            } else if (e.getAction() == MotionEvent.ACTION_MOVE) {

              /*
              view.loadUrl(String.format(
                "javascript:callback('{type:\"move\",x:%d,y:%d}')",
                Math.round(e.getX()),
                Math.round(e.getY())
              ));
              */

            }

            return false;
          }

        });
        view.loadUrl("file:///android_asset/index.html");
    }
}

