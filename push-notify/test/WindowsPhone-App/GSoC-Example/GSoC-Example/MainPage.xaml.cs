using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.IO;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Navigation;
using System.Runtime.Serialization.Json;
using System.Threading;
using Microsoft.Phone.Controls;
using Microsoft.Phone.Shell;
using Microsoft.Phone.Notification;
using GSoC_Example.Resources;
using System.IO.IsolatedStorage;

namespace GSoC_Example
{
    public partial class MainPage : PhoneApplicationPage
    {
        int MAX_ATTEMPTS = 5;
        private IsolatedStorageSettings userSettings = IsolatedStorageSettings.ApplicationSettings;
        private Boolean registered = false;
        private String user = "", password = "" , uri = "";

        public MainPage()
        {
            /// Holds the push channel that is created or found.
            HttpNotificationChannel pushChannel;

            // The name of our push channel.
            string channelName = "SampleChannel";
            
            InitializeComponent();
            
            try
            {
                user       = (string)userSettings["user"];
                password   = (string)userSettings["pass"];
                registered = (Boolean)userSettings["registered"];
            }
            catch (System.Collections.Generic.KeyNotFoundException)
            {
                ;
            }

            try
            {
                string console = (string)userSettings["console"];
                UpdateConsole(console);
            }
            catch (System.Collections.Generic.KeyNotFoundException)
            {
                ;
            }
            // Try to find the push channel.
            pushChannel = HttpNotificationChannel.Find(channelName);

            // If the channel was not found, then create a new connection to the push service.
            if (pushChannel == null)
            {
                UpdateConsole("Creating a new connection to the push service.");
                pushChannel = new HttpNotificationChannel(channelName);

                // Register for all the events before attempting to open the channel.
                pushChannel.ChannelUriUpdated += new EventHandler<NotificationChannelUriEventArgs>(PushChannel_ChannelUriUpdated);
                pushChannel.ErrorOccurred += new EventHandler<NotificationChannelErrorEventArgs>(PushChannel_ErrorOccurred);
                pushChannel.HttpNotificationReceived += new EventHandler<HttpNotificationEventArgs>(PushChannel_HttpNotificationReceived);

                pushChannel.Open();
                
                // Register for this notification only if you need to receive the notifications while your application is running.
                pushChannel.ShellToastNotificationReceived += new EventHandler<NotificationEventArgs>(PushChannel_ShellToastNotificationReceived);
                
                // Bind this new channel for toast events.
                pushChannel.BindToShellToast();

                // Bind this new channel for Tile events.
                pushChannel.BindToShellTile();
            }
            else
            {
                // The channel was already open, so just register for all the events.
                pushChannel.ChannelUriUpdated += new EventHandler<NotificationChannelUriEventArgs>(PushChannel_ChannelUriUpdated);
                pushChannel.ErrorOccurred += new EventHandler<NotificationChannelErrorEventArgs>(PushChannel_ErrorOccurred);
                pushChannel.HttpNotificationReceived += new EventHandler<HttpNotificationEventArgs>(PushChannel_HttpNotificationReceived);

                // Register for this notification to receive the notifications while your application is running.
                pushChannel.ShellToastNotificationReceived += new EventHandler<NotificationEventArgs>(PushChannel_ShellToastNotificationReceived);

                
                if (!registered)
                {
                    uri = pushChannel.ChannelUri.ToString();
                    register();
                }

            }

        }

        private void SaveConsole()
        {
            if (userSettings.Contains("console"))
                userSettings.Remove("console");
            userSettings.Add("console", console.Text);
            userSettings.Save();
        }

        private void UpdateConsole(string str)
        {
            Deployment.Current.Dispatcher.BeginInvoke(delegate()
            {
                console.Text = console.Text + "\n" + str;
            });
        }

        void PushChannel_ChannelUriUpdated(object sender, NotificationChannelUriEventArgs e)
        {
            UpdateConsole("New channel Uri: " + e.ChannelUri.ToString());
            uri = e.ChannelUri.ToString();
            register();
        }

        void PushChannel_ErrorOccurred(object sender, NotificationChannelErrorEventArgs e)
        {
            // Error handling logic for your particular application would be here.
            UpdateConsole(String.Format("A push notification {0} error occurred.  {1} ({2}) {3}",
                    e.ErrorType, e.Message, e.ErrorCode, e.ErrorAdditionalData));
        }

        void PushChannel_HttpNotificationReceived(object sender, HttpNotificationEventArgs e)
        {
            string message;

            using (System.IO.StreamReader reader = new System.IO.StreamReader(e.Notification.Body))
            {
                message = reader.ReadToEnd();
            }

            UpdateConsole(String.Format("Received Notification {0}:\n{1}",
                    DateTime.Now.ToShortTimeString(), message));
        }

        void PushChannel_ShellToastNotificationReceived(object sender, NotificationEventArgs e)
        {
            if (e.Collection.ContainsKey("wp:Param"))
            {
                UpdateConsole(String.Format("Received [{0}]: {1}", DateTime.Now.ToShortTimeString(), e.Collection["wp:Param"].Substring(5)));
            }
        }

        protected override void OnNavigatedTo(System.Windows.Navigation.NavigationEventArgs e)
        {
            base.OnNavigatedTo(e);
            try
            {
                string msg = this.NavigationContext.QueryString["msg"];
                UpdateConsole(String.Format("Received [{0}]: {1}\n", DateTime.Now.ToShortTimeString(), msg));
            }
            catch (System.Collections.Generic.KeyNotFoundException)
            {
                ;
            }
        }

        protected override void OnNavigatedFrom(System.Windows.Navigation.NavigationEventArgs e)
        {
            SaveConsole();
        }

        Boolean register()
        {
            Thread t = new Thread((ThreadStart)delegate
            {
            Random random = new Random();
            int backoff = 2000 + random.Next(1000);
            for (int i = 1; i <= MAX_ATTEMPTS; i++) {
                try {
                    UpdateConsole(String.Format("Trying (attempt {0}/{1}) to register device on Demo Server.",
                    i, MAX_ATTEMPTS));
                    if (SendUserInfo())
                    {
                        UpdateConsole("From Demo Server: successfully added device!");
                        if (userSettings.Contains("registered"))
                            userSettings.Remove("registered");
                        userSettings.Add("registered",true);
                        registered = true;
                        break;
                    }
                    else
                    {
                        Thread.Sleep(backoff);
                        // increase backoff exponentially.
                        backoff *= 2;
                    }
                } catch (WebException e) {
                    UpdateConsole("Exception sending: " + e.Message);
                    if (i == MAX_ATTEMPTS) {
                        break;
                    }
                }
            }
            if(!registered)
                UpdateConsole(String.Format("Could not register device on Demo Server after {0} attempts.", MAX_ATTEMPTS));
            });
            t.Start();
            return true;
        }
        
        public static ManualResetEvent allDone = new ManualResetEvent(false);
        
        public class RequestState
        {
            public HttpWebRequest request;
            public Boolean success;

            public RequestState()
            {
                request = null;
                success = false;
            }
        }

        Boolean SendUserInfo()
        {                          
                Uri url = new Uri("http://192.168.0.52:3000/register");
                RequestState myRequestState = new RequestState();
                HttpWebRequest request = (HttpWebRequest) WebRequest.Create(url);
                myRequestState.request = request;
                request.Method = "POST";
                request.ContentType = "application/x-www-form-urlencoded;charset=UTF-8";
                request.BeginGetRequestStream(new AsyncCallback(GetRequestStreamCallback), myRequestState);
                
                allDone.WaitOne();
                return myRequestState.success;
        }

        void GetRequestStreamCallback(IAsyncResult asynchronousResult)
        {
            try
            {
                RequestState myRequestState = (RequestState) asynchronousResult.AsyncState;
                HttpWebRequest webRequest = myRequestState.request;
                // End the stream request operation
                Stream postStream = webRequest.EndGetRequestStream(asynchronousResult);

                StringBuilder bodyBuilder = new StringBuilder();
                // constructs the POST body using the parameters
                bodyBuilder.Append("user").Append('=').Append(user)
                           .Append('&')
                           .Append("password").Append('=').Append(password)
                           .Append('&')
                           .Append("regId").Append('=').Append(uri)
                           .Append('&')
                           .Append("system").Append('=').Append("WPhone");
                String body = bodyBuilder.ToString();
                
                byte[] bytes = Encoding.UTF8.GetBytes(body);

                postStream.Write(bytes, 0, bytes.Length);
                postStream.Close();

                // Start the web request
                webRequest.BeginGetResponse(new AsyncCallback(GetResponseCallback), myRequestState);
            }
            catch (WebException e)
            {
                UpdateConsole("Exception sending: " + e.Message);
                allDone.Set();
            }
        }

        void GetResponseCallback(IAsyncResult asynchronousResult)
        {
            try
            {
                RequestState myRequestState = (RequestState)asynchronousResult.AsyncState;
                HttpWebRequest webRequest = myRequestState.request;
                HttpWebResponse response = (HttpWebResponse)webRequest.EndGetResponse(asynchronousResult);

                if (response.StatusCode != HttpStatusCode.OK)
                {
                    throw new WebException(response.StatusCode.ToString());
                }
                else
                {
                    myRequestState.success = true;
                }
                response.Close();
                allDone.Set();
            }
            catch (WebException e)
            {
                UpdateConsole("Exception getting response: " + e.Message);
                allDone.Set();
            }
        }

        private void ClearButton_Click(object sender, EventArgs e)
        {
            console.Text = "";
        }

        private void InfoButton_Click(object sender, EventArgs e)
        {
            Dispatcher.BeginInvoke(() => MessageBox.Show("This app is a very simple example to show the MPNS registration and reception of notifications from a Yesod Server.\nThis was developed as part of the GSoC project: \"Communicating with Mobile Devices\""));
        }

        private void ExitButton_Click(object sender, EventArgs e)
        {
            ;
        }
        
    }

}