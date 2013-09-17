using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Navigation;
using Microsoft.Phone.Controls;
using Microsoft.Phone.Shell;
using System.Windows.Media;
using System.IO.IsolatedStorage;

namespace GSoC_Example
{
    public partial class Register : PhoneApplicationPage
    {
        private IsolatedStorageSettings userSettings = IsolatedStorageSettings.ApplicationSettings;
        public Register()
        {
            InitializeComponent();
        }
        private void LoginAppBarButton_Click(object sender, EventArgs e)
        {
            ;
        }
        void OnClick(object sender, RoutedEventArgs e)
        {
            if (UserTB.Text.Length > 4 && PassTB.Password.Length > 4){
                try
                {
                    if (userSettings.Contains("user"))
                        userSettings.Clear();
                    userSettings.Add("user", UserTB.Text);
                    userSettings.Add("pass", PassTB.Password);
                    userSettings.Save();
                    NavigationService.Navigate(new Uri("/MainPage.xaml", UriKind.Relative));
                }
                catch (System.Collections.Generic.KeyNotFoundException)
                {
                    UserTB.Text = ""; PassTB.Password = "";
                }
            }
            else
                Dispatcher.BeginInvoke(() => MessageBox.Show("User or Password too short!"));
        }

    }
}