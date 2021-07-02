using System;
using System.Diagnostics;
using System.Security.Cryptography;
using System.Text;
using System.Text.RegularExpressions;

namespace klbotlib
{

    public static class CryptoExtension
    {
        public static int Next(this RNGCryptoServiceProvider ro)
        {
            byte[] buffer = new byte[4];
            ro.GetBytes(buffer);
            return BitConverter.ToInt32(buffer, 0);
        }
        public static int Next(this RNGCryptoServiceProvider ro, int max)
        {
            byte[] buffer = new byte[4];
            ro.GetBytes(buffer);
            int i = BitConverter.ToInt32(buffer, 0);
            return i > 0 ? i % max : -i % max;
        }
        public static int Next(this RNGCryptoServiceProvider ro, int min, int max)
        {
            byte[] buffer = new byte[4];
            ro.GetBytes(buffer);
            return BitConverter.ToInt32(buffer, 0) % max + min;
        }
    }

}
