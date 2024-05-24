﻿using System;

namespace klbotlib.Extensions
{
    internal static class DateTimeExtension
    {
        private static readonly DateTime _origin =  new(1970, 1, 1, 0, 0, 0, 0, DateTimeKind.Utc);
        public static DateTime AsUnixTimestamp(this double timestamp)
            => _origin.AddSeconds(timestamp);
        public static DateTime AsUnixTimestamp(this long timestamp)
            => _origin.AddSeconds(timestamp);
    }
}
