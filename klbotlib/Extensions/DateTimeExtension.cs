using System;

namespace klbotlib.Extensions
{
    internal static class DateTimeExtension
    {
        public static string ToKLBotTimestampString(this DateTime dateTime)
            => dateTime.ToString("yyMMdd/HH:mm:ss.fff");
    }
}
