#pragma warning disable CS1591
using System;

namespace klbotlib.Extensions
{
    public static class TimeSpanExtension
    {
        public static string ToMsString(this TimeSpan time_span, int decimals = 4) => time_span.TotalMilliseconds.ToString("f" + decimals) + "ms";
    }
}
