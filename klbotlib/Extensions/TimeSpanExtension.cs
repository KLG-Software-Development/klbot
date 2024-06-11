#pragma warning disable CS1591
namespace klbotlib.Extensions;

public static class TimeSpanExtension
{
    public static string ToMsString(this TimeSpan timeSpan, int decimals = 4) => timeSpan.TotalMilliseconds.ToString("f" + decimals) + "ms";
}
