namespace klbotlib.Extensions;

internal static class NumberExtension
{
    private static readonly DateTime s_origin = new(1970, 1, 1, 0, 0, 0, 0, DateTimeKind.Utc);
    public static DateTime AsUnixTimestamp(this double timestamp)
        => s_origin.AddSeconds(timestamp).ToLocalTime();
    public static DateTime AsUnixTimestamp(this long timestamp)
        => s_origin.AddSeconds(timestamp).ToLocalTime();
}
