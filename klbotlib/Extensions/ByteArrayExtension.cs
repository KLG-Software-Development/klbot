#pragma warning disable CS1591
namespace klbotlib.Extensions;

public static class ByteArrayExtension
{
    public static string ToHexString(this byte[] buffer)
    {
        return string.Join(string.Empty, buffer.Select(b => b.ToString("x2")));
    }
}
