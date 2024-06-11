namespace klbotlib.Extensions;

/// <summary>
/// 字符串扩展
/// </summary>
public static class StringExtension
{
    /// <summary>
    /// 将超过指定长度限制的字符串的中间部分省略
    /// </summary>
    /// /// <param name="s">原始字符串</param>
    /// <param name="maxLength">长度上限</param>
    public static string Shorten(this string s, int maxLength)
    {
        return s.Length > maxLength ? string.Concat(s.AsSpan(0, maxLength), "...") : s;
    }
    private static readonly Dictionary<ConsoleColor, int> s_ansiColorCode = new()
    {
        { ConsoleColor.Red, 31 },
        { ConsoleColor.Green, 32 },
        { ConsoleColor.Yellow, 33 },
        { ConsoleColor.Magenta, 35 },
        { ConsoleColor.Cyan, 36 },
    };
    /// <summary>
    /// 为字符串包裹ANSI转义颜色
    /// </summary>
    /// <param name="value">字符串</param>
    /// <param name="color">颜色</param>
    public static string ToAnsiColor(this string value, ConsoleColor color)
    {
        return $"\x1b[{s_ansiColorCode[color]}m{value}\x1b[0m";
    }
}
