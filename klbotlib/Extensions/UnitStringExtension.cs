#pragma warning disable CS1591

namespace klbotlib.Extensions;

public static class UnitStringExtension
{
    //Long bytes -> memory unit
    private static readonly string[] s_memUnits = ["B", "KB", "MB", "GB"];
    /// <summary>
    /// 将字节数自动转换为合适数据单位的字符串
    /// </summary>
    /// <param name="byteCount">字节数量</param>
    /// <param name="decimals">小数位数</param>
    public static string ToMemorySizeString(this long byteCount, int decimals)
    {
        int unitIndex = 0;
        double value = byteCount;
        while (value > 1024f && unitIndex < s_memUnits.Length)
        {
            unitIndex++;
            value /= 1024f;
        }
        return value.ToString($"f{decimals}") + s_memUnits[unitIndex];
    }
    /// <summary>
    /// 将字节数自动转换为合适数据单位的字符串
    /// </summary>
    /// <param name="byteCount">字节数量</param>
    /// <param name="decimals">小数位数</param>
    public static string ToMemorySizeString(this int byteCount, int decimals)
    {
        int unitIndex = 0;
        double value = byteCount;
        while (value > 1024f && unitIndex < s_memUnits.Length)
        {
            unitIndex++;
            value /= 1024f;
        }
        return value.ToString($"f{decimals}") + s_memUnits[unitIndex];
    }

    //double/long ms -> time unit
    private static readonly string[] s_timeUnits = ["毫秒", "秒", "分钟", "小时", "天"];
    private static readonly long[] s_timeFactors = [1000, 60, 60, 24, long.MaxValue];
    /// <summary>
    /// 将毫秒数自动转换为合适时间单位的字符串
    /// </summary>
    /// <param name="ms">毫秒数</param>
    /// <param name="decimals">小数位数</param>
    public static string ToTimeSpanString(this long ms, int decimals)
    {
        int unitIndex = 0;
        double value = ms;
        while (value > s_timeFactors[unitIndex] && unitIndex < s_timeUnits.Length)
        {
            value /= s_timeFactors[unitIndex];
            unitIndex++;
        }
        return value.ToString($"f{decimals}") + s_timeUnits[unitIndex];
    }
    /// <summary>
    /// 将毫秒数自动转换为合适时间单位的字符串
    /// </summary>
    /// <param name="ms">毫秒数</param>
    /// <param name="decimals">小数位数</param>
    public static string ToTimeSpanString(this double ms, int decimals)
    {
        int unitIndex = 0;
        double value = ms;
        while (value > s_timeFactors[unitIndex] && unitIndex < s_timeUnits.Length)
        {
            value /= s_timeFactors[unitIndex];
            unitIndex++;
        }
        return value.ToString($"f{decimals}") + s_timeUnits[unitIndex];
    }
}
