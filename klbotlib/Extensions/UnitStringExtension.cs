#pragma warning disable CS1591

namespace klbotlib.Extensions
{
    public static class UnitStringExtension
    {
        //Long bytes -> memory unit
        private static readonly string[] _memUnits = new string[] { "B", "KB", "MB", "GB" }; 
        /// <summary>
        /// 将字节数自动转换为合适数据单位的字符串
        /// </summary>
        /// <param name="byte_count">字节数量</param>
        /// <param name="decimals">小数位数</param>
        public static string ToMemorySizeString(this long byte_count, int decimals)
        {
            int unit_index = 0;
            double value = byte_count;
            while (value > 1024f && unit_index < _memUnits.Length)
            {
                unit_index++;
                value /= 1024f;
            }
            return value.ToString($"f{decimals}") + _memUnits[unit_index];
        }
        /// <summary>
        /// 将字节数自动转换为合适数据单位的字符串
        /// </summary>
        /// <param name="byte_count">字节数量</param>
        /// <param name="decimals">小数位数</param>
        public static string ToMemorySizeString(this int byte_count, int decimals)
        {
            int unit_index = 0;
            double value = byte_count;
            while (value > 1024f && unit_index < _memUnits.Length)
            {
                unit_index++;
                value /= 1024f;
            }
            return value.ToString($"f{decimals}") + _memUnits[unit_index];
        }

        //double/long ms -> time unit
        private static readonly string[] time_units = new string[] { "毫秒", "秒", "分钟", "小时", "天" };
        private static readonly long[] time_factors = new long[] { 1000, 60, 60, 24, long.MaxValue };
        /// <summary>
        /// 将毫秒数自动转换为合适时间单位的字符串
        /// </summary>
        /// <param name="ms">毫秒数</param>
        /// <param name="decimals">小数位数</param>
        public static string ToTimeSpanString(this long ms, int decimals)
        {
            int unit_index = 0;
            double value = ms;
            while (value > time_factors[unit_index] && unit_index < time_units.Length)
            {
                value /= time_factors[unit_index];
                unit_index++;
            }
            return value.ToString($"f{decimals}") + time_units[unit_index];
        }
        /// <summary>
        /// 将毫秒数自动转换为合适时间单位的字符串
        /// </summary>
        /// <param name="ms">毫秒数</param>
        /// <param name="decimals">小数位数</param>
        public static string ToTimeSpanString(this double ms, int decimals)
        {
            int unit_index = 0;
            double value = ms;
            while (value > time_factors[unit_index] && unit_index < time_units.Length)
            {
                value /= time_factors[unit_index];
                unit_index++;
            }
            return value.ToString($"f{decimals}") + time_units[unit_index];
        }
    }
#pragma warning restore CS1591 
}
