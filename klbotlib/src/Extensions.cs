using klbotlib.Modules;
using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Runtime.Serialization.Formatters.Binary;
using System.Security.Cryptography;

namespace klbotlib.Extensions
{
    /// <summary>
    /// Version扩展
    /// </summary>
    public static class VersionExtension
    {
        /// <summary>
        /// 从版本号计算KLG标准的Build编号
        /// </summary>
        /// <param name="version">版本号</param>
        /// <returns>KLG标准的Build编号</returns>
        public static string ToKLGBuildString(this Version version)
        {
            TimeSpan seconds = new TimeSpan(0, 0, version.Revision * 2);
            return version.Build + "_" + seconds.ToString("hhmm");
        }
    }
#pragma warning disable CS1591
    public static class DictionaryExtension
    {
        /// <summary>
        /// 将字符串字典序列化为二进制
        /// </summary>
        /// <param name="dict">待序列化字典</param>
        public static byte[] Serialize(this Dictionary<string, object> dict)
        {
            var formatter = new BinaryFormatter();
            using (MemoryStream stream = new MemoryStream())
            {
                formatter.Serialize(stream, dict);
                return stream.ToArray();
            }
        }
        /// <summary>
        /// 将二进制反序列化为字典
        /// </summary>
        /// <param name="bin">待反序列化字节数组</param>
        public static Dictionary<string, object> Deserialize(this byte[] bin)
        {
            var formatter = new BinaryFormatter();
            using (MemoryStream stream = new MemoryStream(bin))
            {
                return (Dictionary<string, object>)formatter.Deserialize(stream);
            }
        }
    }
    public static class UnitStringExtension
    {
        //Long bytes -> memory unit
        private static readonly string[] mem_units = new string[] { "B", "KB", "MB", "GB" }; 
        /// <summary>
        /// 将字节数自动转换为合适数据单位的字符串
        /// </summary>
        /// <param name="byte_count">字节数量</param>
        /// <param name="decimals">小数位数</param>
        public static string ToMemorySizeString(this long byte_count, int decimals)
        {
            int unit_index = 0;
            double value = byte_count;
            while (value > 1024f && unit_index < mem_units.Length)
            {
                unit_index++;
                value /= 1024f;
            }
            return value.ToString($"f{decimals}") + mem_units[unit_index];
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
            while (value > 1024f && unit_index < mem_units.Length)
            {
                unit_index++;
                value /= 1024f;
            }
            return value.ToString($"f{decimals}") + mem_units[unit_index];
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
    public static class TypeExtension
    {
        /// <summary>
        /// 获取除object以外最基本的基类
        /// </summary>
        /// <param name="type">类型</param>
        public static Type GetRootBaseType(this Type type)
        {
            Type base_type = type;
            while (base_type != null && base_type.BaseType != typeof(object))
            {
                base_type = base_type.BaseType;
            }
            return base_type;
        }
        /// <summary>
        /// 获取所有属性字段
        /// </summary>
        /// <param name="type">类型</param>
        /// <param name="name">属性字段名称</param>
        public static PropertyInfo GetProperty_All(this Type type, string name) 
            => type.GetProperty(name, BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
        public static FieldInfo GetField_All(this Type type, string name)
            => type.GetField(name, BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
        public static PropertyInfo[] GetProperties_All(this Type type) 
            => type.GetProperties(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
        public static FieldInfo[] GetFields_All(this Type type)
            => type.GetFields(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
    }
    public static class MemberInfoExtension
    {
        public static bool ContainsAttribute(this MemberInfo info, Type attribute_type) => Attribute.GetCustomAttribute(info, attribute_type) != null;
        public static bool IsNonHiddenModuleStatus(this MemberInfo info)
        {
            var status_attribute = Attribute.GetCustomAttribute(info, typeof(ModuleStatusAttribute)) as ModuleStatusAttribute;
            return status_attribute != null && !status_attribute.IsHidden;
        }
        public static bool TryGetValue(this MemberInfo info, object obj, out object value)
        {
            value = null;
            if (info is FieldInfo fi)
            {
                value = fi.GetValue(obj);
                return true;
            }
            else if (info is PropertyInfo pi)
            {
                value = pi.GetValue(obj);
                return true;
            }
            return false;
        }
        public static bool TrySetValue(this MemberInfo info, object obj, object value)
        {
            value = null;
            if (info is FieldInfo fi)
            {
                fi.SetValue(obj, value);
                return true;
            }
            else if (info is PropertyInfo pi)
            {
                pi.SetValue(obj, value);
                return true;
            }
            return false;
        }
    }
    public static class TimeSpanExtension
    {
        public static string ToMsString(this TimeSpan time_span, int decimals = 4) => time_span.TotalMilliseconds.ToString("f" + decimals) + "ms";
    }
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
    public static class StringExtension
    {
        /// <summary>
        /// 将超过指定长度限制的字符串的中间部分省略
        /// </summary>
        /// /// <param name="s">原始字符串</param>
        /// <param name="max_length">长度上限</param>
        public static string Shorten(this string s, int max_length)
        {
            if (s.Length > max_length)
                return s.Substring(0, max_length) + "...";
            else
                return s;
        }
    }
#pragma warning restore CS1591 
}
