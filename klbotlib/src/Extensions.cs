using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Runtime.Serialization.Formatters.Binary;
using System.Security.Cryptography;

namespace klbotlib
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
    internal static class CryptoExtension
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
    internal static class DictionaryExtension
    {
        public static byte[] Serialize(this Dictionary<string, object> dict)
        {
            var formatter = new BinaryFormatter();
            using (MemoryStream stream = new MemoryStream())
            {
                formatter.Serialize(stream, dict);
                return stream.ToArray();
            }
        }
        public static Dictionary<string, object> Deserialize(this byte[] bin)
        {
            var formatter = new BinaryFormatter();
            using (MemoryStream stream = new MemoryStream(bin))
            {
                return (Dictionary<string, object>)formatter.Deserialize(stream);
            }
        }
    }
    internal static class UnitStringExtension
    {
        //Long bytes -> memory unit
        private static readonly string[] mem_units = new string[] { "B", "KB", "MB", "GB" }; 
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

        //double/long ms -> time unit
        private static readonly string[] time_units = new string[] { "毫秒", "秒", "分钟", "小时", "天" };
        private static readonly long[] time_factors = new long[] { 1000, 60, 60, 24, long.MaxValue };
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
    internal static class TypeExtension
    {
        public static Type GetRootBaseType(this Type type)
        {
            Type base_type = type;
            while (base_type.BaseType != typeof(object))
            {
                base_type = base_type.BaseType;
            }
            return base_type;
        }
        public static PropertyInfo GetProperty_All(this Type type, string name) 
            => type.GetProperty(name, BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
        public static FieldInfo GetField_All(this Type type, string name)
            => type.GetField(name, BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
        public static PropertyInfo[] GetProperties_All(this Type type) 
            => type.GetProperties(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
        public static FieldInfo[] GetFields_All(this Type type)
            => type.GetFields(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
    }
    internal static class MemberInfoExtension
    {
        public static bool ContainsAttribute(this MemberInfo info, Type attribute_type) => Attribute.GetCustomAttribute(info, attribute_type) != null;
    }
}
