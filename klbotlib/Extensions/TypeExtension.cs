#pragma warning disable CS1591
using System;
using System.Reflection;

namespace klbotlib.Extensions
{
    public static class TypeExtension
    {
        /// <summary>
        /// 获取除object以外最基本的基类
        /// </summary>
        /// <param name="type">类型</param>
        public static Type GetRootBaseType(this Type type)
        {
            Type? baseType = type;
            while (baseType != null && baseType.BaseType != typeof(object))
            {
                baseType = baseType.BaseType;
            }
            if (baseType == null)
                throw new NullReferenceException("获取次根基类时意外遭遇null");
            return baseType;
        }
        /// <summary>
        /// 获取所有属性字段
        /// </summary>
        /// <param name="type">类型</param>
        /// <param name="name">属性字段名称</param>
        public static PropertyInfo? GetProperty_All(this Type type, string name) 
            => type.GetProperty(name, BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
        public static FieldInfo? GetField_All(this Type type, string name)
            => type.GetField(name, BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
        public static PropertyInfo[] GetProperties_All(this Type type) 
            => type.GetProperties(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
        public static FieldInfo[] GetFields_All(this Type type)
            => type.GetFields(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
    }
}
