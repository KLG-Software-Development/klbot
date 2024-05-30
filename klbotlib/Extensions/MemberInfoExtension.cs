#pragma warning disable CS1591
using klbotlib.Modules;
using System;
using System.Reflection;

namespace klbotlib.Extensions
{
    public static class MemberInfoExtension
    {
        public static bool ContainsAttribute(this MemberInfo info, Type attribute_type) => Attribute.GetCustomAttribute(info, attribute_type) != null;
        public static bool IsNonHiddenModuleStatus(this MemberInfo info)
        {
            var statusAttribute = Attribute.GetCustomAttribute(info, typeof(HiddenStatusAttribute)) as HiddenStatusAttribute;
            return statusAttribute != null;
        }
        public static bool TryGetValue(this MemberInfo info, object obj, out object? value)
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
    }
}
