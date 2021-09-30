using Newtonsoft.Json;
using System;
using System.ComponentModel;
using System.Reflection;

namespace klbotlib.Reflection
{
    public static class ReflectionHelper
    {
        private static TimeSpanConverter _timespan_converter = new TimeSpanConverter();
        /// <summary>
        /// 预存需要通过反射遍历查找的方法
        /// </summary>
        public static class PrefetchMethods
        {
            public static MethodInfo JsonConvert_DeserializeObject_T;
            static PrefetchMethods()
            {
                MethodInfo[] methods = typeof(JsonConvert).GetMethods();
                bool is_found = false;
                foreach (var method in methods)
                {
                    if (method.Name == "DeserializeObject" && method.IsGenericMethod)
                    {
                        JsonConvert_DeserializeObject_T = method;
                        is_found = true;
                        break;
                    }
                }
                if (!is_found)
                    throw new Exception("意外遇到反射异常：无法找到相应的方法。Newtonsoft.Json的API是否有所更改？");
            }
        }

        /// <summary>
        /// NewtonSoft.JsonConvert会把一切整数变成int64，一切浮点数变成double
        /// 丫这么整虽然源码赋值没事(会自动转换)，但反射赋值时会出问题，所以需要手动恢复原本的类型
        /// v0.5更新：加入自动用泛型反序列化进一步处理其他未知类型的功能
        /// </summary>
        /// <param name="original_type">原始类型</param>
        /// <param name="value">待处理对象</param>
        /// <returns>转换为原始类型后的对象（如果无需转换则原样返回）</returns>
        public static object RestoreType(Type original_type, object value)
        {
            if (value.GetType() == original_type)
                return value;
            else if (original_type == typeof(byte) ||
                original_type == typeof(short) ||
                original_type == typeof(int) ||
                original_type == typeof(long) ||
                original_type == typeof(double) ||
                original_type == typeof(float))
                return Convert.ChangeType(value, original_type);
            else if (original_type == typeof(TimeSpan))
                return _timespan_converter.ConvertFromString(value.ToString());
            else
            {
                MethodInfo method = PrefetchMethods.JsonConvert_DeserializeObject_T;
                var deserialize = method.MakeGenericMethod(original_type);
                string json = value.ToString();
                object output = null;
                try    //尝试用Newtonsoft.Json自动转换
                {
                    deserialize.Invoke(null, new object[] { json });
                }
                catch
                {
                    throw new Exception($"RestoreType()遇到无法自动转换的类型。需要向框架中手动添加对\"{original_type.FullName}\"类型的转换支持");
                }
                return output;
            }
        }
    }

}
