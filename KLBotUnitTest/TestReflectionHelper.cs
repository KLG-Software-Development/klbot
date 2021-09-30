using klbotlib;
using klbotlib.Modules;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using System.IO;
using System.Threading;
using klbotlib.Reflection;
using System;
using System.Text.RegularExpressions;

namespace KLBotUnitTest
{
    [TestClass]
    public class TestReflectionHelper
    {
        /// <summary>
        /// 测试反射帮助类
        /// </summary>
        [TestMethod]
        public void RestoreTypes()
        {
            string test_str = "test";
            //原地还原
            Assert.IsTrue((string)ReflectionHelper.RestoreType(typeof(string), test_str) == test_str);
            //还原数字
            Assert.IsTrue((byte)ReflectionHelper.RestoreType(typeof(byte), "42") == 42);
            Assert.IsTrue((short)ReflectionHelper.RestoreType(typeof(short), "42") == 42);
            Assert.IsTrue((int)ReflectionHelper.RestoreType(typeof(int), "42") == 42);
            Assert.IsTrue((long)ReflectionHelper.RestoreType(typeof(long), "42") == 42L);
            //还原TimeSpan
            Assert.IsTrue(((TimeSpan)ReflectionHelper.RestoreType(typeof(TimeSpan), "01:00:00")).TotalMilliseconds == new TimeSpan(1,0,0).TotalMilliseconds);
            //还原Regex
            Regex regex = new Regex(@"^t|e|s|t\n$");
            Regex regex_restore = (Regex)ReflectionHelper.RestoreType(typeof(Regex), JsonConvert.SerializeObject(regex));
            Assert.IsTrue(regex.ToString() == regex_restore.ToString());
        }
    }
}
