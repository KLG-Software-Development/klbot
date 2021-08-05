using klbotlib;
using klbotlib.Modules;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace KLBotUnitTest
{
    [TestClass]
    public class TestClass001_Module
    {
        // 测试模块IsAttach属性
        [TestMethod]
        public void TestMethod000_IsAttach()
        {
            KLBot test_bot = new();
            var tm = new TimeModule();
            Assert.AreEqual(false, tm.IsAttached, "模块初始情况下模块应处于未附加状态（IsAttach=false）");
            test_bot.AddModule(tm);
            Assert.AreEqual(true, tm.IsAttached, "调用AddModule()后模块应处于附加状态（IsAttach=true）");
        }
    }
}
