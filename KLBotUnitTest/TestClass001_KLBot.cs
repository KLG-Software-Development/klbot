using klbotlib;
using klbotlib.Modules;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace KLBotUnitTest
{
    [TestClass]
    public class TestClass001_KLBot
    {
        /// <summary>
        /// 测试模块数量是否返回预期值
        /// </summary>
        [TestMethod]
        public void TestMethod000_ModuleCount()
        {
            KLBot test_bot = new();
            Assert.AreEqual(TestConst.CoreModuleCount, test_bot.ModuleCount);
            new TimeModule().AttachTo(test_bot);
            Assert.AreEqual(TestConst.CoreModuleCount + 1, test_bot.ModuleCount);
            var tm = new TimeModule();
            tm.AttachTo(test_bot);
            Assert.AreEqual(TestConst.CoreModuleCount + 2, test_bot.ModuleCount);
            tm.Detach();
            Assert.AreEqual(TestConst.CoreModuleCount + 1, test_bot.ModuleCount);
        }
    }
}
