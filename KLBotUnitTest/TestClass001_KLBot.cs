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
            Assert.AreEqual(test_bot.ModuleCount, 1, "模块链条初始长度不为1。核心库中引入了新的模块吗？");
            new TimeModule().AttachTo(test_bot);
            Assert.AreEqual(test_bot.ModuleCount, 2);
            var tm = new TimeModule();
            tm.AttachTo(test_bot);
            Assert.AreEqual(test_bot.ModuleCount, 3);
            tm.Detach();
            Assert.AreEqual(test_bot.ModuleCount, 2);
        }
    }
}
