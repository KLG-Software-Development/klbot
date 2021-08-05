using klbotlib;
using klbotlib.Modules;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace KLBotUnitTest
{
    [TestClass]
    public class TestClass000_ModuleChain
    {
        [TestMethod]
        public void TestMethod000_AddModule()
        {
            //先添加后移除 结果应等于核心模块数量
            KLBot bot = new();
            bot.AddModule(new TimeModule());
            Assert.AreEqual(TestConst.CoreModuleCount + 1, bot.ModuleChain.Count);
        }
        [TestMethod]
        public void TestMethod001_Indexer()
        {
            KLBot bot = new();
            var tm1 = new TimeModule();
            bot.AddModule(tm1);
            Assert.AreEqual(tm1, bot.ModuleChain[tm1.ModuleID]);
        }
    }
}
