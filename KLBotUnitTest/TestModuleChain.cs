using klbotlib;
using klbotlib.MessageServer.Debug;
using klbotlib.Modules;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace KLBotUnitTest;

[TestClass]
public class TestModuleChain
{
    [TestMethod]
    public void TestAddModule()
    {
        //先添加后移除 结果应等于核心模块数量
        KLBot bot = new(new DebugMessageServer(), "config/unit_test_config.json");
        bot.AddModule(new TimeModule());
        Assert.AreEqual(TestConst.CoreModuleCount + 1, bot.ModuleChain.Count);
    }
    [TestMethod]
    public void TestIndexer()
    {
        KLBot bot = new(new DebugMessageServer(), "config/unit_test_config.json");
        var tm1 = new TimeModule();
        bot.AddModule(tm1);
        Assert.AreEqual(tm1, bot.ModuleChain[tm1.ModuleID]);
    }
}
