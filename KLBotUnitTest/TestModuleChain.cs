using klbotlib;
using klbotlib.MessageClient.Debug;
using klbotlib.Modules;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace KLBotUnitTest;

[TestClass]
public class TestModuleChain
{
    [TestMethod]
    public void TestAddModule()
    {
        DebugMessageClient client = TestConst.GetTestClient();
        //先添加后移除 结果应等于核心模块数量
        KLBot bot = new(client, "config/unit_test_config.json");
        bot.AddModule(new TimeModule()).Wait();
        Assert.AreEqual(TestConst.CoreModuleCount + 1, bot.ModuleChain.Count);
    }
    [TestMethod]
    public void TestIndexer()
    {
        DebugMessageClient client = TestConst.GetTestClient();
        KLBot bot = new(client, "config/unit_test_config.json");
        var tm1 = new TimeModule();
        bot.AddModule(tm1).Wait();
        Assert.AreEqual(tm1, bot.ModuleChain[tm1.ModuleID]);
    }
}
