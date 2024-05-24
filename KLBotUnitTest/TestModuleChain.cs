using klbotlib;
using klbotlib.MessageDriver.DebugLocal;
using klbotlib.Modules;
using Microsoft.Extensions.Configuration;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace KLBotUnitTest;

[TestClass]
public class TestModuleChain
{
    [TestMethod]
    public void TestAddModule()
    {
        MessageDriver_Debug driver = TestConst.GetTestDriver();
        //先添加后移除 结果应等于核心模块数量
        KLBot bot = new(TestConst.DefaultConfig, driver);
        bot.AddModule(new TimeModule()).Wait();
        Assert.AreEqual(TestConst.CoreModuleCount + 1, bot.ModuleChain.Count);
    }
    [TestMethod]
    public void TestIndexer()
    {
        MessageDriver_Debug driver = TestConst.GetTestDriver();
        KLBot bot = new(TestConst.DefaultConfig, driver);
        var tm1 = new TimeModule();
        bot.AddModule(tm1).Wait();
        Assert.AreEqual(tm1, bot.ModuleChain[tm1.ModuleID]);
    }
}
