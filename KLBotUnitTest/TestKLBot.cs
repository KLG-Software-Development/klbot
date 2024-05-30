using klbotlib;
using klbotlib.Modules;
using klbotlib.Extensions;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace KLBotUnitTest;

[TestClass]
public class TestKLBot
{
    /// <summary>
    /// 测试模块数量
    /// </summary>
    [TestMethod]
    public void TestModuleCount()
    {
        KLBot bot = new(TestConst.DefaultConfig, TestConst.GetTestDriver());
        Assert.AreEqual(TestConst.CoreModuleCount, bot.ModuleCount);
        bot.AddModule(new TimeModule()).Wait();
        Assert.AreEqual(TestConst.CoreModuleCount + 1, bot.ModuleCount);
        var tm = new TimeModule();
        bot.AddModule(tm).Wait();
        Assert.AreEqual(TestConst.CoreModuleCount + 2, bot.ModuleCount);
    }
    /// <summary>
    /// 测试KLBot诊断数据
    /// </summary>
    [TestMethod]
    public void TestKLBotDiagData()
    {
        var driver = TestConst.GetTestDriver();
        KLBot bot = new(TestConst.DefaultConfig, driver);
        Assert.AreEqual(0, bot.DiagData.ReceivedMessageCount);
        Assert.AreEqual(0, bot.DiagData.ProcessedMessageCount);
        driver.AddReceivedMessage(new (-1, TestConst.TargetGroupId), "some non-sense");
        Assert.AreEqual(1, bot.DiagData.ReceivedMessageCount);
        Assert.AreEqual(0, bot.DiagData.ProcessedMessageCount);
        driver.AddReceivedMessage(new (-1, TestConst.TargetGroupId), "##help");
        Assert.AreEqual(2, bot.DiagData.ReceivedMessageCount);
        Assert.AreEqual(1, bot.DiagData.ProcessedMessageCount);
    }
}
