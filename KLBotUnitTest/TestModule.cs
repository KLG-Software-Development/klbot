using klbotlib;
using klbotlib.MessageDriver.DebugLocal;
using klbotlib.Modules;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace KLBotUnitTest;

[TestClass]
public class TestModule
{
    // 测试模块IsAttach属性
    [TestMethod]
    public void TestIsAttach()
    {
        MessageDriver_Debug driver = TestConst.GetTestDriver();
        KLBot test_bot = new(driver, "config/unit_test_config.json");
        var tm = new TimeModule();
        Assert.AreEqual(false, tm.IsAttached, "模块初始情况下模块应处于未附加状态（IsAttach=false）");
        test_bot.AddModule(tm).Wait();
        Assert.AreEqual(true, tm.IsAttached, "调用AddModule()后模块应处于附加状态（IsAttach=true）");
    }
}
