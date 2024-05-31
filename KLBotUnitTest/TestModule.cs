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
        KLBot testBot = new(TestConst.DefaultConfig, driver);
        var tm = new TimeModule();
        Assert.AreEqual(false, tm.IsAttached, "模块初始情况下模块应处于未附加状态（IsAttach=false）");
        testBot.AddModule(tm);
        Assert.AreEqual(true, tm.IsAttached, "调用AddModule()后模块应处于附加状态（IsAttach=true）");
    }
}
