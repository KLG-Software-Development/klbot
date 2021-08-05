using klbotlib;
using klbotlib.Modules;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace KLBotUnitTest
{
    [TestClass]
    public class TestClass000_Module
    {
        // 测试模块IsAttach属性
        [TestMethod]
        public void TestMethod000_IsAttach()
        {
            KLBot test_bot = new();
            var tm = new TimeModule();
            Assert.AreEqual(tm.IsAttached, false, "模块初始情况下模块应处于未附加状态（IsAttach=false）");
            tm.AttachTo(test_bot);
            Assert.AreEqual(tm.IsAttached, true, "调用AttachTo(KLBot)后模块应处于附加状态（IsAttach=true）");
            tm.Detach();
            Assert.AreEqual(tm.IsAttached, false, "调用Detach()后模块应处于未附加状态（IsAttach=false）");
        }
        // 测试模块ModuleIndex属性
        [TestMethod]
        public void TestMethod001_ModuleIndex()
        {
            KLBot test_bot = new();
            var tm = new TimeModule();
            Assert.AreEqual(tm.ModuleIndex, -1, "模块初始情况下模块应处于未附加状态，模块索引为-1");
            tm.AttachTo(test_bot);
            Assert.AreEqual(tm.ModuleIndex, TestConst.CoreModuleCount, "调用AttachTo(KLBot)后模块索引应等于在模块链条中的索引。此处应为当前模块链条长度，即核心模块数");
            var tm2 = new TimeModule();
            tm2.AttachTo(test_bot);
            Assert.AreEqual(tm2.ModuleIndex, TestConst.CoreModuleCount + 1, "调用AttachTo(KLBot)后模块索引应等于在模块链条中的索引。此处应为当前模块链条长度+1，即核心模块数+1");
        }
    }
}
