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
            Assert.AreEqual(false, tm.IsAttached, "模块初始情况下模块应处于未附加状态（IsAttach=false）");
            tm.AttachTo(test_bot);
            Assert.AreEqual(true, tm.IsAttached, "调用AttachTo(KLBot)后模块应处于附加状态（IsAttach=true）");
            tm.Detach();
            Assert.AreEqual(false, tm.IsAttached, "调用Detach()后模块应处于未附加状态（IsAttach=false）");
        }
        // 测试模块ModuleIndex属性
        [TestMethod]
        public void TestMethod001_ModuleIndex()
        {
            KLBot test_bot = new();
            var tm = new TimeModule();
            Assert.AreEqual(tm.ModuleIndex, -1, "模块初始情况下模块应处于未附加状态，模块索引为-1");
            tm.AttachTo(test_bot);
            Assert.AreEqual(0, tm.ModuleIndex,  "调用AttachTo(KLBot)后模块索引应等于在模块链条同类模块中的索引。此处是第一个TimeModule，应为0");
            var tm2 = new TimeModule();
            tm2.AttachTo(test_bot);
            Assert.AreEqual(1, tm2.ModuleIndex, "调用AttachTo(KLBot)后模块索引应等于在模块链条中的索引。此处是第二个TimeModule，应为1");
        }
    }
}
