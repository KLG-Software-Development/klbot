using klbotlib;
using klbotlib.Modules;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace KLBotUnitTest;

[TestClass]
public class TestKLBot
{
    /// <summary>
    /// ����ģ�������Ƿ񷵻�Ԥ��ֵ
    /// </summary>
    [TestMethod]
    public void TestModuleCount()
    {
        KLBot bot = new(TestConst.GetTestClient(), "config/unit_test_config.json");
        Assert.AreEqual(TestConst.CoreModuleCount, bot.ModuleCount);
        bot.AddModule(new TimeModule()).Wait();
        Assert.AreEqual(TestConst.CoreModuleCount + 1, bot.ModuleCount);
        var tm = new TimeModule();
        bot.AddModule(tm).Wait();
        Assert.AreEqual(TestConst.CoreModuleCount + 2, bot.ModuleCount);
    }
    /// <summary>
    /// ����ģ��ͳ�ƽ���Ƿ����Ԥ��
    /// </summary>
    [TestMethod]
    public void TestKLBotDiagData()
    {
        var client = TestConst.GetTestClient();
        KLBot bot = new(client, "config/unit_test_config.json");
        Assert.AreEqual(0, bot.DiagData.ReceivedMessageCount);
        Assert.AreEqual(0, bot.DiagData.ProcessedMessageCount);
        Assert.AreEqual(0, bot.DiagData.SuccessPackageCount);
        MessagePlain msg = new(MessageContext.Group, -1, -1, "some non-sense");
        client.AddReceivedMessage(msg);
        bot.ProcessMessages(bot.FetchMessages().Result).Wait();
        Assert.AreEqual(1, bot.DiagData.ReceivedMessageCount);
        Assert.AreEqual(0, bot.DiagData.ProcessedMessageCount);
        Assert.AreEqual(1, bot.DiagData.SuccessPackageCount);
        msg = new(MessageContext.Group, -1, -1, "##help");
        client.AddReceivedMessage(msg);
        bot.ProcessMessages(bot.FetchMessages().Result).Wait();
        Assert.AreEqual(2, bot.DiagData.ReceivedMessageCount);
        Assert.AreEqual(1, bot.DiagData.ProcessedMessageCount);
        Assert.AreEqual(2, bot.DiagData.SuccessPackageCount);
    }
}
