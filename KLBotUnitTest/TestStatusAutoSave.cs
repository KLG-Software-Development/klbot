using klbotlib;
using klbotlib.MessageServer.Debug;
using klbotlib.Modules;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using System.IO;
using System.Threading;

namespace KLBotUnitTest;

[TestClass]
public class TestStatusAutoSave
{
    /// <summary>
    /// 测试模块状态是否自动保存
    /// </summary>
    [TestMethod]
    public void StatusAutoSaveForAllModuleWhenCoreModuleProcessed()
    {
        DebugMessageServer server = new(TestConst.NullAction, TestConst.NullAction, TestConst.NullAction);
        KLBot bot = new(server, "config/unit_test_config.json");
        FuckModule module = new();
        bot.AddModule(module);
        bool initState = module.Enabled;
        //通过命令模块修改启用状态
        MessagePlain msg = new(MessageContext.Group, -1, -1, "##fuckmod enabled"); //unit_test_config.json中应将-1设置为监听群
        server.AddReceivedMessage(msg);
        bot.ProcessMessages(bot.FetchMessages());   
        Thread.Sleep(25);//等待命令处理完成
        Assert.AreEqual(!initState, module.Enabled, "FuckModule.Enabled should have changed");
        //Test save file
        string savePath = Path.Combine(bot.ModulesSaveDir, module.ModuleID + "_status.json");
        JObject status = JsonConvert.DeserializeObject<JObject>(File.ReadAllText(savePath));
        Assert.AreEqual(!initState, (bool)status["Enabled"], $"FuckModule.Enabled field in \"{savePath}\"should have changed");
    }
}
