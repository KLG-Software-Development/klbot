using klbotlib;
using klbotlib.MessageDriver.DebugLocal;
using klbotlib.Modules;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using System.IO;

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
        MessageDriver_Debug driver = TestConst.GetTestDriver();
        KLBot bot = new(TestConst.DefaultConfig, driver);
        FuckModule module = new();
        bot.AddModule(module).Wait();
        bool initState = module.Enabled;
        //通过命令模块修改启用状态
        MessagePlain msg = new(MessageContext.Group, -1, -1, "##switch FuckModule"); //unit_test_config.json中应将-1设置为监听群
        driver.AddReceivedMessage(msg);
        Assert.AreEqual(!initState, module.Enabled, "FuckModule.Enabled should have changed");
        //Test save file
        string savePath = Path.Combine(bot.ModulesSaveDir, module.ModuleID + "_status.json");
        JObject? status = JsonConvert.DeserializeObject<JObject>(File.ReadAllText(savePath));
        if (status == null)
            throw new JsonException();
        bool? enabledJson = (bool?)status["Enabled"];
        if (enabledJson == null)
            throw new JsonException("Cannot read Enabled field from json file.");
        Assert.AreEqual(!initState, enabledJson, $"FuckModule.Enabled field in \"{savePath}\"should have changed");
    }
}
