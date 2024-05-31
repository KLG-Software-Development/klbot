using klbotlib;
using klbotlib.MessageDriver.DebugLocal;
using klbotlib.Modules;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.IO;
using System.Text.Json;
using System.Text.Json.Nodes;

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
        bot.AddModule(module);
        bool initState = module.Enabled;
        //通过命令模块修改启用状态
        driver.AddReceivedMessage(new (-1, TestConst.TargetGroupId), "##switch FuckModule");
        Assert.AreEqual(!initState, module.Enabled, "FuckModule.Enabled should have changed");
        //Test save file
        string savePath = Path.Combine(bot.ModulesSaveDir, module.ModuleId + "_status.json");
        JsonNode? node = JsonSerializer.Deserialize<JsonNode>(File.ReadAllText(savePath));
        if (node == null)
            throw new JsonException();
        bool? enabledJson = (bool?)node["Enabled"];
        if (enabledJson == null)
            throw new JsonException("Cannot read Enabled field from json file.");
        Assert.AreEqual(!initState, enabledJson, $"FuckModule.Enabled field in \"{savePath}\"should have changed");
    }
}
