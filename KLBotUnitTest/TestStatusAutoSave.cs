using klbotlib;
using klbotlib.MessageDriver.DebugLocal;
using klbotlib.Modules;
using Microsoft.VisualStudio.TestTools.UnitTesting;
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
        string savePath = Path.Combine(bot.ModulesSaveDir, module.ModuleId + ".json");
        if (File.Exists(savePath)) // 提前删除模块存档文件，以避免旧存档影响测试，以及对存档文件自动生成进行验证
            File.Delete(savePath);
        bot.AddModule(module);
        bool initState = module.Enabled;
        long processed = bot.DiagData.ProcessedMessageCount;
        // 通过命令模块修改启用状态
        _ = driver.AddReceivedMessage(new(-1, TestConst.TargetGroupId), "##switch FuckModule");
        Console.WriteLine("Waiting for message to be fully processed");
        while (bot.DiagData.ProcessedMessageCount == processed)
            continue;
        Assert.AreEqual(!initState, module.Enabled, "FuckModule.Enabled should have changed");
        // Test save file
        // 内部WriteAllText()会导致文件短时间内处于长度=0状态，需等待文件完成写入
        Console.WriteLine("Waiting for file to be saved");
        while (new FileInfo(savePath).Length == 0)
            continue;
        JsonNode? node = JsonSerializer.Deserialize<JsonNode>(File.ReadAllText(savePath)) ?? throw new JsonException($"Failed to deserialize {savePath} (1st)");
        bool? enabledJson = (bool?)node["Enabled"] ?? throw new JsonException("Cannot read Enabled field from json file. (1st)");
        Assert.AreEqual(!initState, enabledJson, $"FuckModule.Enabled field in \"{savePath}\"should have changed (1st)");

        // 通过命令模块再次修改启用状态
        _ = driver.AddReceivedMessage(new(-1, TestConst.TargetGroupId), "##switch FuckModule");
        Console.WriteLine("Waiting for message to be fully processed");
        while (bot.DiagData.ProcessedMessageCount == processed)
            continue;
        Assert.AreEqual(initState, module.Enabled, "FuckModule.Enabled should have changed (2nd)");
        // Test save file
        // 内部WriteAllText()会导致文件短时间内处于长度=0状态，需等待文件完成写入
        Console.WriteLine("Waiting for file to be saved");
        while (new FileInfo(savePath).Length == 0)
            continue;
        node = JsonSerializer.Deserialize<JsonNode>(File.ReadAllText(savePath));
        if (node == null)
            throw new JsonException($"Failed to deserialize {savePath} (2nd)");
        enabledJson = (bool?)node["Enabled"];
        if (enabledJson == null)
            throw new JsonException("Cannot read Enabled field from json file. (2nd)");
        Assert.AreEqual(initState, enabledJson, $"FuckModule.Enabled field in \"{savePath}\" should have changed (2nd)");
    }
}
