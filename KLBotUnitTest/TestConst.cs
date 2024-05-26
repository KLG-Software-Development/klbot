using klbotlib.MessageDriver.DebugLocal;
using Microsoft.Extensions.Configuration;
using ConfigValues = System.Collections.Generic.Dictionary<string, string?>;

namespace KLBotUnitTest;

static class TestConst
{
    public const long SelfId = 33550336;
    public const long TargetGroupId = 7355608;
    public const int CoreModuleCount = 1;   //核心模块数量
    public static MessageDriver_Debug GetTestDriver()
        => new(SelfId, _ => { },
            (_, _, _, _, _) => { },
            (_, _, _, _) => { },
            (_, _, _, _) => { },
            (_, _, _) => { });
    public static readonly IConfigurationRoot DefaultConfig = new ConfigurationBuilder().AddInMemoryCollection(
        new ConfigValues()
        {
            { "key", "7355608" },
            { "targets", $"{TargetGroupId}" },
            { "cache_dir", "cache" },
            { "save_dir", "save" }
        })
    .Build();
}
