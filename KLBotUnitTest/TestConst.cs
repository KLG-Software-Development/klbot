using klbotlib.MessageServer.Debug;

namespace KLBotUnitTest;

static class TestConst
{
    public const long SelfID = 33550336;
    public const int CoreModuleCount = 1;   //核心模块数量
    public static DebugMessageServer GetTestServer()
        => new(SelfID, a => { },
            (a, b, c, d, e) => { },
            (a, b, c, d) => { },
            (a, b, c, d) => { },
            (a, b, c) => { });
}
