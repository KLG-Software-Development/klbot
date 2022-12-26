using klbotlib.MessageServer.Debug;

namespace KLBotUnitTest;

static class TestConst
{
    public static DebugMessageServer GetTestServer()
        => new(a => { },
            (a, b, c, d, e) => { },
            (a, b, c, d) => { },
            (a, b, c, d) => { },
            (a, b, c) => { });

    public const int CoreModuleCount = 1;   //核心模块数量
    public const long SelfID = 33550336;
}
