using System;

namespace KLBotUnitTest;

static class TestConst
{
    public static readonly Action<string> NullAction = x => { };
    public const int CoreModuleCount = 1;   //核心模块数量
    public const long SelfID = 33550336;
}
