using klbotlib;
using klbotlib.Modules;

namespace KLBotUnitTest;

static class TestConst
{
    public static void NullAction(Message msg) { }
    public static void NullAction(Module module, MessageContext context, long userId, long groupId, string content) { }
    public static void NullAction(Module module, long groupId, string uploadPath, string filePath) { }
    public const int CoreModuleCount = 1;   //核心模块数量
    public const long SelfID = 33550336;
}
