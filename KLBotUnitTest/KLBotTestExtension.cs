using klbotlib;

namespace KLBotUnitTest;

/// <summary>
/// 测试用扩展类
/// </summary>
static class KLBotTestExtension
{
#pragma warning disable IDE0051 // 删除未使用的私有成员
    private static string GenerateSimulatedMiraiResponse(MessageContext context, long senderId, long groupId, string chain)
    {
        string json = "";
        switch (context)
        {
            case MessageContext.Private:
                json = "{\"code\":0,\"msg\":\"\",\"data\":[{\"type\":\"FriendMessage\"," + chain + ",\"sender\":{\"id\":" + senderId + ",\"nickname\":\"UNIT_TEST\",\"remark\":\"UNIT_TEST\"}}]}";
                break;
            case MessageContext.Group:
                json = "{\"code\":0,\"msg\":\"\",\"data\":[{\"type\":\"GroupMessage\"," + chain + ",\"sender\":{\"id\":" + senderId + ",\"memberName\":\"UNIT_TEST\",\"specialTitle\":\"\",\"permission\":\"MEMBER\",\"joinTimestamp\":6,\"lastSpeakTimestamp\":7,\"muteTimeRemaining\":0,\"group\":{\"id\":" + groupId + ",\"name\":\"UNIT_TEST\",\"permission\":\"MEMBER\"}}}]}";
                break;
            case MessageContext.Temp:
                json = "{\"code\":0,\"msg\":\"\",\"data\":[{\"type\":\"TempMessage\"," + chain + ",\"sender\":{\"id\":" + senderId + ",\"memberName\":\"UNIT_TEST\",\"specialTitle\":\"\",\"permission\":\"MEMBER\",\"joinTimestamp\":6,\"lastSpeakTimestamp\":7,\"muteTimeRemaining\":0,\"group\":{\"id\":" + groupId + ",\"name\":\"UNIT_TEST\",\"permission\":\"MEMBER\"}}}]}";
                break;
        }
        return json;
    }
}
