namespace klbotlib.MessageDriver.OneBot.JsonPrototypes;

internal record JOneBotUser(long UserId, string? Nickname);
internal record JOneBotMessageObj(
    long Time, long SelfId, string? MessageType, string? SubType, uint MessageId, long UserId, long GroupId,
    JOneBotMessageContent? Message); // 忽略sender字段。后者信息未必齐全/准确，且信息用处不大
internal record JOneBotSentMessage(int MessageId);

internal static class JOneBotMessageExtension
{
    public static Message ToMessage(this JOneBotMessageObj jmsg)
    {
        if (jmsg.Message == null)
            throw new OneBotException($"Failed to build message package from JOneBotMessageObj {jmsg}");
        return jmsg.Message.ToMessage();
    }
}
