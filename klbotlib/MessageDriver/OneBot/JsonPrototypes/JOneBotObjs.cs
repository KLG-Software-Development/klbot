namespace klbotlib.MessageDriver.OneBot.JsonPrototypes;

internal record JOneBotUser(long UserId, string? Nickname);
internal record JOneBotMessageObj(
    long Time, long SelfId, string? MessageType, string? SubType, uint MessageId, long UserId, long GroupId,
    JOneBotMessageContent? Message); // 忽略sender字段。后者信息未必齐全/准确，且信息用处不大
internal record JOneBotSentMessage(int MessageId);

internal static class JOneBotMessageExtension
{
    public static MessagePackage ToMessagePackage(this JOneBotMessageObj jmsg)
    {
        if (jmsg.Message == null)
            throw new OneBotException($"Failed to build message package from JOneBotMessageObj {jmsg}");
        long senderId = jmsg.UserId;
        long groupId = jmsg.GroupId == 0 ? -1 : jmsg.GroupId;
        
        switch (jmsg.Message.Type)
        {
            case "text":
                string? text = (string?)jmsg.Message.Data["text"].AsValue();
                if (text == null)
                    return Message.Empty;
                return new MessagePlain(text);
            default:
                return Message.Empty;
        }
    }
}
