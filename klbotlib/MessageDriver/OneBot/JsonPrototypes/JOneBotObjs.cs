namespace klbotlib.MessageDriver.OneBot.JsonPrototypes;

internal record JOneBotUser(long UserId, string? Nickname);
internal record JOneBotMessageObj(
    long Time, long SelfId, string? MessageType, string? SubType, uint MessageId, long UserId, long GroupId,
    JOneBotMessageContent? Message); // 忽略sender字段。后者信息未必齐全/准确，且信息用处不大
internal record JOneBotSentMessage(int MessageId);

internal static class JOneBotMessageExtension
{
    public static Message ToMessage(this JOneBotMessageObj msg)
    {
        if (msg.Message == null)
            return Message.Empty;
        long senderId = msg.UserId;
        long groupId = msg.GroupId == 0 ? -1 : msg.GroupId;
        
        switch (msg.Message.Type)
        {
            case "text":
                string? text = (string?)msg.Message.Data["text"].AsValue();
                if (text == null)
                    return Message.Empty;
                return new MessagePlain(senderId, groupId, text);
            default:
                return Message.Empty;
        }
    }
}