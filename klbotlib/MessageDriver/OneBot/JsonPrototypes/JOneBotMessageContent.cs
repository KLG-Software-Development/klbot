using klbotlib.Extensions;
using System.Text.Json.Nodes;

namespace klbotlib.MessageDriver.OneBot.JsonPrototypes;

internal record JOneBotMessageContent(string? Type, JsonObject? Data);

internal static class JOneBotMessageContentExtension
{
    public static Message ToMessage(this JOneBotMessageContent content)
    {
        if (content.Data == null)
            throw new Exception($"Invalid message content: {content}");
        switch (content.Type)
        {
            case "text":
                return new MessagePlain(content.Data.GetString("text"));
            case "at":
                string qqStr = content.Data.GetString("qq");
                if (!long.TryParse(qqStr, out long qq)) // 暂不支持非纯数字id
                    throw new NotImplementedException($"Non-numeric ID {nameof(MessageAt)} is not supported");
                return new MessageAt(qq);
            case "face":
                return new MessageFace(content.Data.GetString("id"));
            case "image":
                string url;
                url = content.Data.ContainsKey("url") ? content.Data.GetString("url") : content.Data.GetString("file");
                if (content.Data.ContainsKey("type"))
                    return new MessageImage(url, true); ;
                return new MessageImage(url, false);
            default:
                return Message.Empty;
        }
    }
}
