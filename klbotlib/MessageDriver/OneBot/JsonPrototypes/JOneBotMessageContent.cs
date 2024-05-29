using System;
using System.Text.Json.Nodes;
using klbotlib.Extensions;

namespace klbotlib.MessageDriver.OneBot.JsonPrototypes;

internal record JOneBotMessageContent(string? Type, JsonObject? Data);

internal static class JOneBotMessageContentExtension
{
    public static Message BuildMessage(this JOneBotMessageContent content)
    {
        if (content.Data == null)
            throw new Exception($"Invalid message content: {content}");
        switch (content.Type)
        {
            case "text":
                return new MessagePlain(content.Data.GetString("text"));
            case "at":
                return new MessageAt(content.Data.GetLong("text"));
            case "face":
                return new MessageFace(content.Data.GetString("id"));
            case "image":
                string url;
                if (content.Data.ContainsKey("url"))
                    url = content.Data.GetString("url");
                else
                    url = content.Data.GetString("file");
                if (content.Data.ContainsKey("type"))
                    return new MessageImage(url, true);;
                return new MessageImage(url, false);
            default:
                return Message.Empty;
        }
    }
}
