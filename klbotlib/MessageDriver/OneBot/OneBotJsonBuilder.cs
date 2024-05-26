using System.Text.Json;

namespace klbotlib.MessageDriver.OneBot;

internal static class OneBotJsonBuilder
{
    private static string CompileMessageJson<T>(string type, T data)
    {
        string dataJson = JsonSerializer.Serialize(data, OneBotJsonSerializerOptions.Options);
        return $"{{\"type\":\"{type}\",\"data\":{dataJson}}}";
    }

    public static string? CompileMessageJson(Message msg)
    {
        if (msg is MessagePlain msgPlain)
            return CompileMessageJson("text", new { text = msgPlain.Text });
        else
            return null;
    }
}
