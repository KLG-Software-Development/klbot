using System.IO;
using System.Text.Json;
using System.Threading.Tasks;

namespace klbotlib.MessageDriver.OneBot;

internal static class OneBotJsonHelper
{
    private static readonly JsonSerializerOptions _protocolDeserializeOptions = new()
    {
        PropertyNamingPolicy = JsonNamingPolicy.SnakeCaseLower,
    };

    public static T? Deserialize<T>(string json)
        => JsonSerializer.Deserialize<T>(json, _protocolDeserializeOptions);

    public static async Task<T?> DeserializeAsync<T>(Stream utf8Json)
        => await JsonSerializer.DeserializeAsync<T>(utf8Json, _protocolDeserializeOptions);

    private static string CompileMessageJson<T>(string type, T data)
    {
        string dataJson = JsonSerializer.Serialize(data, _protocolDeserializeOptions);
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
