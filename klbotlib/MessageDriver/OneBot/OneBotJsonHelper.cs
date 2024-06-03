using System.IO;
using System.Linq;
using System.Text;
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
        if (msg is MessagePackage msgPkg)
            return $"[{string.Join(',', msgPkg.Select(CompileMessageJson))}]";
        else if (msg is MessagePlain msgPlain)
            return CompileMessageJson("text", new { text = msgPlain.Text });
        else if (msg is MessageAt msgAt)
            return CompileMessageJson("at", new { qq = msgAt.TargetId });
        else if (msg is MessageFace msgFace)
            return CompileMessageJson("face", new { id = msgFace.FaceId });
        else if (msg is MessageVoice msgVoice)
            return CompileMessageJson("record", new { file = msgVoice.Url });
        else if (msg is MessageImage msgImg)
        {
            if (msgImg.IsFlashImage)
                return CompileMessageJson("image", new { file = msgImg.Url, type = "flash" });
            else
                return CompileMessageJson("image", new { file = msgImg.Url });
        }
        return null;
    }
}
