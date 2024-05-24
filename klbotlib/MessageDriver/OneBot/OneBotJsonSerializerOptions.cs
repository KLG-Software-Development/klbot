using System.Text.Json;

namespace klbotlib.MessageDriver.OneBot;

internal class OneBotJsonSerializerOptions
{
    public static JsonSerializerOptions Options { get; } = new()
    {
        PropertyNamingPolicy = JsonNamingPolicy.SnakeCaseLower,
    };
}
