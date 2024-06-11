using klbotlib.MessageDriver.OneBot.JsonPrototypes;
using System.Net.Http.Headers;
using System.Net.Mime;
using System.Text.Json;

namespace klbotlib.MessageDriver.OneBot;

internal class OneBotHttpApiCaller(string serverUrl, string token) : IKLBotLogUnit
{
    private static readonly HttpClient s_client = new();
    private readonly string _token = $"Bearer {token}";

    public string ServerUrl { get; } = serverUrl;

    public string LogUnitName => "Driver/OneBot/Caller";

    public async Task<JOneBotResponse<TOut>?> Call<TOut>(string uri, string? data)
    {
        using HttpRequestMessage request = new();
        request.Method = HttpMethod.Post;
        request.RequestUri = BuildUri(uri);
        if (data != null)
            request.Content = new StringContent(data, new MediaTypeHeaderValue(MediaTypeNames.Application.Json));
        request.Headers.Add("Authorization", _token);
        this.DebugLog($"[{request.RequestUri}] {data}");
        var response = await s_client.SendAsync(request);
        string responseJson = await response.Content.ReadAsStringAsync();
        this.DebugLog(responseJson);
        var ret = OneBotJsonHelper.Deserialize<JOneBotResponse<TOut>>(responseJson) ?? throw new JsonException("Failed to deserialize JSON");
        return ret.Retcode != 0 ? throw new OneBotResponseException(ret.Retcode, response.ToString()) : ret;
    }

    private Uri BuildUri(string uri)
    {
        return new($"{ServerUrl}/{uri}");
    }
}
