using System;
using System.Diagnostics.CodeAnalysis;
using System.Net.Http;
using System.Net.Http.Json;
using System.Text.Json;
using System.Threading.Tasks;
using klbotlib.MessageClient.OneBot.JsonPrototypes;

namespace klbotlib.MessageClient.OneBot;

internal class OneBotHttpApiCaller(string serverUrl, string token)
{
    private static readonly HttpClient _client = new();
    private static readonly JsonSerializerOptions _options = new()
    {
        PropertyNamingPolicy = JsonNamingPolicy.SnakeCaseLower
    };
    private readonly string _token = $"Bearer {token}";

    public string ServerUrl { get; } = serverUrl;

    public async Task<JOneBotResponse<TOut>> Call<TOut>(string uri, string? json)
    {
        JsonContent content = JsonContent.Create(json, options: _options);
        content.Headers.Add("Authorization", _token);
        var response = await _client.PostAsync(BuildUri(uri), content);
        response.EnsureSuccessStatusCode();
        var ret = await JsonSerializer.DeserializeAsync<JOneBotResponse<TOut>>(response.Content.ReadAsStream(), options: _options);
        if (ret == null)
            throw new JsonException("Failed to deserialize JSON");
        if (ret.Retcode != 0)
            throw new Exception(response.ToString());
        return ret;
    }

    private Uri BuildUri(string uri)
    {
        return new($"http://{ServerUrl}/{uri}");
    }
}
