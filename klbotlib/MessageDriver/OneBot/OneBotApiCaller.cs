using System;
using System.Diagnostics;
using System.Net.Http;
using System.Net.Http.Json;
using System.Text.Json;
using System.Threading.Tasks;
using klbotlib.MessageDriver.OneBot.JsonPrototypes;

namespace klbotlib.MessageDriver.OneBot;

internal class OneBotHttpApiCaller(string serverUrl, string token)
{
    private static readonly HttpClient _client = new();
    private readonly string _token = $"Bearer {token}";

    public string ServerUrl { get; } = serverUrl;

    public async Task<JOneBotResponse<TOut>?> Call<TOut>(string uri, string? json)
    {
        using HttpRequestMessage request = new();
        request.Method = HttpMethod.Post;
        request.RequestUri = BuildUri(uri);
        if (json != null)
            request.Content = JsonContent.Create(json, options: OneBotJsonSerializerOptions.Options);
        request.Headers.Add("Authorization", _token);
        Console.WriteLine($"[OneBotAPI] [{request.RequestUri}] {json}");
        var response = await _client.SendAsync(request);
        var ret = await JsonSerializer.DeserializeAsync<JOneBotResponse<TOut>>(await response.Content.ReadAsStreamAsync(), options: OneBotJsonSerializerOptions.Options);
        if (ret == null)
            throw new JsonException("Failed to deserialize JSON");
        if (ret.Retcode != 0)
            throw new OneBotResponseException(ret.Retcode, response.ToString());
        return ret;
    }

    private Uri BuildUri(string uri)
    {
        return new($"{ServerUrl}/{uri}");
    }
}
