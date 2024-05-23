using System;
using System.Diagnostics.CodeAnalysis;
using System.Net;
using System.Net.Http;
using System.Net.Http.Json;
using System.Text.Json;
using System.Text.Json.Nodes;
using System.Threading;
using System.Threading.Tasks;
using klbotlib.MessageDriver.OneBot.JsonPrototypes;

namespace klbotlib.MessageDriver.OneBot;

internal class OneBotWebhookServer(string bindAddr, string token)
{
    private readonly HttpListener _server = new();
    private readonly string _token = token;
    private readonly OneBotEventRaiser _raiser = new();
    public string BindAddr { get; } = bindAddr;

    public async Task Start()
    {
        _server.Prefixes.Add(BindAddr);
        Console.WriteLine($"Webhook server started at {BindAddr}");
        Environment.Exit(0);
        _server.Start();
        while (true)
        {
            var context = await _server.GetContextAsync().ConfigureAwait(false);
            var request = context.Request;
            var auth = request.Headers["Authorization"];
            if (auth == null || auth != $"Bearer {_token}")
                continue;
            if (request.ContentType != "application/json")
                continue;
            var jobj = await JsonSerializer.DeserializeAsync<JsonObject>(context.Request.InputStream, OneBotJsonSerializerOptions.Options).ConfigureAwait(false);
            if (jobj == null)
            {
                context.Response.StatusCode = 500;
                context.Response.Close();
                return;     
            }
            ProcessEvent(jobj);
            context.Response.StatusCode = 204;
            context.Response.Close();
        }
    }

    private (long, long, string) ExtractEventInfo(JsonObject jobj)
    {
        if (!jobj.ContainsKey("time") || !jobj.ContainsKey("self_id") ||!jobj.ContainsKey("post_type"))
            throw new Exception($"Failed to build OneBot event: Invalid event data: {jobj}");
        long time = (long)jobj["time"].AsValue();
        long selfId = (long)jobj["self_id"].AsValue();
        string? postType = (string?)jobj["post_type"].AsValue();
        if (postType == null)
            throw new Exception($"Failed to build OneBot event: Invalid event data: {jobj}");
        jobj.Remove("time");
        jobj.Remove("self_id");
        jobj.Remove("post_type");
        return (time, selfId, postType);
    }

    private void ProcessEvent(JsonObject jobj)
    {
        (long time, long selfId, string postType) = ExtractEventInfo(jobj);
        _raiser.RaiseEvent(time, selfId, postType, jobj);
    }
}
