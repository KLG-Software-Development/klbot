using System;
using System.IO;
using System.Net;
using System.Text.Json;
using System.Text.Json.Nodes;
using System.Threading.Tasks;

namespace klbotlib.MessageDriver.OneBot;

internal class OneBotWebhookServer(string bindAddr, string token)
{
    private readonly HttpListener _server = new();
    private readonly string _token = token;
    private readonly OneBotEventManager _eventMgr = new();
    public string BindAddr { get; } = bindAddr.EndsWith('/') ? bindAddr : $"{bindAddr}/";

    public async Task Start()
    {
        _server.Prefixes.Add(BindAddr);
        Console.WriteLine($"[OneBotWebhook] Webhook server started at {BindAddr}");
        _server.Start();
        while (true)
        {
            Console.WriteLine("[OneBotWebhook] Getting context...");
            var context = await _server.GetContextAsync().ConfigureAwait(false);
            Console.WriteLine($"[OneBotWebhook] [from:{context.Request.RemoteEndPoint}] [@{context.Request.RawUrl}] [type:{context.Request.ContentType}]");
            var request = context.Request;
            var auth = request.Headers["Authorization"];
            if (auth == null || auth != $"Bearer {_token}")
                goto error;
            if (request.ContentType != "application/json")
                goto error;
            try
            {
                var jobj = await JsonSerializer.DeserializeAsync<JsonObject>(context.Request.InputStream, OneBotJsonSerializerOptions.Options).ConfigureAwait(false);
                if (jobj == null)
                    goto error;
                ProcessEvent(jobj);
            }
            catch (Exception ex)
            {
Console.WriteLine(ex.ToString());
                using StreamWriter sw = new(context.Response.OutputStream);
                await sw.WriteLineAsync(ex.ToString());
                sw.Close();
                goto error;
            }
            context.Response.StatusCode = 204;
            context.Response.Close();
            return;
        error:
            Console.WriteLine("[OneBotWebhook] 500 fuckoff");
            context.Response.StatusCode = 500;
            context.Response.StatusDescription = "FUCKOFF";
            context.Response.Close();
            return;
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
        Console.WriteLine("[OneBotWebhook] Event processing begin.");
        (long time, long selfId, string postType) = ExtractEventInfo(jobj);
        Console.WriteLine("[OneBotWebhook] Event raising begin.");
        _eventMgr.RaiseOneBotEvent(time, selfId, postType, jobj);
        Console.WriteLine("[OneBotWebhook] Event processed");
    }
}
