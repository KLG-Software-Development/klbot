using System;
using System.Diagnostics;
using System.IO;
using System.Net;
using System.Text.Json;
using System.Threading.Tasks;
using klbotlib.MessageDriver.OneBot.JsonPrototypes;

namespace klbotlib.MessageDriver.OneBot;

internal class OneBotWebhookServer(string bindAddr, string token)
{
    private readonly HttpListener _server = new();
    private readonly string _token = token;
    public string BindAddr { get; } = bindAddr.EndsWith('/') ? bindAddr : $"{bindAddr}/";
    public event EventHandler<OneBotEventArgs>? OneBotEventReceived;

    public async Task Start()
    {
        _server.Prefixes.Add(BindAddr);
        Console.WriteLine($"[OneBotWebhook] Webhook server started at {BindAddr}");
        _server.Start();
        while (true)
        {
            DebugLog("Getting context...");
            var context = await _server.GetContextAsync().ConfigureAwait(false);
            Debug.Print($"[OneBotWebhook] [from:{context.Request.RemoteEndPoint}] [@{context.Request.RawUrl}] [type:{context.Request.ContentType}]");
            var request = context.Request;
            var auth = request.Headers["Authorization"];
            if (auth == null || auth != $"Bearer {_token}")
            {
Console.WriteLine($"[OneBotWebhook] Bad auth: \"{auth}\"");
                goto error;
            }
            if (request.ContentType != "application/json")
            {
Console.WriteLine($"[OneBotWebhook] Bad ContentType: \"{request.ContentType}\"");
                goto error;
            }
            try
            {
                var jobj = await JsonSerializer.DeserializeAsync<JOneBotEvent>(context.Request.InputStream, OneBotJsonSerializerOptions.Options).ConfigureAwait(false);
                if (jobj == null)
                {
Console.WriteLine($"[OneBotWebhook] Bad ContentType: \"{request.ContentType}\"");
                    goto error;
                }
                ProcessEvent(jobj);
            }
            catch (Exception ex)
            {
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

    private void ProcessEvent(JOneBotEvent rawEvent)
    {
        if (rawEvent.Time == default || rawEvent.SelfId == default ||rawEvent.PostType == null)
            throw new Exception($"Failed to build OneBot event: Invalid event data: {rawEvent}");
        OneBotEventReceived.Invoke(this, new(rawEvent.Time, rawEvent.SelfId, rawEvent.PostType, rawEvent));
    }

    [Conditional("DEBUG")]
    private void DebugLog(string s)
        => Debug.Print($"[OneBotWebhook] {s}");
}
