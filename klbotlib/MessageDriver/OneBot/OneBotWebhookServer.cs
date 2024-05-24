using System;
using System.Diagnostics;
using System.IO;
using System.Net;
using System.Text.Json;
using System.Threading.Tasks;
using klbotlib.MessageDriver.OneBot.JsonPrototypes;

namespace klbotlib.MessageDriver.OneBot;

internal class OneBotWebhookServer(string bindAddr, string token) : IKLBotLogUnit
{
    private readonly HttpListener _server = new();
    private readonly string _token = token;
    public string BindAddr { get; } = bindAddr.EndsWith('/') ? bindAddr : $"{bindAddr}/";
    public event EventHandler<OneBotEventArgs> OneBotEventReceived = (_, _) => { };

    public string LogUnitName => "Driver/OneBot/Webhook";

    public async Task Start()
    {
        _server.Prefixes.Add(BindAddr);
        this.Log($"Webhook server started at {BindAddr}");
        _server.Start();
        while (true)
        {
            var context = await _server.GetContextAsync().ConfigureAwait(false);
            this.Log($"from: {context.Request.RemoteEndPoint} url: {context.Request.RawUrl} type: {context.Request.ContentType} ");
            var request = context.Request;
            if (request.ContentType != "application/json")
            {
                this.Log($"Bad ContentType: \"{request.ContentType}\"");
                goto error;
            }
            try
            {
                var jEvent = await JsonSerializer.DeserializeAsync<JOneBotEvent>(context.Request.InputStream, OneBotJsonSerializerOptions.Options).ConfigureAwait(false);
                this.DebugLog(jEvent.ToString());
                if (jEvent == null)
                {
                    this.Log($"Bad content: {jEvent}");
                    goto error;
                }
                ProcessEvent(jEvent);
            }
            catch (Exception ex)
            {
                using StreamWriter sw = new(context.Response.OutputStream);
                await sw.WriteLineAsync(ex.ToString());
                this.Log(ex.ToString());
                sw.Close();
                goto error;
            }
            context.Response.StatusCode = 204;
            context.Response.Close();
            continue;
        error:
            this.DebugLog("500 fuckoff");
            context.Response.StatusCode = 500;
            context.Response.StatusDescription = "FUCKOFF";
            context.Response.Close();
            continue;
        }
    }

    private void ProcessEvent(JOneBotEvent rawEvent)
    {
        if (rawEvent.Time == default || rawEvent.SelfId == default ||rawEvent.PostType == null)
            throw new Exception($"Failed to build OneBot event: Invalid event data: {rawEvent}");
        Console.WriteLine(OneBotEventReceived.GetInvocationList().Length);
        OneBotEventReceived.Invoke(this, new(rawEvent.Time, rawEvent.SelfId, rawEvent.PostType, rawEvent));
    }
}
