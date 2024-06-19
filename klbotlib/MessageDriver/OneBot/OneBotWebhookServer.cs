using klbotlib.MessageDriver.OneBot.JsonPrototypes;
using System.Net;

namespace klbotlib.MessageDriver.OneBot;

internal class OneBotWebhookServer(string bindAddr) : IKLBotLogUnit
{
    private readonly HttpListener _server = new();
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
                await ResponseWithError(context, $"Bad ContentType: \"{request.ContentType}\"");
                goto endResponse;
            }
            try
            {
                var jEvent = await OneBotJsonHelper.DeserializeAsync<JOneBotEvent>(context.Request.InputStream).ConfigureAwait(false);
                if (jEvent == null)
                {
                    await ResponseWithError(context, "Bad content");
                    goto endResponse;
                }
                this.DebugLog(jEvent.ToString());
                ProcessEvent(jEvent);
            }
            catch (Exception ex)
            {
                await ResponseWithError(context, ex.ToString());
            }
        endResponse:
            context.Response.Close();
        }
    }

    private async Task ResponseWithError(HttpListenerContext context, string message)
    {
        this.Log($"Webhook: {message}");
        context.Response.StatusCode = 500;
        context.Response.StatusDescription = "FUCKOFF";
        using StreamWriter sw = new(context.Response.OutputStream);
        await sw.WriteLineAsync(message);
        sw.Close();
    }

    private void ProcessEvent(JOneBotEvent rawEvent)
    {
        if (rawEvent.Time == default || rawEvent.SelfId == default || rawEvent.PostType == null)
            throw new Exception($"Failed to build OneBot event: Invalid event data: {rawEvent}");
        OneBotEventReceived.Invoke(this, new(rawEvent.Time, rawEvent.SelfId, rawEvent.PostType, rawEvent));
    }
}
