using klbotlib.Events;
using klbotlib.Extensions;
using klbotlib.MessageDriver.OneBot.JsonPrototypes;
using klbotlib.Modules;

namespace klbotlib.MessageDriver.OneBot;

/// <summary>
/// OneBot消息驱动器实现
/// </summary>
public class MessageDriver_OneBotHttp : IMessageDriver
{
    private readonly OneBotHttpApiCaller _caller;
    private readonly OneBotWebhookServer _webhookServer;

    /// <summary>
    /// 创建基于HTTP传输的OneBot协议实例
    /// </summary>
    /// <param name="httpServiceUrl">OneBot HTTP消息服务URL</param>
    /// <param name="webhookBindUrl">OneBot Webhook服务器绑定到的URL</param>
    /// <param name="token">调用API时使用的鉴权token</param>
    public MessageDriver_OneBotHttp(string httpServiceUrl, string webhookBindUrl, string token)
    {
        _caller = new(httpServiceUrl, token);
        _webhookServer = new(webhookBindUrl);
        // 注册webhook事件处理器
        _webhookServer.OneBotEventReceived += OneBotEventLog;
        _webhookServer.OneBotEventReceived += OneBotEventDispatch;
        _ = _webhookServer.Start();
    }

    private void OneBotEventLog(object? obj, OneBotEventArgs e)
    {
        this.DebugLog($"[Event][{e.Time.AsUnixTimestamp().ToKLBotTimestampString()}][{e.PostType}][{e.SelfId}] {e.RawEventData}");
    }

    // 将OneBot事件路由至MessageDriver事件，从而通知KLBot
    private void OneBotEventDispatch(object? obj, OneBotEventArgs e)
    {
        switch (e.PostType)
        {
            case "message":
                var time = e.Time.AsUnixTimestamp();
                var context = GetOneBotMessageEventContext(e.RawEventData);
                var msg = BuildMessageFromEvent(e);
                _ = OnMessageReceived.Invoke(this, new(time, context, msg));
                return;
            default:
                this.Log($"Dispatcher not configured for post type [{e.PostType}]");
                return;
        }
    }

    // 从OneBot事件中构造消息
    private static Message BuildMessageFromEvent(OneBotEventArgs e)
    {
        var data = e.RawEventData;
        return data.Message == null
            ? throw new Exception($"Failed to build Message: Invalid deserialized data: {data}")
            : new MessagePackage(data.Message.Select(msg => msg.ToMessage())).Collapse();
    }

    // 推导message事件的上下文类型
    private static MessageContext GetOneBotMessageEventContext(JOneBotEvent rawEvent)
    {
        var type = rawEvent.MessageType switch
        {
            "group" => MessageContextType.Group,
            "private" => rawEvent.SubType switch
            {
                "friend" => MessageContextType.Private,
                "group" => MessageContextType.Temp,
                null => throw new Exception($"OneBotEvent: Unexpected private message sub_type is null"),
                _ => throw new Exception($"OneBotEvent: Unknown private message sub_type \"{rawEvent.SubType}\""),
            },
            _ => throw new Exception($"OneBotEvent: Unknown message_type \"{rawEvent.MessageType}\""),
        };
        return new MessageContext(type, rawEvent.UserId, rawEvent.GroupId);
    }

    private async Task CallApiAsync<TResponse>(string uri, string? paramJson)
    {
        try
        {
            var response = await _caller.Call<TResponse>(uri, paramJson);
            if (response == null || response.Data == null)
                this.Log($"Invalid response: {response}");
        }
        catch (Exception ex)
        {
            this.Log(ex.ToString());
        }
    }

    private async Task<TOut?> CallApiAsync<TResponse, TOut>(string uri, string? paramJson, Func<TResponse, TOut>? extractor, bool raise = false)
    {
        try
        {
            var response = await _caller.Call<TResponse>(uri, paramJson);
            if (response == null || response.Data == null)
            {
                this.Log($"Invalid response: {response}");
                return default;
            }
            return extractor == null ? default : extractor(response.Data);
        }
        catch (Exception ex)
        {
            if (raise)
                throw;
            this.Log(ex.ToString());
            return default;
        }
    }

    // -------- 以下为接口实现 --------
    /// <inheritdoc/>
    public string LogUnitName => "Driver/OneBotHttp";

    /// <inheritdoc/>
    public string DriverInfo => $"OneBot message driver [HTTP@{_caller.ServerUrl}] [Webhook@{_webhookServer.BindAddr}]";

    /// <inheritdoc/>
    public event AsyncEventHandler<KLBotMessageEventArgs> OnMessageReceived = (_, _) => Task.CompletedTask;

    /// <inheritdoc/>
    public async Task<Message> GetMessageFromId(long target, long messageId)
    {
        return await CallApiAsync<JOneBotMessageObj, Message>($"get_msg", $"{{message_id:{messageId}}}", data => data.ToMessage())
            ?? throw new Exception($"Failed to get message by ID {messageId}");
    }

    /// <inheritdoc/>
    public async Task<long> GetSelfId()
    {
        return await CallApiAsync<JOneBotUser, long>("get_login_info", null, data => data.UserId, raise: true);
    }

    /// <inheritdoc/>
    public async Task Mute(Module module, long userId, long groupId, uint durationSeconds)
    {
        await CallApiAsync<long>("set_group_ban", $"{{user_id:{userId},group_id:{groupId},duration:{durationSeconds}}}");
    }

    /// <inheritdoc/>
    public async Task SendMessage(Module module, MessageContextType context, long userId, long groupId, Message msg)
    {
        string? msgJson = OneBotJsonHelper.CompileMessageJson(msg);
        if (msgJson == null)
        {
            this.LogWarning($"Failed to compile message of type \"{msg.GetType()}\" to JSON");
            return;
        }
        switch (context)
        {
            case MessageContextType.Private:
                _ = await CallApiAsync<JOneBotSentMessage, long>("send_private_msg", $"{{\"user_id\":{userId},\"message\":{msgJson}}}", null);
                return;
            case MessageContextType.Group:
                _ = await CallApiAsync<JOneBotSentMessage, long>("send_group_msg ", $"{{\"group_id\":{groupId},\"message\":{msgJson}}}", null);
                return;
            case MessageContextType.Temp:
                _ = await CallApiAsync<JOneBotSentMessage, long>("send_msg", $"{{\"user_id\":{userId},\"group_id\":{groupId},\"message\":{msgJson}}}", null);
                return;
            default:
                throw new Exception($"Unsupported message context: {context}");
        }
    }

    /// <inheritdoc/>
    public async Task Unmute(Module module, long userId, long groupId)
    {
        await CallApiAsync<long>("set_group_ban", $"{{user_id:{userId},group_id:{groupId},duration:0}}");
    }

    /// <inheritdoc/>
    public Task UploadFile(Module module, long groupId, string uploadPath, string filePath)
    {
        throw new NotImplementedException();
    }

    /// <inheritdoc/>
    public Task<bool> Verify(string key)
    {
        return Task.FromResult(true);
    }
}
