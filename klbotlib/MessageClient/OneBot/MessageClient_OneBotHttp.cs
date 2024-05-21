using System;
using System.Collections.Generic;
using System.Net;
using System.Threading.Tasks;
using klbotlib.MessageClient.OneBot.JsonPrototypes;
using klbotlib.Modules;

namespace klbotlib.MessageClient.OneBot;

/// <summary>
/// OneBot消息服务器实现
/// </summary>
internal class MessageClient_OneBotHttp : IMessageClient
{
    private readonly OneBotHttpApiCaller _caller;
    private readonly List<Message> _msgBuffer = new();

    public MessageClient_OneBotHttp(string serverUrl, string token)
    {
        _caller = new(serverUrl, token);
        Task.Run(UpdateMsgBufferDaemon);
    }

    /// <inheritdoc/>
    public Task<List<Message>?> FetchMessages()
    {
        if (_msgBuffer.Count == 0)
            return Task.FromResult<List<Message>?>(null);
        List<Message> msgs = [.. _msgBuffer];
        return Task.FromResult<List<Message>?>(msgs);
    }

    /// <inheritdoc/>
    public async Task<Message> GetMessageFromID(long target, long messageId)
    {
        return await CallApiAsync<JOneBotMessageObj, Message>($"get_msg?id={messageId}", null, data => data.ToMessage());
    }

    /// <inheritdoc/>
    public async Task<long> GetSelfID()
    {
        return await CallApiAsync<JOneBotUserInfo, long>("get_login_info", null, data => data.UserId);
    }

    /// <inheritdoc/>
    public Task Mute(Module module, long userId, long groupId, uint durationSeconds)
    {
        throw new NotImplementedException();
    }

    /// <inheritdoc/>
    public async Task SendMessage(Module module, MessageContext context, long userId, long groupId, string content)
    {
        
        switch (context)
        {
            case MessageContext.Private:
                await CallApiAsync<JOneBotUserInfo, long>("send_private_msg", $"{{user_id:{userId},message:{}}}", data => data.UserId);
                return;
            case MessageContext.Group:
                await CallApiAsync<JOneBotUserInfo, long>("get_login_info", string.Empty, data => data.UserId);
                return;
            case MessageContext.Temp:
                await CallApiAsync<JOneBotUserInfo, long>("get_login_info", string.Empty, data => data.UserId);
                return;            
            default:
                throw new Exception($"Unsupported message context: {context}");
        }
    }

    /// <inheritdoc/>
    public Task Unmute(Module module, long userId, long groupId)
    {
        throw new NotImplementedException();
    }

    /// <inheritdoc/>
    public Task UploadFile(Module module, long groupId, string uploadPath, string filePath)
    {
        throw new NotImplementedException();
    }

    /// <inheritdoc/>
    public Task<bool> Verify(string key)
    {
        throw new NotImplementedException();
    }

    private async Task<TOut> CallApiAsync<TResponse, TOut>(string uri, string? paramJson, Func<TResponse, TOut> extractor)
    {
        var response = await _caller.Call<TResponse>(uri, paramJson);
        if (response.Data == null)
            throw new Exception(response.ToString());
        return extractor(response.Data);
    }

    private async Task UpdateMsgBufferDaemon()
    {
        while (true)
        {

        }
    }
}
