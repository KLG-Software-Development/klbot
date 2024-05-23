using klbotlib.Modules;
using System;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace klbotlib.MessageDriver.DebugLocal;

/// <summary>
/// 调试用本地模拟消息驱动器
/// </summary>
public class MessageDriver_Debug : IMessageDriver
{
    private readonly Dictionary<long, Message> _msgCache = new(); //id - msg
    private readonly List<Message> _msgBuffer = new();
    private readonly long _selfId;

    /// <summary>
    /// 消息缓冲区加入新消息时触发的回调。参数为消息本身
    /// </summary>
    public Action<Message> AddMessageCallback { get; private set; }
    /// <summary>
    /// 机器人发送消息时触发的回调。参数分别为发送消息的来源模块、消息上下文、目标用户ID、目标群聊ID、MsgMarker内容
    /// </summary>
    public Action<Module, MessageContext, long, long, string> SendMessageCallback { get; private set; }
    /// <summary>
    /// 机器人上传文件时触发的回调。参数为上传文件操作的来源模块、群聊ID、MsgMarker内容
    /// </summary>
    public Action<Module, long, string, string> UploadFileCallback { get; private set; }
    /// <summary>
    /// 机器人禁言他人时触发的回调。参数为禁言操作的来源模块、禁言用户ID、群聊ID、禁言时长（秒）
    /// </summary>
    public Action<Module, long, long, uint> MuteCallback { get; private set; }
    /// <summary>
    /// 机器人解除他人禁言时触发的回调。参数为解除禁言操作的来源模块、解除禁言用户ID、群聊ID
    /// </summary>
    public Action<Module, long, long> UnmuteCallback { get; private set; }


    /// <summary>
    /// 创建本地模拟消息驱动器
    /// </summary>
    /// <param name="selfId">自身ID</param>
    /// <param name="addMsgCallback">消息缓冲区加入新消息时触发的回调</param>
    /// <param name="sendMsgCallback">机器人发送消息时触发的回调</param>
    /// <param name="uploadFileCallback">机器人上传文件时触发的回调</param>
    /// <param name="muteCallback">机器人禁言他人时触发的回调</param>
    /// <param name="unmuteCallback">机器人解除他人禁言时触发的回调</param>
    public MessageDriver_Debug(long selfId, Action<Message> addMsgCallback, 
        Action<Module, MessageContext, long, long, string> sendMsgCallback, 
        Action<Module, long, string, string> uploadFileCallback,
        Action<Module, long, long, uint> muteCallback,
        Action<Module, long, long> unmuteCallback)
    {
        _selfId = selfId;
        AddMessageCallback = addMsgCallback;
        SendMessageCallback = sendMsgCallback;
        UploadFileCallback = uploadFileCallback;
        MuteCallback = muteCallback;
        UnmuteCallback = unmuteCallback;
    }

    /// <inheritdoc/>
    public string DriverInfo => "Local debug message driver";

    /// <inheritdoc/>
    public Task<List<Message>?> FetchMessages()
    {
        List<Message> msgs = new(_msgBuffer);
        _msgBuffer.Clear();  //清理缓冲区
        return Task.FromResult<List<Message>?>(msgs);
    }
    /// <inheritdoc/>
    public Task SendMessage(Module module, MessageContext context, long userId, long groupId, string content)
    {
        SendMessageCallback.Invoke(module, context, userId, groupId, content);
        return Task.CompletedTask;
    }
    /// <inheritdoc/>
    public Task UploadFile(Module module, long groupId, string uploadPath, string filePath)
    {
        UploadFileCallback.Invoke(module, groupId, uploadPath, filePath);
        return Task.FromResult<Exception?>(null);
    }
    /// <summary>
    /// 向消息驱动中添加未读消息
    /// </summary>
    /// <param name="msgs">待加入的消息</param>
    public void AddReceivedMessage(params Message[] msgs)
    {
        _msgBuffer.AddRange(msgs);
        foreach (var msg in msgs)
        {
            _msgCache.Add(_msgCache.Count, msg);
        }
        foreach (var msg in msgs)
        {
            AddMessageCallback.Invoke(msg);
        }
    }

    /// <inheritdoc/>
    public Task<Message> GetMessageFromID(long target, long messageId)
    {
        return Task.FromResult(_msgCache[messageId]);
    }
    /// <inheritdoc/>
    public Task<bool> Verify(string key)
    {
        return Task.FromResult<bool>(true);
    }
    /// <inheritdoc/>
    public Task Mute(Module module, long userId, long groupId, uint durationSeconds)
    {
        MuteCallback.Invoke(module, userId, groupId, durationSeconds);
        return Task.CompletedTask;
    }
    /// <inheritdoc/>
    public Task Unmute(Module module, long userId, long groupId)
    {
        UnmuteCallback.Invoke(module, userId, groupId);
        return Task.CompletedTask;
    }
    /// <inheritdoc/>
    public Task<long> GetSelfID()
    {
        return Task.FromResult(_selfId);
    }
}
