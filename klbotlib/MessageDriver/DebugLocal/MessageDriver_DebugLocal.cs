using klbotlib.Events;
using klbotlib.Modules;

namespace klbotlib.MessageDriver.DebugLocal;

/// <summary>
/// 调试用本地模拟消息驱动器
/// </summary>
/// <remarks>
/// 创建本地模拟消息驱动器
/// </remarks>
/// <param name="selfId">自身ID</param>
/// <param name="addMsgCallback">消息缓冲区加入新消息时触发的回调</param>
/// <param name="sendMsgCallback">机器人发送消息时触发的回调</param>
/// <param name="uploadFileCallback">机器人上传文件时触发的回调</param>
/// <param name="muteCallback">机器人禁言他人时触发的回调</param>
/// <param name="unmuteCallback">机器人解除他人禁言时触发的回调</param>
public class MessageDriver_Debug(long selfId, Action<MessageContext, Message> addMsgCallback,
    Action<Module, MessageContextType, long, long, Message> sendMsgCallback,
    Action<Module, long, string, string> uploadFileCallback,
    Action<Module, long, long, uint> muteCallback,
    Action<Module, long, long> unmuteCallback) : IMessageDriver, IKLBotLogUnit
{
    private readonly Dictionary<long, Message> _msgCache = []; //id - msg
    private readonly List<Message> _msgBuffer = [];
    private readonly long _selfId = selfId;

    /// <summary>
    /// 创建一个空Debug消息驱动
    /// </summary>
    /// <param name="selfId"></param>
    public MessageDriver_Debug(long selfId) : this(selfId,
        (_, _) => { },
        (_, _, _, _, _) => { },
        (_, _, _, _) => { },
        (_, _, _, _) => { },
        (_, _, _) => { })
    { }

    /// <summary>
    /// 消息缓冲区加入新消息时触发的回调。参数为消息本身
    /// </summary>
    public Action<MessageContext, Message> AddMessageCallback { get; private set; } = addMsgCallback;
    /// <summary>
    /// 机器人发送消息时触发的回调。参数分别为发送消息的来源模块、消息上下文、目标用户ID、目标群聊ID、待发送消息
    /// </summary>
    public Action<Module, MessageContextType, long, long, Message> SendMessageCallback { get; private set; } = sendMsgCallback;
    /// <summary>
    /// 机器人上传文件时触发的回调。参数为上传文件操作的来源模块、群聊ID、待发送消息
    /// </summary>
    public Action<Module, long, string, string> UploadFileCallback { get; private set; } = uploadFileCallback;
    /// <summary>
    /// 机器人禁言他人时触发的回调。参数为禁言操作的来源模块、禁言用户ID、群聊ID、禁言时长（秒）
    /// </summary>
    public Action<Module, long, long, uint> MuteCallback { get; private set; } = muteCallback;
    /// <summary>
    /// 机器人解除他人禁言时触发的回调。参数为解除禁言操作的来源模块、解除禁言用户ID、群聊ID
    /// </summary>
    public Action<Module, long, long> UnmuteCallback { get; private set; } = unmuteCallback;

    /// <summary>
    /// 向消息驱动中添加未读消息
    /// </summary>
    /// <param name="context">消息上下文</param>
    /// <param name="msgs">待加入的消息</param>
    public Task AddReceivedMessage(MessageContext context, params Message[] msgs)
    {
        foreach (var msg in msgs)
        {
            _msgBuffer.Add(msg);
            _msgCache.Add(_msgCache.Count, msg);
            OnMessageReceived.Invoke(this, new(DateTime.Now, context, msg)).Wait();
            AddMessageCallback.Invoke(context, msg);
        }
        return Task.CompletedTask;
    }

    // -------- 以下为接口实现 --------
    /// <inheritdoc/>
    public string LogUnitName => "Driver/DebugLocal";

    /// <inheritdoc/>
    public string DriverInfo => "Local debug message driver";

    /// <inheritdoc/>
    public event AsyncEventHandler<KLBotMessageEventArgs> OnMessageReceived = (_, _) => Task.CompletedTask;

    /// <inheritdoc/>
    public Task SendMessage(Module module, MessageContextType context, long userId, long groupId, Message msg)
    {
        SendMessageCallback.Invoke(module, context, userId, groupId, msg);
        return Task.CompletedTask;
    }
    /// <inheritdoc/>
    public Task SendMessage(Module module, MessageContext context, Message msg)
    {
        SendMessageCallback.Invoke(module, context.Type, context.UserId, context.GroupId, msg);
        return Task.CompletedTask;
    }
    /// <inheritdoc/>
    public Task UploadFile(Module module, long groupId, string uploadPath, string filePath)
    {
        UploadFileCallback.Invoke(module, groupId, uploadPath, filePath);
        return Task.FromResult<Exception?>(null);
    }
    /// <inheritdoc/>
    public Task<Message> GetMessageFromId(long target, long messageId)
    {
        return Task.FromResult(_msgCache[messageId]);
    }
    /// <inheritdoc/>
    public Task<bool> Verify(string key)
    {
        return Task.FromResult(true);
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
    public Task<long> GetSelfId()
    {
        return Task.FromResult(_selfId);
    }
}
