using klbotlib.Modules;
using System;
using System.Collections.Generic;

namespace klbotlib.MessageServer.Debug;

/// <summary>
/// 调试用本地模拟消息服务器
/// </summary>
public class DebugMessageServer : IMessageServer
{
    private readonly Dictionary<long, Message> _msgCache = new(); //id - msg
    private readonly List<Message> _msgBuffer = new();

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
    /// 创建本地模拟消息服务器
    /// </summary>
    /// <param name="addMsgCallback">消息缓冲区加入新消息时触发的回调</param>
    /// <param name="sendMsgCallback">机器人发送消息时触发的回调</param>
    /// <param name="uploadFileCallback">机器人上传文件时触发的回调</param>
    public DebugMessageServer(Action<Message> addMsgCallback, Action<Module, MessageContext, long, long, string> sendMsgCallback, Action<Module, long, string, string> uploadFileCallback)
    {
        AddMessageCallback = addMsgCallback;
        SendMessageCallback = sendMsgCallback;
        UploadFileCallback = uploadFileCallback;
    }

    /// <inheritdoc/>
    public List<Message> FetchMessages()
    {
        List<Message> msgs = new(_msgBuffer);
        _msgBuffer.Clear();  //清理缓冲区
        return msgs;
    }
    /// <inheritdoc/>
    public Exception? SendMessage(Module module, MessageContext context, long userId, long groupId, string content)
    {
        SendMessageCallback.Invoke(module, context, userId, groupId, content);
        return null;
    }
    /// <inheritdoc/>
    public Exception? UploadFile(Module module, long groupId, string uploadPath, string filePath)
    {
        UploadFileCallback.Invoke(module, groupId, uploadPath, filePath);
        return null;
    }
    /// <summary>
    /// 向消息服务器中添加未读消息
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
    public Message GetMessageFromID(long id)
    {
        return _msgCache[id];
    }
    /// <inheritdoc/>
    public bool Verify(string key)
    {
        return true;
    }
}
