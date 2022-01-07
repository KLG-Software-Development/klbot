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
    private readonly List<MessageCommon> _msgBuffer = new();

    /// <summary>
    /// 消息缓冲区加入新消息时触发的回调。参数为从消息生成的调试信息
    /// </summary>
    public Action<string> AddMessageCallback { get; private set; }
    /// <summary>
    /// 机器人发送消息时触发的回调。参数为从消息生成的调试信息
    /// </summary>
    public Action<string> SendMessageCallback { get; private set; }
    /// <summary>
    /// 机器人上传文件时触发的回调。参数为从消息生成的调试信息
    /// </summary>
    public Action<string> UploadFileCallback { get; private set; }

    /// <summary>
    /// 创建本地模拟消息服务器
    /// </summary>
    /// <param name="addMsgCallback">消息缓冲区加入新消息时触发的回调</param>
    /// <param name="sendMsgCallback">机器人发送消息时触发的回调</param>
    /// <param name="uploadFileCallback">机器人上传文件时触发的回调</param>
    public DebugMessageServer(Action<string> addMsgCallback, Action<string> sendMsgCallback, Action<string> uploadFileCallback)
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
    public Exception SendMessage(Module module, MessageContext context, long userId, long groupId, string content)
    {
        SendMessageCallback.Invoke(GetMessageDebugInfo(module, context, userId, groupId, content));
        return null;
    }
    /// <inheritdoc/>
    public Exception UploadFile(Module module, long groupId, string uploadPath, string filePath)
    {
        UploadFileCallback.Invoke(GetUploadDebugInfo(module, groupId, uploadPath, filePath));
        return null;
    }
    /// <summary>
    /// 向消息服务器中添加未读消息
    /// </summary>
    /// <param name="msgs">待加入的消息</param>
    public void AddReceivedMessage(params MessageCommon[] msgs)
    {
        _msgBuffer.AddRange(msgs);
        foreach (var msg in msgs)
        {
            _msgCache.Add(_msgCache.Count, msg);
        }
        foreach (var msg in msgs)
        {
            AddMessageCallback.Invoke(GetMessageDebugInfo(msg));
        }
    }

    /// <summary>
    /// 生成消息的调试信息字符串
    /// </summary>
    /// <returns>消息的调试信息</returns>
    private string GetMessageDebugInfo(MessageCommon msg)
    {
        string content;
        if (msg is MessagePlain pmsg)
            content = pmsg.Text;
        else if (msg is MessageImage imsg)
            content = $"[图片x{imsg.UrlList.Count}]";
        else if (msg is MessageImagePlain ipmsg)
            content = $"[图片x{ipmsg.UrlList.Count}]{ipmsg.Text}";
        else if (msg is MessageVoice vmsg)
            content = $"[语音消息]";
        else
            content = $"[未知类型消息：{msg}]";
        return msg.Context switch
        {
            MessageContext.Group => $"* 用户[{msg.SenderID}]向群组[{msg.GroupID}]发送：\n------------------------------------\n  {content}\n------------------------------------",
            MessageContext.Temp => $"* 用户[{msg.SenderID}]通过群组[{msg.GroupID}]发送：\n------------------------------------\n  {content}\n------------------------------------",
            MessageContext.Private => $"* 用户[{msg.SenderID}]发送：\n------------------------------------\n  {content}\n------------------------------------",
            _ => $"* 用户[{msg.SenderID}]向群组[{msg.GroupID}]或机器人发送了未知类型[{msg.Context}]的消息，内容：\n------------------------------------\n  {content}\n------------------------------------",
        };
    }
    /// <summary>
    /// 生成消息的调试信息字符串
    /// </summary>
    /// <param name="module">模块</param>
    /// <param name="context">消息上下文</param>
    /// <param name="userId">目标用户ID</param>
    /// <param name="groupId">目标群组ID</param>
    /// <param name="content">MsgMarker内容</param>
    /// <returns>消息的调试信息</returns>
    private string GetMessageDebugInfo(Module module, MessageContext context, long userId, long groupId, string content)
    {
        return context switch
        {
            MessageContext.Group => $"* 模块[{module}]向群组[{groupId}]发送：\n------------------------------------\n  {content}\n------------------------------------",
            MessageContext.Temp => $"* 模块[{module}]通过群组[{groupId}]向用户[{userId}]发送：\n------------------------------------\n  {content}\n------------------------------------",
            MessageContext.Private => $"* 模块[{module}]向用户[{userId}]发送：\n------------------------------------\n  {content}\n------------------------------------",
            _ => $"* 模块[{module}]向群组[{groupId}]或用户[{userId}]发送了未知类型[{context}]的消息，内容：\n------------------------------------\n  {content}\n------------------------------------",
        };
    }
    /// <summary>
    /// 生成消息的调试信息字符串
    /// </summary>
    /// <param name="module">模块</param>
    /// <param name="groupId">目标群组ID</param>
    /// <param name="uploadPath"></param>
    /// <param name="filePath"></param>
    /// <returns>消息的调试信息</returns>
    private string GetUploadDebugInfo(Module module, long groupId, string uploadPath, string filePath)
    {
        return $"* 模块[{module}]向群组[{groupId}]上传文件[{filePath}]到群文件夹[{uploadPath}]";
    }
    /// <inheritdoc/>
    public Message GetMessageFromID(long id)
    {
        return _msgCache[id];
    }
}
