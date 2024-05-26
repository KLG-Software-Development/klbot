using klbotlib.Events;
using klbotlib.Modules;
using System;
using System.Threading.Tasks;

namespace klbotlib;

/// <summary>
/// 消息服务器接口。提供消息收发API
/// </summary>
public interface IMessageDriver : IKLBotLogUnit
{
    /// <summary>
    /// 获取消息驱动信息
    /// </summary>
    string DriverInfo { get; }
    /// <summary>
    /// 新消息事件
    /// </summary>
    event EventHandler<KLBotMessageEventArgs> OnMessageReceived;
    /// <summary>
    /// 向消息服务器验证身份
    /// </summary>
    /// <param name="key">验证密钥</param>
    /// <returns>验证是否成功</returns>
    Task<bool> Verify(string key);
    /// <summary>
    /// 根据消息ID，从服务器获取给定消息
    /// </summary>
    /// <param name="target">获取消息的对象</param>
    /// <param name="messageId">消息ID</param>
    /// <returns>相应的消息对象</returns>
    Task<Message> GetMessageFromId(long target, long messageId);
    /// <summary>
    /// 发送消息接口
    /// </summary>
    /// <param name="module">编译MsgMarker时使用的模块</param>
    /// <param name="context">发送的消息上下文类型</param>
    /// <param name="userId">用户ID</param>
    /// <param name="groupId">群组ID</param>
    /// <param name="msg">待发送消息</param>
    Task SendMessage(Module module, MessageContext context, long userId, long groupId, Message msg);
    /// <summary>
    /// 上传群文件接口
    /// </summary>
    /// <param name="module">模块</param>
    /// <param name="groupId">群聊ID</param>
    /// <param name="uploadPath">上传的目标路径</param>
    /// <param name="filePath">文件相对于模块私有目录的本地路径</param>
    Task UploadFile(Module module, long groupId, string uploadPath, string filePath);
    /// <summary>
    /// 禁言接口
    /// </summary>
    /// <param name="module">模块</param>
    /// <param name="userId">禁言用户ID</param>
    /// <param name="groupId">群聊ID</param>
    /// <param name="durationSeconds">禁言时长（秒）</param>
    Task Mute(Module module, long userId, long groupId, uint durationSeconds);
    /// <summary>
    /// 解除禁言接口
    /// </summary>
    /// <param name="module">模块</param>
    /// <param name="userId">解除禁言用户ID</param>
    /// <param name="groupId">群聊ID</param>
    Task Unmute(Module module, long userId, long groupId);
    /// <summary>
    /// 获取自身ID接口
    /// </summary>
    Task<long> GetSelfId();
}
