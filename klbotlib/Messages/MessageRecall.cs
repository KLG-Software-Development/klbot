using System;

namespace klbotlib;

/// <summary>
/// 撤回消息Message类
/// </summary>
public class MessageRecall : Message
{
    /// <summary>
    /// 被撤回的消息的ID
    /// </summary>
    public long MessageID { get; }
    /// <summary>
    /// 被撤回消息的发送者ID
    /// </summary>
    public long AuthorID { get; }
    /// <summary>
    /// 来自基类的未定义字段。为避免混淆，此字段在本类型中恒为-1，不应被使用。获取被撤回消息的发送者ID应使用“AuthorID”字段；获取撤回者的消息应使用“OperatorID”字段
    /// </summary>
    [Obsolete("为避免混淆，此字段在本消息类中恒为-1，不应被使用。获取被撤回消息的发送者ID应使用“AuthorID”字段；获取撤回者的消息应使用“OperatorID”字段")]
    public new long SenderID { get; }
    /// <summary>
    /// 撤回者的ID
    /// </summary>
    public long OperatorID { get; }
    /// <summary>
    /// 构造一条撤回消息
    /// </summary>
    /// <param name="authorId">被撤回消息的发送者ID</param>
    /// <param name="operatorId">消息撤回者ID</param>
    /// <param name="groupId">撤回所在群的ID（如果存在）</param>
    /// <param name="msgId">被撤回的消息ID</param>
    public MessageRecall(long authorId, long operatorId, long groupId, long msgId) : base(-1, groupId)
    {
        AuthorID = authorId;
        OperatorID = operatorId;
        MessageID = msgId;
    }
}
