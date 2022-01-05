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
    /// 构造一条撤回消息
    /// </summary>
    /// <param name="senderId">撤回者ID</param>
    /// <param name="groupId">撤回所在群的ID（如果存在）</param>
    /// <param name="msgId">被撤回的消息ID</param>
    public MessageRecall(long senderId, long groupId, long msgId) : base(senderId, groupId)
    {
        MessageID = msgId;
    }
}
