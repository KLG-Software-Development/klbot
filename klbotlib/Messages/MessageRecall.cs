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
    public MessageRecall(long authorId, long operatorId, long groupId, long msgId)
    {
        AuthorID = authorId;
        OperatorID = operatorId;
        GroupID = groupId;
        MessageID = msgId;
    }

    internal override void CopyReferenceTypeMember(Message dstMsg) { }
}
