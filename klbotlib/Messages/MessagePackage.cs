using System.Collections.Generic;
using System.Text;

namespace klbotlib;

/// <summary>
/// 消息数组类. 由若干Message与相应收发相关信息组成
/// </summary>
public record MessagePackage : Message
{
    /// <summary>
    /// 此消息的上下文。私聊=Private；临时会话=Temp；群聊=Group
    /// </summary>
    public MessageContext Context { get; internal set; }
    /// <summary>
    /// 发送者ID
    /// </summary>
    public long SenderId { get; set; } = -1;
    /// <summary>
    /// 此消息来源的群组的ID（群号）。如果消息来源是私聊则为-1；如果消息来源是群组则为群号；如果消息来源是临时会话则为“临时会话所通过的群”的群号。
    /// </summary>
    public long GroupId { get; internal set; } = -1;
    /// <summary>
    /// 消息数组
    /// </summary>
    public List<Message> Data { get; } = [];

    /// <summary>
    /// 构造一条消息数组消息
    /// </summary>
    public MessagePackage(params Message[] msgs)
    {
        Data.AddRange(msgs);
    }
    /// <summary>
    /// 构造一条消息数组消息
    /// </summary>
    public MessagePackage(IEnumerable<Message> msgs)
    {
        Data.AddRange(msgs);
    }
    /// <summary>
    /// 构造一条消息数组消息
    /// </summary>
    public MessagePackage(long senderId, long groupId, params Message[] msgs) : this(msgs)
    {
        Data.AddRange(msgs);
        SenderId = senderId;
        GroupId = groupId;
    }

    /// <inheritdoc/>
    public override string ToString()
    {
        StringBuilder sb = new("Message array: ");
        foreach (var msg in Data)
            sb.AppendLine(msg.ToString());
        return sb.ToString();
    }
    /// <summary>
    /// 添加@ID
    /// </summary>
    /// <param name="id"></param>
    public void AddTargetId(long id)
    {
        Data.Add(new MessageAt(id));
    }
    internal override void CopyReferenceTypeMember(Message dstMsg)
    {
        foreach (var msg in Data)
        {
            if (msg is MessageAt msgAt)
                ((MessagePackage)dstMsg).AddTargetId(msgAt.TargetId);
        }
    }
}
