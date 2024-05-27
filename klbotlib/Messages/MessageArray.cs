using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace klbotlib;

/// <summary>
/// 消息数组类
/// </summary>
public class MessageArray : MessageCommon
{
    /// <summary>
    /// 消息数组
    /// </summary>
    public Message[] Data { get; }
    
    /// <summary>
    /// 构造一条消息数组消息
    /// </summary>
    public MessageArray(long senderId, long groupId, params Message[] msgs) : base(senderId, groupId)
    {
        Data = msgs;
    }

    /// <summary>
    /// 构造一条消息数组消息
    /// </summary>
    public MessageArray(long senderId, long groupId, IEnumerable<Message> msgs) : base(senderId, groupId)
    {
        Data = msgs.ToArray();
    }

    /// <inheritdoc/>
    public override string ToString()
    {
        StringBuilder sb = new("Message array: ");
        foreach (var msg in Data)
            sb.AppendLine(msg.ToString());
        return sb.ToString();
    }

    internal override void CopyReferenceTypeMember(Message dstMsg) { }
}
