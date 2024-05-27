using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace klbotlib;

/// <summary>
/// 消息数组类
/// </summary>
public class MessageArray
    /// <summary>
    /// 消息数组
    /// </summary>
    public List<Message> Data { get; } = [];
    
    /// <summary>
    /// 构造一条消息数组消息
    /// </summary>
    public MessageArray(long senderId, long groupId, params Message[] msgs) : base(senderId, groupId)
    {
        Data.AddRange(msgs);
        List<long> targets = [];
    }

    /// <summary>
    /// 构造一条消息数组消息
    /// </summary>
    public MessageArray(long senderId, long groupId, IEnumerable<Message> msgs) : base(senderId, groupId)
    {
        Data.AddRange(msgs);
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
