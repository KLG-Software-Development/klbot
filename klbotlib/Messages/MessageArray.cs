using System.Text;

namespace klbotlib;

/// <summary>
/// 消息数组类
/// </summary>
public class MessageArray : Message
{
    /// <summary>
    /// 消息数组
    /// </summary>
    public Message[] Data { get; }
    
    /// <summary>
    /// 构造一条消息数组消息
    /// </summary>
    public MessageArray(params Message[] msgs)
    {
        Data = msgs;
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
