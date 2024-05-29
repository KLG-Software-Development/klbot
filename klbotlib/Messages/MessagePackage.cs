using System.Collections.Generic;
using System.Text;

namespace klbotlib;

/// <summary>
/// 消息包类. 由若干Message与相应收发相关信息组成
/// </summary>
public record MessagePackage : Message
{
    /// <summary>
    /// 此消息的上下文。私聊=Private；临时会话=Temp；群聊=Group
    /// </summary>
    public MessageContext Context { get; }
    /// <summary>
    /// 发送者ID
    /// </summary>
    public long SenderId { get; }
    /// <summary>
    /// 此消息来源的群组的ID（群号）。如果消息来源是私聊则为-1；如果消息来源是群组则为群号；如果消息来源是临时会话则为“临时会话所通过的群”的群号。
    /// </summary>
    public long GroupId { get; }
    /// <summary>
    /// 消息包
    /// </summary>
    public List<Message> Data { get; } = [];

    /// <summary>
    /// 构造一条消息包
    /// </summary>
    public MessagePackage(MessageContext context, long senderId, long groupId, string plain)
    {
        Data.Add(new MessagePlain(plain));
        SenderId = senderId;
        GroupId = groupId;
    }
    /// <summary>
    /// 构造一条消息包
    /// </summary>
    public MessagePackage(MessageContext context, long senderId, long groupId, params Message[] msgs)
    {
        Data.AddRange(msgs);
        SenderId = senderId;
        GroupId = groupId;
    }
    /// <summary>
    /// 构造一条消息包
    /// </summary>
    public MessagePackage(MessageContext context, long senderId, long groupId, IEnumerable<Message> msgs)
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
    /// 尝试降解MessagePackage。若MessagePackage有唯一元素则返回该消息，否则降解失败返回自身
    /// </summary>
    /// <returns>消息包内唯一消息的，或消息包自身</returns>
    public Message Collapse()
        => Data.Count == 1 ? Data[0] : this;
}
