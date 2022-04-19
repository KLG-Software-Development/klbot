using System.Text;

namespace klbotlib;

/// <summary>
/// klbot内部使用的消息抽象类。所有QQ消息都继承此类
/// </summary>
public abstract class Message
{
    /// <summary>
    /// 此消息的上下文。私聊=Private；临时会话=Temp；群聊=Group
    /// </summary>
    public MessageContext Context { get; set; }
    /// <summary>
    /// 此消息来源的群组的ID（群号）。如果消息来源是私聊则为-1；如果消息来源是群组则为群号；如果消息来源是临时会话则为“临时会话所通过的群”的群号。
    /// </summary>
    public long GroupID { get; internal set; } = -1;
    /// <inheritdoc/>
    public override string ToString()
    {
        return (Context == MessageContext.Group || Context == MessageContext.Temp)
            ? $"{GetType().Name}\nContext: {Context}\nGroup: {GroupID}"
            : $"{GetType().Name}\nContext: {Context}";
    }

    internal static MessageEmpty Empty = new MessageEmpty();
}
