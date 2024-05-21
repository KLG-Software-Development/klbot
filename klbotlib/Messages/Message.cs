using System;

namespace klbotlib;

/// <summary>
/// klbot内部使用的消息抽象类。所有QQ消息都继承此类
/// </summary>
public abstract class Message
{
    /// <summary>
    /// 此消息的上下文。私聊=Private；临时会话=Temp；群聊=Group
    /// </summary>
    public MessageContext Context { get; internal set; }
    /// <summary>
    /// 此消息来源的群组的ID（群号）。如果消息来源是私聊则为-1；如果消息来源是群组则为群号；如果消息来源是临时会话则为“临时会话所通过的群”的群号。
    /// </summary>
    public long GroupID { get; internal set; } = -1;

    /// <summary>
    /// 返回消息对象的一个深拷贝
    /// </summary>
    /// <returns></returns>
    public Message DeepCopy()
    {
        Message copy = (Message)MemberwiseClone();
        CopyReferenceTypeMember(copy);
        return copy;
    }
    /// <inheritdoc/>
    public override string ToString()
    {
        return (Context == MessageContext.Group || Context == MessageContext.Temp)
            ? $"Type: {GetType().Name}\nContext: {Context}\nGroup: {GroupID}"
            : $"Type: {GetType().Name}\nContext: {Context}";
    }
  
    internal static MessageEmpty Empty = new MessageEmpty();
    /// <summary>
    /// 将此消息中的引用类型字段拷贝到目标消息中
    /// </summary>
    /// <param name="dstMsg">目标消息</param>
    internal abstract void CopyReferenceTypeMember(Message dstMsg);
}
