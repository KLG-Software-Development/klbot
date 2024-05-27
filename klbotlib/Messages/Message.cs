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
    public long GroupId { get; internal set; } = -1;

    /// <summary>
    /// 返回此消息是否@了某个ID
    /// </summary>
    /// <param name="id">待判断ID</param>
    public bool ContainsTargetId(long id)
    {
        if (this is MessageAt msgAt)
            return msgAt.TargetId == id;
        else if (this is MessageArray msgArray)
        {
            foreach (var msg in msgArray.Data)
            {
                if (msg.ContainsTargetId(id))
                    return true;
            }
        }
        return false;
    }
    /// <summary>
    /// 是否为复杂消息
    /// </summary>
    public bool IsComplex => this is MessageArray msgArray ? msgArray.Data.Length > 0 : false;
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
            ? $"Type: {GetType().Name}\nContext: {Context}\nGroup: {GroupId}"
            : $"Type: {GetType().Name}\nContext: {Context}";
    }
  
    internal static MessageEmpty Empty = new MessageEmpty();
    /// <summary>
    /// 将此消息中的引用类型字段拷贝到目标消息中
    /// </summary>
    /// <param name="dstMsg">目标消息</param>
    internal abstract void CopyReferenceTypeMember(Message dstMsg);

    public void AddTargetId(long id)
    {
        if (this is MessageArray msgArray)
            msgArray.Data.Add(new MessageAt(this.UserId));
    }
}
