namespace klbotlib;

/// <summary>
/// klbot内部使用的消息抽象类。所有QQ消息都继承此类
/// </summary>
public abstract record Message
{
    /// <summary>
    /// 返回此消息是否@了某个ID
    /// </summary>
    /// <param name="id">待判断ID</param>
    public bool ContainsTargetId(long id)
    {
        if (this is MessageAt msgAt)
            return msgAt.TargetId == id;
        else if (this is MessagePackage msgArray)
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
    public bool IsComplex => this is MessagePackage msgArray ? msgArray.Data.Count > 0 : false;
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
        return GetType().Name;
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
