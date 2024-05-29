namespace klbotlib;

/// <summary>
/// klbot内部使用的消息抽象类。所有QQ消息都继承此类
/// </summary>
public abstract record Message
{
    /// <summary>
    /// 包含此消息的消息包。可能为null
    /// </summary>
    public MessagePackage? Package { get; set; }
    /// <summary>
    /// 尝试获取发送者ID。若无明确发送者（非接收的信息）则返回false
    /// </summary>
    /// <param name="senderId">发送者ID出参</param>
    /// <returns>获取是否成功</returns>
    public bool TryGetSenderId(out long senderId)
    {
        if (Package == null)
        {
            senderId = 0;
            return false;
        }
        senderId = Package.SenderId;
        return true;
    }
    /// <summary>
    /// 添加@目标
    /// </summary>
    /// <param name="id">@的目标ID</param>
    public void AddTargetId(long id)
    {
        if (this is MessagePackage msgPkg)
            msgPkg.Data.Add(new MessageAt(id));
    }

    /// <summary>
    /// 返回此消息是否@了某个ID
    /// </summary>
    /// <param name="id">待判断ID</param>
    public bool ContainsTargetId(long id)
    {
        if (this is MessageAt msgAt)
            return msgAt.TargetId == id;
        else if (this is MessagePackage msgPkg)
        {
            foreach (var msg in msgPkg.Data)
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
    public bool IsComplex => this is MessagePackage msgPkg ? msgPkg.Data.Count > 0 : false;

    internal static MessageEmpty Empty = new MessageEmpty();
}
