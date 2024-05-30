using System;

namespace klbotlib;

/// <summary>
/// 消息上下文
/// </summary>
/// <param name="Type">上下文类型</param>
/// <param name="UserId">用户ID。接收时代表发送者ID，发送时代表接收者ID</param>
/// <param name="GroupId">群聊ID（若存在）</param>
public readonly record struct MessageContext(MessageContextType Type, long UserId, long GroupId)
{
    /// <summary>
    /// 创建消息上下文。其中，上下文类型通过ID值自动推导
    /// </summary>
    /// <param name="userId">用户ID</param>
    /// <param name="groupId">群聊ID</param>
    public MessageContext(long userId, long groupId) : this(MessageContextType.Temp, userId, groupId)
    {
        if (userId <= 0)
        {
            if (groupId <= 0)
                throw new Exception($"Sender and group are both invalid ({userId} and {groupId})");
            Type = MessageContextType.Group;
        }
        else
            Type = groupId <= 0 ? MessageContextType.Private : MessageContextType.Temp;
    }
}

/// <summary>
/// 消息上下文枚举，包括私聊、临时、群聊
/// </summary>
public enum MessageContextType
{
    /// <summary>
    /// 私聊
    /// </summary>
    Private,  
    /// <summary>
    /// 临时会话
    /// </summary>
    Temp, 
    /// <summary>
    /// 群组
    /// </summary>
    Group
}
