namespace klbotlib.Events;

/// <summary>
/// 通用KLBot事件
/// </summary>
/// <param name="time">事件发生（或最早构建）时间</param>
/// <param name="desc">事件描述</param>
public class KLBotEventArgs(DateTime time, string desc) : EventArgs
{
    /// <summary>
    /// 事件发生（或最早构建）时间
    /// </summary>
    public DateTime Timestamp { get; } = time;
    /// <summary>
    /// 事件描述
    /// </summary>
    public string Description { get; } = desc;
}

/// <summary>
/// KLBot消息事件
/// </summary>
/// <param name="time">事件发生（或最早构建）时间</param>
/// <param name="context">消息上下文</param>
/// <param name="msg">对应的消息</param>
public class KLBotMessageEventArgs(DateTime time, MessageContext context, Message msg) : KLBotEventArgs(time, msg.ToString())
{
    /// <summary>
    /// 消息上下文
    /// </summary>
    public MessageContext Context { get; } = context;
    /// <summary>
    /// 对应的消息
    /// </summary>
    public Message Message { get; } = msg;
}
