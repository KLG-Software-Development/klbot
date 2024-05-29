using System;

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
    /// <summary>
    /// 指示事件是否已被KLBot处理（其它非KLBot订阅者仍可处理）
    /// </summary>
    public bool KLBotProcessed { get; internal set; }
}

/// <summary>
/// KLBot消息事件
/// </summary>
/// <param name="time">事件发生（或最早构建）时间</param>
/// <param name="msg">对应的消息</param>
public class KLBotMessageEventArgs(DateTime time, MessagePackage msg) : KLBotEventArgs(time, msg.ToString())
{
    /// <summary>
    /// 对应的消息
    /// </summary>
    public MessagePackage Message { get; } = msg;
}
