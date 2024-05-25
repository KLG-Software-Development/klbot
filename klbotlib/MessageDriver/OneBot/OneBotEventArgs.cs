using System;
using klbotlib.MessageDriver.OneBot.JsonPrototypes;

namespace klbotlib.MessageDriver.OneBot;

internal class OneBotEventArgs(long time, long selfId, string postType, JOneBotEvent rawEventData) : EventArgs
{
    public long Time { get; } = time;
    public long SelfId { get; } = selfId;
    public string PostType { get; } = postType;
    public JOneBotEvent RawEventData { get; } = rawEventData;
}
