using System;
using System.Text.Json.Nodes;

namespace klbotlib.MessageDriver.OneBot;

internal class OneBotEventArgs(long time, long selfId, string postType, JsonObject? data) : EventArgs
{
    public long Time { get; } = time;
    public long SelfId { get; } = selfId;
    public string PostType { get; } = postType;
    public JsonObject? Data { get; } = data;
}

internal class OneBotEventRaiser
{
    public event EventHandler<OneBotEventArgs>? OneBotEventReceived;
    public void RaiseEvent(long time, long selfId, string postType, JsonObject? data) =>
        OneBotEventReceived?.Invoke(this, new(time, selfId, postType, data));
}
