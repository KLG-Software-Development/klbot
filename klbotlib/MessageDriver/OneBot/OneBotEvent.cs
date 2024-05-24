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

internal class OneBotEventManager
{
    public event EventHandler<OneBotEventArgs>? OneBotEventReceived;

    public OneBotEventManager()
    {
        OneBotEventReceived += OneBotEventLog; // 默认事件
    }

    public void RaiseOneBotEvent(long time, long selfId, string postType, JsonObject? data) =>
        OneBotEventReceived?.Invoke(this, new(time, selfId, postType, data));
    
    private static void OneBotEventLog(object? obj, OneBotEventArgs e)
    {
        Console.WriteLine($"[Event][{e.Time}][{e.PostType}][{e.SelfId}] {e.Data}");
    }
}
