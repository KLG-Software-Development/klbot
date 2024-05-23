using System.Diagnostics;

namespace klbotlib.MessageDriver.OneBot;
internal static class OneBotEventHandler
{
    public static void OneBotEventLog(object obj, OneBotEventArgs e)
    {
        Debug.Print($"[Event][{e.Time}][{e.PostType}][{e.SelfId}] {e.Data}");
    }
}
