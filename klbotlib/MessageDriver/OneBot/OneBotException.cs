using System;

namespace klbotlib.MessageDriver.OneBot;

internal class OneBotException(string msg) : Exception(msg);
internal class OneBotResponseException : OneBotException
{
    public OneBotResponseException(long code, string? msg) : base($"OneBot错误[{code}]：{msg}") { }
}
internal class OneBotProtocolException(string msg) : OneBotException(msg);
internal class OneBotWebhookException(string msg) : OneBotException(msg);
