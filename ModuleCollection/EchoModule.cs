namespace klbotlib.Modules;

/// 消息回显模块
public class EchoModule : Module
{
    /// <inheritdoc/>
    public override string FriendlyName => "回显模块";

    /// <inheritdoc/>
    public override Task<Message?> Processor(MessageContext context, Message msg)
    {
        return Task.FromResult<Message?>(msg);
    }
}
