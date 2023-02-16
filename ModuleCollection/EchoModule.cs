using System.Threading.Tasks;

namespace klbotlib.Modules;

/// 消息回显模块
public class EchoModule : Module
{
    /// <inheritdoc/>
    public override string FriendlyName => "回显模块";

    /// <inheritdoc/>
    public override string Filter(Message msg)
    {
        return "in";
    }
    /// <inheritdoc/>
    public override Task<string> Processor(Message msg, string? filterOut)
    {
        return Task.FromResult(msg.ToString());
    }
}
