using System.Threading.Tasks;

namespace klbotlib.Modules;

/// 消息回显模块
public class EchoModule : Module
{
    /// <inheritdoc/>
    public override string FriendlyName => "回显模块";

    /// <inheritdoc/>
    public override string Filter(MessagePackage msgPkg)
    {
        return "in";
    }
    /// <inheritdoc/>
    public override Task<Message> Processor(MessagePackage msgPkg, string? filterOut)
    {
        return Task.FromResult(msgPkg.ToString());
    }
}
