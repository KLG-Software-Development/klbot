using klbotlib;
using klbotlib.Modules;

namespace ModuleCollection;

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
    public override string Processor(Message msg, string? filterOut)
    {
        return msg.ToString();
    }
}
