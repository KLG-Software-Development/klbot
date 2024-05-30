using System;
using System.Threading.Tasks;

namespace klbotlib.Modules;

/// <summary>
/// 报时模块。一个简单的范例模块
/// </summary>
public class TimeModule : SingleTypeModule<MessagePlain>
{
    /// <inheritdoc/>
    public sealed override string FriendlyName => "报时模块";
    /// <inheritdoc/>
    public sealed override string HelpInfo => "发送“报时”获取当前时间\n发送“设置时区为[整数X]”将时区设为UTC+X";
    /// <inheritdoc/>
    public sealed override bool IsTransparent => true;

    [ModuleStatus]
    private int _timeZone = 8; //默认是UTC+8

    /// <inheritdoc/>
    public override Task<Message?> Processor(MessageContext context, MessagePlain msg)
    {
        if (msg.Text == "报时")
            return new MessagePackage(DateTime.UtcNow.AddHours(_timeZone).ToString(), new MessageFace("0"));
        else if (msg.Text.StartsWith("设置时区为"))
        {
            if (int.TryParse(msg.Text.AsSpan(5), out int result))
            {
                _timeZone = result;
                return (Message)$"时区已设置为UTC{_timeZone:+#;-#;#}";
            }
            else
                return (Message)$"错误：你输了什么狗屁东西？";
        }
        return (Message?)null;
    }
}
