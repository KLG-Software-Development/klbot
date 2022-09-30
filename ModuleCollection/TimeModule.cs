using System;

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
    public override string? Filter(MessagePlain msg)
    {
        return msg.Text == "报时"
            ? "报时"
            : msg.Text.StartsWith("设置时区为")
                ? "设置时区"
                : null;
    }

    /// <inheritdoc/>
    public override string? Processor(MessagePlain msg, string? filterOut)
    {
        switch (filterOut)
        {
            case "报时":
                return DateTime.UtcNow.AddHours(_timeZone).ToString() + @"{\face:大哭}";
            case "设置时区":
                if (int.TryParse(msg.Text.AsSpan(5), out int result))
                {
                    _timeZone = result;
                    return $"时区已设置为UTC{_timeZone:+#;-#;#}";
                }
                else
                    return $"错误：你输了什么狗屁东西？";
            default:
                return $"意外收到未知过滤器输出\"{filterOut}\"";
        }
    }
}
