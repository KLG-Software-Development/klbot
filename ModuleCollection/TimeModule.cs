using System;

namespace klbotlib.Modules
{
    /// <summary>
    /// 报时模块。一个简单的范例模块
    /// </summary>
    public class TimeModule : SingleTypeModule<MessagePlain>
    {
        public sealed override string FriendlyName => "报时模块";
        public sealed override string HelpInfo => "发送“报时”获取当前时间\n发送“设置时区为[整数X]”将时区设为UTC+X";
        /// <inheritdoc/>
        public sealed override bool IsTransparent => true;

        [ModuleStatus]
        private int TimeZone = 8; //默认是UTC+8

        /// <inheritdoc/>
        public override string Filter(MessagePlain msg)
        {
            if (msg.Text == "报时")
                return "报时";
            else if (msg.Text.StartsWith("设置时区为"))
                return "设置";
            else
                return null;
        }

        /// <inheritdoc/>
        public override string Processor(MessagePlain msg, string filter_out)
        {
            switch (filter_out)
            {
                case "报时":
                    return DateTime.UtcNow.AddHours(TimeZone).ToString() + @"{\face:大哭}";
                case "设置时区":
                    if (int.TryParse(msg.Text.Substring(5), out int result))
                    {
                        TimeZone = result;
                        return $"时区已设置为UTC{TimeZone:+#;-#;#}";
                    }
                    else
                        return $"错误：你输了什么狗屁东西？";
                default:
                    return $"意外收到未知过滤器输出\"{filter_out}\"";
            }
        }
    }
}
