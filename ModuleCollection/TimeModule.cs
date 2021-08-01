using System;

namespace klbotlib.Modules
{
    /// <summary>
    /// 报时模块。一个简单的范例模块
    /// </summary>
    public class TimeModule : SingleTypeModule<MessagePlain>
    {
        /// <inheritdoc/>
        public sealed override bool IsTransparent => true;

        [ModuleStatus]
        private int TimeZone = 8; //默认是UTC+8

        /// <inheritdoc/>
        public override int Filter(MessagePlain msg)
        {
            if (msg.Text == "报时")
                return 1;
            else if (msg.Text.StartsWith("设置时区为"))
                return 2;
            else
                return 0;
        }

        /// <inheritdoc/>
        public override string Processor(MessagePlain msg, int code)
        {
            switch (code)
            {
                case 1:
                    return DateTime.UtcNow.AddHours(TimeZone).ToString() + @"{\face:大哭}";
                case 2:
                    if (int.TryParse(msg.Text.Substring(5), out int result))
                    {
                        TimeZone = result;
                        return $"时区已设置为UTC{TimeZone:+#;-#;#}";
                    }
                    else
                        return $"错误：你输了什么狗屁东西？";
                default:
                    return $"意外收到未知状态码{code}";
            }
        }
    }
}
