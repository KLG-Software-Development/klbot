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
        public override bool Filter(MessagePlain msg)
        {
            return msg.Text == "报时" || msg.Text.StartsWith("设置时区为");
        }

        /// <inheritdoc/>
        public override string Processor(MessagePlain msg)
        {
            if (msg.Text == "报时")
                return DateTime.UtcNow.AddHours(TimeZone).ToString() + @"{\face:大哭}";
            else
            {
                if (int.TryParse(msg.Text.Substring(5), out int result))
                {
                    TimeZone = result;
                    return $"时区已设置为UTC{TimeZone:+#;-#;#}";
                }
                else
                    return $"错误：你输了什么狗屁东西？";
            }
        }
    }
}
