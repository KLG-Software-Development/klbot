using System;

namespace klbotlib.Modules
{
    public class TimeModule : SingleTypeModule<MessagePlain>
    {
        public override bool IsTransparent => true;

        [ModuleStatus]
        private int TimeZone = 8; //默认是UTC+8

        public override bool Filter(MessagePlain msg)
        {
            return msg.Text == "报时" || msg.Text.StartsWith("设置时区为");
        }

        public override string Processor(MessagePlain msg)
        {
            if (msg.Text == "报时")
            {
                return DateTime.UtcNow.AddHours(TimeZone).ToString();
            }
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

        public TimeModule(KLBot host_bot) : base(host_bot) { }
    }
}
