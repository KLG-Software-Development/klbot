using System;

namespace klbotlib.Modules
{
    public class TimeModule : SingleTypeModule<MessagePlain>
    {
        public override bool IsTransparent => true;

        [ModuleStatus]
        private int time_zone = 8; //默认是UTC+8

        public override bool Filter(MessagePlain msg)
        {
            return msg.Text == "报时" || msg.Text.StartsWith("设置时区为");
        }

        public override string Processor(MessagePlain msg)
        {
            if (msg.Text == "报时")
            {
                return DateTime.UtcNow.AddHours(time_zone).ToString();
            }
            else
            {
                if (int.TryParse(msg.Text.Substring(5), out int result))
                {
                    time_zone = result;
                    return $"时区已设置为UTC{time_zone:+#;-#;#}";
                }
                else
                    return $"错误：你输了什么狗屁东西？";
            }
        }

        public TimeModule(KLBot host_bot) : base(host_bot) { }
    }
}
