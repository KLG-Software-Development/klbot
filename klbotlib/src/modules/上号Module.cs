namespace klbotlib.Modules
{
    public class 上号Module : SingleTypeModule<MessagePlain>
    {
        [ModuleStatus]
        private string LastMsg = "";
        public override bool UseSignature => false;
        public override bool Filter(MessagePlain msg) => true;
        public override string Processor(MessagePlain msg)
        {
            string output;
            if (msg.Text.Trim() == "上号" && LastMsg != "上号")
                output = "上号";
            else
                output = string.Empty;
            LastMsg = msg.Text.Trim();
            return output;
        }
    }
}
