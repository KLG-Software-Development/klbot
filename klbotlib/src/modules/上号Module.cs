namespace klbotlib.Modules
{
    /// <summary>
    /// 上号模块
    /// </summary>
    public class 上号Module : SingleTypeModule<MessagePlain>
    {
        [ModuleStatus(IsHidden = true)]
        private string LastMsg = "";
        private bool Is上号(string text) => text.Length <= 5 && text.Contains("上号");
        /// <summary>
        /// 关闭模块签名
        /// </summary>
        public override bool UseSignature => false;
        /// <summary>
        /// 过滤器：处理任何消息
        /// </summary>
        public override bool Filter(MessagePlain msg) => true;
        /// <summary>
        /// 处理器：内容包含上号且不长于五个字符，则复读内容；
        /// 另外，缓存当前消息到LastMsg中，用于下一次判断是否是同一轮上号消息。如果是同一轮则不回复。
        /// </summary>
        public override string Processor(MessagePlain msg)
        {
            string msg_text = msg.Text.Trim(), output = string.Empty;
            if (Is上号(msg_text) && !Is上号(LastMsg))
                output = msg_text;
            LastMsg = msg_text;
            return output;
        }
    }
}
