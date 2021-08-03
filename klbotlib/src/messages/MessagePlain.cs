namespace klbotlib
{
    /// <summary>
    /// 纯文本消息类
    /// </summary>
    public class MessagePlain : Message
    {
        /// <summary>
        /// 此消息的文本内容
        /// </summary>
        public string Text { get; private set; }
        internal MessagePlain(long sender_id, long group_id, string text = "") : base(sender_id, group_id)
        {
            Text = text;
        }

        internal void AppendText(string text) => Text += text;
    }
}
