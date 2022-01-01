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
        /// <summary>
        /// 创建纯文本消息
        /// </summary>
        /// <param name="sender_id">发送者ID（若存在）</param>
        /// <param name="group_id">所在群聊ID（若存在）</param>
        /// <param name="text">文本内容</param>
        public MessagePlain(long sender_id, long group_id, string text = "") : base(sender_id, group_id)
        {
            Text = text;
        }

        internal void AppendText(string text) => Text += text;
    }
}
