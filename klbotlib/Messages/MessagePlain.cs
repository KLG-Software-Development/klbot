namespace klbotlib
{
    /// <summary>
    /// 纯文本消息类
    /// </summary>
    public record MessagePlain : Message
    {
        /// <summary>
        /// 此消息的文本内容
        /// </summary>
        public string Text { get; private set; }
        /// <summary>
        /// 创建纯文本消息
        /// </summary>
        /// <param name="text">文本内容</param>
        public MessagePlain(string text = "")
        {
            Text = text;
        }

        internal void AppendText(string text) => Text += text;
        internal override void CopyReferenceTypeMember(Message dstMsg) 
        {
            base.CopyReferenceTypeMember(dstMsg);
        }
    }
}
