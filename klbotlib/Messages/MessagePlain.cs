﻿namespace klbotlib
{
    /// <summary>
    /// 纯文本消息类
    /// </summary>
    public class MessagePlain : MessageCommon
    {
        /// <summary>
        /// 此消息的文本内容
        /// </summary>
        public string Text { get; private set; }
        /// <summary>
        /// 创建纯文本消息
        /// </summary>
        /// <param name="senderId">发送者ID（若存在）</param>
        /// <param name="groupId">所在群聊ID（若存在）</param>
        /// <param name="context">消息上下文（默认为群聊）</param>
        /// <param name="text">文本内容</param>
        public MessagePlain(long senderId, long groupId, MessageContext context = MessageContext.Group, string text = "") : base(senderId, groupId)
        {
            Text = text;
            Context = context;
        }
        /// <inheritdoc/>
        public override string ToString()
        {
            return $"{base.ToString()}\nText: {Text}";
        }

        internal void AppendText(string text) => Text += text;
        internal override void CopyReferenceTypeMember(Message dstMsg) 
        {
            base.CopyReferenceTypeMember(dstMsg);
        }
    }
}
