namespace klbotlib
{
    /// <summary>
    /// 语音消息
    /// </summary>
    public class MessageVoice : MessageCommon
    {
        /// <summary>
        /// 语音的url
        /// </summary>
        public string Url { get; internal set; }

        /// <summary>
        /// 构造语音消息
        /// </summary>
        /// <param name="senderId">发送者ID</param>
        /// <param name="groupId">群聊ID</param>
        /// <param name="url">语音URL</param>
        public MessageVoice(long senderId, long groupId, string url = "") : base(senderId, groupId)
        {
            Url = url;
        }
        /// <summary>
        /// 构造语音消息
        /// </summary>
        /// <param name="senderId">发送者ID</param>
        /// <param name="groupId">群聊ID</param>
        /// <param name="context">消息上下文</param>
        /// <param name="url">语音URL</param>
        public MessageVoice(long senderId, long groupId, MessageContext context, string url = "") : base(senderId, groupId)
        {
            Url = url;
        }
        /// <inheritdoc/>
        public override string ToString()
        {
            return $"{base.ToString()}\nUrl: {Url}";
        }

        internal override void CopyReferenceTypeMember(Message dstMsg) 
        {
            base.CopyReferenceTypeMember(dstMsg);
        }
    }
}
