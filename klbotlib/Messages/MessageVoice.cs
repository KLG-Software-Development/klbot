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

        internal override void CopyReferenceTypeMember(Message dstMsg) { }
    }
}
