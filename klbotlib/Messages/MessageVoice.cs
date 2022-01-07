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

        internal MessageVoice(long senderId, long groupId, string url = "") : base(senderId, groupId)
        {
            Url = url;
        }
    }
}
