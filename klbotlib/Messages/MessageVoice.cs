using System;
using System.Collections.Generic;
using System.Text;

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

        internal MessageVoice(long sender_id, long group_id, string url = "") : base(sender_id, group_id)
        {
            Url = url;
        }
    }
}
