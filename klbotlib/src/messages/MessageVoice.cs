using System;
using System.Collections.Generic;
using System.Text;

namespace klbotlib
{
    public class MessageVoice : Message
    {
        /// <summary>
        /// 语音的url
        /// </summary>
        public string Url { get; internal set; }

        public MessageVoice(long sender_id, long group_id, string url = "") : base(sender_id, group_id)
        {
            Url = url;
        }
    }
}
