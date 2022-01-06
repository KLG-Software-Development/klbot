using System.Collections.Generic;

namespace klbotlib
{
    /// <summary>
    /// 图文消息
    /// </summary>
    public class MessageImagePlain : MessageCommon
    {
        private readonly List<string> _url_list = new List<string>();

        /// <summary>
        /// 图像的Url
        /// </summary>
        public IReadOnlyList<string> UrlList { get => _url_list; }
        /// <summary>
        /// 随图像一同发送的文字
        /// </summary>
        public string Text { get; }

        internal MessageImagePlain(long sender_id, long group_id, string text = "") : base(sender_id, group_id)
        {
            Text = text;
        }
        internal MessageImagePlain(long sender_id, long group_id, string text, IEnumerable<string> url_list) : base(sender_id, group_id)
        {
            Text = text;
            AddRange(url_list);
        }

        internal void Add(params string[] url) => _url_list.AddRange(url);
        internal void AddRange(IEnumerable<string> url) => _url_list.AddRange(url);
    }
}
