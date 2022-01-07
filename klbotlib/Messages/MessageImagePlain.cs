using System.Collections.Generic;

namespace klbotlib
{
    /// <summary>
    /// 图文消息
    /// </summary>
    public class MessageImagePlain : MessageCommon
    {
        private readonly List<string> _urlList = new List<string>();

        /// <summary>
        /// 图像的Url
        /// </summary>
        public IReadOnlyList<string> UrlList { get => _urlList; }
        /// <summary>
        /// 随图像一同发送的文字
        /// </summary>
        public string Text { get; }

        internal MessageImagePlain(long senderId, long groupId, string text = "") : base(senderId, groupId)
        {
            Text = text;
        }
        internal MessageImagePlain(long senderId, long groupId, string text, IEnumerable<string> urlList) : base(senderId, groupId)
        {
            Text = text;
            AddRange(urlList);
        }

        internal void Add(params string[] url) => _urlList.AddRange(url);
        internal void AddRange(IEnumerable<string> url) => _urlList.AddRange(url);
    }
}
