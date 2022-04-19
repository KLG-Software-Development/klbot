using System.Collections.Generic;
using System.Text;

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
        /// <inheritdoc/>
        public override string ToString()
        {
            StringBuilder sb = new();
            sb.AppendLine(base.ToString());
            sb.AppendFormat("Text: {0}\n", Text);
            int urlIndex = 0;
            foreach (var url in UrlList)
            {
                sb.AppendFormat("Url[{0}]: {1}\n", urlIndex, url);
                urlIndex++;
            }
            return sb.ToString();
        }

        internal void Add(params string[] url) => _urlList.AddRange(url);
        internal void AddRange(IEnumerable<string> url) => _urlList.AddRange(url);
    }
}
