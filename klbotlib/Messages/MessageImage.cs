using System.Collections.Generic;
using System.Text;

namespace klbotlib
{
    /// <summary>
    /// 图像消息类
    /// </summary>
    public class MessageImage : MessageCommon
    {
        private readonly List<string> _urlList = new List<string>();
        /// <summary>
        /// 图像的Url的列表，顺序从先到后
        /// </summary>
        public IReadOnlyList<string> UrlList { get => _urlList; }

        internal MessageImage(long senderId, long groupId) : base(senderId, groupId) { }
        /// <inheritdoc/>
        public override string ToString()
        {
            StringBuilder sb = new();
            sb.AppendLine(base.ToString());
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
