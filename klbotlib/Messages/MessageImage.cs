using System;
using System.Collections.Generic;
using System.Text;

namespace klbotlib
{
    /// <summary>
    /// 图像消息类
    /// </summary>
    public class MessageImage : MessageCommon
    {
        private readonly List<string> _url_list = new List<string>();
        /// <summary>
        /// 图像的Url的列表，顺序从先到后
        /// </summary>
        public IReadOnlyList<string> UrlList { get => _url_list; }

        internal MessageImage(long sender_id, long group_id) : base(sender_id, group_id) { }

        internal void Add(params string[] url) => _url_list.AddRange(url);
        internal void AddRange(IEnumerable<string> url) => _url_list.AddRange(url);
    }
}
