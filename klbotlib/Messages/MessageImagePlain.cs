using System.Collections.Generic;
using System.Text;

namespace klbotlib
{
    /// <summary>
    /// 图文消息
    /// </summary>
    public class MessageImagePlain : MessageCommon
    {
        /// <summary>
        /// 图像的Url
        /// </summary>
        public List<string> UrlList { get; internal set; } = new();
        /// <summary>
        /// 随图像一同发送的文字
        /// </summary>
        public string Text { get; }

        /// <summary>
        /// 构造图文消息
        /// </summary>
        /// <param name="senderId">发送者ID</param>
        /// <param name="groupId">群聊ID</param>
        /// <param name="text">文本内容</param>
        public MessageImagePlain(long senderId, long groupId, string text = "") : base(senderId, groupId)
        {
            Text = text;
        }
        /// <summary>
        /// 构造图文消息
        /// </summary>
        /// <param name="senderId">发送者ID</param>
        /// <param name="groupId">群聊ID</param>
        /// <param name="text">文本内容</param>
        /// <param name="urlList">图片URL集合</param>
        public MessageImagePlain(long senderId, long groupId, string text, IEnumerable<string> urlList) : base(senderId, groupId)
        {
            Text = text;
            UrlList.AddRange(urlList);
        }
        /// <summary>
        /// 构造图文消息
        /// </summary>
        /// <param name="context">消息上下文</param>
        /// <param name="senderId">发送者ID</param>
        /// <param name="groupId">群聊ID</param>
        /// <param name="text">文本内容</param>
        /// <param name="urlList">图片URL集合</param>
        public MessageImagePlain(MessageContext context, long senderId, long groupId, string text, IEnumerable<string> urlList) : base(senderId, groupId)
        {
            Context = context;
            Text = text;
            UrlList.AddRange(urlList);
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

        internal override void CopyReferenceTypeMember(Message dstMsg)
        {
            MessageImagePlain dst = dstMsg as MessageImagePlain;
            base.CopyReferenceTypeMember(dst);
            dst.UrlList = new();
            dst.UrlList.AddRange(UrlList);
        }
    }
}
