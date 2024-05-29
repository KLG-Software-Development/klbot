using System.Collections.Generic;
using System.Text;

namespace klbotlib
{
    /// <summary>
    /// 图像消息类
    /// </summary>
    public class MessageImage : Message
    {
        /// <summary>
        /// 图像的Url
        /// </summary>
        public List<string> UrlList { get; internal set; } = new();

        /// <summary>
        /// 构造图片消息
        /// </summary>
        /// <param name="senderId">发送者ID</param>
        /// <param name="groupId">群聊ID</param>
        public MessageImage(long senderId, long groupId) : base(senderId, groupId) { }
        /// <summary>
        /// 构造图片消息
        /// </summary>
        /// <param name="senderId">发送者ID</param>
        /// <param name="groupId">群聊ID</param>
        /// <param name="context">消息上下文</param>
        /// <param name="urlList">图片URL集合</param>
        public MessageImage(MessageContext context, long senderId, long groupId, IEnumerable<string> urlList) : base(senderId, groupId)
        {
            Context = context;
            UrlList.AddRange(urlList);
        }
        /// <summary>
        /// 构造图片消息
        /// </summary>
        /// <param name="senderId">发送者ID</param>
        /// <param name="groupId">群聊ID</param>
        /// <param name="urlList">图片URL集合</param>
        public MessageImage(long senderId, long groupId, IEnumerable<string> urlList) : base(senderId, groupId)
        {
            UrlList.AddRange(urlList);
        }
        /// <inheritdoc/>
        public override string ToString()
        {
            if (UrlList.Count != 0)
            {
                StringBuilder sb = new();
                sb.Append(base.ToString());
                sb.Append('\n');
                for (int i = 0; i < UrlList.Count; i++)
                {
                    sb.AppendFormat("Url[{0}]: {1}\n", i, UrlList[i]);
                }
                sb.Length--;
                return sb.ToString();
            }
            else
                return base.ToString();
        }


        internal override void CopyReferenceTypeMember(Message dstMsg)
        {
            MessageImage dst = (MessageImage)dstMsg;
            base.CopyReferenceTypeMember(dst);
            dst.UrlList = new();
            dst.UrlList.AddRange(UrlList);
        }
    }
}
