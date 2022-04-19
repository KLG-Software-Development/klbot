﻿using System.Collections.Generic;

namespace klbotlib
{
    /// <summary>
    /// 图像消息类
    /// </summary>
    public class MessageImage : MessageCommon
    {
        /// <summary>
        /// 图像的Url的列表，顺序从先到后
        /// </summary>
        public List<string> UrlList { get; internal set; }

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
        /// <param name="urlList">图片URL集合</param>
        public MessageImage(long senderId, long groupId, IEnumerable<string> urlList) : base(senderId, groupId)
        {
            UrlList.AddRange(urlList);
        }

        internal override void CopyReferenceTypeMember(Message dstMsg)
        {
            MessageImage dst = dstMsg as MessageImage;
            dst.UrlList = new();
            dst.UrlList.AddRange(UrlList);
        }
    }
}
