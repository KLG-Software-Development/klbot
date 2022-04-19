using System.Collections.Generic;
using System.Text;

namespace klbotlib;

/// <summary>
/// 图像消息类
/// </summary>
public class MessageFlashImage : MessageCommon
{
    /// <summary>
    /// 图像的Url的列表，顺序从先到后
    /// </summary>
    public List<string> UrlList { get; internal set; } = new();

    /// <summary>
    /// 构造闪照消息
    /// </summary>
    /// <param name="senderId">发送者ID</param>
    /// <param name="groupId">群聊ID</param>
    public MessageFlashImage(long senderId, long groupId) : base(senderId, groupId) { }
    /// <summary>
    /// 构造图片消息
    /// </summary>
    /// <param name="senderId">发送者ID</param>
    /// <param name="groupId">群聊ID</param>
    /// <param name="context">消息上下文</param>
    /// <param name="urlList">图片URL集合</param>
    public MessageFlashImage(MessageContext context, long senderId, long groupId, IEnumerable<string> urlList) : base(senderId, groupId)
    {
        Context = context;
        UrlList.AddRange(urlList);
    }
    /// <summary>
    /// 构造闪照消息
    /// </summary>
    /// <param name="senderId">发送者ID</param>
    /// <param name="groupId">群聊ID</param>
    /// <param name="urlList">图片URL集合</param>
    public MessageFlashImage(long senderId, long groupId, IEnumerable<string> urlList) : base(senderId, groupId)
    {
        UrlList.AddRange(urlList);
    }
    /// <inheritdoc/>
    public override string ToString()
    {
        if (UrlList.Count != 0)
        {
            StringBuilder sb = new();
            sb.AppendLine(base.ToString());
            for (int i = 0; i < UrlList.Count; i++)
            {
                sb.AppendFormat("Url[{0}]: {1}\n", i, UrlList[i]);
            }
            return sb.ToString();
        }
        else
            return base.ToString();
    }

    internal override void CopyReferenceTypeMember(Message dstMsg)
    {
        MessageFlashImage dst = dstMsg as MessageFlashImage;
        base.CopyReferenceTypeMember(dst);
        dst.UrlList = new();
        dst.UrlList.AddRange(UrlList);
    }
}
