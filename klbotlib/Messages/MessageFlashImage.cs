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
    public MessageFlashImage(long senderId, long groupId, MessageContext context, IEnumerable<string> urlList) : base(senderId, groupId)
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

    internal override void CopyReferenceTypeMember(Message dstMsg)
    {
        MessageFlashImage dst = dstMsg as MessageFlashImage;
        base.CopyReferenceTypeMember(dst);
        dst.UrlList = new();
        dst.UrlList.AddRange(UrlList);
    }
}
