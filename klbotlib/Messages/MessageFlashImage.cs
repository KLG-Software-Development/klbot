using System.Collections.Generic;

namespace klbotlib;

/// <summary>
/// 图像消息类
/// </summary>
public class MessageFlashImage : MessageCommon
{
    private readonly List<string> _urlList = new List<string>();
    /// <summary>
    /// 图像的Url的列表，顺序从先到后
    /// </summary>
    public IReadOnlyList<string> UrlList { get => _urlList; }

    internal MessageFlashImage(long senderId, long groupId) : base(senderId, groupId) { }

    internal void Add(params string[] url) => _urlList.AddRange(url);
    internal void AddRange(IEnumerable<string> url) => _urlList.AddRange(url);
}
