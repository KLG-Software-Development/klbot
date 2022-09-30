using System;

namespace klbotlib.Modules;

/// 图像模块
public class PLJJModule : SingleTypeModule<MessagePlain>
{
    private readonly static Random _ro = new();

    [ModuleSetup]
    private string[]? _urlList;
    [ModuleStatus]
    private DateTime _lastActivateTime = DateTime.UnixEpoch;
    /// <inheritdoc/>
    public sealed override bool UseSignature => false;
    /// <inheritdoc/>
    public sealed override string FriendlyName => "漂亮姐姐模块";
    /// <inheritdoc/>
    public sealed override string HelpInfo => "发送“早安”触发每日漂亮姐姐图片";

    /// <inheritdoc/>
    public override string? Filter(MessagePlain msg)
    {
        if (msg.Text.Trim() == "早安" && DateTime.Now.Date != _lastActivateTime.Date)
            return "每日一图";
        else
            return null;
    }
    /// <inheritdoc/>
    public override string? Processor(MessagePlain msg, string? filterOut)
    {
        _lastActivateTime = DateTime.Now;
        Messaging.ReplyMessage(msg, "早安！");
        return $@"\image:\url:{_urlList[_ro.Next(_urlList.Length)]}";
    }

    /// <summary>
    /// 随机返回一条图库内的URL
    /// </summary>
    /// <returns>图片URL</returns>
    public string GetRandomUrl()
    {
        return _urlList[_ro.Next(_urlList.Length)];
    }
}
