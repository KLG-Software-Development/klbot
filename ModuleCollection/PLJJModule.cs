using System;

namespace klbotlib.Modules;

/// 图像模块
public class PLJJModule : SingleTypeModule<MessagePlain>
{
    private readonly static Random _ro = new();

    [ModuleSetup]
    private string[] _urlList;
    [ModuleStatus]
    private DateTime _lastActivateTime = DateTime.UnixEpoch;
    /// <inheritdoc/>
    public sealed override bool UseSignature => false;
    /// <inheritdoc/>
    public sealed override string FriendlyName => "漂亮姐姐模块";
    /// <inheritdoc/>
    public sealed override string HelpInfo => "发送“早安”触发每日漂亮姐姐图片";

    /// <inheritdoc/>
    public override string Filter(MessagePlain msg)
    {
        _urlList = Cache.ReadFileAsStringArrayByLines("urlList.txt");
        string text = msg.Text.Trim();
        if (text == "早安")
            return "每日一图";
        else
            return null;
    }
    /// <inheritdoc/>
    public override string Processor(MessagePlain msg, string filterOut)
    {
        _lastActivateTime = DateTime.Now;
        Messaging.ReplyMessage(msg, $@"\image:\url:{_urlList[_ro.Next(_urlList.Length)]}");
        return "早安！";
    }
}
