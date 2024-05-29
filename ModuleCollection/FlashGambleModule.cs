using System;
using System.Threading.Tasks;

namespace klbotlib.Modules;

/// <summary>
/// 闪照赌博模块
/// </summary>
public class FlashGambleModule : Module
{
    private readonly Random _ro = new();

    [ModuleStatus]
    private int _prob = 5;

    /// <inheritdoc/>
    public override string FriendlyName => "犯贱赌博模块";
    /// <inheritdoc/>
    public override bool IsTransparent => false;
    /// <inheritdoc/>
    public override bool UseSignature => false;

    /// <inheritdoc/>
    public override string? Filter(MessagePackage msgPkg)
    {
        Message msg = msgPkg.Collapse();
        if (msg is MessageRecall)
            return "recall";
        else if (msg is MessageImage imsg && imsg.IsFlashImage)
            return "flash";
        else
            return null;
    }
    /// <inheritdoc/>
    public override async Task<string> Processor(MessagePackage msgPkg, string? filterOut)
    {
        Message msg = msgPkg.Collapse();
        if (_ro.Next(100) < _prob)
        {
            switch (filterOut)
            {
                case "recall":
                    MessageRecall recall = (MessageRecall)msg;
                    long target = msgPkg.Context == MessageContext.Group ? msgPkg.GroupId :HostBot.SelfId;
                    long msgId = recall.MessageId;
                    long operatorId = msgPkg.SenderId;
                    MessagePackage origin = await Messaging.GetMessageFromId(target, msgId);
                    if (origin == null)
                        return string.Empty;
                    string info;
                    if (operatorId == origin.SenderId)
                        info = @$"{{\tag:{operatorId}}} 撤回了自己的";
                    else
                        info = @$"{{\tag:{operatorId}}} 撤回了{{\tag:{origin.SenderId}}}的";
                    Message originMsg = origin.Collapse();
                    if (originMsg is MessagePlain pmsg)
                        return info + "消息：\n\n" + pmsg.Text;
                    else if (originMsg is MessageImage imsg)
                    {
                        string contentDesc = imsg.IsFlashImage ? "闪照" : "图像";
                        await Messaging.ReplyMessage(msgPkg, info + contentDesc);
                        await Messaging.ReplyMessage(msgPkg, new MessageImage(imsg.Url, imsg.IsFlashImage));
                    }
                    else if (originMsg is MessageVoice v)
                    {
                        await Messaging.ReplyMessage(v, info + "语音");
                        return @"\voice:\url:" + v.Url;
                    }
                    return string.Empty;
                case "flash":
                    var flashMsg = msg as MessageFlashImage;
                    foreach (var url in flashMsg.UrlList)
                    {
                        await Messaging.ReplyMessage(flashMsg, @"\image:\url:" + url);
                    }
                    return string.Empty;
                default:
                    return string.Empty;
            }
        }
        else
        {
            ModuleLog("未命中，忽略此条撤回或闪照");
            return string.Empty;
        }
    }
}
