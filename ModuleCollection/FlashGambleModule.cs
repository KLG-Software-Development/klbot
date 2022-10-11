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
    public override string? Filter(Message msg)
    {
        if (msg is MessageRecall)
            return "recall";
        else if (msg is MessageFlashImage)
            return "flash";
        else
            return null;
    }
    /// <inheritdoc/>
    public override async Task<string> Processor(Message msg, string? filterOut)
    {
        if (_ro.Next(100) < _prob)
        {
            switch (filterOut)
            {
                case "recall":
                    MessageRecall recall = (MessageRecall)msg;
                    long target = recall.Context == MessageContext.Group ? recall.GroupID :HostBot.SelfID;
                    long msgId = recall.MessageID;
                    long operatorId = recall.OperatorID;
                    MessageCommon origin = (MessageCommon)await Messaging.GetMessageFromID(target, msgId);
                    if (origin == null)
                        return string.Empty;
                    string info;
                    if (operatorId == origin.SenderID)
                        info = @$"{{\tag:{operatorId}}} 撤回了自己的";
                    else
                        info = @$"{{\tag:{operatorId}}} 撤回了{{\tag:{origin.SenderID}}}的";
                    if (origin is MessagePlain p)
                        return info + "消息：\n\n" + p.Text;
                    else if (origin is MessageImage i)
                    {
                        await Messaging.ReplyMessage(i, info + "图像");
                        foreach (var url in i.UrlList)
                            await Messaging.ReplyMessage(i, @"\image:\url:" + url);
                        return string.Empty;
                    }
                    else if (origin is MessageFlashImage f)
                    {
                        await Messaging.ReplyMessage(f, info + "闪照");
                        foreach (var url in f.UrlList)
                            await Messaging.ReplyMessage(f, @"\image:\url:" + url);
                        return string.Empty;
                    }
                    else if (origin is MessageVoice v)
                    {
                        await Messaging.ReplyMessage(v, info + "语音");
                        return @"\voice:\url:" + v.Url;
                    }
                    else
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
            ModulePrint("未命中，忽略此条撤回或闪照");
            return string.Empty;
        }
    }
}
