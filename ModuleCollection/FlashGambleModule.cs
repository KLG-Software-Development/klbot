using System;

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
    public override string Filter(Message msg)
    {
        int i = _ro.Next(100);
        if (i < _prob)
        {
            if (msg is MessageRecall)
                return "recall";
            else if (msg is MessageFlashImage)
                return "flash";
            else
                return null;
        }
        else
        {
            ModulePrint("未命中，忽略此条撤回或闪照");
            return null;
        }
    }
    /// <inheritdoc/>
    public override string Processor(Message msg, string filterOut)
    {
        switch (filterOut)
        {
            case "recall":
                MessageRecall recall = msg as MessageRecall;
                long msgId = recall.MessageID;
                long operatorId = recall.OperatorID;
                MessageCommon origin = Messaging.GetMessageFromID(msgId) as MessageCommon;
                if (origin is null)
                    return null;
                string info;
                if (operatorId == origin.SenderID)
                    info = @$"{{\tag:{operatorId}}} 撤回了自己的";
                else
                    info = @$"{{\tag:{operatorId}}} 撤回了{{\tag:{origin.SenderID}}}的";
                if (origin is MessagePlain p)
                    return info + "消息：\n\n" + p.Text;
                else if (origin is MessageImage i)
                {
                    Messaging.ReplyMessage(i, info + "图像");
                    foreach (var url in i.UrlList)
                        Messaging.ReplyMessage(i, @"\image:\url:" + url);
                    return null;
                }
                else if (origin is MessageFlashImage f)
                {
                    Messaging.ReplyMessage(f, info + "闪照");
                    foreach (var url in f.UrlList)
                        Messaging.ReplyMessage(f, @"\image:\url:" + url);
                    return null;
                }
                else if (origin is MessageVoice v)
                {
                    Messaging.ReplyMessage(v, info + "语音");
                    return @"\voice:\url:" + v.Url;
                }
                else
                    return null;
            case "flash":
                var flashMsg = msg as MessageFlashImage;
                foreach (var url in flashMsg.UrlList)
                {
                    Messaging.ReplyMessage(flashMsg, @"\image:\url:" + url);
                }
                return null;
        }
        return null;
    }
}
