using System;
using System.Text.Json.Serialization;
using System.Threading.Tasks;

namespace klbotlib.Modules;

/// <summary>
/// 闪照赌博模块
/// </summary>
public class FlashGambleModule : Module
{
    private readonly Random _ro = new();

    [JsonInclude]
    private int _prob = 5;

    /// <inheritdoc/>
    public override string FriendlyName => "犯贱赌博模块";
    /// <inheritdoc/>
    public override bool IsTransparent => false;
    /// <inheritdoc/>
    public override bool UseSignature => false;

    /// <inheritdoc/>
    public override async Task<Message?> Processor(MessageContext context, Message msg)
    {
        if (msg is MessageRecall recall)
            return await ProcessRecall(context, recall);
        else if (msg is MessageImage imsg && imsg.IsFlashImage)
            return await ProcessFlash(context, imsg);
        return null;
    }

    private async Task<Message?> ProcessRecall(MessageContext context, MessageRecall recall)
    {
        if (_ro.Next(100) > _prob)
        {
            ModuleLog("未命中，忽略此条撤回或闪照");
            return null;
        }
        long target = context.Type == MessageContextType.Group ? context.GroupId :HostBot.SelfId;
        long msgId = recall.MessageId;
        long operatorId = context.UserId;
        Message originMsg = await Messaging.GetMessageFromId(target, msgId);
        string info = $"[{operatorId}] 撤回了消息：";
        if (originMsg is MessagePlain pmsg)
            return $"{info}\n\n{pmsg.Text}";
        Message sendMsg;
        if (originMsg is MessageImage imsg)
            sendMsg = new MessagePackage(info, imsg);
        else if (originMsg is MessageVoice vmsg)
            sendMsg = new MessagePackage(info, vmsg);
        else
            return null;
        return new MessagePackage(info, sendMsg);
    }
    private async Task<Message?> ProcessFlash(MessageContext context, MessageImage image)
    {
        if (_ro.Next(100) > _prob)
        {
            ModuleLog("未命中，忽略此条撤回或闪照");
            return null;
        }
        await Messaging.ReplyMessage(context, image);
        return string.Empty;
    }
}
