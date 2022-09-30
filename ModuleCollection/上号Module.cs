using ModuleCollection;
using System;
using System.Collections.Generic;

namespace klbotlib.Modules;

/// <summary>
/// 上号模块
/// </summary>
public class 上号Module : SingleTypeModule<MessagePlain>
{
    //应被忽略的不支持消息
    private readonly HashSet<string> _downgradedMsgKeywords = new() { "[QQ红包]", "[视频游戏]", "你的QQ暂不支持查看视频短片"};
    private const string _halReply = @"蛤儿，我的蛤儿{\face:大哭}{\face:大哭}{\face:大哭}！";
    [ModuleStatus(IsHidden = true)]
    private string _lastMsg = "";
    [ModuleStatus(IsHidden = true)]
    private string _last2Msg = "";
    [ModuleStatus(IsHidden = true)]
    private DateTime _lastHal = new();
    [ModuleSetup]
    private readonly TimeSpan _coolDownTime = new(0, 0, 60);

    private static bool Is上号(string text) => text.Length <= 5 && text.Contains("上号");

    /// <summary>
    /// 关闭模块签名
    /// </summary>
    public sealed override bool UseSignature => false;
    /// <summary>
    /// 模块的友好名称
    /// </summary>
    public sealed override string FriendlyName => "上号模块";
    /// <summary>
    /// 过滤器：处理任何消息
    /// </summary>
    public sealed override string? Filter(MessagePlain msg)
    {
        string msgText = msg.Text.Trim();
        foreach (string keyword in _downgradedMsgKeywords)
        {
            if (msgText.StartsWith(keyword))
                return null;
        }
        string? output = null;
        if (Is上号(msgText) && !Is上号(_lastMsg))
            output = "上号";
        else if (msgText.Contains("蛤儿") && DateTime.Now - _lastHal > _coolDownTime)
        {
            output = "蛤儿";
            //刷新冷却时间
            _lastHal = DateTime.Now;
        }
        else if (!Is上号(_lastMsg) && msgText == _lastMsg && _lastMsg != _last2Msg)
            output = "跟风";
        _last2Msg = _lastMsg;
        _lastMsg = msgText;
        return output;
    }
    /// <summary>
    /// 处理器：内容包含上号且不长于五个字符，则复读内容；
    /// 另外，缓存当前消息到LastMsg中，用于下一次判断是否是同一轮上号消息。如果是同一轮则不回复。
    /// </summary>
    public sealed override string? Processor(MessagePlain msg, string? filterOut)
    {
        string msgText = msg.Text.Trim();
        switch (filterOut)
        {
            case "上号":
            case "跟风":
                return msgText;
            case "蛤儿":
                if (!ModuleAccess.GetModule<ZombieeeModule>().TryFastGenerate(out _, out string? salt))
                    return _halReply;
                else
                    return salt + _halReply;
            default:
                throw new Exception($"意外遇到未实现的过滤器输出\"{filterOut}\"");
        }
    }
}
