using System;
using System.Threading.Tasks;

namespace klbotlib.Modules;

/// <summary>
/// 禁言反制模块
/// </summary>
public class CounterBanModule : SingleTypeModule<MessageMute>
{
    [ModuleStatus]
    private CounterBanMode _mode = CounterBanMode.SelfUnmuteRevenge;

    /// <inheritdoc/>
    public override string FriendlyName => "禁言反制模块";
    /// <inheritdoc/>
    public override bool IsTransparent => true;
    /// <inheritdoc/>
    public override bool UseSignature => false;

    /// <inheritdoc/>
    public override string? Filter(MessageMute msg)
    {
        if (msg.IsUnmute)
            return null;
        else
            return "counterBan";
    }

    /// <inheritdoc/>
    public override async Task<string> Processor(MessageMute msg, string? filterOut)
    {
        switch (filterOut)
        {
            case "counterBan":
                ModulePrint("正在自我解除禁言...");
                await Operating.Unmute(HostBot.SelfID, msg.GroupID);
                if (_mode == CounterBanMode.SelfUnmuteRevenge)
                {
                    ModulePrint("正在反制禁言...");
                    await Operating.Mute(msg.OperatorID, msg.GroupID, msg.DurationSeconds);
                }
                return string.Empty;
            default:
                throw new Exception($"遭遇未知过滤器输出：{filterOut}");
        }
    }

    enum CounterBanMode
    {
        SelfUnmute,         //解除自身禁言
        SelfUnmuteRevenge   //解除自身禁言并反制禁言者
    }
}
