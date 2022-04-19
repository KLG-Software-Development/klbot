using klbotlib;
using klbotlib.Modules;
using System.Linq;

namespace ModuleCollection;

/// <summary>
/// 僵尸文学模块
/// </summary>
public class ZombieeeModule : SingleTypeModule<MessagePlain>
{
    /// <inheritdoc/>
    public override string FriendlyName => "僵尸文学模块";
    /// <inheritdoc/>
    public override bool IsTransparent => false;

    /// <inheritdoc/>
    public override string Filter(MessagePlain msg)
    {
        if (msg.TargetID.Contains(HostBot.SelfID) && msg.Text.Trim() == "生成僵尸文学")
            return "generate";
        else
            return null;
    }
    /// <inheritdoc/>
    public override string Processor(MessagePlain msg, string filterOut)
    {
        switch (filterOut)
        {
            case "generate":
                return GenerateZombieeeText();
            default:
                return $"意外遭遇未知过滤器输出“{filterOut}”。检查模块实现";
        }
    }

    /// <summary>
    /// 生成一段僵尸文学
    /// </summary>
    /// <returns>僵尸文学文本</returns>
    public string GenerateZombieeeText()
    {
        return "僵尸文学！";
    }
}
