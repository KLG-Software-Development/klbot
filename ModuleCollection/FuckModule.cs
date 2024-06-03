using System;
using System.Linq;
using System.Text;
using System.Text.Json.Serialization;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace klbotlib.Modules;

/// 嘴臭模块
public class FuckModule : Module
{
    private readonly Regex _patternRegex;
    [JsonInclude]
    private string Pattern { get; set; } = "default_pattern";
    [JsonInclude]
    public string[]? Sub { get; set; }
    private string[] You { get; init; } = [];
    private string[] V { get; init; } = [];
    private string[] Human { get; init; } = [];
    private string[] Organ { get; init; } = [];
    private string[] Subfix { get; init; } = [];
    private string[] AdjOfOrgan { get; init; } = [];
    private string[] Adv { get; init; } = [];
    private string[] Connector { get; init; } = [];
    private string[] Combine { get; init; } = [];
    private string[] Stuff { get; init; } = [];
    private string[] Status { get; init; } = [];

    // TagMe开关. 决定嘴臭模块是否只处理@自身的消息（不适用于聊天模块。聊天模块永远只处理@自身的消息）
    [JsonInclude]
    private bool IsTagMe { get; set; } = false;
    // 串联模式 开启时嘴臭模块会输出一系列长嘴臭句子. 否则将输出单句.
    [JsonInclude]
    private bool IsCascade { get; set; } = true;
    // 生成连续嘴臭句子时的终止概率
    [JsonInclude]
    private int TermProb { get; set; } = 25;
    // 超过此长度时，串联模式不再累加
    [JsonInclude]
    private int MaxLength { get; set; } = 20;

    /// <inheritdoc/>
    public FuckModule()
    {
        _patternRegex = new(Pattern, RegexOptions.Compiled);
    }

    private string? Pick(string[]? a)
    {
        if (a == null || a.Length == 0)
        {
            ModuleLog("警告：待选择序列为空");
            return null;
        }
        return a[Random.Shared.Next(a.Length)];
    }
    private string? GenerateFuck()
    {
        if (IsCascade)
        {
            StringBuilder sb = new();
            sb.Append(SingleSentence());
            while (Random.Shared.Next(100) > TermProb && sb.Length <= MaxLength)
            {
                sb.AppendFormat(" {0}", SingleSentence());
            }
            return sb.ToString();
        }
        else
            return SingleSentence();
    }

    /// <inheritdoc/>
    public sealed override bool UseSignature => false; //隐藏返回消息中的模块签名
    /// <inheritdoc/>
    public sealed override string FriendlyName => "嘴臭模块";
    /// <inheritdoc/>
    public sealed override Task<Message?> Processor(MessageContext _, Message msg)
    {
        if (IsTagMe)
        {
            if (msg is not MessagePackage pmsg || !pmsg.TargetIds.Contains(HostBot.SelfId) || !_patternRegex.IsMatch(pmsg.AsPlain()))
                return (Message?)null;
        }
        else if (msg is not MessagePlain)
            return Task.FromResult<Message?>(null);
        return Task.FromResult<Message?>(GenerateFuck());
    }

    /// <summary>
    /// 生成一句脏话
    /// </summary>
    /// <returns>脏话字符串</returns>
    public string? SingleSentence()
    {
        int mode = Random.Shared.Next(20);
        return mode switch
        {
            0 => Pick(Sub) + Pick(V) + Pick(Human),//(主)谓宾 (我)操你妈
            1 => Pick(Sub) + Pick(V) + Pick(Human) + Pick(Connector) + Pick(Organ),//(主)谓宾连接词器官 (我)操你妈了个比
            2 => Pick(Sub) + Pick(V) + Pick(Human) + Pick(Connector) + Pick(AdjOfOrgan) + Pick(Organ),//(主)谓宾连接词形容词器官 (我)操你妈了个臭比
            3 => Pick(Sub) + Pick(V) + Pick(Adv) + Pick(Human) + Pick(Connector) + Pick(AdjOfOrgan) + Pick(Organ),//(主)谓副宾连接词形容词器官
            4 => Pick(Sub) + Pick(V) + Pick(Adv) + Pick(Human) + Pick(Connector) + Pick(AdjOfOrgan) + Pick(Organ) + " " + Pick(V) + Pick(Human) + "的",
            //(主)谓副宾连接词形容词器官 谓宾
            5 => Pick(Sub) + Pick(V) + Pick(Human) + Pick(Connector) + Pick(Combine) + Pick(Stuff),//(主)谓宾连接词称号玩意
            6 => Pick(Sub) + Pick(V) + Pick(Adv) + Pick(Human) + Pick(Connector) + Pick(Combine) + Pick(Stuff),//(主)谓副宾连接词称号玩意
            7 => Pick(Combine) + Pick(Stuff),//称号玩意
            8 => Pick(You) + Pick(Status) + Pick(Combine) + Pick(Stuff),//(你)(他妈)称号玩意
            9 => Pick(Sub) + Pick(V) + Pick(Human) + Pick(Connector) + Pick(Organ) + Pick(Subfix),//(主)谓宾连接词器官
            10 => Pick(Sub) + Pick(V) + Pick(Human) + Pick(Connector) + Pick(AdjOfOrgan) + Pick(Organ) + Pick(Subfix),//(主)谓宾连接词形容词器官
            11 => Pick(Sub) + Pick(V) + Pick(Adv) + Pick(Human) + Pick(Connector) + Pick(AdjOfOrgan) + Pick(Organ) + Pick(Subfix),//(主)谓副宾连接词形容词器官
            12 => Pick(Sub) + Pick(V) + Pick(Adv) + Pick(Human) + Pick(Connector) + Pick(AdjOfOrgan) + Pick(Organ) + Pick(Subfix) + Pick(V) + Pick(Human) + "的",
            //(主)谓副宾连接词形容词器官谓宾
            13 => Pick(Human) + Pick(Connector) + Pick(Organ),//宾连接词器官
            14 => Pick(Human) + Pick(Connector) + Pick(AdjOfOrgan) + Pick(Organ),//宾连接词形容词器官
            15 => Pick(Human) + Pick(Connector) + Pick(AdjOfOrgan) + Pick(Organ) + Pick(V) + Pick(Human) + "的",
            //谓副宾连接词形容词器官谓宾
            16 => Pick(Sub) + Pick(V) + Pick(Human) + Pick(Connector) + Pick(Combine) + Pick(Stuff),//(主)谓宾连接词称号玩意
            17 => Pick(Sub) + Pick(V) + Pick(Human) + Pick(Connector) + Pick(Organ) + Pick(Subfix),//谓宾连接词器官
            18 => Pick(Sub) + Pick(V) + Pick(Human) + Pick(Connector) + Pick(AdjOfOrgan) + Pick(Organ) + Pick(Subfix),//谓宾连接词形容词器官
            19 => Pick(Sub) + Pick(V) + Pick(Human) + Pick(Connector) + Pick(AdjOfOrgan) + Pick(Organ) + Pick(Subfix) + " " + Pick(V) + Pick(Human) + "的",
            _ => "cnmd",
        };
    }
}
