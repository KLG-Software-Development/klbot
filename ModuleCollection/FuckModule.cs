using System.Text;
using System.Text.Json.Serialization;
using System.Text.RegularExpressions;

namespace klbotlib.Modules;

/// 嘴臭模块
public class FuckModule : Module
{
    private Regex? _patternRegex;

    [JsonInclude]
    private string Pattern { get; set; } = "default_pattern";
    [JsonInclude]
    private string[] Sub { get; set; } = [];
    [JsonInclude]
    private string[] You { get; set; } = [];
    [JsonInclude]
    private string[] V { get; set; } = [];
    [JsonInclude]
    private string[] Human { get; set; } = [];
    [JsonInclude]
    private string[] Organ { get; set; } = [];
    [JsonInclude]
    private string[] Suffix { get; set; } = [];
    [JsonInclude]
    private string[] AdjOfOrgan { get; set; } = [];
    [JsonInclude]
    private string[] Adv { get; set; } = [];
    [JsonInclude]
    private string[] Connector { get; set; } = [];
    [JsonInclude]
    private string[] Combine { get; set; } = [];
    [JsonInclude]
    private string[] Stuff { get; set; } = [];
    [JsonInclude]
    private string[] Status { get; set; } = [];

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

    private static string? Pick(IList<string> a)
    {
        return a == null || a.Count == 0 ? null : a[Random.Shared.Next(a.Count)];
    }
    private string? GenerateFuck()
    {
        if (IsCascade)
        {
            StringBuilder sb = new();
            _ = sb.Append(SingleSentence());
            while (Random.Shared.Next(100) > TermProb && sb.Length <= MaxLength)
            {
                _ = sb.AppendFormat(" {0}", SingleSentence());
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
        _patternRegex ??= new(Pattern, RegexOptions.Compiled);
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
            9 => Pick(Sub) + Pick(V) + Pick(Human) + Pick(Connector) + Pick(Organ) + Pick(Suffix),//(主)谓宾连接词器官
            10 => Pick(Sub) + Pick(V) + Pick(Human) + Pick(Connector) + Pick(AdjOfOrgan) + Pick(Organ) + Pick(Suffix),//(主)谓宾连接词形容词器官
            11 => Pick(Sub) + Pick(V) + Pick(Adv) + Pick(Human) + Pick(Connector) + Pick(AdjOfOrgan) + Pick(Organ) + Pick(Suffix),//(主)谓副宾连接词形容词器官
            12 => Pick(Sub) + Pick(V) + Pick(Adv) + Pick(Human) + Pick(Connector) + Pick(AdjOfOrgan) + Pick(Organ) + Pick(Suffix) + Pick(V) + Pick(Human) + "的",
            //(主)谓副宾连接词形容词器官谓宾
            13 => Pick(Human) + Pick(Connector) + Pick(Organ),//宾连接词器官
            14 => Pick(Human) + Pick(Connector) + Pick(AdjOfOrgan) + Pick(Organ),//宾连接词形容词器官
            15 => Pick(Human) + Pick(Connector) + Pick(AdjOfOrgan) + Pick(Organ) + Pick(V) + Pick(Human) + "的",
            //谓副宾连接词形容词器官谓宾
            16 => Pick(Sub) + Pick(V) + Pick(Human) + Pick(Connector) + Pick(Combine) + Pick(Stuff),//(主)谓宾连接词称号玩意
            17 => Pick(Sub) + Pick(V) + Pick(Human) + Pick(Connector) + Pick(Organ) + Pick(Suffix),//谓宾连接词器官
            18 => Pick(Sub) + Pick(V) + Pick(Human) + Pick(Connector) + Pick(AdjOfOrgan) + Pick(Organ) + Pick(Suffix),//谓宾连接词形容词器官
            19 => Pick(Sub) + Pick(V) + Pick(Human) + Pick(Connector) + Pick(AdjOfOrgan) + Pick(Organ) + Pick(Suffix) + " " + Pick(V) + Pick(Human) + "的",
            _ => "cnmd",
        };
    }
}
