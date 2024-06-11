using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.Json.Serialization;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace klbotlib.Modules;

/// 嘴臭模块
public class FuckModule : Module
{
    private readonly Regex? _patternRegex;

    [JsonInclude]
    private readonly string _pattern = "default_pattern";
    [JsonInclude]
    private readonly List<string> _sub = [];
    [JsonInclude]
    private List<string> _you = [];
    [JsonInclude]
    private List<string> _v = [];
    [JsonInclude]
    private List<string> _human = [];
    [JsonInclude]
    private List<string> _organ = [];
    [JsonInclude]
    private List<string> _subfix = [];
    [JsonInclude]
    private List<string> _adjOfOrgan = [];
    [JsonInclude]
    private List<string> _adv = [];
    [JsonInclude]
    private List<string> _connector = [];
    [JsonInclude]
    private List<string> _combine = [];
    [JsonInclude]
    private List<string> _stuff = [];
    [JsonInclude]
    private List<string> _status = [];

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
        if (a == null || a.Count == 0)
            return null;
        return a[Random.Shared.Next(a.Count)];
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
        if (_patternRegex == null)
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
            0 => Pick(_sub) + Pick(_v) + Pick(_human),//(主)谓宾 (我)操你妈
            1 => Pick(_sub) + Pick(_v) + Pick(_human) + Pick(_connector) + Pick(_organ),//(主)谓宾连接词器官 (我)操你妈了个比
            2 => Pick(_sub) + Pick(_v) + Pick(_human) + Pick(_connector) + Pick(_adjOfOrgan) + Pick(_organ),//(主)谓宾连接词形容词器官 (我)操你妈了个臭比
            3 => Pick(_sub) + Pick(_v) + Pick(_adv) + Pick(_human) + Pick(_connector) + Pick(_adjOfOrgan) + Pick(_organ),//(主)谓副宾连接词形容词器官
            4 => Pick(_sub) + Pick(_v) + Pick(_adv) + Pick(_human) + Pick(_connector) + Pick(_adjOfOrgan) + Pick(_organ) + " " + Pick(_v) + Pick(_human) + "的",
            //(主)谓副宾连接词形容词器官 谓宾
            5 => Pick(_sub) + Pick(_v) + Pick(_human) + Pick(_connector) + Pick(_combine) + Pick(_stuff),//(主)谓宾连接词称号玩意
            6 => Pick(_sub) + Pick(_v) + Pick(_adv) + Pick(_human) + Pick(_connector) + Pick(_combine) + Pick(_stuff),//(主)谓副宾连接词称号玩意
            7 => Pick(_combine) + Pick(_stuff),//称号玩意
            8 => Pick(_you) + Pick(_status) + Pick(_combine) + Pick(_stuff),//(你)(他妈)称号玩意
            9 => Pick(_sub) + Pick(_v) + Pick(_human) + Pick(_connector) + Pick(_organ) + Pick(_subfix),//(主)谓宾连接词器官
            10 => Pick(_sub) + Pick(_v) + Pick(_human) + Pick(_connector) + Pick(_adjOfOrgan) + Pick(_organ) + Pick(_subfix),//(主)谓宾连接词形容词器官
            11 => Pick(_sub) + Pick(_v) + Pick(_adv) + Pick(_human) + Pick(_connector) + Pick(_adjOfOrgan) + Pick(_organ) + Pick(_subfix),//(主)谓副宾连接词形容词器官
            12 => Pick(_sub) + Pick(_v) + Pick(_adv) + Pick(_human) + Pick(_connector) + Pick(_adjOfOrgan) + Pick(_organ) + Pick(_subfix) + Pick(_v) + Pick(_human) + "的",
            //(主)谓副宾连接词形容词器官谓宾
            13 => Pick(_human) + Pick(_connector) + Pick(_organ),//宾连接词器官
            14 => Pick(_human) + Pick(_connector) + Pick(_adjOfOrgan) + Pick(_organ),//宾连接词形容词器官
            15 => Pick(_human) + Pick(_connector) + Pick(_adjOfOrgan) + Pick(_organ) + Pick(_v) + Pick(_human) + "的",
            //谓副宾连接词形容词器官谓宾
            16 => Pick(_sub) + Pick(_v) + Pick(_human) + Pick(_connector) + Pick(_combine) + Pick(_stuff),//(主)谓宾连接词称号玩意
            17 => Pick(_sub) + Pick(_v) + Pick(_human) + Pick(_connector) + Pick(_organ) + Pick(_subfix),//谓宾连接词器官
            18 => Pick(_sub) + Pick(_v) + Pick(_human) + Pick(_connector) + Pick(_adjOfOrgan) + Pick(_organ) + Pick(_subfix),//谓宾连接词形容词器官
            19 => Pick(_sub) + Pick(_v) + Pick(_human) + Pick(_connector) + Pick(_adjOfOrgan) + Pick(_organ) + Pick(_subfix) + " " + Pick(_v) + Pick(_human) + "的",
            _ => "cnmd",
        };
    }
}
