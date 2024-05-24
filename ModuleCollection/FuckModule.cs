using System.Linq;
using System.Security.Cryptography;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace klbotlib.Modules;

/// 嘴臭模块
public class FuckModule : SingleTypeModule<MessagePlain>
{
    private readonly StringBuilder _sb = new();
    [ModuleSetup]
    private readonly Regex _pattern = new("default_pattern");
    [ModuleSetup]
    private readonly string[]? _sub, _you, _v, _human, _organ, _subfix, _adjOfOrgan, _adv, _connector, _combine, _stuff, _status;

    // TagMe开关. 决定嘴臭模块是否只处理@自身的消息（不适用于聊天模块。聊天模块永远只处理@自身的消息）
    [ModuleStatus]
    private bool IsTagMe { get; set; } = false;
    // 串联模式 开启时嘴臭模块会输出一系列长嘴臭句子. 否则将输出单句.
    [ModuleStatus]
    private bool IsCascade { get; set; } = true;
    // 生成连续嘴臭句子时的终止概率
    [ModuleStatus]
    private int TermProb { get; set; } = 25;
    // 超过此长度时，串联模式不再累加
    [ModuleStatus]
    private int MaxLength { get; set; } = 20;

    private static string Pick(string[]? a) => a[RandomNumberGenerator.GetInt32(a.Length)];
    private string GenerateFuck()
    {
        if (IsCascade)
        {
            lock (_sb)
            {
                _sb.Clear();
                _sb.Append(SingleSentence());
                while (RandomNumberGenerator.GetInt32(100) > TermProb && _sb.Length <= MaxLength)
                {
                    _sb.AppendFormat(" {0}", SingleSentence());
                }
                return _sb.ToString();
            }
        }
        else
            return SingleSentence();
    }

    /// <inheritdoc/>
    public sealed override bool UseSignature => false; //隐藏返回消息中的模块签名
    /// <inheritdoc/>
    public sealed override string FriendlyName => "嘴臭模块";
    /// <inheritdoc/>
    public sealed override string? Filter(MessagePlain msg)
    {
        return _pattern.IsMatch(msg.Text) && (!IsTagMe || msg.TargetID.Contains(HostBot.SelfId))
            ? "ok"
            : null;
    }
    /// <inheritdoc/>
    public sealed override Task<string> Processor(MessagePlain msg, string? _)
        => Task.FromResult(GenerateFuck());

    /// <summary>
    /// 生成一句脏话
    /// </summary>
    /// <returns>脏话字符串</returns>
    public string SingleSentence()
    {
        int mode = RandomNumberGenerator.GetInt32(20);
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
            _ => "操你妈的",
        };
    }
}
