using klbotlib.Modules.ModuleUtils;
using System.Security.Cryptography;
using System.Text;
using System.Text.Json.Serialization;
using System.Text.RegularExpressions;

namespace klbotlib.Modules;

/// <summary>
/// 僵尸文学模块
/// </summary>
public partial class ZombieeeModule : SingleTypeModule<MessagePackage>
{
    private static readonly Dictionary<string, string> s_errorMsg = new()
    {
        { "52000", "成功" },
        { "52001", "请求超时" },
        { "52002", "系统错误" },
        { "52003", "未授权用户" },
        { "54000", "必填参数为空" },
        { "54001", "签名错误" },
        { "54003", "访问频率受限" },
        { "54005", "长query请求频繁" },
    };
    private static readonly char[] s_marks = ['。', '。', '。', '？', '？', '，', '，', '，', '，', '，', '！', '！', '…'];
    private static readonly HashSet<string> s_typeNames =
    [
        "N",
        "V",
        "ADJ",
        "ADV",
        "CLAS",
        "ECHO",
        "STRU",
        "AUX",
        "COOR",
        "CONJ",
        "SUFFIX",
        "PREFIX",
        "PREP",
        "PRON",
        "QUES",
        "NUM",
        "IDIOM"
    ];
    private readonly StringBuilder _sb = new();
    private static readonly Random s_ro = new();
    private static readonly MD5 s_md5 = MD5.Create();
    private readonly HttpHelper _helper = new();
    private readonly Regex _dstPat = DstPattern();
    private readonly Regex _errorCodePat = ErrorCodePattern();
    private readonly Dictionary<int, double> _generalizedHarmonicNumber = [];

    [JsonInclude]
    private readonly string _baseUrl = string.Empty;
    [JsonInclude]
    private readonly string _appID = string.Empty; //APP ID
    [JsonInclude]
    private readonly string _key = string.Empty;   //APP KEY
    [JsonInclude]
    private readonly string _srcLang = string.Empty;       //源语言
    [JsonInclude]
    private readonly string _dstLang = string.Empty;         //目标语言
    [JsonInclude]
    private readonly double _power = -0.5;     //幂
    [JsonInclude]
    private readonly int _reflectNum = 3;     //反射次数
    [JsonInclude]
    private readonly List<string>[] _patterns = [];  //句式模板
    [JsonInclude]
    private readonly Dictionary<string, string[]> _dicOf = [];   //词性字典

    /// <inheritdoc/>
    public override string FriendlyName => "僵尸文学模块";
    /// <inheritdoc/>
    public override string HelpInfo => "使用方法：@机器人并发送“生成僵尸文学”";

    /// <inheritdoc/>
    public override async Task<Message?> Processor(MessageContext context, MessagePackage msg)
    {
        if (!msg.TargetIds.Contains(HostBot.SelfId))
            return null;
        string text = msg.AsPlain().Trim();
        return text == "生成僵尸文学" ? (Message)await GenerateZombieeeText() : null;
    }

    /// <summary>
    /// 生成一段僵尸文学
    /// </summary>
    /// <returns>僵尸文学文本</returns>
    public async Task<string> GenerateZombieeeText()
    {
        (bool success, string message, string result) = await TryGenerate();
        return !success ? $"生成失败：{message}" : result;
    }
    /// <summary>
    /// 尝试用当前设定的源语言和目标语言翻译指定内容
    /// </summary>
    /// <param name="query">待翻译内容</param>
    /// <returns>元组：(翻译是否成功, 返回消息, 翻译结果)</returns>
    public async Task<(bool, string, string)> TryTranslate(string query)
        => await TryTranslate(query, _srcLang, _dstLang);
    /// <summary>
    /// 尝试用给定的源语言和当前设定的目标语言翻译指定内容
    /// </summary>
    /// <param name="query">待翻译内容</param>
    /// <param name="srcLang">源语言</param>
    /// <returns>元组：(翻译是否成功, 返回消息, 翻译结果)</returns>
    public async Task<(bool, string, string)> TryTranslate(string query, string srcLang)
        => await TryTranslate(query, srcLang, _dstLang);
    /// <summary>
    /// 尝试生成一段僵尸文学
    /// </summary>
    /// <returns>元组：(生成是否成功, 返回消息, 生成结果)</returns>
    public async Task<(bool, string, string)> TryGenerate()
    {
        string seed = GenerateSentences(s_ro.Next(1, 7));
        ModuleLog($"反射种子：{seed}");
        return await TryReflect(_reflectNum, seed);
    }
    /// <summary>
    /// 尝试快速生成一短句僵尸文学。
    /// 此方法中句子长度锁定为1，反射次数锁定为2，以保证速度。
    /// </summary>
    /// <returns>元组：(生成是否成功, 返回消息, 生成结果)</returns>
    public async Task<(bool, string, string)> TryFastGenerate()
    {
        string seed = GenerateSingleSentence();
        ModuleLog($"反射种子：{seed}");
        return await TryReflect(2, seed);
    }

    private string GetRequestUrl(string query, string srcLang, string dstLang, string salt, string sign)
        => $"{_baseUrl}?q={Uri.EscapeDataString(query)}&from={srcLang}&to={dstLang}&appid={_appID}&salt={salt}&sign={sign}";
    private async Task<(bool, string, string)> TryTranslate(string query, string srcLang, string dstLang)
    {

        string result = string.Empty;
        string message;
        try
        {
            string salt = s_ro.Next().ToString();
            string sign = CalculateSign(query, salt);
            string url = GetRequestUrl(query, srcLang, dstLang, salt, sign);
            HttpResponseMessage response = await _helper.GetAsync(url);
            if (!response.IsSuccessStatusCode)
            {
                message = $"请求返回异常状态码[{response.StatusCode}]";
                return (false, message, result);
            }
            else
            {
                string re = response.Content.ReadAsStringAsync().Result;
                response.Dispose();
                if (_dstPat.IsMatch(re))
                {
                    result = Regex.Unescape(_dstPat.Match(re).Groups[1].Value);
                    message = "翻译成功";
                    return (true, message, result);
                }
                else
                {
                    message = _errorCodePat.IsMatch(re) ? $"翻译失败：{s_errorMsg[_errorCodePat.Match(re).Groups[1].Value]}" : "翻译失败：未正确识别到翻译结果";
                    return (true, message, result);
                }
            }
        }
        catch (Exception ex)
        {
            message = $"翻译时发生异常：{ex.Message}";
            return (false, message, result);
        }
    }
    private async Task<(bool, string, string)> TryReflect(string query)
    {
        (bool success, string message, string result) = await TryTranslate(query, _srcLang, _dstLang);
        if (!success)
            return (false, message, result);
        Thread.Sleep(1000);
        (success, message, result) = await TryTranslate(result, _dstLang, _srcLang);
        return !success ? ((bool, string, string))(false, message, result) : ((bool, string, string))(true, message, result);
    }
    private async Task<(bool, string, string)> TryReflect(int reflectNum, string query)
    {
        string result = string.Empty;
        for (int n = 0; n < reflectNum; n++)
        {
            ModuleLog($"正在进行反射#{n}...");
            (bool suceess, string message, result) = await TryReflect(query);
            if (!suceess)
                return (false, message, result);
            Thread.Sleep(1000);
        }
        return (true, "反射成功", result);
    }
    private string GenerateSingleSentence()
    {
        _ = _sb.Clear();
        int patNumber = s_ro.Next(_patterns.Length);
        List<string> pattern = _patterns[patNumber];
        int N = pattern.Count;
        for (int n = 0; n < N; n++)
        {
            if (s_typeNames.Contains(pattern[n]))
            {
                string typeName = pattern[n];
                int wordIndex = PowerLawDist(_dicOf[typeName].Length);
                string word = _dicOf[typeName][wordIndex];
                _ = _sb.Append(word);
            }
            else
                _ = _sb.Append(pattern[n]);
        }
        _ = _sb.Append(s_marks[s_ro.Next(s_marks.Length)]);
        return _sb.ToString();
    }
    private string GenerateSentences(int numSentenceCount)
    {
        _ = _sb.Clear();
        for (int i = 0; i < numSentenceCount; i++)
        {
            int patNumber = s_ro.Next(_patterns.Length);
            List<string> pattern = _patterns[patNumber];
            int N = pattern.Count;
            for (int n = 0; n < N; n++)
            {
                if (s_typeNames.Contains(pattern[n]))
                {
                    string typeName = pattern[n];
                    int wordIndex = PowerLawDist(_dicOf[typeName].Length);
                    string word = _dicOf[typeName][wordIndex];
                    _ = _sb.Append(word);
                }
                else
                    _ = _sb.Append(pattern[n]);
            }
            _ = _sb.Append(s_marks[s_ro.Next(s_marks.Length)]);
        }
        return _sb.ToString();
    }
    private string CalculateSign(string query, string salt)    //签名
    {
        string hashString = $"{_appID}{query}{salt}{_key}";
        byte[] hash = s_md5.ComputeHash(Encoding.UTF8.GetBytes(hashString));
        _ = _sb.Clear();
        for (int i = 0; i < hash.Length; i++)
        {
            _ = _sb.Append(Convert.ToString(hash[i], 16).PadLeft(2, '0'));
        }
        return _sb.ToString();
    }
    private int PowerLawDist(int N)
    {
        double ss = 1 - _power;
        int index = Convert.ToInt32(Math.Round(Math.Pow(Hns(N) * ss * s_ro.NextDouble(), 1 / ss)));
        return index > N ? N : index;
    }
    //计算generalized harmonic number
    private double Hns(int N)
    {
        if (_generalizedHarmonicNumber.TryGetValue(N, out double value))
            return value;
        else
        {
            double sum = 0;
            for (int n = 1; n <= N; n++)
            {
                sum += 1.0 / Math.Pow(n, _power);   //Sum 1/(1~N)^_power
            }
            _generalizedHarmonicNumber.Add(N, sum);
            return sum;
        }
    }

    [GeneratedRegex(@""",""dst"":""(.*)""}]}$", RegexOptions.Compiled)]
    private static partial Regex DstPattern();
    [GeneratedRegex(@"""error_code"":""(\d+)""", RegexOptions.Compiled)]
    private static partial Regex ErrorCodePattern();
}
