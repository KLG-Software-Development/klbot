using klbotlib.Extensions;
using klbotlib.Modules.ModuleUtils;
using System.Collections.Specialized;
using System.Diagnostics;
using System.Text.Json;
using System.Text.Json.Serialization;
using System.Text.RegularExpressions;
using System.Web;

namespace klbotlib.Modules;

/// 图像模块
public partial class ImageModule : SingleTypeModule<MessagePlain>
{
    private readonly static Regex s_pattern = Pattern();
    private readonly HttpHelper _helper = new();
    private readonly Stopwatch _sw = new();
    private readonly NameValueCollection _query = HttpUtility.ParseQueryString(string.Empty);

    [JsonInclude]
    private readonly string _url = "https://image.baidu.com/search/acjson";
    [JsonInclude]
    private readonly HashSet<string> _enhanceKeyword = [];
    [JsonInclude]
    [HiddenStatus]
    private readonly Dictionary<string, int> _listNumCache = [];  //缓存每个搜索词的结果数量

#pragma warning disable CS1591 // 缺少对公共可见类型或成员的 XML 注释

    [JsonInclude]
    protected int _cacheCount = 0;
    [JsonInclude]
    protected string _lastDownloadTime = "N/A";
    [JsonInclude]
    protected string _lastParseTime = "N/A";

#pragma warning restore CS1591 // 缺少对公共可见类型或成员的 XML 注释

    [JsonInclude]
    private int Fraction { get; set; } = 50;   //只在前n%的结果内随机
    /// <inheritdoc/>
    public sealed override bool UseSignature => false;
    /// <inheritdoc/>
    public sealed override string FriendlyName => "搜图模块";
    /// <inheritdoc/>
    public sealed override string HelpInfo => "发送“[关键词]图来”或“来点[关键词]图”，在百度图片中搜索相关图片";
    /// 构造函数
    public ImageModule()
    {
        _helper.InnerClient.DefaultRequestHeaders.Add("Accept-Language", "zh-CN,zh;q=0.8,zh-TW;q=0.7,zh-HK;q=0.5,en-US;q=0.3,en;q=0.2");
        _helper.InnerClient.DefaultRequestHeaders.Add("Upgrade-Insecure-Requests", "1");
        _query.Clear();
        _query["charset"] = "UTF-8";
        _query["tn"] = "resultjson_com";
        _query["ipn"] = "rj";
        _query["ct"] = "201326592";
        _query["fp"] = "result";
        _query["cl"] = "2";
        _query["lm"] = "-1";
        _query["ie"] = "utf-8";
        _query["oe"] = "utf-8";
        _query["st"] = "-1";
        _query["ic"] = "0";
        _query["istype"] = "2";
        _query["qc"] = "";
        _query["nc"] = "1";
        _query["rn"] = "60";
    }
    /// <inheritdoc/>
    public override async Task<Message?> Processor(MessageContext context, MessagePlain msg)
    {
        string? url, text = msg.Text.Trim();
        string word;
        if (text.EndsWith("图来") && text.Length != 2)
            word = text[..(msg.Text.Length - 2)];
        else if (s_pattern.IsMatch(text))
            word = s_pattern.Match(text).Groups[1].Value;
        else
            return null;
        if (_enhanceKeyword.Contains(word))
        {
            (bool success, url) = await ModuleAccess.GetModule<PLJJModule>(0).GetRandomUrl(word, context);
            return success ? (Message)$@"\image:\url:{url}" : (Message)"运气太差，放弃获取";
        }
        //每次都使用上一次缓存的list_num（如果存在）
        bool isCached = false;
        int listNum = 0;
        if (_listNumCache.TryGetValue(word, out int value))
        {
            isCached = true;
            listNum = value;
            if (listNum == 0)
                goto not_found; //缓存的值为0，意味着无结果
        }
        int max_index = Convert.ToInt32(Math.Round(listNum * (Fraction / 100f)));
        int pn = Random.Shared.Next(max_index);
        string json = await FetchData(pn, word);
        ModuleLog($"成功获取json，pn={pn}");
        _sw.Restart();
        JResult result = JsonSerializer.Deserialize<JResult>(json) ?? throw new JsonException("返回结果解析失败：产生了null结果");
        //更新字典
        if (!isCached)
        {
            _listNumCache.Add(word, result.ListNum);
            _cacheCount++;
        }
        else
            _listNumCache[word] = result.ListNum;
        if (json == null || result.Data == null)
            throw new JsonException("返回结果解析失败：产生了null结果");
        int index = Random.Shared.Next(result.Data.Length);
        url = result.Data[index].MiddleUrl;
        if (url == null)
            throw new JsonException("返回结果解析失败：产生了null结果");
        ;
        _sw.Stop();
        _lastParseTime = _sw.Elapsed.ToMsString();
        if (string.IsNullOrEmpty(url))
            goto not_found;
        else
            return $@"\image:\url:{url}";
        not_found:
        return $"{HostBot.GetModule<FuckModule>().SingleSentence()}，找不到";
    }

    private async Task<string> FetchData(int pn, string word)
    {
        _sw.Restart();
        _query["word"] = word;
        _query["pn"] = pn.ToString();
        _sw.Stop();
        _lastDownloadTime = _sw.Elapsed.ToMsString();
        var response = await _helper.GetAsync($"{_url}?{_query}");

        return await response.Content.ReadAsStringAsync();
    }

    private record JResult(int ListNum, JImage[]? Data);
    private record JImage(string MiddleUrl);

    [GeneratedRegex(@"来点(\S+?)图", RegexOptions.Compiled)]
    private static partial Regex Pattern();
}
