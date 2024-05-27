using klbotlib.Extensions;
using klbotlib.Modules.ModuleUtils;
using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Diagnostics;
using System.Text.Json;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using System.Web;

namespace klbotlib.Modules;

/// 图像模块
public class ImageModule : SingleTypeModule<MessagePlain>
{
    private readonly static Regex _pattern = new(@"来点(\S+?)图", RegexOptions.Compiled);
    private readonly static Random _ro = new();
    private readonly HttpHelper _helper = new();
    private readonly Stopwatch _sw = new();
    private readonly NameValueCollection _query = HttpUtility.ParseQueryString(string.Empty);

    [ModuleSetup]
    private readonly string _url = "https://image.baidu.com/search/acjson";
    [ModuleSetup]
    private readonly HashSet<string> _enhanceKeyword = new();
    [ModuleStatus(IsHidden = true)]
    private readonly Dictionary<string, int> _listNumCache = new();  //缓存每个搜索词的结果数量
    [ModuleStatus]
    private int _cacheCount = 0;
    [ModuleStatus]
    private string _lastDownloadTime = "N/A";
    [ModuleStatus]
    private string _lastParseTime = "N/A";
    [ModuleStatus]
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
        _query["tn"]= "resultjson_com";
        _query["ipn"]= "rj";
        _query["ct"]= "201326592";
        _query["fp"]= "result";
        _query["cl"]= "2";
        _query["lm"]= "-1";
        _query["ie"]= "utf-8";
        _query["oe"]= "utf-8";
        _query["st"]= "-1";
        _query["ic"]= "0";
        _query["istype"]= "2";
        _query["qc"]= "";
        _query["nc"]= "1";
        _query["rn"]= "60";
    }
    /// <inheritdoc/>
    public override string? Filter(MessagePlain msg)
    {
        string text = msg.Text.Trim();
        return text.EndsWith("图来") && text.Length != 2
            ? "X图来"
            : _pattern.IsMatch(text)
                ? "来点X图"
                : null;
    }
    /// <inheritdoc/>
    public override async Task<string> Processor(MessagePlain msg, string? filterOut)
    {
        string? url, text = msg.Text.Trim();
        string word = filterOut switch
        {
            "X图来" => text[..(msg.Text.Length - 2)],
            "来点X图" => _pattern.Match(text).Groups[1].Value,
            _ => throw new Exception("意外遇到未实现的情况。检查处理器实现是否完整"),
        };
        if (_enhanceKeyword.Contains(word))
        {
            (bool success, url) = await ModuleAccess.GetModule<PLJJModule>(0).GetRandomUrl(word, msg);
            if (success)
                return $@"\image:\url:{url}";
            else
                return "运气太差，放弃获取";
        }
        //每次都使用上一次缓存的list_num（如果存在）
        bool isCached = false;
        int listNum = 0;
        if (_listNumCache.ContainsKey(word))
        {
            isCached = true;
            listNum = _listNumCache[word];
            if (listNum == 0)
                goto not_found; //缓存的值为0，意味着无结果
        }
        int max_index = Convert.ToInt32(Math.Round(listNum * (Fraction / 100f)));
        int pn = _ro.Next(max_index);
        string json = await FetchData(pn, word);
        ModuleLog($"成功获取json，pn={pn}");
        _sw.Restart();
        JResult? result = JsonSerializer.Deserialize<JResult>(json);
        //更新字典
        if (!isCached)
        {
            _listNumCache.Add(word, result.listNum);
            _cacheCount++;
        }
        else
            _listNumCache[word] = result.listNum;
        if (json == null || result.data == null)
            throw new JsonException("返回结果解析失败：产生了null结果");
        int index = _ro.Next(result.data.Length);
        url = result.data[index].middleUrl;
        if (url == null)
            throw new JsonException("返回结果解析失败：产生了null结果"); ;
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

    private class JResult { public int listNum; public JImage[]? data; }
    private class JImage { public string? middleUrl; }
}
