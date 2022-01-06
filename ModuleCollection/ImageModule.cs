using klbotlib.Extensions;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Diagnostics;
using System.Net.Http;
using System.Text.RegularExpressions;
using System.Web;

namespace klbotlib.Modules;

/// 图像模块
public class ImageModule : SingleTypeModule<MessagePlain>
{
    private readonly static Regex _pattern = new(@"来点(\S+?)图");
    private readonly static Random _ro = new();
    private readonly HttpClient _client = new();
    private readonly Stopwatch _sw = new();
    private readonly NameValueCollection _query = HttpUtility.ParseQueryString(string.Empty);

    [ModuleSetup]
    private readonly string _url = "https://image.baidu.com/search/acjson";
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
        _client.DefaultRequestHeaders.Add("Accept-Language", "zh-CN,zh;q=0.8,zh-TW;q=0.7,zh-HK;q=0.5,en-US;q=0.3,en;q=0.2");
        _client.DefaultRequestHeaders.Add("User-Agent", "Mozilla/5.0 (X11; Linux x86; rv:60.0) Gecko/20100101 Firefox/60.0");
        _client.DefaultRequestHeaders.Add("Upgrade-Insecure-Requests", "1");
        //client.Headers.Add("X-Forwarded-For", $"{ro.Next(255)}.{ro.Next(255)}.{ro.Next(255)}.{ro.Next(255)}");
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
    public override string Filter(MessagePlain msg)
    {
        string text = msg.Text.Trim();
        return text.EndsWith("图来") && text.Length != 2
            ? "X图来"
            : _pattern.IsMatch(text)
                ? "来点X图"
                : null;
    }
    /// <inheritdoc/>
    public override string Processor(MessagePlain msg, string filter_out)
    {
        string url, text = msg.Text.Trim();
        string word = filter_out switch
        {
            "X图来" => text[..(msg.Text.Length - 2)],
            "来点X图" => _pattern.Match(text).Groups[1].Value,
            _ => throw new Exception("意外遇到未实现的情况。检查处理器实现是否完整"),
        };
        //每次都使用上一次缓存的list_num（如果存在）
        bool is_cached = false;
        int list_num = 0;
        if (_listNumCache.ContainsKey(word))
        {
            is_cached = true;
            list_num = _listNumCache[word];
            if (list_num == 0)
                goto not_found; //缓存的值为0，意味着无结果
        }
        int max_index = Convert.ToInt32(Math.Round(list_num * (Fraction / 100f)));
        int pn = _ro.Next(max_index);
        string json = FetchData(pn, word);
        ModulePrint($"成功获取json，pn={pn}");
        _sw.Restart();
        JResult result = JsonConvert.DeserializeObject<JResult>(json);
        //更新字典
        if (!is_cached)
        {
            _listNumCache.Add(word, result.listNum);
            _cacheCount++;
        }
        else
            _listNumCache[word] = result.listNum;
        url = result.data[_ro.Next(result.data.Length)].middleURL;
        _sw.Stop();
        _lastParseTime = _sw.Elapsed.ToMsString();
        if (string.IsNullOrEmpty(url))
            goto not_found;
        else
            return $@"\image:\url:{url}";
not_found:
        return $"{HostBot.GetModule<FuckModule>().SingleSentence()}，找不到";
    }

    private string FetchData(int pn, string word)
    {
        _sw.Restart();
        _query["word"] = word;
        _query["pn"] = pn.ToString();
        _sw.Stop();
        _lastDownloadTime = _sw.Elapsed.ToMsString();
        return _client.GetAsync($"{_url}?{_query}").Result.Content.ReadAsStringAsync().Result;
    }

    private class JResult { public int listNum; public JImage[] data; }
    private class JImage { public string middleURL; }
}
