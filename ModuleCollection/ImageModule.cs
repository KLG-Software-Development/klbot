using klbotlib.Extensions;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Net;
using System.Text;
using System.Text.RegularExpressions;

namespace klbotlib.Modules
{
    // 图像模块的demo
    public class ImageModule : SingleTypeModule<MessagePlain>
    {
        private readonly static Regex _pattern = new Regex(@"来点(\S+?)图");
        private readonly static Random _ro = new Random();
        private readonly WebClient _client = new WebClient();
        private readonly Stopwatch _sw = new Stopwatch();

        [ModuleStatus(IsHidden = true)]
        private readonly Dictionary<string, int> _listNumCache = new Dictionary<string, int>();  //缓存每个搜索词的结果数量
        [ModuleStatus]
        private int _cacheCount = 0;
        [ModuleStatus]
        private string _lastDownloadTime = "N/A";
        [ModuleStatus]
        private string _lastParseTime = "N/A";
        [ModuleStatus]
        public int Fraction = 50;   //只在前n%的结果内随机
        public sealed override bool UseSignature => false;
        public sealed override string FriendlyName => "搜图模块";
        public sealed override string HelpInfo => "发送“[关键词]图来”或“来点[关键词]图”，在百度图片中搜索相关图片";
        public ImageModule()
        {
            _client.Encoding = Encoding.UTF8;
            _client.Headers.Add("Accept-Language", "zh-CN,zh;q=0.8,zh-TW;q=0.7,zh-HK;q=0.5,en-US;q=0.3,en;q=0.2");
            _client.Headers.Add("User-Agent", "Mozilla/5.0 (X11; Linux x86; rv:60.0) Gecko/20100101 Firefox/60.0");
            _client.Headers.Add("Upgrade-Insecure-Requests", "1");
            //client.Headers.Add("X-Forwarded-For", $"{ro.Next(255)}.{ro.Next(255)}.{ro.Next(255)}.{ro.Next(255)}");
            _client.QueryString.Add("charset", "UTF-8");
            _client.QueryString.Add("tn", "resultjson_com");
            _client.QueryString.Add("ipn", "rj");
            _client.QueryString.Add("ct", "201326592");
            _client.QueryString.Add("fp", "result");
            _client.QueryString.Add("cl", "2");
            _client.QueryString.Add("lm", "-1");
            _client.QueryString.Add("ie", "utf-8");
            _client.QueryString.Add("oe", "utf-8");
            _client.QueryString.Add("st", "-1");
            _client.QueryString.Add("ic", "0");
            _client.QueryString.Add("istype", "2");
            _client.QueryString.Add("qc", "");
            _client.QueryString.Add("nc", "1");
            _client.QueryString.Add("rn", "60");
        }
        public override string Filter(MessagePlain msg)
        {
            string text = msg.Text.Trim();
            return text.EndsWith("图来") && text.Length != 2
                ? "X图来"
                : _pattern.IsMatch(text)
                    ? "来点X图"
                    : null;
        }
        public override string Processor(MessagePlain msg, string filter_out)
        {
            string url, word, text = msg.Text.Trim();
            //根据不同的过滤器输出，用不同方式取出搜索词
            switch (filter_out)
            {
                case "X图来":
                    word = text.Substring(0, msg.Text.Length - 2);
                    break;
                case "来点X图":
                    word = _pattern.Match(text).Groups[1].Value;
                    break;
                default:
                    throw new Exception("意外遇到未实现的情况。检查处理器实现是否完整");

            }
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
            _client.QueryString.Remove("word");
            _client.QueryString.Remove("pn");
            _client.QueryString.Add("word", word);
            _client.QueryString.Add("pn", pn.ToString());
            _sw.Stop();
            _lastDownloadTime = _sw.Elapsed.ToMsString();
            return _client.DownloadString("https://image.baidu.com/search/acjson");
        }

        private class JResult { public int listNum; public JImage[] data; }
        private class JImage { public string middleURL; }
    }
}