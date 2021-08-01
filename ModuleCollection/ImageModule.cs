using klbotlib.Extensions;
using klbotlib.Modules.ImageModuleNamespace;
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
        readonly static Regex pattern1 = new Regex(@"来点(\S+?)图");
        readonly static Random ro = new Random();
        readonly WebClient client = new WebClient();
        readonly Stopwatch sw = new Stopwatch();

        [ModuleStatus(IsHidden = true)]
        Dictionary<string, int> ListNumCache = new Dictionary<string, int>();  //缓存每个搜索词的结果数量
        [ModuleStatus]
        int CacheCount = 0;
        [ModuleStatus]
        string LastDownloadTime = "N/A";
        [ModuleStatus]
        string LastParseTime = "N/A";
        [ModuleStatus]
        public int Fraction = 50;   //只在前n%的结果内随机
        public sealed override bool UseSignature => false;
        public ImageModule()
        {
            client.Encoding = Encoding.UTF8;
            client.Headers.Add("Accept-Language", "zh-CN,zh;q=0.8,zh-TW;q=0.7,zh-HK;q=0.5,en-US;q=0.3,en;q=0.2");
            client.Headers.Add("User-Agent", "Mozilla/5.0 (X11; Linux x86; rv:60.0) Gecko/20100101 Firefox/60.0");
            client.Headers.Add("Upgrade-Insecure-Requests", "1");
            //client.Headers.Add("X-Forwarded-For", $"{ro.Next(255)}.{ro.Next(255)}.{ro.Next(255)}.{ro.Next(255)}");
            client.QueryString.Add("charset", "UTF-8");
            client.QueryString.Add("tn", "resultjson_com");
            client.QueryString.Add("ipn", "rj");
            client.QueryString.Add("ct", "201326592");
            client.QueryString.Add("fp", "result");
            client.QueryString.Add("cl", "2");
            client.QueryString.Add("lm", "-1");
            client.QueryString.Add("ie", "utf-8");
            client.QueryString.Add("oe", "utf-8");
            client.QueryString.Add("st", "-1");
            client.QueryString.Add("ic", "0");
            client.QueryString.Add("istype", "2");
            client.QueryString.Add("qc", "");
            client.QueryString.Add("nc", "1");
            client.QueryString.Add("rn", "60");
        }
        public override bool Filter(MessagePlain msg)
        {
            string text = msg.Text.Trim();
            return (text.EndsWith("图来") && text.Length != 2) || pattern1.IsMatch(text);
        }
        public override string Processor(MessagePlain msg)
        {
            string url, word, text = msg.Text.Trim();
            if (pattern1.IsMatch(text))
                word = pattern1.Match(text).Groups[1].Value;
            else 
                word = text.Substring(0, msg.Text.Length - 2);

            //每次都使用上一次缓存的list_num（如果存在）
            bool is_cached = false;
            int list_num = 0;
            if (ListNumCache.ContainsKey(word))
            {
                is_cached = true;
                list_num = ListNumCache[word];
                if (list_num == 0)
                    goto not_found; //缓存的值为0，意味着无结果
            }
            int max_index = Convert.ToInt32(Math.Round(list_num * (Fraction / 100f)));
            int pn = ro.Next(max_index);
            string json = FetchData(pn, word);
            ModulePrint($"成功获取json，pn={pn}");
            sw.Restart();
            JResult result = JsonConvert.DeserializeObject<JResult>(json);
            //更新字典
            if (!is_cached)
            {
                ListNumCache.Add(word, result.listNum);
                CacheCount++;
            }
            else
                ListNumCache[word] = result.listNum;
            url = result.data[ro.Next(result.data.Length)].middleURL;
            sw.Stop();
            LastParseTime = sw.Elapsed.ToMsString();
            if (string.IsNullOrEmpty(url))
                goto not_found;
            else
                return $@"\image:\url:{url}";
            not_found:
            return $"{HostBot.GetModule<FuckModule>(this).SingleSentence()}，找不到";
        }

        private string FetchData(int pn, string word)
        {
            sw.Restart();
            client.QueryString.Remove("word");
            client.QueryString.Remove("pn");
            client.QueryString.Add("word", word);
            client.QueryString.Add("pn", pn.ToString());
            sw.Stop();
            LastDownloadTime = sw.Elapsed.ToMsString();
            return client.DownloadString("https://image.baidu.com/search/acjson");
        }
    }
}

namespace klbotlib.Modules.ImageModuleNamespace
{
    class JResult { public int listNum; public JImage[] data; }
    class JImage { public string middleURL; }
}