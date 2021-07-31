using klbotlib.Modules.ImageModuleNamespace;
using Newtonsoft.Json;
using System;
using System.Diagnostics;
using System.Net;
using System.Text;

namespace klbotlib.Modules
{
    // 图像模块的demo
    public class ImageModule : SingleTypeModule<MessagePlain>
    {
        static readonly Random ro = new Random();
        readonly WebClient client = new WebClient();

        [ModuleStatus]
        public string LastDownloadTime = "N/A";
        [ModuleStatus]
        public string LastParseTime = "N/A";
        [ModuleStatus]
        public int Fraction { get; set; } = 100;   //只在前n%的结果内随机
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
            return ((text.EndsWith("图来") || text.EndsWith("图来!")|| text.EndsWith("图来！"))
                && text.Length != 2);
        }
        public override string Processor(MessagePlain msg)
        {
            Stopwatch sw = new Stopwatch();
            string url;
            string word = msg.Text.Trim().Substring(0, msg.Text.Length - 2);
            client.QueryString.Remove("word");
            client.QueryString.Remove("pn");
            client.QueryString.Add("word", word);
            client.QueryString.Add("pn", ro.Next(6).ToString());    //只用前6页
            sw.Restart();
            string json = client.DownloadString("https://image.baidu.com/search/acjson");
            sw.Stop();
            LastDownloadTime = sw.Elapsed.ToMsString();
            sw.Restart();
            JResult result = JsonConvert.DeserializeObject<JResult>(json);
            int max_index = Convert.ToInt32(Math.Round(result.data.Length * (Fraction / 100f)));
            url = result.data[ro.Next(max_index)].middleURL;
            sw.Stop();
            LastParseTime = sw.Elapsed.ToMsString();
            return $@"\image:\url:{url}";
        }
    }
}

namespace klbotlib.Modules.ImageModuleNamespace
{
    class JResult { public JImage[] data; }
    class JImage { public string middleURL; }
    public static class TimeSpanExtension
    {
        public static string ToMsString(this TimeSpan time_span, int decimals = 4) => time_span.TotalMilliseconds.ToString("f" + decimals) + "ms";
    }
}