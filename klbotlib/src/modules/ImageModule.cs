using System;
using System.Collections.Generic;
using System.Net;
using System.Text;
using System.Text.RegularExpressions;

namespace klbotlib.Modules
{
    public class ImageModule : SingleTypeModule<MessagePlain>
    {
        static Random ro = new Random();
        WebClient client = new WebClient();
        Regex middle_url = new Regex(@"""middleURL""\s*:\s*""(https?://.+?)"",");
        public override bool UseSignature => false;

        public ImageModule()
        {
            client.Encoding = Encoding.UTF8;
            client.Headers.Add("Accept-Language", "zh-CN,zh;q=0.8,zh-TW;q=0.7,zh-HK;q=0.5,en-US;q=0.3,en;q=0.2");
            client.Headers.Add("User-Agent", "Mozilla/5.0 (X11; Linux x86_64; rv:60.0) Gecko/20100101 Firefox/60.0");
            client.Headers.Add("Upgrade-Insecure-Requests", "1");
            client.Headers.Add("X-Forwarded-For", $"{ro.Next(255)}.{ro.Next(255)}.{ro.Next(255)}.{ro.Next(255)}");
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
            return (text.EndsWith("图来") && text.Length != 2);
        }
        public override string Processor(MessagePlain msg)
        {
            int retry = -1;
            string word = msg.Text.Trim().Substring(0, msg.Text.Length - 2);
            client.QueryString.Add("word", word);
            client.QueryString.Add("pn", ro.Next(100).ToString());
            string json = client.DownloadString("https://image.baidu.com/search/acjson");
            int slice_len = json.Length / 6;
        retry:
            retry++;
            string slice = json.Substring(ro.Next(json.Length - slice_len - 1), slice_len);
            if (!middle_url.IsMatch(slice) && retry < 10)
                goto retry;
            if (retry == 10)
                return "[ImageModule]\n重试10次仍无法找到url。图片获取失败";
            string url = middle_url.Match(slice).Groups[1].Value;
            return $@"\image:\url:{url}";
        }
    }
}