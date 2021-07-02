using Newtonsoft.Json;
using System;
using System.Net;
using System.Text.RegularExpressions;

namespace klbotlib.Modules
{
    //聊天bot模块
    public static class ChatQYKModule
    {
        const string url = "http://api.qingyunke.com/api.php?key=free&appid=0&msg=";
        static readonly WebClient client = new WebClient();
        public static string GetReply(string msg)
        {
            Uri host = new Uri(url + msg);
            string jreply = client.DownloadString(host);
            return JsonConvert.DeserializeObject<ChatterBotReply>(jreply).FormattedContent();
        }
    }

    class ChatterBotReply
    {
        public int result;
        public string content;
        public static Regex trashPat = new Regex(@"{r\+}");
        public static Regex facePat = new Regex(@"{face:[\d]+}");
        //TODO: 把发文本消息的方法扩展成发消息链的方法，然后设计一个消息链标记语法
        public string FormattedContent() => trashPat.Replace(facePat.Replace(content.Replace("{br}", "\r\n"), ""), "");
    }
}
