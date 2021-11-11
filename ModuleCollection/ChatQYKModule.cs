using Newtonsoft.Json;
using System;
using System.Linq;
using System.Net;
using System.Text.RegularExpressions;

namespace klbotlib.Modules
{
    //聊天bot模块
    public class ChatQYKModule : SingleTypeModule<MessagePlain>
    {
        private const string _url = "http://api.qingyunke.com/api.php?key=free&appid=0&msg=";
        private static readonly WebClient _client = new WebClient();

        public sealed override bool IsTransparent => false;
        public sealed override bool UseSignature => false;
        public sealed override bool IsAsync => true;
        public sealed override string FriendlyName => "聊天模块";
        public sealed override string Filter(MessagePlain msg) => msg.TargetID.Contains(HostBot.SelfID) ? "ok" : null;
        public sealed override string Processor(MessagePlain msg, string _)
        {
            Uri host = new Uri(_url + msg.Text);
            string jreply = _client.DownloadString(host);
            return JsonConvert.DeserializeObject<ChatterBotReply>(jreply).FormattedContent();
        }

        private class ChatterBotReply
        {
            public int result;
            public string content;
            public static Regex trashPat = new Regex(@"{r\+}");
            public static Regex facePat = new Regex(@"{face:[\d]+}");
            public string FormattedContent() => trashPat.Replace(facePat.Replace(content.Replace("{br}", "\r\n"), ""), "");
        }
    }
}