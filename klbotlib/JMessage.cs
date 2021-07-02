using System;
using Newtonsoft.Json;

namespace klbotlib
{

    /// <summary>
    /// 直接从JSON解析的消息对象
    /// </summary>
    public class JMessage
    {
        public string type;
        public long id;
        public long time;
        public long target;
        public string text;
    }
}
