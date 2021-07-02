using System;

namespace klbotlib
{
    /// <summary>
    /// 直接从JSON解析出的消息包对象
    /// </summary>
    public class JMessagePackage
    {
        public string type;
        public JMessage[] messageChain;
        public JUser sender;
    }
}
