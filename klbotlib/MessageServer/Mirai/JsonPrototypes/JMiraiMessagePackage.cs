namespace klbotlib.MessageServer.Mirai.JsonPrototypes
{
    /// <summary>
    /// 直接从JSON解析出的消息包对象
    /// </summary>
    internal class JMiraiMessagePackage
    {
        public string type;
        public JMiraiMessage[] messageChain;
        public JMiraiUser sender;
    }
}
