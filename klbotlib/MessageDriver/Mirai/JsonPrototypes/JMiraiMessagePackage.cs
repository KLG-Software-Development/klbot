namespace klbotlib.MessageDriver.Mirai.JsonPrototypes
{
    /// <summary>
    /// 直接从JSON解析出的消息包对象
    /// </summary>
    internal class JMiraiMessagePackage
    {
        public string? type;

        //普通消息
        public JMiraiMessage[]? messageChain;
        public JMiraiUser? sender;

        //群撤回消息
        public long messageId;
        public long authorId;           //被撤回消息发送者ID
        public JMiraiUser? @operator;    //撤回/禁言者

        //禁言消息
        public JMiraiUser? member;       //被禁言成员
        public uint durationSeconds;     //禁言时长
    }
}
