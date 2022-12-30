﻿namespace klbotlib.MessageServer.Mirai.JsonPrototypes
{
    /// <summary>
    /// 直接从JSON解析的消息对象
    /// </summary>
    internal class JMiraiMessage
    {
        public string? type;
        public long time;
        public long target;
        public string? text;
        public string? url;
        public string? faceId;
        public string? name;
    }
}
