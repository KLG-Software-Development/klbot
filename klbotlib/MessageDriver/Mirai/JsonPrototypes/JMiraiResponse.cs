using System;

namespace klbotlib.MessageDriver.Mirai.JsonPrototypes
{
    internal class JMiraiResponse
    {
        public int code;
        public string? msg;

        public void CheckStatusCode()
        {
            if (code != 0)
            {
                if (msg == null)
                    throw new Exception("JMiraiResponse中的消息字段为null");
                throw new MiraiException(code, msg);
            }
        }
    }
}
