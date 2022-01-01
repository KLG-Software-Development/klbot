using klbotlib.Exceptions;

namespace klbotlib.MessageServer.Mirai.JsonPrototypes
{
    internal class JMiraiResponse
    {
        public int code;
        public string msg;

        public void CheckStatusCode()
        {
            if (code != 0)
                throw new MiraiException(code, msg);
        }
    }
}
