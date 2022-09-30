using System.Collections.Generic;

namespace klbotlib.MessageServer.Mirai.JsonPrototypes
{
    internal class JMiraiFetchMessageResponse : JMiraiResponse
    {
        public List<JMiraiMessagePackage>? data;
    }
}
