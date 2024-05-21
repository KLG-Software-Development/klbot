using System.Collections.Generic;

namespace klbotlib.MessageClient.Mirai.JsonPrototypes
{
    internal class JMiraiFetchMessageResponse : JMiraiResponse
    {
        public List<JMiraiMessagePackage>? data;
    }
}
