using System.Collections.Generic;

namespace klbotlib.MessageDriver.Mirai.JsonPrototypes
{
    internal class JMiraiFetchMessageResponse : JMiraiResponse
    {
        public List<JMiraiMessagePackage>? data;
    }
}
