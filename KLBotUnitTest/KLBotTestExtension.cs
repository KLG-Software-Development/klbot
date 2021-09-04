using klbotlib;
using System.Collections.Generic;
using System.Web;

namespace KLBotUnitTest
{
    static class KLBotTestExtension
    {
        private static string GenerateSimulatedResponse(MessageContext context, long sender_id, long group_id, string chain)
        {
            string json = "";
            switch (context)
            {
                case MessageContext.Private:
                    json = "{\"code\":0,\"msg\":\"\",\"data\":[{\"type\":\"FriendMessage\"," + chain + ",\"sender\":{\"id\":" + sender_id + ",\"nickname\":\"UNIT_TEST\",\"remark\":\"UNIT_TEST\"}}]}";
                    break;
                case MessageContext.Group:
                    json = "{\"code\":0,\"msg\":\"\",\"data\":[{\"type\":\"GroupMessage\"," + chain + ",\"sender\":{\"id\":" + sender_id + ",\"memberName\":\"UNIT_TEST\",\"specialTitle\":\"\",\"permission\":\"MEMBER\",\"joinTimestamp\":6,\"lastSpeakTimestamp\":7,\"muteTimeRemaining\":0,\"group\":{\"id\":" + group_id + ",\"name\":\"UNIT_TEST\",\"permission\":\"MEMBER\"}}}]}";
                    break;
                case MessageContext.Temp:
                    json = "{\"code\":0,\"msg\":\"\",\"data\":[{\"type\":\"TempMessage\"," + chain + ",\"sender\":{\"id\":" + sender_id + ",\"memberName\":\"UNIT_TEST\",\"specialTitle\":\"\",\"permission\":\"MEMBER\",\"joinTimestamp\":6,\"lastSpeakTimestamp\":7,\"muteTimeRemaining\":0,\"group\":{\"id\":" + group_id + ",\"name\":\"UNIT_TEST\",\"permission\":\"MEMBER\"}}}]}";
                    break;
            }
            return json;
        }

        /// <summary>
        /// 模拟传入一条纯文本消息
        /// </summary>
        public static void SimulateMessagePlainInput(this KLBot bot, MessageContext context, long sender_id, long group_id, string text)
        {
            string chain = "\"messageChain\":[{\"type\":\"Plain\",\"text\":\"" + HttpUtility.JavaScriptStringEncode(text) + "\"}]";
            string json = GenerateSimulatedResponse(context, sender_id, group_id, chain);
            var msgs = bot.SimulateFetchMessages(json);
            bot.ProcessMessages(msgs);
        }
    }
}
