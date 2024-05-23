using System;
using System.Web;

namespace klbotlib.MessageDriver.Mirai
{
    static class MiraiJsonHelper
    {
        //构建完整消息
        internal static class MiraiMessageJsonBuilder
        {
            internal static string BuildPrivateMessageJson(long user_id, string chain)
                => $"{{\"target\":\"{user_id}\",\"messageChain\":[{chain}]}}";
            internal static string BuildGroupMessageJson(long group_id, string chain)
                => $"{{\"target\":\"{group_id}\",\"messageChain\":[{chain}]}}";
            internal static string BuildTempMessageJson(long user_id, long group_id, string chain)
                => $"{{\"qq\":\"{user_id}\",\"group\":\"{group_id}\",\"messageChain\":[{chain}]}}";
            internal static string BuildMessageJson(long user_id, long group_id, MessageContext context, string chain)
            {
                if (context == MessageContext.Group)
                    return MiraiJsonHelper.MiraiMessageJsonBuilder.BuildGroupMessageJson(group_id, chain);
                else if (context == MessageContext.Private)
                    return MiraiJsonHelper.MiraiMessageJsonBuilder.BuildPrivateMessageJson(user_id, chain);
                else if (context == MessageContext.Temp)
                    return MiraiJsonHelper.MiraiMessageJsonBuilder.BuildTempMessageJson(user_id, group_id, chain);
                else throw new Exception($"暂不支持的消息上下文类型 \"{context}\"");
            }
        }
        //构建消息链上的元素
        internal static class MiraiMessageElementBuilder
        {
            internal static string BuildPlainElement(string text)
            {
                if (text.Length == 0)
                    return "";
                else
                    return "{\"type\":\"Plain\",\"text\":\"" + HttpUtility.JavaScriptStringEncode(text) + "\"}";
            }
            internal static string BuildFaceElement(string face_name)
                => "{\"type\":\"Face\",\"name\":\"" + face_name + "\"}";
            internal static string BuildTagElement(long target_id)
                => "{\"type\":\"At\",\"target\":\"" + target_id + "\"}";
            internal static string BuildImageElement(string key, string value)
                => "{\"type\":\"Image\",\"" + key + "\":\"" + HttpUtility.JavaScriptStringEncode(value) + "\"}";
            internal static string BuildVoiceElement(string key, string value)
                => "{\"type\":\"Voice\",\"" + key + "\":\"" + HttpUtility.JavaScriptStringEncode(value) + "\"}";
        }
    }
}
