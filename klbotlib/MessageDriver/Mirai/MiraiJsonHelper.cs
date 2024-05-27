using System;
using System.Text.Json;
using System.Web;

namespace klbotlib.MessageDriver.Mirai
{
    internal static class MiraiJsonHelper
    {
        private static readonly JsonSerializerOptions _protocolDeserializeOptions = new()
        {
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
            IncludeFields = true
        };

        internal static T? Deserialize<T>(string json)
        {
            return JsonSerializer.Deserialize<T>(json, _protocolDeserializeOptions);
        }
        //构建完整消息
        internal static class MiraiMessageJsonBuilder
        {
            internal static string BuildPrivateMessageJson(long userId, string chain)
                => $"{{\"target\":\"{userId}\",\"messageChain\":[{chain}]}}";
            internal static string BuildGroupMessageJson(long groupId, string chain)
                => $"{{\"target\":\"{groupId}\",\"messageChain\":[{chain}]}}";
            internal static string BuildTempMessageJson(long userId, long groupId, string chain)
                => $"{{\"qq\":\"{userId}\",\"group\":\"{groupId}\",\"messageChain\":[{chain}]}}";
            private static string BuildMessageJsonChain(Message msg)
                => throw new NotImplementedException();
            internal static string BuildMessageJson(long userId, long groupId, MessageContext context, Message msg)
            {
                string chain = BuildMessageJsonChain(msg);
                if (context == MessageContext.Group)
                    return BuildGroupMessageJson(groupId, chain);
                else if (context == MessageContext.Private)
                    return BuildPrivateMessageJson(userId, chain);
                else if (context == MessageContext.Temp)
                    return BuildTempMessageJson(userId, groupId, chain);
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
            internal static string BuildFaceElement(string faceName)
                => "{\"type\":\"Face\",\"name\":\"" + faceName + "\"}";
            internal static string BuildTagElement(long targetId)
                => "{\"type\":\"At\",\"target\":\"" + targetId + "\"}";
            internal static string BuildImageElement(string key, string value)
                => "{\"type\":\"Image\",\"" + key + "\":\"" + HttpUtility.JavaScriptStringEncode(value) + "\"}";
            internal static string BuildVoiceElement(string key, string value)
                => "{\"type\":\"Voice\",\"" + key + "\":\"" + HttpUtility.JavaScriptStringEncode(value) + "\"}";
        }
    }
}
