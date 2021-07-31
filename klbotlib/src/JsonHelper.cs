using Newtonsoft.Json;
using System.Web;

namespace klbotlib.Json
{
    static class JsonHelper
    {
        internal static class JsonSettings
        {
            //用于文件存储的Json序列化配置
            internal static JsonSerializerSettings FileSetting = new JsonSerializerSettings
            {
                TypeNameHandling = TypeNameHandling.All,
                Formatting = Formatting.Indented,
            };
            //用于网络传输的Json序列化配置
            internal static JsonSerializerSettings JsonNetworkSetting = new JsonSerializerSettings
            {
                TypeNameHandling = TypeNameHandling.None,
                Formatting = Formatting.None,
            };
        }

        //构建完整消息
        internal static class MessageBuilder
        {
            internal static string BuildPrivateMessageJson(long target_id, string chain)
                => $"{{\"target\":\"{target_id}\",\"messageChain\":[{chain}]}}";
            internal static string BuildGroupMessageJson(long target_id, string chain)
                => $"{{\"target\":\"{target_id}\",\"messageChain\":[{chain}]}}";
            internal static string BuildTempMessageJson(long target_id, long group_id, string chain)
                => $"{{\"qq\":\"{target_id}\",\"group\":\"{group_id}\",\"messageChain\":[{chain}]}}";
        }
        //构建消息链上的元素
        internal static class MessageElementBuilder
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
