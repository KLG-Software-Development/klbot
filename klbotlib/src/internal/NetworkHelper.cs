using Newtonsoft.Json;
using System;
using System.IO;
using System.Net;
using System.Text;

namespace klbotlib.Internal
{
    internal static class NetworkHelper
    {
        // POST一条JSON字符串到给定URL
        internal static void PostJSON(string url, string json_string)
        {
            HttpWebRequest request = WebRequest.CreateHttp(url);
            request.Method = "POST";
            request.ContentType = "application/json";
            using (var stream = request.GetRequestStream())
            {
                byte[] data = Encoding.UTF8.GetBytes(json_string);
                stream.Write(data, 0, data.Length);
                stream.Close();
            }
            using (var stream = request.GetResponse().GetResponseStream())
            {
                StreamReader reader = new StreamReader(stream, Encoding.UTF8);
                string response_str = reader.ReadToEnd();
                JsonConvert.DeserializeObject<JMiraiResponse>(response_str).CheckStatusCode();
            }
        }
        //返回发送特定上下文消息的url
        internal static string GetSendMessageUrl(string server_url, MessageContext context)
        {
            string url;
            if (context == MessageContext.Group)
                url = $"{server_url}/sendGroupMessage";
            else if (context == MessageContext.Private)
                url = $"{server_url}/sendFriendMessage";
            else if (context == MessageContext.Temp)
                url = $"{server_url}/sendTempMessage";
            else
                throw new Exception($"暂不支持的消息上下文类型 \"{context}\"");
            return url;
        }
        //返回获取消息的url
        internal static string GetFetchMessageUrl(string server_url)
            => $"{server_url}/fetchMessage";
        internal static string FetchMessageListJSON(string server_url)
        {
            HttpWebRequest request = WebRequest.CreateHttp(GetFetchMessageUrl(server_url));
            request.Method = "GET";
            string response_str = "";
            using (var stream = request.GetResponse().GetResponseStream())
            {
                StreamReader reader = new StreamReader(stream, Encoding.UTF8);
                response_str = reader.ReadToEnd();
            }
            return response_str;
        }
    }
}
