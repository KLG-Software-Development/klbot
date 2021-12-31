using Newtonsoft.Json;
using System;
using System.IO;
using System.Net;
using System.Net.Http;
using System.Text;

namespace klbotlib.Internal
{
    internal static class NetworkHelper
    {
        private static HttpClient _client = new();
        // POST一条纯文本字符串到给定URL
        internal static bool PostPlainText(string url, string jsonString, out string responseMsg)
        {
            StringContent content = new StringContent(jsonString, Encoding.UTF8);
            var result = _client.PostAsync(url, content).Result;
            responseMsg = result.Content.ReadAsStringAsync().Result;
            return result.IsSuccessStatusCode;
        }
        // POST multipart信息到给定URL
        internal static bool PostMultipart(string url, MultipartFormDataContent content, out string responseMsg)
        {
            //content.Headers.ContentType;
            var result = _client.PostAsync(url, content).Result;
            responseMsg = result.Content.ReadAsStringAsync().Result;
            return result.IsSuccessStatusCode;
        }
        //返回发送特定上下文消息的url
        internal static string GetSendMessageUrl(string serverUrl, MessageContext context)
        {
            string url;
            if (context == MessageContext.Group)
                url = $"{serverUrl}/sendGroupMessage";
            else if (context == MessageContext.Private)
                url = $"{serverUrl}/sendFriendMessage";
            else if (context == MessageContext.Temp)
                url = $"{serverUrl}/sendTempMessage";
            else
                throw new Exception($"暂不支持的消息上下文类型 \"{context}\"");
            return url;
        }
        internal static string GetUploadFileUrl(string serverUrl, MessageContext context)
            => serverUrl + "/file/upload";

        //返回获取消息的url
        internal static string GetFetchMessageUrl(string serverUrl)
            => $"{serverUrl}/fetchMessage";
        internal static string FetchMessageListJSON(string serverUrl, string encoding = "utf-8")
        {
            _client.DefaultRequestHeaders.AcceptEncoding.Clear();
            _client.DefaultRequestHeaders.AcceptEncoding.TryParseAdd(encoding);
            return _client.GetStringAsync(GetFetchMessageUrl(serverUrl)).Result;
        }
    }
}