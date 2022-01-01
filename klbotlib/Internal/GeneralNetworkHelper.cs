using System;
using System.Net.Http;
using System.Text;

namespace klbotlib.Internal
{
    internal static class GeneralNetworkHelper
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
    }
}