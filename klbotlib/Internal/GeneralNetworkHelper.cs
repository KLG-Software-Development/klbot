using System.Net.Http;
using System.Text;
using System.Threading.Tasks;

namespace klbotlib.Internal
{
    internal static class GeneralNetworkHelper
    {
        private static HttpClientHandler _handler = new() { UseProxy = false };
        private static HttpClient _client = new(_handler);
        // POST一条纯文本字符串到给定URL
        internal static async Task<(bool, string)> PostPlainText(string url, string jsonString)
        {
            StringContent content = new StringContent(jsonString, Encoding.UTF8);
            HttpResponseMessage result = await _client.PostAsync(url, content);
            result.EnsureSuccessStatusCode();
            string responseMsg = await result.Content.ReadAsStringAsync();
            return (result.IsSuccessStatusCode, responseMsg);
        }
        // POST multipart信息到给定URL
        internal static async Task<(bool, string)> PostMultipart(string url, MultipartFormDataContent content)
        {
            //content.Headers.ContentType;
            var result = await _client.PostAsync(url, content);
            result.EnsureSuccessStatusCode();
            string responseMsg = result.Content.ReadAsStringAsync().Result;
            return (result.IsSuccessStatusCode, responseMsg);
        }
    }
}