using System.Net.Http;
using System.Text;
using System.Threading.Tasks;

namespace klbotlib.Internal
{
    internal static class GeneralNetworkHelper
    {
        private static HttpClient _client = new();
        // POST一条纯文本字符串到给定URL
        internal static async Task<(bool, string)> PostPlainText(string url, string jsonString)
        {
            StringContent content = new StringContent(jsonString, Encoding.UTF8);
            HttpResponseMessage result = await _client.PostAsync(url, content);
            string responseMsg = await result.Content.ReadAsStringAsync();
            return (result.IsSuccessStatusCode, responseMsg);
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