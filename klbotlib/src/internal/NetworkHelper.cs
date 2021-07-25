using Newtonsoft.Json;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Text;

namespace klbotlib.Internal
{
    internal static class NetworkHelper
    {
        /// <summary>
        /// POST一条JSON字符串到给定URL
        /// </summary>
        /// <param name="url">目标url</param>
        /// <param name="json_string">JSON字符串</param>
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
        internal static string FetchMessageListJSON(string url)
        {
            HttpWebRequest request = WebRequest.CreateHttp(url);
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
