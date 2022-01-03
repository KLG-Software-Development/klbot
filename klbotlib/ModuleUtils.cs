using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using ImgEncoder = System.Drawing.Imaging.Encoder;

namespace klbotlib.Modules.ModuleUtils
{
    /// <summary>
    /// 为模块准备的Http帮助类
    /// </summary>
    public class HttpHelper
    {
        private CancellationToken _cancellationToken = new CancellationToken();
        private HttpClient _client = new();
        /// <summary>
        /// 进行所有请求时的超时时间（秒）。默认为15。
        /// </summary>
        public int Timeout 
        {
            get => _client.Timeout.Milliseconds;
            set
            {
                lock (_client)
                    _client.Timeout = new TimeSpan(0, 0, 0, value, 0);
            } 
        }
        /// <summary>
        /// 进行所有请求时使用的编码
        /// </summary>
        public string ContentEncoding 
        { 
            get => _client.DefaultRequestHeaders.AcceptEncoding.First().ToString();
            set
            {
                lock (_client)
                {
                    _client.DefaultRequestHeaders.AcceptEncoding.Clear();
                    if (!_client.DefaultRequestHeaders.AcceptEncoding.TryParseAdd(value))
                        Console.WriteLine($"警告: 设置编码为\"{value}\"失败。编码未改变");
                }
            }
        }
        /// <summary>
        /// 其他自定义Header
        /// </summary>
        public Dictionary<string, string> Headers { get; } = new Dictionary<string, string>();
        /// <summary>
        /// 创建新的HttpHelper对象
        /// </summary>
        /// <param name="timeout">超时(毫秒)</param>
        /// <param name="contentEncoding">默认Encoding</param>
        public HttpHelper(int timeout = 15, string contentEncoding = "utf-8")
        {
            Timeout = timeout;
            ContentEncoding = contentEncoding;
        }

        /// <summary>
        /// 从指定地址GET字节数组
        /// </summary>
        /// <param name="url">地址</param>
        public async Task<byte[]> GetBytesAsync(string url)
        {
            foreach (var kvp in Headers)
            {
                if (!_client.DefaultRequestHeaders.TryAddWithoutValidation(kvp.Key, kvp.Value))
                    Console.WriteLine($"警告：设置header \"{kvp.Key}\"-\"{kvp.Value}\"失败。Header未添加");
            }
            return await _client.GetByteArrayAsync(url, _cancellationToken);
        }
        /// <summary>
        /// 从指定地址GET一条字符串
        /// </summary>
        /// <param name="url">地址</param>
        public async Task<string> GetStringAsync(string url)
        {
            foreach (var kvp in Headers)
            {
                if (!_client.DefaultRequestHeaders.TryAddWithoutValidation(kvp.Key, kvp.Value))
                    Console.WriteLine($"警告：设置header \"{kvp.Key}\"-\"{kvp.Value}\"失败。Header未添加");
            }
            return await _client.GetStringAsync(url, _cancellationToken);
        }
        /// <summary>
        /// 向指定地址POST一条字符串
        /// </summary>
        /// <param name="url">地址</param>
        /// <param name="body">内容</param>
        public async Task<string> PostStringAsync(string url, string body)
        {
            foreach (var kvp in Headers)
            {
                if (!_client.DefaultRequestHeaders.TryAddWithoutValidation(kvp.Key, kvp.Value))
                    Console.WriteLine($"警告：设置header \"{kvp.Key}\"-\"{kvp.Value}\"失败。Header未添加");
            }
            StringContent content = new(body, System.Text.Encoding.GetEncoding(ContentEncoding));
            return await _client.PostAsync(url, content, _cancellationToken).Result.Content.ReadAsStringAsync();
        }
        /// <summary>
        /// 向指定地址POST一组x-www-form-urlencoded内容
        /// </summary>
        /// <param name="url"></param>
        /// <param name="body"></param>
        /// <returns></returns>
        public async Task<string> PostFormUrlEncodedAsync(string url, string body)
        {
            foreach (var kvp in Headers)
            {
                if (!_client.DefaultRequestHeaders.TryAddWithoutValidation(kvp.Key, kvp.Value))
                    Console.WriteLine($"警告：设置header \"{kvp.Key}\"-\"{kvp.Value}\"失败。Header未添加");
            }
            if (body.StartsWith("?"))
                body = body[1..];
            //解析formUrlEncoded
            List<KeyValuePair<string, string>> form = new();
            string[] kvps = body.Split('&');
            foreach (var kvpString in kvps)
            {
                string[] kvp = kvpString.Split('=');
                if (kvp.Length >= 2)
                    form.Add(new KeyValuePair<string, string>(kvp[0], kvp[1]));
                else
                    form.Add(new KeyValuePair<string,string>(kvp[0], ""));
            }
            FormUrlEncodedContent content = new(form);
            return await _client.PostAsync(url, content, _cancellationToken).Result.Content.ReadAsStringAsync();
        }
    }
    /// <summary>
    /// 为模块开发准备的图像帮助类
    /// </summary>
    public class ImageHelper
    {
        private HttpClient _client = new();

        /// <summary>
        /// 下载一张图片，并解析为Bitmap对象。默认使用伪装的Firefox UserAgent
        /// </summary>
        /// <param name="url">图片地址</param>
        /// <param name="size">输出图片的字节数</param>
        /// <param name="ua">下载时使用的UserAgent</param>
        public Bitmap DownloadImage(string url, out int size, string ua = "User-Agent:Mozilla/5.0 (Windows NT 6.1; rv:2.0.1) Gecko/20210713 Firefox/90.0")
        {
            byte[] bin;
            lock (_client)
            {
                _client.DefaultRequestHeaders.UserAgent.Clear();
                _client.DefaultRequestHeaders.UserAgent.ParseAdd(ua);
                bin = _client.GetByteArrayAsync(url).Result;
            }
            size = bin.Length;
            using (var ms = new MemoryStream(bin))
            {
                Bitmap bmp = new Bitmap(Image.FromStream(ms));
                Console.WriteLine($"完成");
                return bmp;
            }
        }
        /// <summary>
        /// 下载图片为Base64字符串
        /// </summary>
        /// <param name="url">图像地址</param>
        public string DownloadAsBase64(string url)
        {
            lock (_client)
            {
                return Convert.ToBase64String(_client.GetByteArrayAsync(url).Result);
            }
        } 
        /// <summary>
        /// 缩放一张图片到指定分辨率
        /// </summary>
        /// <param name="bmp">待缩放图像</param>
        /// <param name="width">目标宽度</param>
        /// <param name="height">目标高度px</param>
        public Bitmap Resize(Bitmap bmp, int width, int height)
        {
            Bitmap re = new Bitmap(width, height);
            using (Graphics graphic = Graphics.FromImage(re))
            {
                graphic.DrawImage(bmp, 0, 0, width, height);
            }
            return re;
        }
        /// <summary>
        /// 缩放一张图片，使其最大可能大小不超过某个值
        /// </summary>
        /// <param name="bmp">待缩放图片</param>
        /// <param name="size_limit">大小限制（字节）</param>
        public Bitmap ResizeToLimit(Bitmap bmp, int size_limit)
        {
            //计算当前最大可能占用空间
            float current_size = bmp.Width * bmp.Height * 3f;
            //如果未达到限制，不用缩放直接返回
            if (current_size < size_limit)
                return bmp;
            //边缩放比例 = Sqrt(体积缩放比例)
            double diag_factor = Math.Sqrt(size_limit / current_size);
            int _width = (int)(bmp.Width * diag_factor);
            int _height = (int)(bmp.Height * diag_factor);
            return Resize(bmp, _width, _height);
        }
    }
}
