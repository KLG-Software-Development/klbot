using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Net;
using System.Text;
using System.Threading.Tasks;
using ImgEncoder = System.Drawing.Imaging.Encoder;

namespace klbotlib.Modules.ModuleUtils
{
    /// <summary>
    /// 为模块准备的Http帮助类
    /// </summary>
    public class HttpHelper
    {
        /// <summary>
        /// 进行所有请求时使用的UA标识
        /// </summary>
        public string UA { get; set; } = "User-Agent:Mozilla/5.0 (Windows NT 6.1; rv:2.0.1) Gecko/20210713 Firefox/90.0";
        /// <summary>
        /// 进行所有请求时使用的ContentType
        /// </summary>
        public string ContentType { get; set; } = "application/x-www-form-urlencoded";
        /// <summary>
        /// 进行所有请求时使用的编码
        /// </summary>
        public Encoding Encoding { get; set; } = Encoding.UTF8;
        /// <summary>
        /// 其他自定义Header
        /// </summary>
        public Dictionary<string, string> Headers { get; } = new Dictionary<string, string>();
        /// <summary>
        /// 向指定地址POST一条字符串
        /// </summary>
        /// <param name="url">地址</param>
        /// <param name="body">内容</param>
        public string PostString(string url, string body)
        {
            HttpWebRequest request = WebRequest.CreateHttp(url);
            request.Method = "POST";
            request.UserAgent = UA;
            request.ContentType = ContentType;
            foreach (var kvp in Headers)
            {
                request.Headers.Add(kvp.Key, kvp.Value);
            }
            using (var stream = request.GetRequestStream())
            {
                byte[] data = Encoding.GetBytes(body);
                stream.Write(data, 0, data.Length);
                stream.Close();
            }
            using (var stream = request.GetResponse().GetResponseStream())
            {
                StreamReader reader = new StreamReader(stream, Encoding.UTF8);
                return reader.ReadToEnd();
            }
        }
        /// <summary>
        /// 向指定地址POST一条字符串 (异步版)
        /// </summary>
        /// <param name="url">地址</param>
        /// <param name="body">内容</param>
        /// <returns></returns>
        public async Task<string> PostStringAsync(string url, string body)
        {
            HttpWebRequest request = WebRequest.CreateHttp(url);
            request.Method = "POST";
            request.UserAgent = UA;
            request.ContentType = ContentType;
            foreach (var kvp in Headers)
            {
                request.Headers.Add(kvp.Key, kvp.Value);
            }
            using (var stream = await request.GetRequestStreamAsync())
            {
                byte[] data = Encoding.GetBytes(body);
                stream.Write(data, 0, data.Length);
                stream.Close();
            }
            using (var stream = request.GetResponse().GetResponseStream())
            {
                StreamReader reader = new StreamReader(stream, Encoding.UTF8);
                return reader.ReadToEnd();
            }
        }
    }
    /// <summary>
    /// 为模块开发准备的图像帮助类
    /// </summary>
    public class ImageHelper
    {
        WebClient client = new WebClient();
        /// <summary>
        /// 下载一张图片，并解析为Bitmap对象。默认使用伪装的Firefox UserAgent
        /// </summary>
        /// <param name="url">图片地址</param>
        /// <param name="size">输出图片的字节数</param>
        /// <param name="ua">下载时使用的UserAgent</param>
        public Bitmap DownloadImage(string url, out int size, string ua = "User-Agent:Mozilla/5.0 (Windows NT 6.1; rv:2.0.1) Gecko/20210713 Firefox/90.0")
        {
            byte[] bin = client.DownloadData(url);
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
        public string DownloadAsBase64(string url) => Convert.ToBase64String(client.DownloadData(url));
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
