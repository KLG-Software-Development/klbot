using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Net;
using System.Text;
using ImgEncoder = System.Drawing.Imaging.Encoder;

namespace klbotlib.Modules.ModuleUtils
{
    public class HttpHelper
    {
        public string UA { get; set; } = "User-Agent:Mozilla/5.0 (Windows NT 6.1; rv:2.0.1) Gecko/20210713 Firefox/90.0";
        public string ContentType { get; set; } = "application/x-www-form-urlencoded";
        public Encoding Encoding { get; set; } = Encoding.UTF8;
        public Dictionary<string, string> Headers { get; } = new Dictionary<string, string>();

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
    }
    public class ImageHelper
    {
        WebClient client = new WebClient();
        ImageCodecInfo jpeg_codec;

        public Bitmap DownloadImage(string url, out int size)
        {
            byte[] bin = client.DownloadData(url);
            size = bin.Length;
            using (var ms = new MemoryStream(bin))
            {
                Bitmap bmp = new Bitmap(Image.FromStream(ms));
                return bmp;
            }
        }
        public void CompressSave(Bitmap bmp, long quality, string path)
        {
            EncoderParameters eps = new EncoderParameters(1);
            EncoderParameter ep = new EncoderParameter(ImgEncoder.Quality, quality);
            eps.Param[0] = ep;
            bmp.Save(path, jpeg_codec, eps);
        }
        public Bitmap Resize(Bitmap bmp, int width, int height)
        {
            Bitmap re = new Bitmap(width, height);
            using (Graphics graphic = Graphics.FromImage(re))
            {
                graphic.DrawImage(bmp, 0, 0, width, height);
            }
            return re;
        }
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
        public string DownloadAsBase64(string url) => Convert.ToBase64String(client.DownloadData(url));
        public ImageHelper()
        {
            ImageCodecInfo[] icis = ImageCodecInfo.GetImageEncoders();
            foreach (var ici in icis)
            {
                if (ici.MimeType == "image/jpeg")
                    jpeg_codec = ici;
                return;
            }
            throw new Exception("ImageHelper初始化错误：找不到JPEG编码器");
        }
    }
}
