using klbotlib.Extensions;
using klbotlib.Modules.ImgRecgModuleNamespace;
using klbotlib.Modules.ModuleUtils;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;

namespace klbotlib.Modules
{
    public class IMGPModule : SingleTypeModule<MessageImagePlain>
    {
        public sealed override bool UseSignature => false;
        public sealed override bool IsAsync => true;
        public sealed override string FriendlyName => "图像处理模块";
        public sealed override string HelpInfo
        {
            get
            {
                StringBuilder sb = new StringBuilder();
                //纯文本消息
                sb.AppendLine(("输入\"[处理类型]\"的同时发送图片，可以对图片进行处理，例如\"上色\"。目前支持的处理类型有："));
                foreach (var key in _type_by_word_proc.Keys)
                {
                    sb.Append(" " + key);
                }
                sb.AppendLine("；\n");
                sb.AppendLine(("输入\"什么[关键词]\"，识别附带图片中的内容。例如，\"什么地方\"。目前支持的关键词有："));
                foreach (var key in _type_by_word_recg.Keys)
                {
                    sb.Append(" " + key);
                }
                sb.AppendLine("；\n");
                sb.AppendLine("输入\"如何评价\"，可以让AI对附带照片中的人脸打分；\n");
                sb.AppendLine("选择两张图并输入\"换脸/交配\"，可以把后一张图的脸换到前一张图片的脸上；\n");
                sb.AppendLine("由于傻逼百度不允许处理大图片，一些图片可能处理不了。可以输入\"压缩\"让本模块手动压缩并返图。");
                return sb.ToString();
            }
        }

        private const string _post_url = "https://ai.baidu.com/aidemo";
        private static readonly Regex _pattern = new Regex(@"什么(东西)");
        private static readonly Dictionary<string, string> _type_by_word_recg = new Dictionary<string, string>
        {
            { "东西", "advanced_general"},
            { "玩意", "advanced_general"},
            { "动物", "animal"},
            { "植物", "plant"},
            { "牌子", "logo"},
            { "品牌", "logo"},
            { "菜", "dish"},
            { "蔬菜", "ingredient"},
            { "地方", "landmark"},
            { "车", "car"}
        };
        private static readonly Dictionary<string, string> _type_by_word_proc = new Dictionary<string, string>
        {
            { "清晰增强", "https://aip.baidubce.com/rest/2.0/image-process/v1/image_definition_enhance"},
            { "色彩增强", "https://aip.baidubce.com/rest/2.0/image-process/v1/color_enhance"},
            { "无损放大", "img_quality_enhance"},
            { "上色", "colourize"},
            { "动漫化", "https://aip.baidubce.com/rest/2.0/image-process/v1/selfie_anime"}
        };
        private static readonly List<string> _merge_keyword = new List<string> { "换脸", "囍", "杂交", "交配" };
        private static readonly HttpHelper _http_helper = new HttpHelper();
        private static readonly ImageHelper _img_helper = new ImageHelper();
        private string ErrorString(int code, string msg) => $"错误[{code}]：{msg}";
        private string GetFuck() => ModuleAccess.GetModule<FuckModule>().SingleSentence();

        public override string Filter(MessageImagePlain msg)
        {
            if (msg.TargetContains(HostBot.SelfID))  //图文消息
            {
                string text = msg.Text.Trim();
                if (msg.UrlList.Count == 1)   //单图文
                {
                    if (text.StartsWith("什么") && text.Length != 2)
                        return "recogn";
                    else if (text == "如何评价")
                        return "face";
                    else if (text == "压缩")
                        return "compress";
                    else if (_type_by_word_proc.ContainsKey(text))
                        return "image process";
                }
                //多图文
                else if (msg.UrlList.Count == 2 && _merge_keyword.Contains(text))
                    return "merge";
            }
            return null;
        }
        public override string Processor(MessageImagePlain msg, string filter_out)
        {
            StringBuilder sb = new StringBuilder();
            //之后都是图文消息，统一转换类型为MessageImagePlain
            //多图文消息
            switch (filter_out)
            {
                case "merge":
                    Messaging.ReplyMessage(msg, "转换中...");
                    //HostBot.ReplyPlainMessage(this, msg, "正在下载父本并转换为base64...");
                    string b641 = _img_helper.DownloadAsBase64(msg.UrlList[0]);
                    //HostBot.ReplyPlainMessage(this, msg, "正在下载母本并转换为base64...");
                    string b642 = _img_helper.DownloadAsBase64(msg.UrlList[1]);
                    string query_string = "?type=merge&apiType=face";
                    string body = "{\"image_template\":{\"image\":\"" + b641 + "\",\"image_type\":\"BASE64\"},\"image_target\":{\"image\":\"" + b642 + "\",\"image_type\":\"BASE64\"},\"version\":\"2.0\"}";
                    string json = _http_helper.PostString(_post_url + query_string, body);
                    JReplySingle reply = JsonConvert.DeserializeObject<JReplySingle>(json);
                    //错误检查
                    if (reply.errno != 0)
                        return ErrorString(reply.errno, reply.msg);
                    if (reply.data.error_code != 0)
                        return ErrorString(reply.data.error_code, reply.data.error_msg);
                    string b64 = reply.data.result.merge_image;
                    return $@"\image:\base64:{b64}";
            }
            //以下都是单图文消息 可以统一把图片Url拿出来做URL encode
            string esc_url = Uri.EscapeDataString(msg.UrlList[0]);
            switch (filter_out)
            {
                case "recogn": //识别
                    string word = msg.Text.Trim().Substring(2);
                    if (!_type_by_word_recg.ContainsKey(word))
                        return ModuleAccess.GetModule<FuckModule>().SingleSentence() + "，这个不会";
                    string type = _type_by_word_recg[word];
                    string body = $"image&image_url={esc_url}&type={type}&show=true";
                    Messaging.ReplyMessage(msg, "识别中...");
                    if (type == "landmark")
                    {
                        //只有一个result对象，用JReplySingle
                        JReplySingle reply = JsonConvert.DeserializeObject<JReplySingle>(_http_helper.PostString(_post_url, body));
                        //错误检查
                        if (reply.errno != 0 || reply.msg.Trim().ToLower() != "success")
                            return ErrorString(reply.errno, reply.msg);
                        string landmark = reply.data.result.landmark;
                        return string.IsNullOrEmpty(landmark) 
                            ? GetFuck() + "，看不出来" 
                            : landmark;
                    }
                    else
                    {
                        string json = _http_helper.PostString(_post_url, body);
                        JReplyMulti reply = JsonConvert.DeserializeObject<JReplyMulti>(json);
                        if (reply.errno != 0 || reply.msg.Trim().ToLower() != "success")
                            return ErrorString(reply.errno, reply.msg);
                        if (type == "advanced_general")
                            return $"有{reply.data.result[0].score * 100:f1}%的概率是{reply.data.result[0].keyword}";
                        else if (type == "logo")
                            return $"有{reply.data.result[0].probability * 100:f1}%的概率是{reply.data.result[0].name}";
                        else
                            reply.data.result.ForEach(x => sb.AppendLine($"有{x.score * 100:f1}%的概率是{x.name}"));
                    }
                    return sb.ToString();
                case "face": //人脸评分
                    body = $"image&image_url={esc_url}&type=face&show=true&max_face_num=2&face_field=age%2Cbeauty&image_type=BASE64";
                    JFaceReply reply_face = JsonConvert.DeserializeObject<JFaceReply>(_http_helper.PostString(_post_url, body));
                    if (reply_face.errno != 0 || reply_face.msg.Trim().ToLower() != "success")
                        return ErrorString(reply_face.errno, reply_face.msg);
                    if (reply_face.data.result.face_num == 0)
                        return GetFuck() + "，识别不到人脸";
                    if (reply_face.data.result.face_num != 1)
                        return GetFuck() + "，识别出的人脸超过一个";
                    int age = reply_face.data.result.face_list[0].age;
                    float beauty = reply_face.data.result.face_list[0].beauty;
                    return $"{age}岁，{beauty}分";
                case "compress": //本地压缩
                    Messaging.ReplyMessage(msg, $"正在下载图片...");
                    Bitmap bmp = _img_helper.DownloadImage(msg.UrlList[0], out int original_size);
                    MemoryStream ms = new MemoryStream();
                    Messaging.ReplyMessage(msg, "本地压缩中...");
                    bmp.Save(ms, ImageFormat.Jpeg);
                    byte[] bin = ms.ToArray();
                    string b64 = Convert.ToBase64String(bin);
                    Messaging.ReplyMessage(msg, $"压缩完成。原始大小为{original_size.ToMemorySizeString(1)}，返图大小为{bin.Length.ToMemorySizeString(1)}");
                    return $@"\image:\base64:{b64}";
                case "image process": //图像处理
                    word = msg.Text.Trim();
                    type = Uri.EscapeDataString(_type_by_word_proc[word]);
                    body = $"image&image_url={esc_url}&type={type}&show=true";
                    //HostBot.ReplyPlainMessage(this, msg, "处理中...");
                    JProcReply reply_proc = JsonConvert.DeserializeObject<JProcReply>(_http_helper.PostString(_post_url, body));
                    //错误检查
                    if (reply_proc.errno != 0 || reply_proc.msg.Trim().ToLower() != "success")
                        return ErrorString(reply_proc.errno, reply_proc.msg);
                    b64 = reply_proc.data.image;
                    return $@"\image:\base64:{b64}";
                default:
                    return null;
            }
        }
    }
}

namespace klbotlib.Modules.ImgRecgModuleNamespace
{
    public class JReply { public int errno; public string msg; }
    public class JReplyMulti : JReply { public JDataMulti data; }
    public class JDataMulti { public List<JResultMulti> result; }
    public class JResultMulti { public float score; public float probability; public string name; public string keyword; }

    public class JReplySingle : JReply { public JDataSingle data; }
    public class JDataSingle { public int error_code; public string error_msg; public JResultSingle result; }   //error_code和error_msg：合成
    public class JResultSingle { public string landmark; public string merge_image; }   //merge_image：合成；landmark：地标

    public class JFaceReply : JReply { public JFaceData data; }
    public class JFaceData { public int errno; public JFaceResult result; }
    public class JFaceResult { public int face_num; public JFace[] face_list; }
    public class JFace { public int age; public float beauty; }

    public class JProcReply : JReply { public JProcData data; }
    public class JProcData { public string image; }
}
