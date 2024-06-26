﻿#pragma warning disable IDE1006 // 命名样式
#pragma warning disable IDE0052 // 删除未读的私有成员
#pragma warning disable IDE0051 // 删除未使用的私有成员
#pragma warning disable CS0649 // 删除未使用的私有成员

using klbotlib.Modules.ModuleUtils;
using System.Text;
using System.Text.Json;
using System.Text.RegularExpressions;

namespace klbotlib.Modules;

/// <summary>
/// 图像处理模块
/// </summary>
public partial class IMGPModule : SingleTypeModule<MessagePackage>
{
    /// <inheritdoc/>
    public sealed override bool UseSignature => false;
    /// <inheritdoc/>
    public sealed override bool IsAsync => true;
    /// <inheritdoc/>
    public sealed override string FriendlyName => "图像处理模块";
    /// <inheritdoc/>
    public sealed override string HelpInfo
    {
        get
        {
            _ = _sb.Clear();
            //纯文本消息
            _ = _sb.AppendLine("输入\"[处理类型]\"的同时发送图片，可以对图片进行处理，例如\"上色\"。目前支持的处理类型有：");
            foreach (var key in _typeByWordProc.Keys)
            {
                _ = _sb.Append(" " + key);
            }
            _ = _sb.AppendLine("；\n");
            _ = _sb.AppendLine("输入\"什么[关键词]\"，识别附带图片中的内容。例如，\"什么地方\"。目前支持的关键词有：");
            foreach (var key in s_typeByWordRecg.Keys)
            {
                _ = _sb.Append(" " + key);
            }
            _ = _sb.AppendLine("；\n");
            _ = _sb.AppendLine("输入\"如何评价\"，可以让AI对附带照片中的人脸打分；\n");
            _ = _sb.AppendLine("选择两张图并输入\"换脸/交配\"，可以把后一张图的脸换到前一张图片的脸上；\n");
            _ = _sb.AppendLine("由于傻逼百度不允许处理大图片，一些图片可能处理不了。可以输入\"压缩\"让本模块手动压缩并返图。");
            return _sb.ToString();
        }
    }

    private const string PostUrl = "https://ai.baidu.com/aidemo";
    private static readonly Regex s_pattern = Pattern();
    private static readonly Dictionary<string, string> s_typeByWordRecg = new()
    {
        { "东西", "advanced_general" },
        { "玩意", "advanced_general" },
        { "动物", "animal" },
        { "植物", "plant" },
        { "牌子", "logo" },
        { "品牌", "logo" },
        { "菜", "dish" },
        { "蔬菜", "ingredient" },
        { "地方", "landmark" },
        { "车", "car" }
    };
    private static readonly Dictionary<string, string> _typeByWordProc = new()
    {
        { "清晰增强", "https://aip.baidubce.com/rest/2.0/image-process/v1/image_definition_enhance" },
        { "色彩增强", "https://aip.baidubce.com/rest/2.0/image-process/v1/color_enhance" },
        { "无损放大", "img_quality_enhance" },
        { "上色", "colourize" },
        { "动漫化", "https://aip.baidubce.com/rest/2.0/image-process/v1/selfie_anime" }
    };
    private static readonly List<string> _mergeKeyword = ["换脸", "囍", "杂交", "交配"];
    private static readonly HttpHelper _httpHelper = new();
    private static readonly StringBuilder _sb = new();  //caller clear
    private static string ErrorString(int code, string? msg)
    {
        return msg == null ? $"错误[{code}]：(无错误信息)" : $"错误[{code}]：{msg}";
    }
    private string? GetFuck() => ModuleAccess.GetModule<FuckModule>().SingleSentence();
    private static async Task<(bool, string)> TryMergeWithBase64Async(MessageImage msgA, MessageImage msgB)
    {
        string b641 = await _httpHelper.GetAsBase64Async(msgA.Url);
        string b642 = await _httpHelper.GetAsBase64Async(msgB.Url);
        string queryString = "?type=merge&apiType=face";
        JMergeRequest request = new(new JImage(b642, "BASE64"), new JImage(b641, "BASE64"), "2.0");
        string json = await _httpHelper.PostJsonAsync(PostUrl + queryString, request);
        JReplySingle? reply = JsonSerializer.Deserialize<JReplySingle>(json);
        //错误检查
        return reply == null || reply.errno != 0 || reply.data == null || reply.data.result == null
            ? ((bool, string))(false, string.Empty)
            : reply.data.error_code != 0 ? ((bool, string))(false, string.Empty) : (true, $@"\image:\base64:{reply.data.result.merge_image}");
    }
    private static bool TryMergeWithUrl(MessageImage msgA, MessageImage msgB, out string result)
    {
        string queryString = "?type=merge&apiType=face";
        string url0 = JsonEncodedText.Encode(msgA.Url).ToString();
        string url1 = JsonEncodedText.Encode(msgB.Url).ToString();
        string body = "{\"image_template\":{\"image\":" + url0 + ",\"image_type\":\"URL\"},\"image_target\":{\"image\":" + url1 + ",\"image_type\":\"URL\"},\"version\":\"2.0\"}";
        string json = _httpHelper.PostStringAsync(PostUrl + queryString, body).Result;
        JReplySingle? reply = JsonSerializer.Deserialize<JReplySingle>(json);
        //错误检查
        if (reply == null)
        {
            result = ErrorString(-1, "未知错误：反序列化得到Null结果");
            return false;
        }
        else if (reply.errno != 0 || reply.data == null)
        {
            result = ErrorString(reply.errno, reply.msg);
            return false;
        }
        else if (reply.data.error_code != 0 || reply.data.result == null)
        {
            result = ErrorString(reply.data.error_code, reply.data.error_msg);
            return false;
        }
        else
        {
            result = $@"\image:\base64:{reply.data.result.merge_image}";
            return true;
        }
    }

    /// <inheritdoc/>
    public override Task<Message?> Processor(MessageContext context, MessagePackage msg)
    {
        // TODO: 重做IMGP模块
        return (Message?)null;
    }

    private class JReply { public int errno; public string? msg; }
    private class JReplyMulti : JReply { public JDataMulti? data; }
    private class JDataMulti { public List<JResultMulti>? result; }
    private class JResultMulti { public float score; public float probability; public string? name; public string? keyword; }

    private class JReplySingle : JReply { public JDataSingle? data; }
    private class JDataSingle { public int error_code; public string? error_msg; public JResultSingle? result; }   //error_code和error_msg：合成
    private class JResultSingle { public string? landmark; public string? merge_image; }   //merge_image：合成；landmark：地标

    private class JFaceReply : JReply { public JFaceData? data; }
    private class JFaceData { public int errno; public JFaceResult? result; }
    private class JFaceResult { public int face_num; public JFace[]? face_list; }
    private class JFace { public int age; public float beauty; }

    private class JProcReply : JReply { public JProcData? data; }
    private class JProcData { public string? image; }

    private record JMergeRequest(JImage? image_target, JImage? image_template, string? version);
    private record JImage(string? image, string? image_type);

    [GeneratedRegex(@"什么(东西)", RegexOptions.Compiled)]
    private static partial Regex Pattern();
}
