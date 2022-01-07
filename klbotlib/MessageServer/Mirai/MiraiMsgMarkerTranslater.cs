using klbotlib.Exceptions;
using klbotlib.Json;
using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace klbotlib.MessageServer.Mirai
{
    // 翻译MessageMarker文本到mirai_json文本
    internal static class MiraiMsgMarkerTranslater
    {
        private static readonly Exception ParseMessageMarkerException = new Exception("解析MsgMarker文本时发生错误");
        private static readonly Regex _prefixPattern = new Regex(@"\\(\w+?):(.+)", RegexOptions.Compiled);
        private static readonly Regex _codePat = new Regex(@"{([^{\\]*(?:\\.[^}\\]*)*)}", RegexOptions.Compiled);   //匹配{} 但是排除转义\{\}
        private static readonly Regex _facePat = new Regex(@"face:(\w+)", RegexOptions.Compiled);
        private static readonly Regex _protoPat = new Regex(@"^\w+://", RegexOptions.Compiled);
        private static bool TryParsePrefix(string content, out string prefix, out string body, bool startOnly = false)
        {
            prefix = "";
            body = content;
            if (!_prefixPattern.IsMatch(content))
                return false;
            var groups = _prefixPattern.Match(content).Groups;
            if (startOnly && groups[0].Index != 0)
                return false;
            prefix = groups[1].Value.ToLower(); //prefix不区分大小写
            body = groups[2].Value;
            return true;
        }

        //按类型分类的helper
        //把文本类型的MsgMarker编译为MessageChain
        private static string CompilePlainChainJson(string content)
        {
            List<string> elements = new List<string>();
            var matches = _codePat.Matches(content);
            int lhsStart = 0;
            foreach (Match match in matches)
            {
                string code = match.Groups[1].Value;
                int lhsEnd = match.Index;  //不包括该位本身 即[ , )
                string lhs = content.Substring(lhsStart, lhsEnd - lhsStart);    //到上一个表情之间的文本消息
                if (lhs.Length != 0)
                    elements.Add(JsonHelper.MessageElementBuilder.BuildPlainElement(lhs));    //先添加文本消息
                if (TryParsePrefix(code, out string prefix, out string body))
                    if (prefix == "face")   //表情消息。格式：{\face:face_name}
                        elements.Add(JsonHelper.MessageElementBuilder.BuildFaceElement(body));
                    else if (prefix == "tag")     //@消息。格式：{\tag:目标id}
                    {
                        if (!long.TryParse(body, out long id))
                            throw new MsgMarkerException($"无法将{body}转换为目标ID");
                        elements.Add(JsonHelper.MessageElementBuilder.BuildTagElement(id));
                    }
                    else
                        throw new MsgMarkerException($"未知或不支持的嵌入消息类型\"{prefix}\"");
                else
                    elements.Add(JsonHelper.MessageElementBuilder.BuildPlainElement(match.Value)); //如果无法匹配prefix语法，则当作笔误处理，按照纯文本输出
                lhsStart = lhsEnd + match.Value.Length;
            }
            //处理最后可能剩下的文本
            if (lhsStart != content.Length)
                elements.Add(JsonHelper.MessageElementBuilder.BuildPlainElement(content.Substring(lhsStart)));
            return string.Join(",", elements);
        }
        //把图像类型的MsgMarker编译为MessageChain
        private static string CompileImageChainJson(string content)
        {
            if (!TryParsePrefix(content, out string key, out string value))
                throw new MsgMarkerException($"无法解析图像消息\"{content}\"");
            if (key != "url" && key != "base64")
                throw new MsgMarkerException($"不支持的图像来源类型\"{key}\"");
            return JsonHelper.MessageElementBuilder.BuildImageElement(key, value);
        }
        //把语音类型的MsgMarker编译为MessageChain
        private static string CompileVoiceChainJson(string content)
        {
            if (!TryParsePrefix(content, out string key, out string value))
                throw new MsgMarkerException($"无法解析音频消息\"{content}\"");
            if (key != "url" && key != "base64")
                throw new MsgMarkerException($"不支持的音频来源类型\"{key}\"");
            return JsonHelper.MessageElementBuilder.BuildVoiceElement(key, value);
        }
        //把文件类型的MsgMarker编译为MessageChain

        private static string CompileFileChainJson(string content)
        {
            if (!TryParsePrefix(content, out string key, out string value))
                throw new MsgMarkerException($"无法解析文件消息\"{content}\"");
            if (key != "url" && key != "path" && key != "id")
                throw new MsgMarkerException($"不支持的文件来源类型\"{key}\"");
            string element = JsonHelper.MessageElementBuilder.BuildFileElement(key, value);
            return element;
        }

        //把任意类型MsgMarker转换成相应的MessageChain
        internal static string CompileMessageChainJson(string content, bool alwaysPlain = false)  //锁定纯文本
        {
            if (alwaysPlain)
                return CompilePlainChainJson(content);
            if (!TryParsePrefix(content, out string type, out string body, startOnly: true))
            {
                //无prefix语法则默认当作纯文本
                type = "plain";
                body = content;
            }
            switch (type)
            {
                case "plain":
                    return CompilePlainChainJson(body);
                case "image":
                    return CompileImageChainJson(body);
                case "voice":
                    return CompileVoiceChainJson(body);
                case "file":
                    return CompileFileChainJson(body);
            }
            throw new MsgMarkerException($"不支持的消息类型\"{type}\"");
        }
    }
}
