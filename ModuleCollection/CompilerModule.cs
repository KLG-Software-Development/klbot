﻿using klbotlib;
using klbotlib.Modules;
using klbotlib.Modules.ModuleUtils;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Text;

namespace ModuleCollection
{
    public class CompilerModule : SingleTypeModule<MessagePlain>
    {
        private readonly HttpHelper _httpHelper = new HttpHelper();
        private static readonly Dictionary<string, string> _fileExts = new Dictionary<string, string>
        {
            { "c", "c"},
            { "c++", "cpp"},
            { "cpp", "cpp"},
            { "c#", "cs"},
            { "java", "java"},
            { "cs", "cs"},
        };
        private static readonly HashSet<string> _supportedLocalLanguages = new HashSet<string> { };
        private static readonly StringBuilder _sb = new StringBuilder();
        private readonly string _onlineCommand = "$编译";
        private readonly string _localCommand = "$本地编译";

        [ModuleSetup]
        private readonly string _urlA = "https://tool.runoob.com/compile2.php";
        [ModuleSetup]
        private readonly string _token = "4381fe197827ec87cbac9552f14ec62a";

        public override string FriendlyName => "编译模块";
        public override string HelpInfo => $"使用“{_onlineCommand} [语言]”在线编译代码；\n使用 “{_localCommand} [语言]” 本地编译代码";
        public override bool UseSignature => false;
        public override string Filter(MessagePlain msg)
        {
            return msg.Text.StartsWith(_onlineCommand) ? "compile_ol" : msg.Text.StartsWith(_localCommand) ? "compile" : null;
        }
        public override string Processor(MessagePlain msg, string filter_out)
        {
            string text = msg.Text.TrimStart();
            int ptr = 4;
            while (ptr < text.Length && text[ptr] != '\n')  //获取第一行行尾索引
                ptr++;
            if (ptr == text.Length) //到最后也没遇到 骂人
                return ModuleAccess.GetModule<FuckModule>().SingleSentence();
            string code = text.Substring(ptr);
            switch (filter_out)
            {
                case "compile_ol":
                    string language = text.Substring(_onlineCommand.Length, ptr - _onlineCommand.Length).Trim().ToLower();
                    _fileExts.TryGetValue(language, out string fileExt);
                    if (fileExt == null)
                        return $"不支持语言\"{language}\"";
                    string response = _httpHelper.PostString(_urlA, BuildPostBody(language, fileExt, code));
                    JReply jreply = JsonConvert.DeserializeObject<JReply>(response);
                    _sb.Clear();
                    if (!string.IsNullOrWhiteSpace(jreply.errors))
                    {
                        _sb.AppendLine("编译输出：");
                        _sb.AppendLine(jreply.errors.Replace("\\n", "\n"));
                    }
                    _sb.AppendLine("程序输出：");
                    _sb.Append(string.IsNullOrWhiteSpace(jreply.output) ? "[无输出]" : jreply.output.Replace("\\n", "\n"));
                    return _sb.ToString();
                case "compile":
                    language = text.Substring(_localCommand.Length, ptr - _localCommand.Length).Trim().ToLower();
                    if (!_supportedLocalLanguages.Contains(language))
                        return $"语言\"{language}\"暂时无法本地编译，改用\"{_onlineCommand} \"尝试在线编译";
                    throw new NotImplementedException();
                default:
                    throw new Exception($"意外遭遇未知过滤器输出\"{filter_out}\"。添加该输出的对应处理");
            }
        }

        private string BuildPostBody(string language, string fileExt, string code)
        {
            _sb.Clear();
            _sb.Append("code=");
            _sb.Append(Uri.EscapeDataString(code));
            _sb.Append("&token=");
            _sb.Append(_token);
            _sb.Append("&language=");
            _sb.Append(language);
            _sb.Append("&fileext=");
            _sb.Append(fileExt);
            return _sb.ToString();
        }

        private class JReply { public string output; public string errors; }
    }
}