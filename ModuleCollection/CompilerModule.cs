using klbotlib.Modules.ModuleUtils;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace klbotlib.Modules;

/// <summary>
/// 编译模块
/// </summary>
public class CompilerModule : SingleTypeModule<MessagePlain>
{
    private readonly HttpHelper _httpHelper = new();
    private static readonly Dictionary<string, string> s_fileExts = new()
    {
            { "c", "c"},
            { "c++", "cpp"},
            { "cpp", "cpp"},
            { "c#", "cs"},
            { "java", "java"},
            { "cs", "cs"},
    };
    private static readonly HashSet<string> s_supportedLocalLanguages = [];
    private static readonly StringBuilder s_sb = new();
    private readonly string _onlineCommand = "$编译";
    private readonly string _localCommand = "$本地编译";

    [JsonInclude]
    private string? UrlA { get; set; }
    [JsonInclude]
    private string? Token { get; set; }

    ///<inheritdoc/>
    public override string FriendlyName => "编译模块";
    ///<inheritdoc/>
    public override string HelpInfo => $"使用“{_onlineCommand} [语言]”在线编译代码；\n使用 “{_localCommand} [语言]” 本地编译代码";
    ///<inheritdoc/>
    public override bool UseSignature => false;
    ///<inheritdoc/>
    public override async Task<Message?> Processor(MessageContext context, MessagePlain msg)
    {
        string text = msg.Text.TrimStart();
        return text.StartsWith(_onlineCommand) ? await CompileOnline(text) : msg.Text.StartsWith(_localCommand) ? await Compile(text) : null;
    }

    private Task<Message?> Compile(string text)
    {
        int ptr = 4;
        while (ptr < text.Length && text[ptr] != '\n')  //获取第一行行尾索引
            ptr++;
        if (ptr == text.Length) //到最后也没遇到 骂人
            return Task.FromResult<Message?>(ModuleAccess.GetModule<FuckModule>().SingleSentence());
        string language = text[_localCommand.Length..ptr].Trim().ToLower();
        return !s_supportedLocalLanguages.Contains(language)
            ? Task.FromResult<Message?>($"语言\"{language}\"暂时无法本地编译，改用\"{_onlineCommand} \"尝试在线编译")
            : throw new NotImplementedException();
    }

    private async Task<Message?> CompileOnline(string text)
    {
        int ptr = 4;
        while (ptr < text.Length && text[ptr] != '\n')  //获取第一行行尾索引
            ptr++;
        if (ptr == text.Length) //到最后也没遇到 骂人
            return ModuleAccess.GetModule<FuckModule>().SingleSentence();
        string language = text[_onlineCommand.Length..ptr].Trim().ToLower();
        _ = s_fileExts.TryGetValue(language, out string? fileExt);
        if (fileExt == null)
            return $"不支持语言\"{language}\"";
        string code = text[ptr..];
        if (UrlA == null)
        {
            string msg = "未加载有效API URL，无法继续执行，将退出";
            ModuleLog(msg);
            return msg;
        }
        string response = await _httpHelper.PostFormUrlEncodedAsync(UrlA, BuildPostBody(language, fileExt, code));
        ModuleLog($"Response: {response}");
        JReply? jreply = JsonSerializer.Deserialize<JReply>(response) ?? throw new JsonException("返回结果解析失败：产生了null结果");
        _ = s_sb.Clear();
        if (!string.IsNullOrWhiteSpace(jreply.Errors))
        {
            _ = s_sb.AppendLine("编译输出：");
            _ = s_sb.AppendLine(jreply.Errors.Replace("\\n", "\n"));
        }
        _ = s_sb.AppendLine("程序输出：");
        _ = s_sb.Append(string.IsNullOrWhiteSpace(jreply.Output) ? "[无输出]" : jreply.Output.Replace("\\n", "\n"));
        return s_sb.ToString();
    }

    private string BuildPostBody(string language, string fileExt, string code)
    {
        _ = s_sb.Clear();
        _ = s_sb.Append("code=");
        _ = s_sb.Append(code);
        _ = s_sb.Append("&token=");
        _ = s_sb.Append(Token);
        _ = s_sb.Append("&language=");
        _ = s_sb.Append(language);
        _ = s_sb.Append("&fileext=");
        _ = s_sb.Append(fileExt);
        return s_sb.ToString();
    }
    private record JReply(string? Output, string? Errors);
}
