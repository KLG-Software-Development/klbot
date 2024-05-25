using klbotlib.Modules.ModuleUtils;
using Newtonsoft.Json;
using System;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace klbotlib.Modules;

///聊天bot模块
public class ChatQYKModule : SingleTypeModule<MessagePlain>
{
    private const string _url = "http://api.qingyunke.com/api.php?key=free&appid=0&msg=";
    private static readonly HttpHelper _helper = new();

    /// <inheritdoc/>
    public sealed override bool IsTransparent => false;
    /// <inheritdoc/>
    public sealed override bool UseSignature => false;
    /// <inheritdoc/>
    public sealed override bool IsAsync => false;
    /// <inheritdoc/>
    public sealed override string FriendlyName => "聊天模块";
    /// <inheritdoc/>
    public sealed override string? Filter(MessagePlain msg) => msg.TargetID.Contains(HostBot.SelfId) ? "ok" : null;
    /// <inheritdoc/>
    public sealed override async Task<string> Processor(MessagePlain msg, string? _)
    {
        string jreply = await _helper.GetStringAsync(_url + msg.Text);
        return JsonConvert.DeserializeObject<ChatterBotReply>(jreply).FormattedContent();
    }

    private class ChatterBotReply
    {
        public int result;
        public string content = string.Empty;
        public static Regex trashPat = new(@"{r\+}", RegexOptions.Compiled);
        public static Regex facePat = new(@"{face:[\d]+}", RegexOptions.Compiled);
        public string FormattedContent() => trashPat.Replace(facePat.Replace(content.Replace("{br}", "\r\n"), ""), "");
    }
}
