using klbotlib.Modules.ModuleUtils;
using System;
using System.Linq;
using System.Text.Json;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace klbotlib.Modules;

///聊天bot模块
public class ChatQYKModule : SingleTypeModule<MessagePackage>
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
    public sealed override async Task<Message?> Processor(MessageContext context, MessagePackage msg)
    {
        if (msg.Count != 2 || !msg.ContainsTargetId(HostBot.SelfId))
            return null;
        string jreply = await _helper.GetStringAsync(_url + msg.AsPlain());
        return JsonSerializer.Deserialize<ChatterBotReply>(jreply).FormattedContent();
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
