using klbotlib.Json;
using klbotlib.Modules.ModuleUtils;
using System.Text.Json;
using System.Text.RegularExpressions;

namespace klbotlib.Modules;

///聊天bot模块
public partial class ChatQYKModule : SingleTypeModule<MessagePackage>
{
    private const string Url = "http://api.qingyunke.com/api.php?key=free&appid=0&msg=";
    private static readonly HttpHelper s_helper = new();

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
        string jreply = await s_helper.GetStringAsync(Url + msg.AsPlain());
        ChatterBotReply? reply = KLBotJsonHelper.DeserializeDefault<ChatterBotReply>(jreply);
        return reply?.FormattedContent();
    }

    private partial record ChatterBotReply
    {
        public int result = 0;
        public string content = string.Empty;
        public static Regex trashPat = TrashPattern();
        public static Regex facePat = FacePattern();
        public string FormattedContent() => trashPat.Replace(facePat.Replace(content.Replace("{br}", "\r\n"), ""), "");
        [GeneratedRegex(@"{r\+}", RegexOptions.Compiled)]
        private static partial Regex TrashPattern();
        [GeneratedRegex(@"{face:[\d]+}", RegexOptions.Compiled)]
        private static partial Regex FacePattern();
    }
}
