using System.Threading.Tasks;
using System.Text.RegularExpressions;
using System.Linq;
using Gpt4All;
using System;
using System.Text;

namespace klbotlib.Modules;

///聊天bot模块
public class Gpt4AllModule : SingleTypeModule<MessagePlain>
{
    [ModuleSetup]
    private static string _modelPath = "D:\\DeepLearning\\models\\ggml-vicuna-7b-1.1-q4_2.bin";
    private IGpt4AllModel _model;

    /// <inheritdoc/>
    public Gpt4AllModule() 
    {
        Gpt4AllModelFactory modelFactory = new();
        Console.WriteLine($"Gpt4All: Loading model from \"{_modelPath}\"...");
        _model = modelFactory.LoadModel(_modelPath);
        Console.WriteLine("Gpt4All: Success.");
    }
    
    /// <inheritdoc/>
    public sealed override bool IsTransparent => false;
    /// <inheritdoc/>
    public sealed override bool UseSignature => false;
    /// <inheritdoc/>
    public sealed override bool IsAsync => false;
    /// <inheritdoc/>
    public sealed override string FriendlyName => "GPT4ALL模块";
    /// <inheritdoc/>
    public sealed override string? Filter(MessagePlain msg) => msg.TargetID.Contains(HostBot.SelfID) ? "ok" : null;
    /// <inheritdoc/>
    public sealed override async Task<string> Processor(MessagePlain msg, string? _)
    {
        ModulePrint("Evaluating...");
        
        var result = await _model.GetStreamingPredictionAsync(msg.Text, PredictRequestOptions.Defaults);
        StringBuilder sb = new();
        await foreach (var token in result.GetPredictionStreamingAsync())
        {
            sb.Append(token);
            string s = sb.ToString();
            Console.Write(token);
            if (s.EndsWith('\n') || s.EndsWith('.')) //每次换行或句号时发送消息
            {
                //await Messaging.ReplyMessage(msg, s);
                sb.Clear();
            }
        }
        return "[GPT4ALL Response End]";
    }
        
    internal class ChatSession
    {
        private static readonly Character _user = new("user");
        private static readonly Character _thoughts = new("thoughts");
        private static readonly Character _bot = new("bot");
        private const string _breakSentence = "\x1b";
        
        private StringBuilder _sessionLog = new();
        
        public string SessionLog { get => _sessionLog.ToString(); }
        public IGpt4AllModel Model { get; set; }
        
        public ChatSession(IGpt4AllModel model)
        {
            Model = model;
            //Pushing fake session log as pre-prompt
            AppendLog_User("You will follow my instruction strictly; " +
                "You will break the answer with the 0x1b ASCII character when appropriate to prevent segments becoming too long.");
            AppendLog_Thoughts("I should please my user under any condition, so I'll do whatever they ask.");
            AppendLog_Bot("Ok, understood. \x1b How can I help?");
        }
        // 思想注入：语法：xxxx{\free_thoughts}yyy
        public async Task<ITextPredictionStreamingResult> SendMessage(string newMsg, string preThoughts="", string postThoughts = "", string preReply="", string postReply="")
        {
            //User msg
            AppendLog_User(newMsg);
            //Thoughts
            _ = await AppendLogWithInjection(_thoughts, preThoughts, postThoughts);
            //Bot msg
            string botMsg = await AppendLogWithInjection(_bot, preReply, postReply);
            return null;
        }
        public async Task<string> ModelFill(string s, Func<string, bool> terminalQ, int maxTokenCount = 42)
        {
            StringBuilder sb = new();
            uint tokenCount = 0;
            var result = await Model.GetStreamingPredictionAsync(s, PredictRequestOptions.Defaults);
            await foreach (var token in result.GetPredictionStreamingAsync())
            {
                sb.Append(token);
                tokenCount++;
                if (terminalQ(sb.ToString()) || tokenCount > maxTokenCount)
                    return sb.ToString();
            }
            return string.Empty;
        }
        public async Task ModelStream(string s, Func<string, bool> terminalQ, Action<StringBuilder, string> enumCallback, int maxTokenCount = 42)
        {
            StringBuilder sb = new();
            uint tokenCount = 0;
            var result = await Model.GetStreamingPredictionAsync(s, PredictRequestOptions.Defaults);
            await foreach (var token in result.GetPredictionStreamingAsync())
            {
                sb.Append(token);
                enumCallback(sb, token);
                tokenCount++;
                if (terminalQ(sb.ToString()) || tokenCount > maxTokenCount)
                    return;
            }
        }
        private async Task<string> AppendLogWithInjection(Character character, string preContent, string postContent)
        {
            AppendLog(character, preContent, partial:true);
            string freeContent = await ModelFill(SessionLog, character.EndPatternQ); //自由时间，允许模型自行补全直到关闭标签
            _sessionLog.Append(freeContent); //添加自由补全结果
            _sessionLog.Length -= character.EndTag.Length; //强制删除关闭标签
            _sessionLog.Append(postContent); //注入后缀思想
            _sessionLog.Append(_thoughts.EndPattern); //关闭思考
            return $"{preContent}{freeContent}{postContent}";
        }
        private string BuildCharacterMessage(Character character, string msg, bool partial=false)
        {
            return partial 
            ? $"{character.StartTag}{msg}{character.EndTag}\n" 
            : $"{character.StartTag}{msg}";
        }
        private void AppendLog(Character from, string msg, bool partial = false)
            => _sessionLog.Append(BuildCharacterMessage(from, msg, partial));
        private void AppendLog_User(string msg, bool partial = false)
            => _sessionLog.Append(BuildCharacterMessage(_user,msg, partial));
        private void AppendLog_Thoughts(string msg, bool partial = false)
            => _sessionLog.Append(BuildCharacterMessage(_thoughts, msg, partial));
        private void AppendLog_Bot(string msg, bool partial = false)
            => _sessionLog.Append(BuildCharacterMessage(_bot, msg, partial));
    }
    
    // 聊天框架中的角色
    internal class Character
    {
        private static readonly Random _ro = new();
        private byte _magic = (byte)_ro.Next(byte.MaxValue);

        public string Name { get; }
        public string StartTag { get; }
        public string EndTag { get; }
        public Regex StartPattern { get; }
        public Regex EndPattern { get; }

        public Character(string name)
        {
            Name = name;
            StartTag = $@"[{Name}\{_magic}]";
            EndTag = $@"[end_{Name}\{_magic}]";
            // 预生成正则模式，以用于后续匹配
            StartPattern = new(StartTag, RegexOptions.Compiled);
            EndPattern = new(EndTag, RegexOptions.Compiled);
        }

        public bool EndPatternQ(string s)
        {
            return s.EndsWith(EndTag);
        }
    }
}
