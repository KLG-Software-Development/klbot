using System.Threading.Tasks;
using System.Linq;
using Gpt4All;
using System;
using System.Text;

namespace klbotlib.Modules;

///聊天bot模块
public class Gpt4AllModule : SingleTypeModule<MessagePlain>
{
    [ModuleSetup]
    private static string _modelPath = "ggml-alpaca-7b-q4.bin";
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
            if (token.EndsWith('\n') || token.EndsWith('.')) //每次换行或句号时发送消息
            {
                await Messaging.ReplyMessage(msg, sb.ToString());
                sb.Clear();
            }
        }
        return string.Empty;
    }
}
