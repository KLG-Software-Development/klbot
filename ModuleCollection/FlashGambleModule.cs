using System;
using System.Linq;

namespace klbotlib.Modules;

/// <summary>
/// 闪照赌博模块
/// </summary>
public class FlashGambleModule : Module
{
    private readonly Random _ro = new();

    [ModuleStatus]
    private int _prob = 50;

    /// <inheritdoc/>
    public override string FriendlyName => "犯贱赌博模块";
    /// <inheritdoc/>
    public override bool IsTransparent => true;
    /// <inheritdoc/>
    public override bool UseSignature => false;

    /// <inheritdoc/>
    public override string Filter(Message msg)
    {
        int i = _ro.Next(100);
        if (i < _prob)
        {
            if (msg is MessageRecall)
                return "recall";
            else if (msg is MessageFlashImage)
                return "flash";
            else
                return null;
        }
        else
            return null;
    }
    /// <inheritdoc/>
    public override string Processor(Message msg, string filterOut)
    {
        return null;
    }
}
