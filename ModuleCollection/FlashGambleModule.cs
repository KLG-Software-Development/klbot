using System;
using System.Diagnostics;
using System.Linq;

namespace klbotlib.Modules;

/// <summary>
/// 闪照赌博模块
/// </summary>
public class FlashGambleModule : SingleTypeModule<MessageFlashImage>
{
    private readonly Random _ro = new();

    [ModuleStatus]
    private int _prob = 50;

    /// <inheritdoc/>
    public override string FriendlyName => "闪照赌博模块";
    /// <inheritdoc/>
    public override bool IsTransparent => true;
    /// <inheritdoc/>
    public override bool UseSignature => false;

    /// <inheritdoc/>
    public override string Filter(MessageFlashImage msg)
    {
        int i = _ro.Next(100);
        Debug.Print(i.ToString());
        if (i < _prob)
            return "yes";
        else
            return null;
    }
    /// <inheritdoc/>
    public override string Processor(MessageFlashImage msg, string filterOut)
    {
        return @"\image:\url:" + msg.UrlList.First();
    }
}
