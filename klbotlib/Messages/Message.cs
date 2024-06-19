using System.Diagnostics.CodeAnalysis;

namespace klbotlib;

/// <summary>
/// klbot内部使用的消息抽象类。所有QQ消息都继承此类
/// </summary>
public record Message
{
    /// <summary>
    /// 是否为复杂消息
    /// </summary>
    public bool IsComplex => this is MessagePackage msgPkg && msgPkg.Count > 0;
    /// <summary>
    /// 将消息包装为MessagePackage
    /// </summary>
    public MessagePackage Pack()
        => new(this);
    /// <inheritdoc/>
    [return: NotNullIfNotNull(nameof(text))]
    public static implicit operator Message?(string? text) => string.IsNullOrEmpty(text) ? null : new MessagePlain(text);
    /// <inheritdoc/>
    public static implicit operator Task<Message?>(Message? msg) => Task.FromResult(msg);
    /// <summary>
    /// 空消息
    /// </summary>
    public static readonly MessageEmpty Empty = new();
}
