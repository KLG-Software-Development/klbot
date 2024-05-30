using System.Linq;
using System.Threading.Tasks;

namespace klbotlib;

/// <summary>
/// klbot内部使用的消息抽象类。所有QQ消息都继承此类
/// </summary>
public record Message
{
    /// <summary>
    /// 是否为复杂消息
    /// </summary>
    public bool IsComplex => this is MessagePackage msgPkg ? msgPkg.Count > 0 : false;
    /// <summary>
    /// 将消息包装为MessagePackage
    /// </summary>
    public MessagePackage Pack()
        => new(this);
    /// <inheritdoc/>
    public static implicit operator Message(string? text) => string.IsNullOrEmpty(text) ? Message.Empty : new MessagePlain(text);
    /// <inheritdoc/>
    public static implicit operator Task<Message?>(Message? msg) => Task.FromResult(msg);
    /// <summary>
    /// 空消息
    /// </summary>
    public static MessageEmpty Empty = new MessageEmpty();

    /// <summary>
    /// 返回此消息是否@了某个ID
    /// </summary>
    /// <param name="id">待判断ID</param>
    public bool ContainsTargetId(long id)
    {
        if (this is MessagePackage pmsg)
            return pmsg.TargetIds.Contains(id);
        else if (this is MessageAt amsg)
            return amsg.TargetId == id;
        return false;
    }
}
