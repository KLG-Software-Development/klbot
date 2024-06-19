using System.Collections;
using System.Text;

namespace klbotlib;

// 忽略的Message类。显然其内部所有不需要处理的JMessage直接对象都会被构建成这个类型。bot在后续处理中会把这种类型的消息全部忽略
/// <summary>
/// 一条空消息
/// </summary>
public record MessageEmpty : Message { internal MessageEmpty() { } }
/// <summary>
/// 纯文本消息类
/// </summary>
public record MessagePlain(string Text) : Message;
/// <summary>
/// @消息
/// </summary>
public record MessageAt(long TargetId) : Message;
/// <summary>
/// 表情消息
/// </summary>
public record MessageFace(string FaceId) : Message;
/// <summary>
/// 撤回消息
/// </summary>
/// <param name="AuthorId">消息作者ID</param>
/// <param name="MessageId">消息ID</param>
public record MessageRecall(long AuthorId, long MessageId) : Message;
/// <summary>
/// 禁言/解除禁言消息
/// </summary>
/// <param name="Unmute">是否为解除禁言</param>
/// <param name="MemberId">目标群成员ID</param>
/// <param name="DurationSeconds">持续时间。若为解除禁言则此字段无意义</param>
public record MessageMute(bool Unmute, long MemberId, uint DurationSeconds = 0) : Message;
/// <summary>
/// 语音消息
/// </summary>
/// <param name="Url">语音的URL</param>
public record MessageVoice(string Url) : Message;
/// <summary>
/// 图像消息类
/// </summary>
public record MessageImage(string Url, bool IsFlashImage) : Message;
/// <summary>
/// 消息包类. 由若干Message与相应收发相关信息组成
/// </summary>
public record MessagePackage : Message, IReadOnlyList<Message>
{
    private readonly List<Message> _data;
    private readonly List<long> _targetIds = [];

    /// <summary>
    /// @ID列表
    /// </summary>
    public IReadOnlyList<long> TargetIds { get; }

    /// <summary>
    /// 构造一条消息包
    /// </summary>
    public MessagePackage(string plain)
    {
        _data = [new MessagePlain(plain)];
        TargetIds = _targetIds;
    }
    /// <summary>
    /// 构造一条消息包
    /// </summary>
    public MessagePackage(params Message[] msgs)
    {
        _data = [];
        _data.AddRange(msgs);
        TargetIds = _targetIds;
        foreach (var msg in msgs)
        {
            if (msg is MessageAt amsg)
                _targetIds.Add(amsg.TargetId);
        }
    }
    /// <summary>
    /// 构造一条消息包
    /// </summary>
    public MessagePackage(IEnumerable<Message> msgs)
    {
        _data = [];
        TargetIds = _targetIds;
        foreach (var msg in msgs)
        {
            _data.Add(msg);
            if (msg is MessageAt amsg)
                _targetIds.Add(amsg.TargetId);
        }
    }
    /// <summary>
    /// 串联若干个消息包
    /// </summary>
    public static MessagePackage Join(params Message[] msgs)
    {
        List<Message> allMsgs = [];
        foreach (var msg in msgs)
        {
            if (msg is MessagePackage msgPkg)
                allMsgs.AddRange(msgPkg);
            else
                allMsgs.Add(msg);
        }
        return new MessagePackage(allMsgs);
    }

    /// <inheritdoc/>
    public override string ToString()
    {
        StringBuilder sb = new("Message package: ");
        foreach (var msg in _data)
            _ = sb.AppendLine(msg.ToString());
        return sb.ToString();
    }

    /// <summary>
    /// 尝试降解MessagePackage。若MessagePackage有唯一元素则返回该消息，否则降解失败返回自身
    /// </summary>
    /// <returns>消息包内唯一消息的，或消息包自身</returns>
    public Message Collapse()
        => _data.Count == 1 ? _data[0] : this;

    /// <summary>
    /// 添加@目标
    /// </summary>
    /// <param name="id">@的目标ID</param>
    public void AddTargetId(long id)
    {
        _data.Add(new MessageAt(id));
    }
    /// <summary>
    /// 按顺序提取消息包中的纯文本并拼接
    /// </summary>
    public string AsPlain()
    {
        StringBuilder sb = new();
        foreach (var msg in _data)
        {
            if (msg is MessagePlain pmsg)
                _ = sb.Append(pmsg.Text);
        }
        return sb.ToString();
    }

    // --- 以下为接口实现 ---

    /// <inheritdoc/>
    public int Count => _data.Count;
    /// <inheritdoc/>
    public Message this[int index] => _data[index];
    /// <inheritdoc/>
    public IEnumerator<Message> GetEnumerator()
        => _data.GetEnumerator();
    IEnumerator IEnumerable.GetEnumerator()
        => _data.GetEnumerator();
}
