namespace klbotlib;

/// <summary>
/// 禁言Message类
/// </summary>
public class MessageMute : Message
{
    private uint _durationSeconds;
    
    /// <summary>
    /// 该消息是否是解除禁言时间
    /// </summary>
    public bool IsUnmute { get; }
    /// <summary>
    /// 禁言操作者的ID
    /// </summary>
    public long OperatorId { get; }
    /// <summary>
    /// 被禁言者的ID
    /// </summary>
    public long MemberId { get; }
    /// <summary>
    /// 禁言时长。单位：秒。
    /// 若该消息为解除禁言消息，则此字段值为0。
    /// </summary>
    public uint DurationSeconds { get => IsUnmute? 0 : _durationSeconds; }
    /// <summary>
    /// 构造一条禁言事件消息
    /// </summary>
    /// <param name="isUnmute">该消息是否是解除禁言消息</param>
    /// <param name="groupId">所在群的ID</param>
    /// <param name="operatorId">禁言操作者ID</param>
    /// <param name="memberId">被禁言者的ID。若输入0则为自身</param>
    /// <param name="durationSeconds">禁言时长</param>
    public MessageMute(bool isUnmute, long groupId, long operatorId, long memberId, uint durationSeconds = 0)
    {
        IsUnmute= isUnmute;
        OperatorId = operatorId;
        MemberId = memberId;
        GroupId = groupId;
        _durationSeconds = durationSeconds;
    }
    /// <inheritdoc/>
    public override string ToString()
    {
        return $"{base.ToString()}\nOperator: {OperatorId}\nDuration: {DurationSeconds}s";
    }

    internal override void CopyReferenceTypeMember(Message dstMsg)
    {
        //此类型中无引用字段，忽略
    }
}
