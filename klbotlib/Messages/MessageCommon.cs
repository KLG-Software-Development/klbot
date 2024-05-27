namespace klbotlib
{
    /// <summary>
    /// 一般消息。由用户发送的消息均为此类型
    /// </summary>
    public abstract class MessageCommon : Message
    {
        /// <summary>
        /// 发送者的ID（QQ号）。如果没有则为-1
        /// </summary>
        public long SenderId { get; internal set; } = -1;

        internal MessageCommon(long senderId, long groupId)
        {
            SenderId = senderId;
            GroupId = groupId;
        }
        internal override void CopyReferenceTypeMember(Message dstMsg) { }
    }

    /// <summary>
    /// 消息上下文枚举，包括私聊、临时、群聊
    /// </summary>
    public enum MessageContext
    {
        /// <summary>
        /// 私聊
        /// </summary>
        Private,  
        /// <summary>
        /// 临时会话
        /// </summary>
        Temp, 
        /// <summary>
        /// 群组
        /// </summary>
        Group
    }
}
