namespace klbotlib
{
    /// <summary>
    /// @消息
    /// </summary>
    public class MessageAt(long targetId) : Message
    {
        /// <summary>
        /// @目标的ID
        /// </summary>
        public long TargetId { get; set; } = targetId;

        internal override void CopyReferenceTypeMember(Message dstMsg) { }
    }
}
