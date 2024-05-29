namespace klbotlib
{
    /// <summary>
    /// 表情消息
    /// </summary>
    public class MessageFace(string id)
    {
        /// <summary>
        /// 表情ID
        /// </summary>
        public string Id { get; set; } = id;

        internal override void CopyReferenceTypeMember(Message dstMsg)
        {
            ((MessageFace)dstMsg).Id = Id;
        }
    }
}
