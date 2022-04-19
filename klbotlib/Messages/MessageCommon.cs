using klbotlib.Json;
using System;
using System.Collections.Generic;
using System.Text;

namespace klbotlib
{
    /// <summary>
    /// 一般消息。由用户发送的消息均为此类型
    /// </summary>
    public abstract class MessageCommon : Message
    {
        private HashSet<long> _targetHashSet = new();
        private List<long> _targetList = new();

        /// <summary>
        /// 发送者的ID（QQ号）。如果没有则为-1
        /// </summary>
        public long SenderID { get; internal set; } = -1;
        /// <summary>
        /// 此消息@的目标的ID列表（QQ号）。如果没有则长度为0。
        /// </summary>
        public IReadOnlyList<long> TargetID 
        { 
            get => _targetList;
            internal set
            {
                _targetList = (List<long>)value;
                _targetHashSet = new();
                foreach (long id in _targetList)
                {
                    _targetHashSet.Add(id);
                }
            }
        }
        /// <summary>
        /// 返回此消息是否@了某个ID
        /// </summary>
        /// <param name="id">待判断ID</param>
        public bool ContainsTargetID(long id) => _targetHashSet.Contains(id);
        /// <summary>
        /// 添加目标@ID
        /// </summary>
        /// <param name="id">目标ID</param>
        public void AddTargetID(long id)
        {
            _targetHashSet.Add(id);
            _targetList.Add(id);
        }
        /// <summary>
        /// 添加多个目标@ID
        /// </summary>
        /// <param name="ids">目标ID集合</param>
        public void AddTargetID(IEnumerable<long> ids)
        {
            foreach (var id in ids)
            {
                _targetHashSet.Add(id);
            }
            _targetList.AddRange(ids);
        }
        /// <summary>
        /// 清空目标@ID列表
        /// </summary>
        public void ClearTargetID()
        {
            _targetList.Clear();
            _targetHashSet.Clear();
        }
        /// <inheritdoc/>
        public override string ToString()
        {
            StringBuilder sb = new();
            sb.AppendLine(base.ToString());
            sb.AppendFormat("From: {0}\n", SenderID);
            int targetIndex = 0;
            foreach (var targetID in TargetID)
            {
                sb.AppendFormat("Target[{0}]: {1}\n", targetIndex, targetID);
                targetIndex++;
            }
            sb.Length -= 1; //删除多余的\n
            return sb.ToString();
        }

        internal MessageCommon(long senderId, long groupId)
        {
            SenderID = senderId;
            GroupID = groupId;
        }
        internal string BuildReplyMessageJson(string chain)
        {
            var context = Context;
            if (context == MessageContext.Group)
                return JsonHelper.MessageJsonBuilder.BuildGroupMessageJson(GroupID, chain);
            else if (context == MessageContext.Private)
                return JsonHelper.MessageJsonBuilder.BuildPrivateMessageJson(SenderID, chain);
            else if (context == MessageContext.Temp)
                return JsonHelper.MessageJsonBuilder.BuildTempMessageJson(SenderID, GroupID, chain);
            else throw new Exception($"暂不支持的消息上下文类型 \"{context}\"");
        }
        internal override void CopyReferenceTypeMember(Message dstMsg)
        {
            var dst = dstMsg as MessageCommon;
            dst.TargetID = new List<long>();
            foreach (var id in TargetID)
            {
                dst.AddTargetID(id);
            }
        }
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
