using klbotlib.Json;
using System;
using System.Collections.Generic;

namespace klbotlib
{
    /// <summary>
    /// 一般消息。由用户发送的消息均为此类型
    /// </summary>
    public abstract class MessageCommon : Message
    {
        private readonly List<long> _targetId = new List<long>();
        /// <summary>
        /// 发送者的ID（QQ号）。如果没有则为-1
        /// </summary>
        public long SenderID { get; internal set; } = -1;
        /// <summary>
        /// 此消息@的目标的ID列表（QQ号）。如果没有则长度为0。
        /// </summary>
        public IEnumerable<long> TargetID { get => _targetId; }
        /// <summary>
        /// 返回此消息是否@了某个ID
        /// </summary>
        /// <param name="id">待判断ID</param>
        public bool TargetContains(long id) => _targetId.Contains(id);

        internal MessageCommon(long senderId, long groupId)
        {
            SenderID = senderId;
            GroupID = groupId;
        }
        internal void AddTargetID(params long[] ids)
        {
            _targetId.AddRange(ids);
        }
        internal void AddTargetID(IEnumerable<long> ids)
        {
            _targetId.AddRange(ids);
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
