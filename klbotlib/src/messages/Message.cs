using klbotlib.Json;
using System;
using System.Collections.Generic;

namespace klbotlib
{
    /// <summary>
    /// klbot内部使用的消息抽象类。所有QQ消息都继承此类
    /// </summary>
    public abstract class Message
    {
        private List<long> target_id = new List<long>();
        /// <summary>
        /// 发送者的ID（QQ号）。如果没有则为-1
        /// </summary>
        public long SenderID { get; internal set; } = -1;
        /// <summary>
        /// 此消息@的目标的ID列表（QQ号）。如果没有则长度为0。
        /// </summary>
        public IEnumerable<long> TargetID { get => target_id; }
        /// <summary>
        /// 此消息来源的群组的ID（群号）。如果消息来源是私聊则为-1；如果消息来源是群组则为群号；如果消息来源是临时会话则为“临时会话所通过的群”的群号。
        /// </summary>
        public long GroupID { get; internal set;  } = -1;
        /// <summary>
        /// 此消息的上下文。私聊=Private；临时会话=Temp；群聊=Group
        /// </summary>
        public MessageContext Context { get; internal set; }
        /// <summary>
        /// 返回此消息是否@了某个ID
        /// </summary>
        /// <param name="id">待判断ID</param>
        public bool TargetContains(long id) => target_id.Contains(id);
        internal static MessageEmpty Empty = new MessageEmpty();

        internal Message(long sender_id, long group_id)
        {
            SenderID = sender_id;
            GroupID = group_id;
        }
        internal void AddTargetID(params long[] ids)
        {
            target_id.AddRange(ids);
        }
        internal void AddTargetID(IEnumerable<long> ids)
        {
            target_id.AddRange(ids);
        }
        internal string BuildReplyMessageJson(string chain)
        {
            var context = Context;
            if (context == MessageContext.Group)
                return JsonHelper.MessageBuilder.BuildGroupMessageJson(GroupID, chain);
            else if (context == MessageContext.Private)
                return JsonHelper.MessageBuilder.BuildPrivateMessageJson(SenderID, chain);
            else if (context == MessageContext.Temp)
                return JsonHelper.MessageBuilder.BuildTempMessageJson(SenderID, GroupID, chain);
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
