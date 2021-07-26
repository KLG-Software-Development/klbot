using klbotlib.Internal;
using System;
using System.Web;

namespace klbotlib
{
    /// <summary>
    /// klbot内部使用的消息抽象类。所有QQ消息都继承此类
    /// </summary>
    public abstract class Message
    {
        /// <summary>
        /// 消息类型。文本消息="Plain"；空消息="Ignore"；其他类型的QQ消息暂时没有实现。
        /// </summary>
        public string Type { get; } = "Ignore";
        /// <summary>
        /// 发送者的ID（QQ号）。如果没有则为-1
        /// </summary>
        public long SenderID { get; set; } = -1;
        /// <summary>
        /// 此消息第一个@的目标的ID（QQ号）。如果没有则为-1。
        /// 这个字段未来会改成ID列表，不然没法支持处理同时@多个人的信息。
        /// </summary>
        public long TargetID { get; set; }
        /// <summary>
        /// 此消息来源的群组的ID（群号）。如果消息来源是私聊则为-1；如果消息来源是群组则为群号；如果消息来源是临时会话则为“临时会话所通过的群”的群号。
        /// </summary>
        public long GroupID { get; set;  } = -1;
        /// <summary>
        /// 此消息的上下文。私聊=Private；临时会话=Temp；群聊=Group
        /// </summary>
        public MessageContext Context { get; set; }
        internal static MessageEmpty Empty = new MessageEmpty();

        internal Message(string type, long sender_id = -1, long group_id = -1, long target_id = -1)
        {
            Type = type;
            SenderID = sender_id;
            GroupID = group_id;
            TargetID = target_id;
        }

        internal string BuildReplyPlainMessageBody(string text)
        {
            text = HttpUtility.JavaScriptStringEncode(text);
            var context = Context;
            if (context == MessageContext.Group || context == MessageContext.Private)
                return $"{{\"target\":\"{GroupID}\",\"messageChain\":[{{\"type\":\"Plain\", \"text\":\"{text}\" }}]}}";
            else if (context == MessageContext.Private)
                return $"{{\"target\":\"{SenderID}\",\"messageChain\":[{{\"type\":\"Plain\", \"text\":\"{text}\" }}]}}";
            else if (context == MessageContext.Temp)
                return $"{{\"qq\":\"{SenderID}\",\"group\":\"{GroupID}\",\"messageChain\":[{{\"type\":\"Plain\", \"text\":\"{text}\" }}]}}";
            else throw new Exception($"暂不支持的消息上下文类型 \"{context}\"");
        }

    }

    //消息工厂类。用来从JMessagePackage对象中生成相应的Message类型
    internal static class MessageFactory
    {
        internal static Message BuildMessage(JMessagePackage msg_package)
        {
            bool is_after_at = false;
            Message ret = null;
            //第一轮：判断主类型（文本、图片、文件...），从而生成对应的文本对象
            foreach (var msg in msg_package.messageChain)
            {
                switch (msg.type)
                {
                    case "At":
                        is_after_at = true;             //专门处理@后面无缘无故冒出来的傻逼空格
                        break;
                    case "Plain":
                        if (ret == null)    //运行至今未确定主类型，意味着这是第一个，所以构建相应的对象。其他情况逻辑相同。
                            ret = new MessagePlain(msg_package.sender.id, -1, msg.text.Trim());
                        else if (is_after_at)           //专门处理@后面无缘无故冒出来的傻逼空格
                            ((MessagePlain)ret).AppendText(msg.text.Substring(1));
                        else                //意味着之前还有别的Plain消息，则简单将文本追加到已有对象的文本中
                            ((MessagePlain)ret).AppendText(msg.text);
                        break;
                    //TODO:暂时还没处理文本以外的其他类型
                    default:
                        continue;
                }
            }
            if (ret == null)    //全程无法确定主类型，意味着本身为非关心的消息类型，所以直接返回忽略类型
                return Message.Empty;
            //第二轮：判断附加类型（At、AtAll、Quote...），并据此修改已有ret对象的属性
            foreach (var msg in msg_package.messageChain)
            {
                switch (msg.type)
                {
                    case "At":
                        ret.TargetID = msg.target;
                        break;
                    default:
                        continue;
                }
            }
            //最后：判断是私聊、群聊还是临时会话
            switch (msg_package.type)
            {
                case "FriendMessage":
                    ret.Context = MessageContext.Private;
                    break;
                case "TempMessage":
                    ret.Context = MessageContext.Temp;
                    ret.GroupID = msg_package.sender.group.id;
                    break;
                case "GroupMessage":
                    ret.Context = MessageContext.Group;
                    ret.GroupID = msg_package.sender.group.id;
                    break;
            }
            return ret;
        }
    }

    //忽略的Message类。显然其内部所有不需要处理的JMessage直接对象都会被构建成这个类型。bot在后续处理中会把这种类型的消息全部忽略
    internal class MessageEmpty : Message
    {
        public MessageEmpty() : base("ignore") { }
    }

    /// <summary>
    /// 纯文本消息类
    /// </summary>
    public class MessagePlain : Message
    {
        /// <summary>
        /// 此消息的文本内容
        /// </summary>
        public string Text { get; private set; }
        internal MessagePlain(long sender_id, long group_id, string text) : base("Plain", sender_id, group_id)
        {
            Text = text;
        }

        internal void AppendText(string text) => Text += text;
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
