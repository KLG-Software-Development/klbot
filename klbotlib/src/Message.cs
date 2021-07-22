using klbotlib.Internal;

namespace klbotlib
{
    //klbot内部使用的消息抽象类
    public abstract class Message
    {
        public string Type { get; } = "Ignore";
        public long SenderID { get; set; } = -1;
        public long TargetID { get; set; } //消息At的对象 如果没有则为-1
        public long GroupID { get; set;  } = -1;
        public MessageContext Context { get; set; } //是否是临时消息
        public static MessageEmpty Empty = new MessageEmpty();


        public Message(string type, long sender_id = -1, long group_id = -1, long target_id = -1)
        {
            Type = type;
            SenderID = sender_id;
            GroupID = group_id;
            TargetID = target_id;
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
                    ///TODO:暂时还没处理文本以外的其他类型
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
    public class MessageEmpty : Message
    {
        public MessageEmpty() : base("ignore") { }
    }

    //纯文本消息类型
    public class MessagePlain : Message
    {
        public string Text { get; private set; }
        public MessagePlain(long sender_id, long group_id, string text) : base("Plain", sender_id, group_id)
        {
            Text = text;
        }

        public void AppendText(string text) => Text += text;
    }

    /// <summary>
    /// 消息上下文枚举，包括私聊、临时、群聊
    /// </summary>
    public enum MessageContext
    {
        Private,  Temp, Group
    }

}
