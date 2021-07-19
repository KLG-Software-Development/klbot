using System;

namespace klbotlib.Modules.CommandModule
{
    public abstract class Command
    {
        public virtual AuthorType AuthorityRequirment { get; } = AuthorType.管理员;
        /// <summary>
        /// 判断特定字符串是否在调用此命令
        /// </summary>
        /// <param name="cmd">待判断字符串</param>
        /// <returns></returns>
        public abstract bool IsCmd(string cmd);
        /// <summary>
        /// 命令的使用格式, 类似'pti [time]'
        /// </summary>
        public abstract string Format { get; }
        /// <summary>
        /// 命令的作用
        /// </summary>
        public abstract string Usage { get; }
        /// <summary>
        /// 执行命令的操作
        /// </summary>
        /// <param name="bot">需要执行的机器人</param>
        /// <param name="cmd_msg">命令的原始文本消息对象</param>
        /// <param name="cmd_str">命令的文本(不含前缀)</param>
        /// <returns></returns>
        public abstract string Task(KLBot bot, MessagePlain cmd_msg, string cmd_str);

        public string Run(KLBot bot, MessagePlain msg, string cmd)  //目前类型限定为文本消息, 因为暂时没看到其它形式的命令的可能性
        {
            try
            {
                AuthorType authority = bot.CommandModule.GetAuthorType(msg.SenderID);
                if (authority < AuthorityRequirment)
                    return $"错误：拒绝访问。\n调用者权限级别：{authority}\n命令权限级别：{AuthorityRequirment}";
                else
                    return Task(bot, msg, cmd);
            }
            catch (Exception ex)
            {
                return $"命令执行出现异常：{ex.Message}\n调用栈：\n{ex.StackTrace}\n\n请联系开发者检查该命令的实现";
            }
        }
    }
    /// <summary>
    /// Command类型的泛型版本. 用于命令的使能对象不是/不限于KLBot的情况(因为没用用到，这个类暂时处于零维护零测试状态)
    /// </summary>
    /// <typeparam name="T">执行指令的对象类型</typeparam>
    public abstract class Command<T>
    {
        public virtual AuthorType AuthorityRequirment { get; } = AuthorType.管理员;
        /// <summary>
        /// 判断特定字符串是否在调用此命令
        /// </summary>
        /// <param name="cmd">待判断字符串</param>
        /// <returns></returns>
        public abstract bool IsCmd(string cmd);
        /// <summary>
        /// 命令的使用格式, 类似'pti [time]'
        /// </summary>
        public abstract string Format { get; }
        /// <summary>
        /// 命令的作用
        /// </summary>
        public abstract string Usage { get; }
        /// <summary>
        /// 执行命令的操作
        /// </summary>
        /// <param name="host">命令执行的对象</param>
        /// <param name="cmd_msg">命令的原始文本消息对象</param>
        /// <param name="cmd_str">命令的文本(不含前缀)</param>
        /// <returns></returns>
        public abstract string Task(KLBot bot, T target_object, MessagePlain cmd_msg, string cmd_str);

        public string Run(KLBot bot, T target_object, MessagePlain msg, string cmd)  //目前类型限定为文本消息, 因为暂时没看到其它形式的命令的可能性
        {
            AuthorType authority = bot.CommandModule.GetAuthorType(msg.SenderID);
            if (authority < AuthorityRequirment)
                return $"错误：拒绝访问。\r\n调用者权限级别：{authority}\r\n命令权限级别：{AuthorityRequirment}";
            else
                return Task(bot, target_object, msg, cmd);
        }
    }
}
