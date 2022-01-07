using Gleee.Consoleee;
using klbotlib.Extensions;
using klbotlib.Modules.CommandModuleNamespace;
using System;
using System.Collections.Generic;
using System.Reflection;
using System.Text.RegularExpressions;

namespace klbotlib.Modules
{
    //命令模块
    internal class CommandModule : SingleTypeModule<MessagePlain>
    {
        const string _prefix = "##";
        private readonly Regex _cmdPat = new($@"^{_prefix}(.+)$", RegexOptions.Compiled);

        [ModuleSetup]
        public Dictionary<long, AuthorType> Users { get; private set; } = new();

        internal List<Command> _cmds = new();
        internal AuthorType GetAuthorType(long id)
        {
            if (!Users.ContainsKey(id))
                return AuthorType.野人;
            else
                return Users[id];
        }

        public CommandModule(KLBot hostBot, params Command[] cmds)
        {
            hostBot.ObjectPrint(hostBot, "正在加载命令...", ConsoleMessageType.Task);
            //自动实例化并添加所有已经定义的、带有[DefaultCommand]标记的Command类
            var types = Assembly.GetExecutingAssembly().GetTypes();
            foreach (var type in types)
            {
                if (type.Namespace == "klbotlib.Modules.CommandModuleNamespace.Commands" 
                    && type.GetRootBaseType() == typeof(Command) 
                    && Attribute.GetCustomAttribute(type, typeof(DefaultCommandAttribute)) != null)
                {
                    var constructors = type.GetConstructors();
                    if (constructors.Length > 0)
                        _cmds.Add((Command)constructors[0].Invoke(Array.Empty<object>()));
                }
            }
            int defaultCmdCount = _cmds.Count;
            _cmds.AddRange(cmds);
            hostBot.ObjectPrint(hostBot, $"成功加载{_cmds.Count}条命令（{defaultCmdCount}条默认命令, {_cmds.Count - defaultCmdCount}条自定义命令).");
        }

        public sealed override bool IsTransparent => false;
        public sealed override string FriendlyName => "命令模块";
        public sealed override string HelpInfo => $"发送“{_prefix}[命令]”执行指定命令。可以用“##help”查看已载入命令列表";
        public sealed override string Filter(MessagePlain msg) => _cmdPat.IsMatch(msg.Text.Trim()) ? "ok" : null;
        public sealed override string Processor(MessagePlain msg, string _)
        {
            string cmdStr = _cmdPat.Match(msg.Text).Groups[1].Value.ToLower();
            //遍历命令模块中的命令列表，寻找第一个匹配
            foreach (var cmd in _cmds)
            {
                if (cmd.IsCmd(cmdStr))
                    return cmd.Run(HostBot, msg, cmdStr);
            }
            return $"错误：未知命令'{cmdStr}'";
        }
    }
}

namespace klbotlib.Modules.CommandModuleNamespace
{
    internal enum AuthorType { 野人, 普通用户, 管理员, 开发者 }
    [AttributeUsage(AttributeTargets.Class)]
    internal class DefaultCommandAttribute : Attribute { }
    internal abstract class Command
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
        /// <param name="cmdMsg">命令的原始文本消息对象</param>
        /// <param name="cmdStr">命令的文本(不含前缀)</param>
        /// <returns></returns>
        public abstract string CommandTask(KLBot bot, MessagePlain cmdMsg, string cmdStr);

        public string Run(KLBot bot, MessagePlain msg, string cmd)  //目前类型限定为文本消息, 因为暂时没看到其它形式的命令的可能性
        {
            try
            {
                AuthorType authority = bot.GetModule<CommandModule>().GetAuthorType(msg.SenderID);
                if (authority < AuthorityRequirment)
                    return $"错误：拒绝访问。\n调用者权限级别：{authority}\n命令权限级别：{AuthorityRequirment}";
                else
                    return CommandTask(bot, msg, cmd);
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
    internal abstract class Command<T>
    {
        public virtual AuthorType AuthorityRequirment { get; } = AuthorType.管理员;
        public abstract bool IsCmd(string cmd);
        public abstract string Format { get; }
        public abstract string Usage { get; }
        public abstract string Task(KLBot host, T targetObject, MessagePlain cmdMsg, string cmdStr);

        public string Run(KLBot bot, T targetObject, MessagePlain msg, string cmd)  //目前类型限定为文本消息, 因为暂时没看到其它形式的命令的可能性
        {
            AuthorType authority = bot.GetModule<CommandModule>().GetAuthorType(msg.SenderID);
            if (authority < AuthorityRequirment)
                return $"错误：拒绝访问。\r\n调用者权限级别：{authority}\r\n命令权限级别：{AuthorityRequirment}";
            else
                return Task(bot, targetObject, msg, cmd);
        }
    }
}

