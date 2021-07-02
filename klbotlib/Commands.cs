using System.Text.RegularExpressions;

///所有命令都在本文件中定义
namespace klbotlib.Modules.Commands
{
    public abstract class Command
    {
        AuthorType AuthorityRequirment { get; set; }
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

        public Command(AuthorType type = AuthorType.管理员)
        {
            AuthorityRequirment = type;
        }
        public string Run(KLBot bot, MessagePlain msg, string cmd)  //目前类型限定为文本消息, 因为暂时没看到其它形式的命令的可能性
        {
            AuthorType authority = bot.CommandModule.GetAuthorType(msg.SenderID);
            if (authority < AuthorityRequirment)
                return $"错误：拒绝访问。\r\n调用者权限级别：{authority}\r\n命令权限级别：{AuthorityRequirment}";
            else
                return Task(bot, msg, cmd);
        }
    }
    public enum AuthorType { 野人, 普通用户, 管理员 }

    //方便实现开关型命令的基类
    public abstract class SwitchCommand : Command
    {
        public abstract string SwitchName { get; }
        public abstract bool GetBotProperty(KLBot bot);
        public abstract void SetBotProperty(KLBot bot, bool value);
        public override string Usage => "打开或关闭" + SwitchName;
        public override string Task(KLBot bot, MessagePlain _, string __)
        {
            bool current_state = GetBotProperty(bot);
            SetBotProperty(bot, !current_state);
            string action = current_state ? "关闭" : "打开";
            return $"[命令模块]{SwitchName}已{action}";
        }
        public override bool IsCmd(string cmd) => cmd == Format;
    }
    //方便实现赋值型命令的基类
    public abstract class AssignmentCommand<T> : Command
    {
        public abstract string PropertyName { get; }
        public abstract T GetBotProperty(KLBot bot);
        public abstract void SetBotProperty(KLBot bot, T value);
        public abstract bool TryParseCmdStringValue(string cmd, out T val);
        public override string Usage => "设置" + PropertyName;
        public override string Task(KLBot bot, MessagePlain msg, string cmd)
        {
            T current_value = GetBotProperty(bot);
            bool result = TryParseCmdStringValue(cmd, out T val);
            if (result)
            {
                SetBotProperty(bot, val);
                return $"[命令模块]{PropertyName}已修改：\r\n旧值：{current_value}\r\n新值：{val}";
            }
            else
                return $"[命令模块]修改{PropertyName}失败：无法从'{cmd}'中解析出合法的值";
        }
    }

    public class HelpCmd : Command
    {
        public override string Format => "help";
        public override string Usage => "打印可用命令和帮助";
        public override bool IsCmd(string cmd) => cmd == "help";
        public override string Task(KLBot bot, MessagePlain msg, string cmd_str)
        {
            string re = "命令列表: \r\n";
            foreach (Command cmd in bot.CommandModule.Cmds)
            {
                re += $"{cmd.Format} {cmd.Usage.ToString().PadLeft(6, '-')}\r\n";
            }
            return re + "\r\n发送'##[命令]'以执行指令\r\n";
        }
    }
    public class TagMeCmd : SwitchCommand
    {
        public override string SwitchName => "TagMe模式";
        public override string Format => "tag-me";
        public override bool GetBotProperty(KLBot bot) => bot.IsTagMe;
        public override void SetBotProperty(KLBot bot, bool value) => bot.IsTagMe = value;
    }
    public class PtiCmd : AssignmentCommand<int>
    {
        readonly Regex sleepPat = new Regex(@"^pti (.+)$");
        public override string PropertyName => "轮询时间间隔";
        public override string Format => "pti [时间(毫秒)]";
        public override int GetBotProperty(KLBot bot) => bot.PollingTimeInterval;
        public override void SetBotProperty(KLBot bot, int value) => bot.PollingTimeInterval = value;
        public override bool IsCmd(string cmd) => sleepPat.IsMatch(cmd);
        public override bool TryParseCmdStringValue(string cmd, out int val)
        {
            string val_str = sleepPat.Match(cmd).Groups[1].Value;
            bool result = int.TryParse(val_str, out val);
            return result;
        }
    }
    public class ShutdownCmd : SwitchCommand
    {
        public override string SwitchName => "程序总开关";
        public override string Format => "shutdown";
        public override bool GetBotProperty(KLBot bot) => bot.IsOn;
        public override void SetBotProperty(KLBot bot, bool value) => bot.IsOn = value;
    }
    public class FuckModCmd : SwitchCommand
    {
        public override string SwitchName => "嘴臭模块";
        public override string Format => "fuckmod";
        public override bool GetBotProperty(KLBot bot) => bot.IsFuckModuleEnabled;
        public override void SetBotProperty(KLBot bot, bool value) => bot.IsFuckModuleEnabled = value;
    }
    public class FuckModCascadeCmd : SwitchCommand
    {
        public override string SwitchName => "嘴臭模块-串联模式";
        public override string Format => "fuckmod-cascade";
        public override bool GetBotProperty(KLBot bot) => bot.FuckModule.IsCascadeMode;
        public override void SetBotProperty(KLBot bot, bool value) => bot.FuckModule.IsCascadeMode = value;
    }
}