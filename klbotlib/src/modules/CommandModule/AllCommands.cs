using System;
using System.Diagnostics;
using System.Linq;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;

///所有命令都在本文件本命名空间中定义
namespace klbotlib.Modules.CommandModuleNamespace.Commands
{
    /// <summary>
    /// 执行型通用任务的命令基类-无参数
    /// </summary>
    public abstract class SimpleActionCommand : Command
    {
        private static readonly Stopwatch sw = new Stopwatch();
        public abstract string CommandString { get; }
        public abstract string ActionDescription { get; } //命令所执行操作的内容
        public abstract void Action(KLBot bot, MessagePlain cmd_msg);

        public sealed override string Format => CommandString;
        public sealed override string Usage => $"执行{ActionDescription}操作";
        public sealed override bool IsCmd(string cmd) => cmd == CommandString;
        public sealed override string CommandTask(KLBot bot, MessagePlain cmd_msg, string __)
        {
            sw.Restart();
            Action(bot, cmd_msg);
            sw.Stop();
            return $"{ActionDescription}执行成功，耗时{sw.ElapsedMilliseconds.ToTimeSpanString(2)}。";
        } 
    }
    /// <summary>
    /// 执行通用任务的命令基类-有参数
    /// </summary>
    /// <typeparam name="T">参数的类型</typeparam>
    public abstract class ActionCommand<T> : Command
    {
        private static readonly Stopwatch sw = new Stopwatch();
        public abstract string CommandString { get; }
        public abstract string ActionName { get; }       //命令所执行操作的名称
        public abstract string ActionDescription { get; }       //命令所执行操作的名称
        public abstract string ParameterDescription { get; }  //参数的格式或类型
        public abstract void Action(KLBot bot, MessagePlain cmd_msg, T parameter);     //命令所执行的操作
        public abstract bool TryParseCmdStringValue(string value_string, out T value);     //TryParse的规范：接收值字符串并尝试解析，失败则返回false，成功则返回true且写入out value参数中

        public sealed override string Format => $"{CommandString} [{ParameterDescription}]";
        public sealed override string Usage => ActionDescription;
        public sealed override bool IsCmd(string cmd) => cmd.StartsWith($"{CommandString} ");
        public sealed override string CommandTask(KLBot bot, MessagePlain cmd_msg, string cmd)
        {
            sw.Restart();
            string value_string = cmd.Substring(CommandString.Length + 1);
            TryParseCmdStringValue(value_string, out T parameter);
            Action(bot, cmd_msg, parameter);
            sw.Stop();
            return $"{ActionName}执行成功，耗时{sw.ElapsedMilliseconds.ToTimeSpanString(2)}。";
        }
    }
    /// <summary>
    /// 返回信息的命令 的基类
    /// </summary>
    public abstract class InfoCommand : Command
    {
        public abstract string CommandString { get; }
        public abstract string InfoDescription { get; }
        public abstract string GetInfo(KLBot bot);

        public sealed override AuthorType AuthorityRequirment => AuthorType.野人;
        public sealed override string Format => CommandString;
        public sealed override string Usage => $"获取{InfoDescription}信息";
        public sealed override bool IsCmd(string cmd) => cmd == CommandString;
        public sealed override string CommandTask(KLBot bot, MessagePlain _, string __) => GetInfo(bot);
    }
    /// <summary>
    /// 开关型命令的基类. 统一调用方法：^[命令字符串]$
    /// </summary>
    public abstract class SwitchCommand : Command
    {
        public abstract string SwitchName { get; }
        public abstract bool GetBotProperty(KLBot bot);
        public abstract void SetBotProperty(KLBot bot, bool value);
        public sealed override string Usage => "打开/关闭" + SwitchName;
        public sealed override string CommandTask(KLBot bot, MessagePlain _, string __)
        {
            bool current_state = GetBotProperty(bot);
            SetBotProperty(bot, !current_state);
            string action = current_state ? "关闭" : "打开";
            return $"[命令模块]{SwitchName}已{action}";
        }
        public sealed override bool IsCmd(string cmd) => cmd == Format;
    }
    /// <summary>
    /// 赋值型命令的基类. 统一调用方法：^[命令字符串] 参数$
    /// </summary>
    /// <typeparam name="T">参数的类型</typeparam>
    public abstract class AssignmentCommand<T> : Command
    {
        public abstract string CommandString { get; }   //调用的命令
        public abstract string ParameterDescription { get; }  //参数的格式或类型
        public abstract string PropertyName { get; }
        public abstract T GetBotProperty(KLBot bot);
        public abstract void SetBotProperty(KLBot bot, T value);
        public abstract bool TryParseCmdStringValue(string value_string, out T value);     //TryParse的规范：接收值字符串并尝试解析，失败则返回false，成功则返回true且写入out value参数中
        
        public sealed override string Usage => "查询/设置" + PropertyName;
        public sealed override string Format => $"查询：{CommandString} ?\r\n设置：{CommandString} [{ParameterDescription}]";
        public sealed override bool IsCmd(string cmd) => cmd.StartsWith($"{CommandString} ");
        public sealed override string CommandTask(KLBot bot, MessagePlain msg, string cmd)
        {
            T current_value = GetBotProperty(bot);
            string value_string = cmd.Substring(CommandString.Length + 1);
            if (value_string == "?")
                return $"[命令模块]{PropertyName}当前的值为{current_value}";
            else
            {
                bool result = TryParseCmdStringValue(value_string, out T val);
                if (result)
                {
                    SetBotProperty(bot, val);
                    return $"[命令模块]{PropertyName}已修改：\r\n旧值：{current_value}\r\n新值：{val}";
                }
                else
                    return $"[命令模块]修改{PropertyName}失败：无法从'{val}'中解析出合法的值";
            }
        }
    }

//所有命令
    //通用命令
    [DefaultCommand]
    public class HelpCmd : InfoCommand
    {
        public override string CommandString => "help";
        public override string InfoDescription => "可用命令和帮助";
        public override string GetInfo(KLBot bot)
        {
            Version exe_version = Assembly.GetEntryAssembly().GetName().Version;
            Version lib_version = Info.LibInfo.GetLibVersion();
            StringBuilder sb = new StringBuilder();
            sb.AppendLine($"KLBot via mirai");
            sb.AppendLine($"Build {lib_version.ToKLGBuildString()}");
            sb.AppendLine($"主函数版本: v{exe_version.Major}.{exe_version.Minor}");
            sb.AppendLine($"核心库版本: v{lib_version.Major}.{lib_version.Minor}\r\n");
            sb.AppendLine("命令列表: \r\n");
            foreach (Command cmd in bot.GetModule<CommandModule>(this).Cmds)
            {
                sb.AppendLine($"{cmd.Format}\r\n{cmd.Usage}\r\n<权限级别：{cmd.AuthorityRequirment}>\r\n");
            }
            return sb.AppendLine("\r\n提示：发送'##[命令]'以执行指令").ToString();
        }
    }
    [DefaultCommand]
    public class StatusCmd : InfoCommand
    {
        private readonly Regex multi_white = new Regex(@"\s+");
        private string GetCoreUtilization()
        {
            Process p = new Process();
            p.StartInfo.FileName = "mpstat";
            p.StartInfo.UseShellExecute = false;
            p.StartInfo.RedirectStandardOutput = true;
            p.Start();
            string raw = p.StandardOutput.ReadToEnd();
            p.WaitForExit();

            string last_line = raw.Split('\n')[3];
            string idle = multi_white.Replace(last_line, "-").Split('-').Last();
            return (100-Convert.ToSingle(idle)).ToString("f2") + "%";
        }
        private string GetRAMUtilization()
        {
            Process p = new Process();
            p.StartInfo.FileName = "free";
            p.StartInfo.Arguments = "-h";
            p.StartInfo.CreateNoWindow = true;
            p.StartInfo.RedirectStandardOutput = true;
            p.Start();
            p.WaitForExit();
            string output = p.StandardOutput.ReadToEnd().Split('\n')[1];
            string[] outputs = multi_white.Replace(output, "-").Split('-');
            string total = outputs[1].Substring(0, outputs[1].Length - 1);
            string available = outputs[6].Substring(0, outputs[6].Length - 1);
            return $"{available}B/{total}B";
        }
        public override string CommandString => "status";
        public override string InfoDescription => "系统和Bot状态";
        public override string GetInfo(KLBot bot)
        {
            Process process = Process.GetCurrentProcess();
            StringBuilder sb = new StringBuilder($"[平台信息]\nOS描述：{RuntimeInformation.OSDescription}\r\n");
            sb.AppendLine($"OS架构：{RuntimeInformation.OSArchitecture}");
            sb.AppendLine($"运行时: {RuntimeInformation.FrameworkDescription}");
            sb.AppendLine($"逻辑核心数量：{Environment.ProcessorCount}\n");

            sb.AppendLine($"[性能信息]\nCPU使用率：{GetCoreUtilization()}");
            sb.AppendLine($"可用内存：{GetRAMUtilization()}\n");

            sb.AppendLine($"[进程信息]\n进程架构：{RuntimeInformation.ProcessArchitecture}");
            sb.AppendLine($"当前内存：{process.WorkingSet64.ToMemorySizeString(3)}");
            sb.AppendLine($"峰值内存：{process.PeakWorkingSet64.ToMemorySizeString(3)}");
            sb.AppendLine($"线程数量：{process.Threads.Count}");
            sb.AppendLine($"总处理器时间：{process.TotalProcessorTime.TotalMilliseconds.ToTimeSpanString(1)}\n");

            sb.AppendLine("[模块状态]");
            foreach (var module in bot.Modules)
            {
                sb.AppendLine($"> {module.ModuleID}");
                var module_properties = module.GetType().GetProperties();
                foreach (var module_property in module_properties)
                {
                    if (module_property.ContainsAttribute(typeof(ModuleStatusAttribute)))
                    {
                        sb.AppendLine($"{module_property.Name} = {module_property.GetValue(module)}");
                    }
                }
                sb.AppendLine();
            }
            return sb.ToString();
        }
    }
    [DefaultCommand]
    public class GCCmd : SimpleActionCommand
    {
        public override AuthorType AuthorityRequirment => AuthorType.开发者;
        public override string CommandString => "gc";
        public override string ActionDescription => "手动GC回收内存";
        public override void Action(KLBot bot, MessagePlain _) => GC.Collect();
    }
    [DefaultCommand]
    public class SleepCmd : ActionCommand<int>
    {
        public override string CommandString => "sleep";
        public override string ActionName => "睡眠";
        public override string ActionDescription => "将程序挂起指定时间，期间KLBot将停止轮询。";
        public override string ParameterDescription => "挂起时间(整数秒)";

        public override bool TryParseCmdStringValue(string value_string, out int value) => int.TryParse(value_string, out value);
        public override void Action(KLBot bot, MessagePlain cmd_msg, int parameter)
        {
            bot.ReplyMessagePlain(cmd_msg, $"[命令模块]开始休眠。时长：{parameter}秒");
            float remain = parameter / 2f;
            Thread.Sleep((int)(remain * 1000));
            Task.Run(() => bot.ReplyMessagePlain(cmd_msg, $"[命令模块]休眠还有一半时间（{remain:f1}/{parameter}秒）结束"));
            Thread.Sleep((int)(remain * 1000));
            bot.ReplyMessagePlain(cmd_msg, $"[命令模块]轮询休眠结束");
        }
    }
    [DefaultCommand]
    public class PtiCmd : AssignmentCommand<int>
    {
        public override AuthorType AuthorityRequirment => AuthorType.开发者;
        public override string PropertyName => "轮询时间间隔";
        public override string CommandString => "pti";
        public override string ParameterDescription => "间隔时间(ms)";
        public override int GetBotProperty(KLBot bot) => bot.PollingTimeInterval;
        public override void SetBotProperty(KLBot bot, int value) => bot.PollingTimeInterval = value;
        public override bool TryParseCmdStringValue(string value_string, out int value) => int.TryParse(value_string, out value);
    }
    [DefaultCommand]
    public class ShutdownCmd : SwitchCommand
    {
        public override string SwitchName => "KLBot总开关";
        public override string Format => "shutdown";
        public override bool GetBotProperty(KLBot bot) => bot.IsLoopOn;
        public override void SetBotProperty(KLBot bot, bool value) => bot.IsLoopOn = value;
    }
    //嘴臭模块命令
    [DefaultCommand]
    public class FuckModEnabledCmd : SwitchCommand
    {
        public override AuthorType AuthorityRequirment => AuthorType.野人;
        public override string SwitchName => "嘴臭模块-总开关";
        public override string Format => "fuckmod enabled";
        public override bool GetBotProperty(KLBot bot) => bot.GetModule<FuckModule>(this).Enabled;
        public override void SetBotProperty(KLBot bot, bool value) => bot.GetModule<FuckModule>(this).Enabled = value;
    }
    [DefaultCommand]
    public class FuckModCascadeCmd : SwitchCommand
    {
        public override AuthorType AuthorityRequirment => AuthorType.野人;
        public override string SwitchName => "嘴臭模块-串联模式";
        public override string Format => "fuckmod cascade";
        public override bool GetBotProperty(KLBot bot) => bot.GetModule<FuckModule>(this).IsCascadeMode;
        public override void SetBotProperty(KLBot bot, bool value) => bot.GetModule<FuckModule>(this).IsCascadeMode = value;
    }
    [DefaultCommand]
    public class FuckModTerminateProbCmd : AssignmentCommand<int>
    {
        public override AuthorType AuthorityRequirment => AuthorType.野人;
        public override string PropertyName => "嘴臭模块-终止概率";
        public override string CommandString => "fuckmod terminal-prob";
        public override string ParameterDescription => "整数概率(%)";
        public override int GetBotProperty(KLBot bot) => bot.GetModule<FuckModule>(this).TerminateProbability;
        public override void SetBotProperty(KLBot bot, int value) => bot.GetModule<FuckModule>(this).TerminateProbability = value;
        public override bool TryParseCmdStringValue(string value_string, out int val) => int.TryParse(value_string, out val);
    }
    [DefaultCommand]
    public class FuckModMaxLengthCmd : AssignmentCommand<int>
    {
        public override AuthorType AuthorityRequirment => AuthorType.野人;
        public override string PropertyName => "嘴臭模块-最大长度";
        public override string CommandString => "fuckmod max-length";
        public override string ParameterDescription => "长度(整数)";
        public override int GetBotProperty(KLBot bot) => bot.GetModule<FuckModule>(this).MaximumLength;
        public override void SetBotProperty(KLBot bot, int value) => bot.GetModule<FuckModule>(this).MaximumLength = value;
        public override bool TryParseCmdStringValue(string value_string, out int val) => int.TryParse(value_string, out val);
    }
    //聊天模块命令
    [DefaultCommand]
    public class TagMeCmd : SwitchCommand
    {
        public override AuthorType AuthorityRequirment => AuthorType.野人;
        public override string SwitchName => "TagMe模式";
        public override string Format => "tag-me";
        public override bool GetBotProperty(KLBot bot) => bot.GetModule<FuckModule>(this).IsTagMe;
        public override void SetBotProperty(KLBot bot, bool value) => bot.GetModule<FuckModule>(this).IsTagMe = value;
    }
}