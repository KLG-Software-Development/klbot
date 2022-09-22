using klbotlib.Extensions;
using System;
using System.Diagnostics;
using System.Linq;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;
using System.Text.RegularExpressions;

// 所有命令都在本文件、本命名空间中定义
namespace klbotlib.Modules.CommandModuleNamespace.Commands;

/// <summary>
/// 执行型通用任务的命令基类-无参数
/// </summary>
internal abstract class SimpleActionCommand : Command
{
    private static readonly Stopwatch _sw = new();
    public abstract string CommandString { get; }
    public abstract string ActionDescription { get; } //命令所执行操作的内容
    public abstract void Action(KLBot bot, MessagePlain cmdMsg);

    public sealed override string Format => CommandString;
    public sealed override string Usage => $"执行{ActionDescription}操作";
    public sealed override bool IsCmd(string cmd) => cmd == CommandString;
    public sealed override string CommandTask(KLBot bot, MessagePlain cmdMsg, string __)
    {
        _sw.Restart();
        Action(bot, cmdMsg);
        _sw.Stop();
        return $"{ActionDescription}执行成功，耗时{_sw.ElapsedMilliseconds.ToTimeSpanString(2)}。";
    }
}
/// <summary>
/// 执行通用任务的命令基类-有参数
/// </summary>
/// <typeparam name="T">参数的类型</typeparam>
internal abstract class ActionCommand<T> : Command
{
    private static readonly Stopwatch _sw = new();
    public abstract string CommandString { get; }
    public abstract string ActionName { get; }       //命令所执行操作的名称
    public abstract string ActionDescription { get; }       //命令所执行操作的名称
    public abstract string ParameterDescription { get; }  //参数的格式或类型
    public abstract void Action(KLBot bot, MessagePlain cmdMsg, T parameter);     //命令所执行的操作
    public abstract bool TryParseCmdStringValue(string valueString, out T value);     //TryParse的规范：接收值字符串并尝试解析，失败则返回false，成功则返回true且写入out value参数中

    public sealed override string Format => $"{CommandString} [{ParameterDescription}]";
    public sealed override string Usage => ActionDescription;
    public sealed override bool IsCmd(string cmd) => cmd.StartsWith($"{CommandString} ");
    public sealed override string CommandTask(KLBot bot, MessagePlain cmdMsg, string cmd)
    {
        _sw.Restart();
        string valueString = cmd[(CommandString.Length + 1)..];
        if (TryParseCmdStringValue(valueString, out T parameter))
        {
            Action(bot, cmdMsg, parameter);
            _sw.Stop();
            return $"{ActionName}执行成功，耗时{_sw.ElapsedMilliseconds.ToTimeSpanString(2)}。";
        }
        else
            return $"{ActionName}执行失败，无法解析参数“{valueString}”";
    }
}
/// <summary>
/// 返回信息的命令 的基类
/// </summary>
internal abstract class InfoCommand : Command
{
    public abstract string CommandString { get; }
    public abstract string InfoDescription { get; }
    public abstract string GetInfo(KLBot bot);

    public sealed override AuthorType AuthorityRequirment => AuthorType.野人;
    public sealed override string Format => CommandString;
    public sealed override string Usage => $"获取{InfoDescription}";
    public sealed override bool IsCmd(string cmd) => cmd == CommandString;
    public sealed override string CommandTask(KLBot bot, MessagePlain _, string __) => GetInfo(bot);
}
/// <summary>
/// 开关型命令的基类. 统一调用方法：^[命令字符串]$
/// </summary>
internal abstract class SwitchCommand : Command
{
    public abstract string SwitchName { get; }
    public abstract bool GetBotProperty(KLBot bot);
    public abstract void SetBotProperty(KLBot bot, bool value);
    public sealed override string Usage => "打开/关闭" + SwitchName;
    public sealed override string CommandTask(KLBot bot, MessagePlain _, string __)
    {
        bool currentState = GetBotProperty(bot);
        SetBotProperty(bot, !currentState);
        string action = currentState ? "关闭" : "打开";
        return $"{SwitchName}已{action}";
    }
    public sealed override bool IsCmd(string cmd) => cmd == Format;
}
/// <summary>
/// 专门管理外部模块的开关型命令基类
/// </summary>
internal abstract class ExternalSwitchCommand : SwitchCommand
{
    public abstract string TargetModuleID { get; }
    public abstract string MemberName { get; }           //此命令要修改的属性或者字段名

    public sealed override bool GetBotProperty(KLBot bot)
    {
        if (!bot[TargetModuleID].ModuleAccess.TryGetFieldAndProperty(MemberName, out bool value))
            throw new Exception($"找不到字段\"{MemberName}\"");
        return value;
    }
    public sealed override void SetBotProperty(KLBot bot, bool value)
    {
        if (!bot[TargetModuleID].TrySetFieldAndProperty(MemberName, value))
            throw new Exception($"找不到可设置的布尔字段\"{MemberName}\"");
    }
}
/// <summary>
/// 赋值型命令的基类. 统一调用方法：^[命令字符串] 参数$
/// </summary>
/// <typeparam name="T">参数的类型</typeparam>
internal abstract class AssignmentCommand<T> : Command
{
    public abstract string CommandString { get; }   //调用的命令
    public abstract string ParameterDescription { get; }  //参数的格式或类型
    public abstract string PropertyName { get; }
    public abstract T GetBotProperty(KLBot bot);
    public abstract void SetBotProperty(KLBot bot, T value);
    public abstract bool TryParseCmdStringValue(string valueString, out T value);     //TryParse的规范：接收值字符串并尝试解析，失败则返回false，成功则返回true且写入out value参数中

    public sealed override string Usage => "查询/设置" + PropertyName;
    public sealed override string Format => $"查询：{CommandString} ?\r\n设置：{CommandString} [{ParameterDescription}]";
    public sealed override bool IsCmd(string cmd) => cmd.StartsWith($"{CommandString} ");
    public sealed override string CommandTask(KLBot bot, MessagePlain msg, string cmd)
    {
        T currentValue = GetBotProperty(bot);
        string valueString = cmd[(CommandString.Length + 1)..];
        if (valueString == "?")
            return $"{PropertyName}当前的值为{currentValue}";
        else
        {
            bool result = TryParseCmdStringValue(valueString, out T val);
            if (result)
            {
                SetBotProperty(bot, val);
                return $"{PropertyName}已修改：\r\n旧值：{currentValue}\r\n新值：{val}";
            }
            else
                return $"修改{PropertyName}失败：无法从'{val}'中解析出合法的值";
        }
    }
}
/// <summary>
/// 专门管理外部模块的赋值型命令基类
/// </summary>
internal abstract class IntExternalAssignmentCommand : ExternalAssignmentCommand<int>
{
    public virtual int MinInclusive { get; } = int.MinValue;
    public virtual int MaxExclusive { get; } = int.MaxValue;
    public sealed override bool TryParseCmdStringValue(string valueString, out int val)
    {
        if (int.TryParse(valueString, out val) && val >= 0 && val <= 100)
            return true;
        return false;
    }
}
/// <summary>
/// 专门管理外部模块的赋值型命令基类
/// </summary>
/// <typeparam name="T">参数的类型</typeparam>
internal abstract class ExternalAssignmentCommand<T> : AssignmentCommand<T>
{
    private readonly string _typeName = typeof(T).Name;

    public abstract string TargetModuleID { get; }
    public abstract string MemberName { get; }           //此命令要修改的属性或者字段名

    public sealed override T GetBotProperty(KLBot bot)
    {
        if (!bot[TargetModuleID].ModuleAccess.TryGetFieldAndProperty(MemberName, out T value))
            throw new Exception($"找不到{_typeName}字段\"{MemberName}\"");
        return value;
    }
    public sealed override void SetBotProperty(KLBot bot, T value)
    {
        if (!bot[TargetModuleID].TrySetFieldAndProperty(MemberName, value))
            throw new Exception($"找不到可设置的{_typeName}字段\"{MemberName}\"");
    }
}

//所有命令
//通用命令
[DefaultCommand]
internal class HelpCmd : InfoCommand
{
    private readonly StringBuilder _sb = new();     //调用者清理
    public override string CommandString => "help";
    public override string InfoDescription => "可用命令和帮助";
    public override string GetInfo(KLBot bot)
    {
        _sb.Clear();
        _sb.AppendLine("命令列表: ");
        foreach (Command cmd in bot.GetModule<CommandModule>()._cmds)
        {
            _sb.AppendLine($"{cmd.Format}\n{cmd.Usage}\r\n<权限级别：{cmd.AuthorityRequirment}>\n");
        }
        return _sb.AppendLine("\n提示：发送“##status”可以查看当前模块链条；发送“[模块名]帮助”可以查看模块信息").ToString();
    }
}
[DefaultCommand]
internal class InfoCmd : InfoCommand
{
    private readonly StringBuilder _sb = new();     //调用者清理
    private readonly Stopwatch _sw = new();
    private readonly Regex _multiWhite = new(@"\s+", RegexOptions.Compiled);
    private string GetCoreUtilization()
    {
        Process p = new();
        if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux))
        {
            p.StartInfo.FileName = "mpstat";
            p.StartInfo.UseShellExecute = false;
            p.StartInfo.RedirectStandardOutput = true;
            p.Start();
            string raw = p.StandardOutput.ReadToEnd();
            p.WaitForExit();

            string lastLine = raw.Split('\n')[3];
            string idle = _multiWhite.Replace(lastLine, "-").Split('-').Last();
            return (100 - Convert.ToSingle(idle)).ToString("f2") + "%";
        }
        else if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
        {
            p.StartInfo.FileName = "wmic";
            p.StartInfo.Arguments = "CPU get LoadPercentage /Value";
            p.StartInfo.CreateNoWindow = true;
            p.StartInfo.RedirectStandardOutput = true;
            p.Start();
            p.WaitForExit();
            string output = p.StandardOutput.ReadToEnd().Trim();
            string load = output.Split('=')[1];
            return $"{load}%";
        }
        else
            return $"暂时不支持获取此平台下的CPU占用信息";

    }
    private string GetRAMUtilization()
    {
        Process p = new();
        if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux))
        {
            p.StartInfo.FileName = "free";
            p.StartInfo.Arguments = "-h";
            p.StartInfo.CreateNoWindow = true;
            p.StartInfo.RedirectStandardOutput = true;
            p.Start();
            p.WaitForExit();
            string output = p.StandardOutput.ReadToEnd().Split('\n')[1];
            string[] outputs = _multiWhite.Replace(output, "-").Split('-');
            string total = outputs[1][0..^1];
            string available = outputs[6][0..^1];
            return $"{available}B/{total}B";
        }
        else if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
        {
            p.StartInfo.FileName = "wmic";
            p.StartInfo.Arguments = "OS get FreePhysicalMemory,TotalVisibleMemorySize /Value";
            p.StartInfo.CreateNoWindow = true;
            p.StartInfo.RedirectStandardOutput = true;
            p.Start();
            p.WaitForExit();
            string[] outputs = p.StandardOutput.ReadToEnd().Trim().Split('\n');
            string available = (Convert.ToInt64(outputs[0].Split('=')[1]) * 1024L).ToMemorySizeString(1);
            string total = (Convert.ToInt64(outputs[1].Split('=')[1]) * 1024L).ToMemorySizeString(1);
            return $"{available}/{total}";
        }
        else
            return $"暂时不支持获取此平台下的内存占用信息";
    }

    public InfoCmd()
    {
        _sw.Start();
    }
    public override string CommandString => "info";
    public override string InfoDescription => "硬件和软件信息";
    public override string GetInfo(KLBot bot)
    {
        Process process = Process.GetCurrentProcess();
        Version exeVersion = Assembly.GetEntryAssembly().GetName().Version;
        Version libVersion = Info.CoreLibInfo.GetLibVersion();
        Version mcVersion = Info.ModuleCollectionInfo.GetMCVersion();
        _sb.Clear();
        _sb.AppendLine($"KLBot via mirai");
        _sb.AppendLine($"主函数版本: v{exeVersion.Major}.{exeVersion.Minor}-{exeVersion.ToKLGBuildString()}");
        _sb.AppendLine($"核心库版本: v{libVersion.Major}.{libVersion.Minor}-{libVersion.ToKLGBuildString()}");
        if (mcVersion != null)
            _sb.AppendLine($"模块合集版本: v{mcVersion.Major}.{mcVersion.Minor}-{mcVersion.ToKLGBuildString()}");
        else
            _sb.AppendLine($"模块合集版本: 未注册任何模块合集");
        _sb.AppendLine($"\n[平台信息]\nOS描述：{RuntimeInformation.OSDescription}");
        _sb.AppendLine($"运行时: {RuntimeInformation.FrameworkDescription}");
        _sb.AppendLine($"逻辑核心数量：{Environment.ProcessorCount}");
        _sb.AppendLine($"\n[性能信息]\nCPU使用率：{ GetCoreUtilization()}");
        _sb.AppendLine($"可用内存：{ GetRAMUtilization()}");
        _sb.AppendLine($"\n[进程信息]\n进程架构：{RuntimeInformation.ProcessArchitecture}");
        _sb.AppendLine($"当前内存：{process.WorkingSet64.ToMemorySizeString(3)}");
        _sb.AppendLine($"峰值内存：{process.PeakWorkingSet64.ToMemorySizeString(3)}");
        _sb.AppendLine($"线程数量：{process.Threads.Count}");
        _sb.AppendLine($"总处理器时间：{process.TotalProcessorTime.TotalMilliseconds.ToTimeSpanString(1)}");
        TimeSpan elapsed = _sw.Elapsed;
        _sb.Append($"\n已运行：{elapsed.Days}天，{elapsed.Hours}小时{elapsed.Minutes}分钟{elapsed.Seconds}秒");
        return _sb.ToString();
    }
}
[DefaultCommand]
internal class StatusCmd : InfoCommand
{
    private readonly StringBuilder _sb = new();     //调用者清理
    public sealed override string CommandString => "status";
    public sealed override string InfoDescription => "KLBot状态";
    public sealed override string GetInfo(KLBot bot)
    {
        _sb.Clear();
        _sb.Append($"[配置信息]\n");
        _sb.Append(bot.GetListeningGroupListString());
        _sb.AppendLine("\n[模块信息]");
        _sb.Append(bot.GetModuleChainString() + "\n");
        _sb.AppendLine("\n[统计信息]");
        _sb.Append(bot.DiagData.GetSummaryString());
        return _sb.ToString();
    }
}
[DefaultCommand]
internal class StatusAllCmd : InfoCommand
{
    private readonly StringBuilder _sb = new();     //调用者清理

    public sealed override string CommandString => "status all";
    public sealed override string InfoDescription => "KLBot详细状态";
    public sealed override string GetInfo(KLBot bot)
    {
        _sb.Clear();
        _sb.Append($"[配置信息]\n");
        _sb.Append(bot.GetListeningGroupListString());
        _sb.AppendLine("\n[模块信息]");
        _sb.Append(bot.GetModuleStatusString() + "\n");
        _sb.AppendLine("\n[统计信息]");
        _sb.Append(bot.DiagData.GetSummaryString());
        return _sb.ToString();
    }
}
internal class PtiCmd : AssignmentCommand<int>
{
    public override AuthorType AuthorityRequirment => AuthorType.开发者;
    public override string PropertyName => "轮询时间间隔";
    public override string CommandString => "pti";
    public override string ParameterDescription => "间隔时间(ms)";
    public override int GetBotProperty(KLBot bot) => bot.PollingTimeInterval;
    public override void SetBotProperty(KLBot bot, int value) => bot.PollingTimeInterval = value;
    public override bool TryParseCmdStringValue(string valueString, out int value) => int.TryParse(valueString, out value);
}
//嘴臭模块命令
[DefaultCommand]
internal class FuckModEnabledCmd : SwitchCommand
{
    public override AuthorType AuthorityRequirment => AuthorType.野人;
    public override string SwitchName => "嘴臭模块-总开关";
    public override string Format => "fuckmod enabled";
    public override bool GetBotProperty(KLBot bot) => bot["FuckModule"].Enabled;
    public override void SetBotProperty(KLBot bot, bool value) => bot["FuckModule"].Enabled = value;
}
[DefaultCommand]
internal class FuckModCascadeCmd : ExternalSwitchCommand
{
    public override AuthorType AuthorityRequirment => AuthorType.野人;
    public override string SwitchName => "嘴臭模块-串联模式";
    public override string Format => "fuckmod cascade";
    public override string TargetModuleID => "FuckModule";
    public override string MemberName => "IsCascade";
}
[DefaultCommand]
internal class FuckModMaxLengthCmd : ExternalAssignmentCommand<int>
{
    public override AuthorType AuthorityRequirment => AuthorType.野人;
    public override string PropertyName => "嘴臭模块-最大长度";
    public override string CommandString => "fuckmod max-length";
    public override string ParameterDescription => "长度(整数)";
    public override string TargetModuleID => "FuckModule";
    public override string MemberName => "MaxLength";
    public override bool TryParseCmdStringValue(string valueString, out int val) => int.TryParse(valueString, out val);
}
[DefaultCommand]
internal class TagMeCmd : ExternalSwitchCommand
{
    public override AuthorType AuthorityRequirment => AuthorType.野人;
    public override string SwitchName => "TagMe模式";
    public override string Format => "tag-me";
    public override string TargetModuleID => "FuckModule";
    public override string MemberName => "IsTagMe";
}
//图像模块命令
[DefaultCommand]
internal class ImgModFracCmd : ExternalAssignmentCommand<int>
{
    public override AuthorType AuthorityRequirment => AuthorType.野人;
    public override string PropertyName => "图像模块-选取比例";
    public override string CommandString => "imgmod frac";
    public override string ParameterDescription => "整数百分率(%)";
    public override string TargetModuleID => "ImageModule";
    public override string MemberName => "Fraction";
    public override bool TryParseCmdStringValue(string valueString, out int val)
    {
        if (int.TryParse(valueString, out val) && val >= 0 && val <= 100)
            return true;
        return false;
    }
}
