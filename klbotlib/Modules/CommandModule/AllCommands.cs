﻿using klbotlib.Extensions;
using System.Diagnostics;
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
    private static readonly Stopwatch s_sw = new();
    public abstract string CommandString { get; }
    public abstract string ActionDescription { get; } //命令所执行操作的内容
    public abstract Task Action(KLBot bot, MessagePlain cmdMsg);

    public sealed override string Format => CommandString;
    public sealed override string Usage => $"执行{ActionDescription}操作";
    public sealed override bool IsCmd(string cmd) => cmd == CommandString;
    public sealed override async Task<string> CommandTask(KLBot bot, MessagePlain cmdMsg, string _, CommandArgument __)
    {
        s_sw.Restart();
        await Action(bot, cmdMsg);
        s_sw.Stop();
        return $"{ActionDescription}执行成功，耗时{s_sw.ElapsedMilliseconds.ToTimeSpanString(2)}。";
    }
}
/// <summary>
/// 执行通用任务的命令基类-有参数
/// </summary>
/// <typeparam name="T">参数的类型</typeparam>
internal abstract class ActionCommand<T> : Command
{
    private static readonly Stopwatch s_sw = new();
    public abstract string CommandString { get; }
    public abstract string ActionName { get; }       //命令所执行操作的名称
    public abstract string ActionDescription { get; }       //命令所执行操作的名称
    public abstract string ParameterDescription { get; }  //参数的格式或类型
    public abstract Task Action(KLBot bot, MessagePlain cmdMsg, T parameter);     //命令所执行的操作
    public abstract bool TryParseCmdStringValue(string valueString, out T value);     //TryParse的规范：接收值字符串并尝试解析，失败则返回false，成功则返回true且写入out value参数中

    public sealed override string Format => $"{CommandString} [{ParameterDescription}]";
    public sealed override string Usage => ActionDescription;
    public sealed override bool IsCmd(string cmd) => cmd.StartsWith($"{CommandString} ");
    public sealed override async Task<string> CommandTask(KLBot bot, MessagePlain cmdMsg, string cmd, CommandArgument _)
    {
        s_sw.Restart();
        string valueString = cmd[(CommandString.Length + 1)..];
        if (TryParseCmdStringValue(valueString, out T parameter))
        {
            await Action(bot, cmdMsg, parameter);
            s_sw.Stop();
            return $"{ActionName}执行成功，耗时{s_sw.ElapsedMilliseconds.ToTimeSpanString(2)}。";
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
    public abstract Task<string> GetInfo(KLBot bot);

    public sealed override AuthorType AuthorityRequirment => AuthorType.野人;
    public sealed override string Format => CommandString;
    public sealed override string Usage => $"获取{InfoDescription}";
    public sealed override bool IsCmd(string cmd) => cmd == CommandString;
    public sealed override async Task<string> CommandTask(KLBot bot, MessagePlain _, string __, CommandArgument ___) => await GetInfo(bot);
}
/// <summary>
/// 开关型命令的基类. 统一调用方法：^[命令字符串]$
/// </summary>
internal abstract class SwitchCommand : Command
{
    public abstract string SwitchName { get; }
    public abstract bool GetBotProperty(KLBot bot);
    public abstract Task SetBotProperty(KLBot bot, bool value);
    public sealed override string Usage => "打开/关闭" + SwitchName;
    public sealed override async Task<string> CommandTask(KLBot bot, MessagePlain _, string __, CommandArgument ___)
    {
        bool currentState = GetBotProperty(bot);
        await SetBotProperty(bot, !currentState);
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
    public abstract string TargetModuleId { get; }
    public abstract string MemberName { get; }           //此命令要修改的属性或者字段名

    public sealed override bool GetBotProperty(KLBot bot)
    {
        return !bot[TargetModuleId].ModuleAccess.TryGetFieldAndProperty(MemberName, out bool value)
            ? throw new Exception($"找不到字段\"{MemberName}\"")
            : value;
    }
    public sealed override Task SetBotProperty(KLBot bot, bool value)
    {
        return Task.Run(() =>
        {
            if (!bot[TargetModuleId].TrySetFieldAndProperty(MemberName, value))
                throw new Exception($"找不到可设置的布尔字段\"{MemberName}\"");
        });
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
    public sealed override Task<string> CommandTask(KLBot bot, MessagePlain msg, string cmd, CommandArgument _)
    {
        T currentValue = GetBotProperty(bot);
        string valueString = cmd[(CommandString.Length + 1)..];
        if (valueString == "?")
            return Task.FromResult($"{PropertyName}当前的值为{currentValue}");
        else
        {
            bool result = TryParseCmdStringValue(valueString, out T val);
            if (result)
            {
                SetBotProperty(bot, val);
                return Task.FromResult($"{PropertyName}已修改：\r\n旧值：{currentValue}\r\n新值：{val}");
            }
            else
                return Task.FromResult($"修改{PropertyName}失败：无法从'{val}'中解析出合法的值");
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
        return int.TryParse(valueString, out val) && val >= 0 && val <= 100;
    }
}
/// <summary>
/// 专门管理外部模块的赋值型命令基类
/// </summary>
/// <typeparam name="T">参数的类型</typeparam>
internal abstract class ExternalAssignmentCommand<T> : AssignmentCommand<T> where T : struct
{
    private readonly string _typeName = typeof(T).Name;

    public abstract string TargetModuleId { get; }
    public abstract string MemberName { get; }           //此命令要修改的属性或者字段名

    public sealed override T GetBotProperty(KLBot bot)
    {
        return !bot[TargetModuleId].ModuleAccess.TryGetFieldAndProperty(MemberName, out T value)
            ? throw new Exception($"找不到{_typeName}字段\"{MemberName}\"")
            : value;
    }
    public sealed override void SetBotProperty(KLBot bot, T value)
    {
        if (!bot[TargetModuleId].TrySetFieldAndProperty(MemberName, value))
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
    public override Task<string> GetInfo(KLBot bot)
    {
        _ = _sb.Clear();
        _ = _sb.AppendLine("命令列表: ");
        foreach (Command cmd in bot.GetModule<CommandModule>()._cmds)
        {
            _ = _sb.AppendLine($"{cmd.Format}\n{cmd.Usage}\r\n<权限级别：{cmd.AuthorityRequirment}>\n");
        }
        return Task.FromResult(_sb.AppendLine("\n提示：发送“##status”可以查看当前模块链条；发送“[模块名]帮助”可以查看模块信息").ToString());
    }
}
[DefaultCommand]
internal partial class InfoCmd : InfoCommand
{
    private readonly StringBuilder _sb = new();     //调用者清理
    private readonly Stopwatch _sw = new();
    private readonly Regex _multiWhite = InfoCmdPattern();
    private async Task<string> GetCoreUtilization()
    {
        Process p = new();
        if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux))
        {
            p.StartInfo.FileName = "mpstat";
            p.StartInfo.UseShellExecute = false;
            p.StartInfo.RedirectStandardOutput = true;
            _ = p.Start();
            string raw = await p.StandardOutput.ReadToEndAsync();
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
            _ = p.Start();
            p.WaitForExit();
            string output = (await p.StandardOutput.ReadToEndAsync()).Trim();
            string load = output.Split('=')[1];
            return $"{load}%";
        }
        else
            return $"暂时不支持获取此平台下的CPU占用信息";
    }
    private async Task<string> GetRAMUtilization()
    {
        Process p = new();
        if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux))
        {
            p.StartInfo.FileName = "free";
            p.StartInfo.Arguments = "-h";
            p.StartInfo.CreateNoWindow = true;
            p.StartInfo.RedirectStandardOutput = true;
            _ = p.Start();
            p.WaitForExit();
            string output = (await p.StandardOutput.ReadToEndAsync()).Split('\n')[1];
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
            _ = p.Start();
            p.WaitForExit();
            string[] outputs = (await p.StandardOutput.ReadToEndAsync()).Trim().Split('\n');
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
    public override async Task<string> GetInfo(KLBot bot)
    {
        Process process = Process.GetCurrentProcess();
        var exeAsm = Assembly.GetEntryAssembly();
        Version exeVersion = exeAsm == null ? new() : exeAsm.GetName().Version ?? new();
        Version libVersion = Info.CoreLibInfo.GetLibVersion();
        Version? mcVersion = Info.ModuleCollectionInfo.GetMCVersion();
        _ = _sb.Clear();
        _ = _sb.AppendLine($"KLBot");
        _ = _sb.AppendLine($"主函数版本: v{exeVersion.Major}.{exeVersion.Minor}-{exeVersion.ToKLGBuildString()}");
        _ = _sb.AppendLine($"核心库版本: v{libVersion.Major}.{libVersion.Minor}-{libVersion.ToKLGBuildString()}");
        _ = mcVersion != null
            ? _sb.AppendLine($"模块合集版本: v{mcVersion.Major}.{mcVersion.Minor}-{mcVersion.ToKLGBuildString()}")
            : _sb.AppendLine($"模块合集版本: 未注册任何模块合集");
        _ = _sb.AppendLine($"消息驱动器信息：{bot.MessageDriverType}");
        _ = _sb.AppendLine($"\n[平台信息]\nOS描述：{RuntimeInformation.OSDescription}");
        _ = _sb.AppendLine($"运行时: {RuntimeInformation.FrameworkDescription}");
        _ = _sb.AppendLine($"逻辑核心数量：{Environment.ProcessorCount}");
        _ = _sb.AppendLine($"\n[性能信息]\nCPU使用率：{await GetCoreUtilization()}");
        _ = _sb.AppendLine($"可用内存：{await GetRAMUtilization()}");
        _ = _sb.AppendLine($"\n[进程信息]\n进程架构：{RuntimeInformation.ProcessArchitecture}");
        _ = _sb.AppendLine($"当前内存：{process.WorkingSet64.ToMemorySizeString(3)}");
        _ = _sb.AppendLine($"峰值内存：{process.PeakWorkingSet64.ToMemorySizeString(3)}");
        _ = _sb.AppendLine($"线程数量：{process.Threads.Count}");
        _ = _sb.AppendLine($"总处理器时间：{process.TotalProcessorTime.TotalMilliseconds.ToTimeSpanString(1)}");
        TimeSpan elapsed = _sw.Elapsed;
        _ = _sb.Append($"\n已运行：{elapsed.Days}天，{elapsed.Hours}小时{elapsed.Minutes}分钟{elapsed.Seconds}秒");
        return _sb.ToString();
    }

    [GeneratedRegex(@"\s+", RegexOptions.Compiled)]
    private static partial Regex InfoCmdPattern();
}
[DefaultCommand]
internal class StatusCmd : InfoCommand
{
    private readonly StringBuilder _sb = new();     //调用者清理
    public sealed override string CommandString => "status";
    public sealed override string InfoDescription => "KLBot状态";
    public sealed override Task<string> GetInfo(KLBot bot)
    {
        _ = _sb.Clear();
        _ = _sb.Append($"[配置信息]\n");
        _ = _sb.Append(bot.GetListeningGroupListString());
        _ = _sb.AppendLine("\n[模块信息]");
        _ = _sb.Append(bot.GetModuleChainString() + "\n");
        _ = _sb.AppendLine("\n[统计信息]");
        _ = _sb.Append(bot.DiagData.GetSummaryString());
        return Task.FromResult(_sb.ToString());
    }
}
[DefaultCommand]
internal class StatusAllCmd : InfoCommand
{
    private readonly StringBuilder _sb = new();     //调用者清理

    public sealed override string CommandString => "status all";
    public sealed override string InfoDescription => "KLBot详细状态";
    public sealed override Task<string> GetInfo(KLBot bot)
    {
        _ = _sb.Clear();
        _ = _sb.Append($"[配置信息]\n");
        _ = _sb.Append(bot.GetListeningGroupListString());
        _ = _sb.AppendLine("\n[模块信息]");
        _ = _sb.Append(bot.GetModuleStatusString() + "\n");
        _ = _sb.AppendLine("\n[统计信息]");
        _ = _sb.Append(bot.DiagData.GetSummaryString());
        return Task.FromResult(_sb.ToString());
    }
}
[DefaultCommand]
internal class ModuleSwitchCmd : Command
{
    public override AuthorType AuthorityRequirment => AuthorType.野人;
    public override string Format => "switch [module-id]";
    public override string Usage => "启用/禁用模块";

    public override bool IsCmd(string cmd)
        => cmd.StartsWith("switch");
    public override Task<string> CommandTask(KLBot bot, MessagePlain cmdMsg, string cmdStr, CommandArgument args)
    {
        if (args.Length < 1)
            return Task.FromResult($"参数错误。格式：{Format}");
        if (!bot.ModuleChain.TryGetModule(args.Arguments[0], out Module? module))
            return Task.FromResult($"找不到ID为{args.Arguments[0]}的模块");
        if (module.GetType().Assembly == GetType().Assembly)
            return Task.FromResult($"{module.FriendlyName}为核心模块，禁止禁用");
        module.Enabled = !module.Enabled;
        string action = module.Enabled ? "启用" : "禁用";
        return Task.FromResult($"{module.FriendlyName}已{action}");
    }
}
//嘴臭模块命令
[DefaultCommand]
internal class FuckModCascadeCmd : ExternalSwitchCommand
{
    public override AuthorType AuthorityRequirment => AuthorType.野人;
    public override string SwitchName => "嘴臭模块-串联模式";
    public override string Format => "fuckmod cascade";
    public override string TargetModuleId => "FuckModule";
    public override string MemberName => "IsCascade";
}
[DefaultCommand]
internal class FuckModMaxLengthCmd : ExternalAssignmentCommand<int>
{
    public override AuthorType AuthorityRequirment => AuthorType.野人;
    public override string PropertyName => "嘴臭模块-最大长度";
    public override string CommandString => "fuckmod max-length";
    public override string ParameterDescription => "长度(整数)";
    public override string TargetModuleId => "FuckModule";
    public override string MemberName => "MaxLength";
    public override bool TryParseCmdStringValue(string valueString, out int val) => int.TryParse(valueString, out val);
}
[DefaultCommand]
internal class TagMeCmd : ExternalSwitchCommand
{
    public override AuthorType AuthorityRequirment => AuthorType.野人;
    public override string SwitchName => "TagMe模式";
    public override string Format => "tag-me";
    public override string TargetModuleId => "FuckModule";
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
    public override string TargetModuleId => "ImageModule";
    public override string MemberName => "Fraction";
    public override bool TryParseCmdStringValue(string valueString, out int val)
    {
        return int.TryParse(valueString, out val) && val >= 0 && val <= 100;
    }
}
