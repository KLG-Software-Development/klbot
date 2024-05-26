using System;
using System.Collections.Generic;
using System.Diagnostics;
using klbotlib.Extensions;

namespace klbotlib;

/// <summary>
/// KLBot Log Unit
/// </summary>
public interface IKLBotLogUnit
{
    /// <summary>
    /// Name of log unit
    /// </summary>
    string LogUnitName { get; }
}

internal static class IKLBotLogUnitExtension
{
    private static readonly object _globalLogLock = new();
    private static readonly string TaskTag = "[Task]".ToAnsiColor(ConsoleColor.Magenta);
    private static readonly string InfoTag = "[Info]".ToAnsiColor(ConsoleColor.Cyan);
    private static readonly string WarningTag = "[Warn]".ToAnsiColor(ConsoleColor.Yellow);
    private static readonly string ErrorTag = "[Error]".ToAnsiColor(ConsoleColor.Red);
    private static readonly Dictionary<LogType, string> _tagsByLogType = new()
    {
        { LogType.Task, TaskTag },
        { LogType.Info, InfoTag },
        { LogType.Warning, WarningTag },
        { LogType.Error, ErrorTag },
    };
    [Conditional("DEBUG")]
    public static void DebugLog(this IKLBotLogUnit unit, string s)
    {
        lock(_globalLogLock)
            Trace.WriteLine($"[{DateTime.Now.ToKLBotTimestampString()}][{unit.LogUnitName}][DEBUG] {s}");
    }
    public static void Log(this IKLBotLogUnit unit, string s)
    {
        lock(_globalLogLock)
            Trace.WriteLine($"[{DateTime.Now.ToKLBotTimestampString()}][{unit.LogUnitName}] {s}");
    }
    public static void Log(this IKLBotLogUnit unit, string s, LogType logType)
    {
        if (!_tagsByLogType.TryGetValue(logType, out string? tag))
            Log(unit, s);
        unit.Log(tag + s);
    }
    public static void LogTask(this IKLBotLogUnit unit, string s)
    {
        unit.Log(TaskTag + s);
    }
    public static void LogInfo(this IKLBotLogUnit unit, string s)
    {
        unit.Log(InfoTag + s);
    }
    public static void LogWarning(this IKLBotLogUnit unit, string s)
    {
        unit.Log(WarningTag + s);
    }
    public static void LogError(this IKLBotLogUnit unit, string s)
    {
        unit.Log(ErrorTag + s);
    }
}
