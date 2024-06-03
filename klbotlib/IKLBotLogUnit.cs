﻿using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
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
    [Conditional("DEBUG")]
    public static void DebugLog(this IKLBotLogUnit unit, string tag, string s)
    {
        lock(_globalLogLock)
            Trace.WriteLine($"[{DateTime.Now.ToKLBotTimestampString()}]{tag}[{unit.LogUnitName}][DEBUG] {s}");
    }
    public static void Log(this IKLBotLogUnit unit, string s)
    {
        lock(_globalLogLock)
            Trace.WriteLine($"[{DateTime.Now.ToKLBotTimestampString()}][{unit.LogUnitName}] {s}");
    }
    public static void Log(this IKLBotLogUnit unit, string tag, string s)
    {
        lock(_globalLogLock)
            Trace.WriteLine($"[{DateTime.Now.ToKLBotTimestampString()}]{tag}[{unit.LogUnitName}] {s}");
    }
    public static void Log(this IKLBotLogUnit unit, string s, LogType logType)
    {
        if (!_tagsByLogType.TryGetValue(logType, out string? tag))
            unit.Log(s);
        else
            unit.Log(tag, s);
    }
    public static void LogTask(this IKLBotLogUnit unit, string s)
    {
        unit.Log(TaskTag, s);
    }
    public static void LogInfo(this IKLBotLogUnit unit, string s)
    {
        unit.Log(InfoTag, s);
    }
    public static void LogWarning(this IKLBotLogUnit unit, string s)
    {
        unit.Log(WarningTag, s);
    }
    public static void LogNullAndThrow<T>(this IKLBotLogUnit unit, [NotNull] T? value, string msgWhenNull)
    {
        if (value == null)
            unit.LogError(msgWhenNull, true);
    }
    public static void LogError(this IKLBotLogUnit unit, string s, [DoesNotReturnIf(true)] bool throwException = false)
    {
        unit.Log(ErrorTag, s);
        if (throwException)
            throw new Exception(s);
    }
}
