using klbotlib.Extensions;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;

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
    private static readonly object s_globalLogLock = new();
    private static readonly string s_taskTag = "[Task]".ToAnsiColor(ConsoleColor.Magenta);
    private static readonly string s_infoTag = "[Info]".ToAnsiColor(ConsoleColor.Cyan);
    private static readonly string s_warningTag = "[Warn]".ToAnsiColor(ConsoleColor.Yellow);
    private static readonly string s_errorTag = "[Error]".ToAnsiColor(ConsoleColor.Red);
    private static readonly Dictionary<LogType, string> s_tagsByLogType = new()
    {
        { LogType.Task, s_taskTag },
        { LogType.Info, s_infoTag },
        { LogType.Warning, s_warningTag },
        { LogType.Error, s_errorTag },
    };
    [Conditional("DEBUG")]
    public static void DebugLog(this IKLBotLogUnit unit, string s)
    {
        lock (s_globalLogLock)
            Trace.WriteLine($"[{DateTime.Now.ToKLBotTimestampString()}][{unit.LogUnitName}][DEBUG] {s}");
    }
    [Conditional("DEBUG")]
    public static void DebugLog(this IKLBotLogUnit unit, string tag, string s)
    {
        lock (s_globalLogLock)
            Trace.WriteLine($"[{DateTime.Now.ToKLBotTimestampString()}]{tag}[{unit.LogUnitName}][DEBUG] {s}");
    }
    public static void Log(this IKLBotLogUnit unit, string s)
    {
        lock (s_globalLogLock)
            Trace.WriteLine($"[{DateTime.Now.ToKLBotTimestampString()}][{unit.LogUnitName}] {s}");
    }
    public static void Log(this IKLBotLogUnit unit, string tag, string s)
    {
        lock (s_globalLogLock)
            Trace.WriteLine($"[{DateTime.Now.ToKLBotTimestampString()}]{tag}[{unit.LogUnitName}] {s}");
    }
    public static void Log(this IKLBotLogUnit unit, string s, LogType logType)
    {
        if (!s_tagsByLogType.TryGetValue(logType, out string? tag))
            unit.Log(s);
        else
            unit.Log(tag, s);
    }
    public static void LogTask(this IKLBotLogUnit unit, string s)
    {
        unit.Log(s_taskTag, s);
    }
    public static void LogInfo(this IKLBotLogUnit unit, string s)
    {
        unit.Log(s_infoTag, s);
    }
    public static void LogWarning(this IKLBotLogUnit unit, string s)
    {
        unit.Log(s_warningTag, s);
    }
    public static void LogNullAndThrow<T>(this IKLBotLogUnit unit, [NotNull] T? value, string msgWhenNull)
    {
        if (value == null)
            unit.LogError(msgWhenNull, true);
    }
    public static void LogError(this IKLBotLogUnit unit, string s, [DoesNotReturnIf(true)] bool throwException = false)
    {
        unit.Log(s_errorTag, s);
        if (throwException)
            throw new Exception(s);
    }
}
