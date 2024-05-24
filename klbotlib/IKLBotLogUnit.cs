using System.Diagnostics;

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
    [Conditional("DEBUG")]
    public static void DebugLog(this IKLBotLogUnit component, string s)
        => Trace.WriteLine($"[{component.LogUnitName}] {s}");
    public static void Log(this IKLBotLogUnit component, string s)
        => Trace.WriteLine($"[{component.LogUnitName}] {s}");
}
