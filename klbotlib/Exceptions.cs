using klbotlib.Modules;

namespace klbotlib.Exceptions;

/// <summary>
/// KLBot初始化失败异常
/// </summary>
/// <inheritdoc/>
public class KLBotInitializationException(string msg) : Exception($"KLBot初始化失败：{msg}") { }
/// <summary>
/// 模块状态异常
/// </summary>
public class ModuleStatusException : Exception
{
    internal ModuleStatusException(Module source, string msg) : base($"{source}模块状态异常：{msg}") { }
}
/// <summary>
/// 找不到模块异常
/// </summary>
/// <remarks>
/// 新建一个找不到模块异常的实例
/// </remarks>
/// <param name="msg">异常信息</param>
public class ModuleMissingException(string msg) : Exception($"找不到模块：{msg}") { }
/// <summary>
/// JSON反序列化异常
/// </summary>
internal class JsonDeserializationException(string msg, string originalJson) : Exception(msg)
{
    /// <summary>
    /// 触发JSON反序列化异常的原始JSON字符串
    /// </summary>
    public string OriginalJson { get; } = originalJson;
}
internal class ModuleException(Module source, string msg) : Exception($"模块{source}出现异常：{msg}") { }
