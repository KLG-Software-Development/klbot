using klbotlib.Modules;
using System;

namespace klbotlib.Exceptions
{
    /// <summary>
    /// KLBot初始化失败异常
    /// </summary>
    public class KLBotInitializationException : Exception
    {
        internal KLBotInitializationException(string msg) : base($"KLBot初始化失败：{msg}") { }
    }
    /// <summary>
    /// 模块配置异常
    /// </summary>
    public class ModuleSetupException : Exception
    {
        internal ModuleSetupException(Module source, string msg) : base($"{source}模块配置异常：{msg}") { }
    }
    internal class ModuleMissingException : Exception
    {
        public ModuleMissingException(string msg) : base($"模块类型不匹配：{msg}") { }
    }
    internal class ModuleException : Exception
    {
        public ModuleException(Module source, string msg) : base($"模块{source}出现异常：{msg}") { }
    }
}
