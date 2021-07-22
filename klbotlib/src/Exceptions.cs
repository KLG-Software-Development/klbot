using klbotlib.Modules;
using System;

namespace klbotlib.Exceptions
{
    public class KLBotInitializationException : Exception
    {
        public KLBotInitializationException(string msg) : base($"KLBot初始化失败：{msg}") { }
    }
    public class ModuleMissingException : Exception
    {
        public ModuleMissingException(string msg) : base($"模块类型不匹配：{msg}") { }
    }
    public class ModuleException : Exception
    {
        public ModuleException(Module source, string msg) : base($"模块{source}出现异常：{msg}") { }
    }
    public class ModuleSetupException : Exception
    {
        public ModuleSetupException(Module source, string msg) : base($"{source}模块配置异常：{msg}") { }
    }
}
