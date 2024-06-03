﻿using klbotlib.Modules;
using System;

namespace klbotlib.Exceptions
{
    /// <summary>
    /// KLBot初始化失败异常
    /// </summary>
    public class KLBotInitializationException : Exception
    {
        /// <inheritdoc/>
        public KLBotInitializationException(string msg) : base($"KLBot初始化失败：{msg}") { }
    }
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
    public class ModuleMissingException : Exception
    {
        /// <summary>
        /// 新建一个找不到模块异常的实例
        /// </summary>
        /// <param name="msg">异常信息</param>
        public ModuleMissingException(string msg) : base($"找不到模块：{msg}") { }
    }
    /// <summary>
    /// JSON反序列化异常
    /// </summary>
    internal class JsonDeserializationException : Exception
    {
        /// <summary>
        /// 触发JSON反序列化异常的原始JSON字符串
        /// </summary>
        public string OriginalJson { get; }
        public JsonDeserializationException(string msg, string originalJson) : base(msg)
        {
            OriginalJson = originalJson;
        }
    }
    internal class ModuleException : Exception
    {
        public ModuleException(Module source, string msg) : base($"模块{source}出现异常：{msg}") { }
    }
}
