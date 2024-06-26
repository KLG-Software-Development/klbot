using klbotlib.Exceptions;
using Microsoft.Extensions.Configuration;
using System.Diagnostics.CodeAnalysis;

namespace klbotlib.Extensions;

/// <summary>
/// ConfigurationManager扩展方法
/// </summary>
public static class IConfigurationExtension
{
    /// <summary>
    /// 读取当前配置中给定项的值
    /// </summary>
    /// <param name="config"></param>
    /// <param name="key">键名</param>
    /// <param name="value">获取的值。若失败则为null</param>
    /// <param name="section">区段名（可选）</param>
    /// <returns>若键不存在则抛出异常；成功则返回该配置项的值</returns>
    /// <exception cref="KLBotInitializationException"></exception>
    public static bool TryReadValue(this IConfiguration config, string key, [NotNullWhen(true)] out string? value, string? section = null)
    {
        value = null;
        if (section != null)
        {
            var configSection = config.GetSection(section);
            if (!configSection.Exists())
                return false;
            value = configSection.GetValue<string>(key);
            if (value == null)
                return false;
        }
        value = config.GetValue<string>(key);
        return value != null;
    }
    /// <summary>
    /// 读取当前配置中给定项的值
    /// </summary>
    /// <param name="config"></param>
    /// <param name="key">键名</param>
    /// <param name="section">区段名（可选）</param>
    /// <returns>若键不存在则抛出异常；成功则返回该配置项的值</returns>
    /// <exception cref="KLBotInitializationException"></exception>
    public static string ReadValue(this IConfiguration config, string key, string? section = null)
    {
        string keyDesc = section == null ? $"Field \"{key}\"" : $"Section \"{section}\" field \"{key}\"";
        if (section != null)
        {
            var configSection = config.GetSection(section);
            if (!configSection.Exists())
                throw new KLBotInitializationException($"{keyDesc} is required, but the section is not found in the config file");
            string? value = configSection.GetValue<string>(key);
            return value ?? throw new KLBotInitializationException($"{keyDesc} is required, but the value is not found in the config file");
        }
        string? rootValue = config.GetValue<string>(key);
        return rootValue ?? throw new KLBotInitializationException($"{keyDesc} is required but not found in the config file");
    }
    /// <summary>
    /// 读取当前配置中给定项的值并转换为分隔数组。默认分隔符为','
    /// </summary>
    /// <param name="config"></param>
    /// <param name="key">键名</param>
    /// <param name="section">区段名（可选）</param>
    /// <param name="delim">分隔符，默认为','</param>
    /// <returns>若键不存在则抛出异常；成功则返回该配置项的值</returns>
    public static string[] ReadArray(this IConfiguration config, string key, string? section = null, char delim = ',')
    {
        return config.ReadValue(key, section).Split(delim, StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);
    }
}
