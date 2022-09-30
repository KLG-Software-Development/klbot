using System;
using System.Reflection;

namespace ModuleCollection.Info;

/// <summary>
/// klbotlib的程序集信息
/// </summary>
public static class CollectionInfo
{
    /// <summary>
    /// 获取ModuleCollection程序集版本
    /// </summary>
    /// <returns>ModuleCollection程序集版本</returns>
    public static Version? GetLibVersion() => Assembly.GetExecutingAssembly().GetName().Version;
}
