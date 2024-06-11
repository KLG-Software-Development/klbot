using System.Reflection;

namespace klbotlib.Info;

/// <summary>
/// klbotlib的程序集信息
/// </summary>
public static class CoreLibInfo
{
    /// <summary>
    /// 获取程序集版本
    /// </summary>
    /// <returns>程序集版本</returns>
    public static Version GetLibVersion() => Assembly.GetExecutingAssembly().GetName().Version ?? new();
}
/// <summary>
/// 模块合集程序集信息
/// </summary>
public static class ModuleCollectionInfo
{
    private static Version? s_mcVersion = null;

    /// <summary>
    /// 保存模块合集版本
    /// </summary>
    public static void SetMCVersion(Assembly mcAssembly)
    {
        s_mcVersion = mcAssembly.GetName().Version;
    }
    /// <summary>
    /// 保存模块合集版本
    /// </summary>
    public static void SetMCVersion(Type mcType)
    {
        var asm = Assembly.GetAssembly(mcType) ?? throw new NullReferenceException("获取MC assembly意外失败");
        s_mcVersion = asm.GetName().Version;
    }
    /// <summary>
    /// 获取模块合集版本
    /// </summary>
    /// <returns>模块合集版本</returns>
    public static Version? GetMCVersion() => s_mcVersion;
}
