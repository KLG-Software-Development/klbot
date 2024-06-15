using klbotlib.Json;
using System.Reflection;
using System.Text.Json;
using Module = klbotlib.Modules.Module;

namespace klbotlib;

/// <summary>
/// 用于加载/保存模块
/// </summary>
internal static class ModuleLoader
{
    private class ModuleLoaderLogger : IKLBotLogUnit
    {
        public string LogUnitName => "Core/ModuleLoader";
    }
    private static readonly ModuleLoaderLogger s_logger = new();
    /// <summary>
    /// 保存模块至JSON
    /// </summary>
    internal static string SaveModule(Module module)
    {
        return KLBotJsonHelper.SerializeModule(module);
    }
    /// <summary>
    /// 保存模块至JSON并写入文件
    /// </summary>
    internal static async Task SaveModule(Module module, string path)
    {
        await File.WriteAllTextAsync(path, SaveModule(module));
    }

    /// <summary>
    /// 从文件读取JSON并加载模块
    /// </summary>
    public static async Task<object> LoadModuleFromFileByName(string moduleTypeName, string path)
    {
        string json;
        // 读取文件
        if (!File.Exists(path)) // 存档不存在时使用空json进行加载以获取默认模块实例
        {
            s_logger.LogWarning($"为模块[{moduleTypeName}]指定的存档\"{path}\"不存在。将自动使用空存档创建默认模块实例。这可能导致问题");
            json = "{}";
        }
        else
            json = await File.ReadAllTextAsync(path);
        // 查找类型并检查其是否为模块类型
        Type moduleType = GetModuleTypeByName(moduleTypeName);
        if (!moduleType.IsSubclassOf(typeof(Module)))
            s_logger.LogError($"\"{moduleTypeName}\"指向[{moduleType.FullName}]，但后者未继承[{typeof(Module).FullName}]，不是合法模块", throwException: true);
        s_logger.LogTask($"正在从[{moduleType.Assembly.FullName}]加载模块类型[{moduleType.FullName}]");
        // 检查并获取/预存的泛型加载方法并注入类型参数
        s_logger.LogNullAndThrow(s_method_LoadModule_T, $"泛型加载方法[{nameof(__LoadModule_T__)}]未预存（严重状态不一致）");
        var loader = s_method_LoadModule_T.MakeGenericMethod(moduleType);
        // 调用泛型加载方法获取产物
        object? output = loader.Invoke(null, [json]) ?? throw new Exception($"从文件[{path}]加载[{moduleType.FullName}]失败：解析失败，结果为null");
        Module? module = Convert.ChangeType(output, moduleType) as Module;
        s_logger.LogNullAndThrow(module, "加载结果无法被安全转换为模块（严重状态不一致）");
        return output;
    }

    private static Type GetModuleTypeByName(string moduleTypeName)
    {
        Assembly? foundAssembly = null;
        Type? moduleType = null;
        var assemblies = AppDomain.CurrentDomain.GetAssemblies();
        foreach (var assembly in assemblies)
        {
            if (assembly.GetCustomAttribute(typeof(ModuleCollectionAttribute)) == null)
                continue;
            s_logger.LogInfo($"正在查找：{assembly}");
            var types = assembly.GetTypes().Where(type => type.Name == moduleTypeName).ToArray();
            if (types.Length == 0)
                continue;
            if (types.Length > 1)
                s_logger.LogError($"[{moduleTypeName}]有多于一个匹配：程序集[{assembly}]中包含{types.Length}个匹配", throwException: true);
            moduleType = types[0];
            if (foundAssembly != null)
                throw new Exception($"[{moduleTypeName}]有多于一个匹配：匹配类型至少存在于[{foundAssembly}]和[{assembly}]两个assembly中");
            foundAssembly = assembly;
        }
        s_logger.LogNullAndThrow(moduleType, $"未找到模块类型[{moduleTypeName}]");
        return moduleType;
    }

    // 从JSON加载模块. 专用于运行时反射调用
#pragma warning disable IDE1006 // 命名样式
    private static T __LoadModule_T__<T>(string json) where T : Module
#pragma warning restore IDE1006 // 命名样式
    {
        Type moduleRuntimeType = typeof(T);
        T module = (KLBotJsonHelper.DeserializeModule(json, moduleRuntimeType) as T)
            ?? throw new JsonException("模块存档文件解析失败");
        return module;
    }
    private static readonly MethodInfo? s_method_LoadModule_T;
    static ModuleLoader()
    {
        // 通过反射获取泛型加载器
        s_method_LoadModule_T = typeof(ModuleLoader).GetMethod(nameof(__LoadModule_T__), BindingFlags.NonPublic | BindingFlags.Static);
        if (s_method_LoadModule_T == null)
            s_logger.LogError($"未找到模块泛型加载方法[{nameof(__LoadModule_T__)}]（严重状态不一致）", throwException: true);
        else if (!s_method_LoadModule_T.IsGenericMethod)
            s_logger.LogError("意外查找到非泛型方法，检查模块加载器实现", throwException: true);
    }
}
