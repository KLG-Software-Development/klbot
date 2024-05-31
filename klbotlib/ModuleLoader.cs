using System;
using System.IO;
using System.Reflection;
using System.Text.Json;
using System.Threading.Tasks;
using klbotlib.Json;
using Module = klbotlib.Modules.Module;

namespace klbotlib;

/// <summary>
/// 用于加载/保存模块
/// </summary>
internal static class ModuleLoader
{
    private class ModuleLoaderLogger : IKLBotLogUnit
    {
        public string LogUnitName => $"{nameof(ModuleLoader)}";
    }
    private static ModuleLoaderLogger _logger = new();
    /// <summary>
    /// 保存模块至JSON
    /// </summary>
    /// <typeparam name="T">模块类型</typeparam>
    public static string SaveModule<T>(T module) where T : Module
    {
        return KLBotJsonHelper.SerializeFile(module);
    }
    /// <summary>
    /// 保存模块至JSON并写入文件
    /// </summary>
    /// <typeparam name="T">模块类型</typeparam>
    public static async Task SaveModule<T>(T module, string path) where T : Module
    {
        await File.WriteAllTextAsync(path, SaveModule(module));
    }

    /// <summary>
    /// 从文件读取JSON并加载模块
    /// </summary>
    public static async Task<Module> LoadModuleFromFileByName(string moduleTypeName, string path)
    {
        // 查找类型并检查其是否为模块类型
        Type? moduleType = Type.GetType(moduleTypeName);
        _logger.LogNullAndThrow(moduleType, $"未找到模块类型[{moduleTypeName}]");
        if (!moduleType.IsSubclassOf(typeof(Module)))
            _logger.LogError($"\"{moduleTypeName}\"指向[{moduleType.FullName}]，但后者未继承[{typeof(Module).FullName}]，不是合法模块", throwException: true);
        _logger.LogTask($"正在从[{moduleType.AssemblyQualifiedName}]加载模块类型[{moduleType.FullName}]");
        // 检查并获取/预存的泛型加载方法并注入类型参数
        _logger.LogNullAndThrow(_method_LoadModule_T, $"泛型加载方法[{nameof(__LoadModule_T__)}]未预存（严重状态不一致）");
        var loader = _method_LoadModule_T.MakeGenericMethod(moduleType);
        // 读取文件
        string json = await File.ReadAllTextAsync(path);
        object? output = null;
        // 调用泛型加载方法获取产物
        try    
        {
            output = loader.Invoke(null, [json]);
        }
        catch
        {
            throw new Exception($"从文件[{path}]加载[{moduleType.FullName}]失败");
        }
        if (output == null)
            throw new Exception($"从文件[{path}]加载[{moduleType.FullName}]失败");
        Module? module = Convert.ChangeType(output, moduleType) as Module;
        _logger.LogNullAndThrow(module, "加载结果无法被安全转换为模块（严重状态不一致）");
        return module;
    }

    // 从JSON加载模块. 专用于运行时反射调用
    private static T __LoadModule_T__<T>(string json) where T : Module
    {
        T module = KLBotJsonHelper.DeserializeFile<T>(json)
            ?? throw new JsonException("模块JSON文件解析失败");
        return module;
    }
    private static MethodInfo? _method_LoadModule_T;
    static ModuleLoader()
    {
        // 通过反射获取泛型加载器
        MethodInfo? _method_LoadModule_T = typeof(ModuleLoader).GetMethod(nameof(__LoadModule_T__), BindingFlags.NonPublic | BindingFlags.Static);
        if (_method_LoadModule_T == null)
            _logger.LogError($"未找到模块泛型加载方法[{nameof(__LoadModule_T__)}]（严重状态不一致）", throwException: true);
        else if (_method_LoadModule_T.GetGenericArguments().Length != 0)
            _logger.LogError("意外查找到非泛型方法，检查模块加载器实现", throwException: true);
    }
}
