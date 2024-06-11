using klbotlib.Modules;
using System.Net.Http.Headers;
using System.Text.Encodings.Web;
using System.Text.Json;

namespace klbotlib.Json;

internal static class KLBotJsonHelper
{
    internal static MediaTypeHeaderValue JsonMime { get; } = MediaTypeHeaderValue.Parse("application/json");
    internal static StringContent CreateAsJson(string s)
        => new(s, JsonMime);
    // 用于模块加载的JSON反序列化配置
    private static readonly JsonSerializerOptions s_moduleDeserializeOptions = new()
    {
        WriteIndented = true,
        IncludeFields = false,
        Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping,
    };
    // 用于模块保存的JSON序列化配置
    private static readonly JsonSerializerOptions s_moduleSerializeOptions = new()
    {
        WriteIndented = true,
        IncludeFields = false,
        IgnoreReadOnlyProperties = true,
        Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping,
    };

    //用于文件存储的Json序列化配置
    private static readonly JsonSerializerOptions s_fileOptions = new()
    {
        WriteIndented = true
    };

    //用于网络传输的Json序列化配置
    private static readonly JsonSerializerOptions s_networkOptions = new()
    {
        WriteIndented = false,
    };

    internal static string SerializeNetworkData<T>(T obj)
        => JsonSerializer.Serialize(obj, s_networkOptions);
    internal static string SerializeModule(Module module)
    {
        return JsonSerializer.Serialize(module, module.GetType(), s_moduleSerializeOptions);
    }
    internal static Module? DeserializeModule(string json, Type moduleRuntimeType)
    {
        return !moduleRuntimeType.IsSubclassOf(typeof(Module))
            ? throw new Exception($"Type [{moduleRuntimeType}] is no a sub-type of {nameof(Module)}")
            : JsonSerializer.Deserialize(json, moduleRuntimeType, s_moduleDeserializeOptions) as Module;
    }
    internal static string SerializeFile<T>(T obj)
        => JsonSerializer.Serialize(obj, s_fileOptions);
    internal static T? DeserializeFile<T>(string json)
        => JsonSerializer.Deserialize<T>(json, s_fileOptions);
}
