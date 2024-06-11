using System.Text.Json;
using System.Net.Http;
using System.Net.Http.Headers;
using klbotlib.Modules;
using System;
using System.Text.Encodings.Web;

namespace klbotlib.Json
{
    static class KLBotJsonHelper
    {
        internal static MediaTypeHeaderValue JsonMime { get; } = MediaTypeHeaderValue.Parse("application/json");
        internal static StringContent CreateAsJson(string s)
            => new StringContent(s, JsonMime);
        // 用于模块加载的JSON反序列化配置
        private static readonly JsonSerializerOptions _moduleDeserializeOptions = new()
        {
            WriteIndented = true,
            IncludeFields = false,
            Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping,
        };
        // 用于模块保存的JSON序列化配置
        private static readonly JsonSerializerOptions _moduleSerializeOptions = new()
        {
            WriteIndented = true,
            IncludeFields = false,
            IgnoreReadOnlyProperties = true,
            Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping,
        };

        //用于文件存储的Json序列化配置
        private static readonly JsonSerializerOptions _fileOptions = new()
        {
            WriteIndented = true
        };

        //用于网络传输的Json序列化配置
        private static readonly JsonSerializerOptions _networkOptions = new()
        {
            WriteIndented = false,
        };

        internal static string SerializeNetworkData<T>(T obj)
            => JsonSerializer.Serialize(obj, _networkOptions);
        internal static string SerializeModule(Module module)
        {
            return JsonSerializer.Serialize(module, module.GetType(), _moduleSerializeOptions);
        }
        internal static Module? DeserializeModule(string json, Type moduleRuntimeType)
        {
            if (!moduleRuntimeType.IsSubclassOf(typeof(Module)))
                throw new Exception($"Type [{moduleRuntimeType}] is no a sub-type of {nameof(Module)}");
            return JsonSerializer.Deserialize(json, moduleRuntimeType, _moduleDeserializeOptions) as Module;
        }
        internal static string SerializeFile<T>(T obj)
            => JsonSerializer.Serialize(obj, _fileOptions);
        internal static T? DeserializeFile<T>(string json)
            => JsonSerializer.Deserialize<T>(json, _fileOptions);
    }
}
