using System.Text.Json;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text.Json.Serialization;
using System;

namespace klbotlib.Json
{
    static class KLBotJsonHelper
    {
        internal static MediaTypeHeaderValue JsonMime { get; } = MediaTypeHeaderValue.Parse("application/json");
        internal static StringContent CreateAsJson(string s)
            => new StringContent(s, JsonMime);
        // 用于模块保存的JSON序列化配置
        private static readonly JsonSerializerOptions _moduleOptions = new()
        {
            WriteIndented = true,
            DefaultIgnoreCondition = JsonIgnoreCondition.Always,
            IncludeFields = false
        };

        //用于文件存储的Json序列化配置
        private static readonly JsonSerializerOptions _fileOptions = new()
        {
            WriteIndented = true
        };
        private static readonly JsonSerializerOptions _fileDeserializeOptions = new()
        {
            WriteIndented = true
        };

        //用于网络传输的Json序列化配置
        private static readonly JsonSerializerOptions _networkOptions = new()
        {
            WriteIndented = false,
        };
        private static readonly AutoCastJsonConverter _autoCastConverter = new();

        static KLBotJsonHelper()
        {
            _fileDeserializeOptions.Converters.Add(_autoCastConverter);
        }

        internal static string SerializeFile<T>(T obj)
            => JsonSerializer.Serialize(obj, _fileOptions);
        internal static string SerializeNetworkData<T>(T obj)
            => JsonSerializer.Serialize(obj, _networkOptions);
        internal static T? DeserializeFile<T>(string json)
            => JsonSerializer.Deserialize<T>(json, _fileDeserializeOptions);
    }
    internal class AutoCastJsonConverter : JsonConverter<object>
    {
        public override object? Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
        {
            switch (reader.TokenType)
            {
                case JsonTokenType.True:
                    return true;
                case JsonTokenType.False:
                    return false;
                case JsonTokenType.Number:
                    if (reader.TryGetInt64(out long i64))
                        return i64;        
                    return reader.GetDouble();
                case JsonTokenType.String:
                    if (reader.TryGetDateTime(out DateTime datetime))
                        return datetime;
                    return reader.GetString();
                default:
                    using (JsonDocument document = JsonDocument.ParseValue(ref reader))
                    {
                        return document.RootElement.Clone();
                    }
            }
        }
        public override void Write(Utf8JsonWriter writer, object value, JsonSerializerOptions options)
        {
            throw new InvalidOperationException("不支持的操作：仅用于反序列化");
        }
    }
}
