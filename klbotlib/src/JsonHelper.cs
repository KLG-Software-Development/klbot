using Newtonsoft.Json;
using System;

namespace klbotlib.Json
{
    static class JsonHelper
    {
        //用于文件存储的Json序列化配置
        public static JsonSerializerSettings FileSetup = new JsonSerializerSettings 
        { 
            TypeNameHandling = TypeNameHandling.All, 
            Formatting = Formatting.Indented,
            //TypeNameAssemblyFormatHandling = TypeNameAssemblyFormatHandling.Full,
        };

    }
    //因为仍然是Json库自动用Int64的锅...
    public class CustomConverter : JsonConverter
    {
        public override bool CanConvert(Type type) => type == typeof(int);
        public override bool CanWrite => true;
        public override object ReadJson(JsonReader reader, Type type, object obj, JsonSerializer serializer)
        {
            reader.Read();
            if (reader.TokenType == JsonToken.Integer)
                return Convert.ToInt32(reader.Value);
            else
                return serializer.Deserialize(reader);
        }
        public override void WriteJson(JsonWriter writer, object value, JsonSerializer serializer)
        {
            throw new NotImplementedException();
        }
    }
}
