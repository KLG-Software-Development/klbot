using Newtonsoft.Json;

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
}
