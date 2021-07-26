using Newtonsoft.Json;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;

namespace klbotlib
{
    internal class Config
    {
        public virtual bool HasNull(out string null_field_name)
        {
            var fields = GetType().GetFields(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance);
            foreach (var field in fields)
            {
                if (field.GetValue(this) == null)
                {
                    null_field_name = $"{GetType().Name}.{field.Name}";
                    return true;
                }
            }
            null_field_name = string.Empty;
            return false;
        }
    }

    /// <summary>
    /// KLBot启动配置类
    /// </summary>
    [JsonObject(MemberSerialization.Fields)]
    public class KLBotConfig
    {
        internal QQConfig QQ;
        internal NetworkConfig Network;
        internal PathesConfig Pathes;

        /// <summary>
        /// 创建一个KLBot配置
        /// </summary>
        /// <param name="url">mirai服务器所在URL</param>
        /// <param name="self_id">KLBot自身ID (QQ号)</param>
        /// <param name="targets">监听群组列表</param>
        /// <param name="modules_cache_dir">模块缓存目录（相对）。所有模块的缓存目录集中在该目录下</param>
        /// <param name="modules_save_dir">模块状态和模块配置的存档目录（相对）。所有模块的状态存档和配置存档文件集中在该目录下</param>
        public KLBotConfig(string url, long self_id, IEnumerable<long> targets, string modules_cache_dir, string modules_save_dir)
        {
            QQ = new QQConfig(self_id, targets);
            Network = new NetworkConfig(url);
            Pathes = new PathesConfig(modules_cache_dir, modules_save_dir);
        }
        internal bool HasNull(out string null_field_name)
        {
            var fields = GetType().GetFields(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance);
            foreach (var field in fields)
            {
                if ((Config)field.GetValue(this) == null)
                {
                    null_field_name = $"BotConfig.{field.Name}";
                    return true;
                }
                else if (((Config)field.GetValue(this)).HasNull(out null_field_name))
                    return true;
            }
            null_field_name = string.Empty;
            return false;
        }
    }
    [JsonObject(MemberSerialization.Fields)]
    internal class QQConfig : Config
    {
        //Bot账号的自身ID
        public long SelfID;
        //Bot监听的群组的ID列表
        public List<long> TargetGroupIDList;
        public QQConfig(long selfID, IEnumerable<long> targetGroupIDList)
        {
            SelfID = selfID;
            TargetGroupIDList = targetGroupIDList.ToList();
        }
    }
    [JsonObject(MemberSerialization.Fields)]
    internal class NetworkConfig : Config
    {
        //mirai服务器的URL
        public string ServerURL;
        public NetworkConfig(string server)
        {
            ServerURL = server;
        }
    }
    [JsonObject(MemberSerialization.Fields)]
    internal class PathesConfig : Config
    {
        public string ModulesCacheDir = "cache/modules";   //模块私有目录 用来存取模块自己的自定义文件
        public string ModulesSaveDir = "saves";  //模块存档目录 保存或读取模块配置和模块状态的路径
        public PathesConfig(string modules_cache_dir, string modules_save_dir)
        {
            ModulesCacheDir = modules_cache_dir;
            ModulesSaveDir = modules_save_dir;
        }
    }
}
