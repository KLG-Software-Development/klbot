using klbotlib.Extensions;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;

namespace klbotlib
{
    internal class Config
    {
        public virtual bool HasNull(out string nullFieldName)
        {
            var fields = GetType().GetFields(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance);
            foreach (var field in fields)
            {
                if (field.GetValue(this) == null)
                {
                    nullFieldName = $"{GetType().Name}.{field.Name}";
                    return true;
                }
            }
            nullFieldName = string.Empty;
            return false;
        }
    }

    /// <summary>
    /// KLBot启动配置类
    /// </summary>
    [JsonObject(MemberSerialization.Fields)]
    internal class JKLBotConfig
    {
        internal JVerifyConfig Verification;
        internal JQQConfig QQ;
        internal JPathesConfig Pathes;

        /// <summary>
        /// 创建一个KLBot配置
        /// </summary>
        /// <param name="selfId">KLBot自身ID (QQ号)</param>
        /// <param name="key">验证Key</param>
        /// <param name="targets">监听群组列表</param>
        /// <param name="modulesCacheDir">模块缓存目录（相对）。所有模块的缓存目录集中在该目录下</param>
        /// <param name="modulesSaveDir">模块状态和模块配置的存档目录（相对）。所有模块的状态存档和配置存档文件集中在该目录下</param>
        public JKLBotConfig(long selfId, string key, IEnumerable<long> targets, string modulesCacheDir, string modulesSaveDir)
        {
            Verification = new JVerifyConfig(key);
            QQ = new JQQConfig(selfId, targets);
            Pathes = new JPathesConfig(modulesCacheDir, modulesSaveDir);
        }
        internal bool HasNull(out string nullFieldName)
        {
            var fields = GetType().GetFields(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance);
            foreach (var field in fields)
            {
                if (field.GetValue(this) == null)
                {
                    nullFieldName = $"{field.Name}";
                    return true;
                }
                else if (((Config)field.GetValue(this)).HasNull(out nullFieldName))
                    return true;
            }
            nullFieldName = string.Empty;
            return false;
        }
        internal bool HasNullFast(out string nullFieldName)
        {
            nullFieldName = string.Empty;
            Type type = GetType();
            var fields = type.GetFields_All().Where(f => f.FieldType.GetRootBaseType() == typeof(Config));
            foreach (var field in fields)
            {
                if (field.GetValue(this) == null)
                {
                    nullFieldName = $"{field.Name}";
                    return true;
                }
                else if (((Config)field.GetValue(this)).HasNull(out nullFieldName))
                    return true;
            }
            return false;
        }
    }
    [JsonObject(MemberSerialization.Fields)]
    internal class JQQConfig : Config
    {
        //Bot账号的自身ID
        public long SelfID;
        //Bot监听的群组的ID列表
        public HashSet<long> TargetGroupIDList;
        public JQQConfig(long selfID, IEnumerable<long> targetGroupIDList)
        {
            SelfID = selfID;
            TargetGroupIDList = new();
            foreach (var target in targetGroupIDList)
            {
                TargetGroupIDList.Add(target);
            }
        }
    }
    [JsonObject(MemberSerialization.Fields)]
    internal class JNetworkConfig : Config
    {
        //mirai服务器的URL
        public string ServerURL;
        public JNetworkConfig(string server)
        {
            ServerURL = server;
        }
    }
    [JsonObject(MemberSerialization.Fields)]
    internal class JPathesConfig : Config
    {
        public string ModulesCacheDir = "cache/modules";   //模块私有目录 用来存取模块自己的自定义文件
        public string ModulesSaveDir = "saves";  //模块存档目录 保存或读取模块配置和模块状态的路径
        public JPathesConfig(string modulesCacheDir, string modulesSaveDir)
        {
            ModulesCacheDir = modulesCacheDir;
            ModulesSaveDir = modulesSaveDir;
        }
    }
    [JsonObject(MemberSerialization.Fields)]
    internal class JVerifyConfig : Config
    {
        public string Key = string.Empty;        //验证用Key
        public JVerifyConfig(string key)
        {
            Key = key;
        }
    }

}
