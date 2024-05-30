using klbotlib.Exceptions;
using klbotlib.Extensions;
using klbotlib.Json;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text.Json.Nodes;
using System.Text.Json.Serialization;
using System.Threading.Tasks;

namespace klbotlib.Modules
{
    /// <summary>
    /// 消息处理模块基类.
    /// 这是KLBot功能实现的基本单位
    /// </summary>
    public abstract class Module : IFileAPI, IMessagingAPI, IOperationAPI, IModuleAccessAPI, IKLBotLogUnit
    {
        //后台变量
        private KLBot? _hostBot;
        //消息处理Task
        private Task _processWorker;

        /// <summary>
        /// 模块名. 是模块种类的唯一标识. 直接等于模块在源码中的类名。
        /// </summary>
        public string ModuleName { get; }
        /// <inheritdoc/>
        public string LogUnitName => ModuleName;
        /// <summary>
        /// 模块ID. 是模块对象的唯一标识. 
        /// 当模块未附加到KLBot上时，等于模块名；
        /// 当模块附加到KLBot上时，等于“模块类名#在同类模块中的排位”
        /// </summary>
        public string ModuleId { get; private set; }
        /// <summary>
        /// 返回此模块是否已经被附加到宿主KLBot上
        /// </summary>
        public bool IsAttached { get; private set; } = false;
        /// <summary>
        /// 决定此模块是否是透明模块(默认为否).
        /// 透明模块处理消息之后会继续向后传递，以使得Bot内部在它之后的模块能继续处理这条消息.
        /// 非透明模块处理消息之后会销毁消息.
        /// </summary>
        public virtual bool IsTransparent { get; } = false;
        /// <summary>
        /// 决定是否在输出前自动加上模块签名"[模块ID]"（默认开启）。
        /// </summary>
        public virtual bool UseSignature { get; } = true;
        /// <summary>
        /// 决定是否使用纯异步执行. 
        /// 此项为true时, 处理器处理消息时不会阻塞, 但完成消息处理的顺序不能得到保证. 
        /// 此项为false时, 处理器仅在处理完上一条消息后才会开始处理下一条消息.
        /// </summary>
        public virtual bool IsAsync { get; } = false;
        /// <summary>
        /// 模块的友好名称（别名）。仅用于打印模块链条信息/触发帮助信息，默认和ModuleName字段相同，即模块类名。
        /// </summary>
        public virtual string FriendlyName { get => ModuleName; }
        /// <summary>
        /// 模块的帮助信息。KLBot在接收到“[模块友好名称]帮助”时，会回复此字符串中的内容。
        /// </summary>
        public virtual string HelpInfo { get => $"[{FriendlyName}]的开发者很懒，没有提供任何帮助信息"; }
        /// <summary>
        /// 处理器(Message -> string)。模块通过这个函数处理所有消息。
        /// </summary>
        /// <param name="context">待处理消息的上下文信息</param>
        /// <param name="msg">待处理消息</param>
        /// <returns>用字符串表示的处理结果。如果你的模块不打算输出/回复处理结果，应返回null或空字符串</returns>
        public abstract Task<Message?> Processor(MessageContext context, Message msg);
        /// <summary>
        /// 模块所附加到的宿主KLBot
        /// </summary>
        public KLBot? HostBot 
        { 
            get { AssertAttachedStatus(true); return _hostBot; }
            private set => _hostBot = value;
        }
        /// <summary>
        /// 模块统计和诊断信息
        /// </summary>
        public ModuleDiagnosticData DiagData { get; } = new ModuleDiagnosticData();
        /// <summary>
        /// 获取此模块的缓存目录。仅当模块已附加到宿主KLBot上时有效，否则会抛出异常。
        /// </summary>
        public string ModuleCacheDir { get => HostBot.GetModuleCacheDir(this); }
        /// <summary>
        /// 缓存接口
        /// </summary>
        public IFileAPI Cache { get => this; }
        /// <summary>
        /// 消息接口
        /// </summary>
        public IMessagingAPI Messaging { get => this; }
        /// <summary>
        /// 操作接口
        /// </summary>
        public IOperationAPI Operating { get => this; }
        /// <summary>
        /// 访问模块接口
        /// </summary>
        public IModuleAccessAPI ModuleAccess { get => this; }

        /// <summary>
        /// 模块的总开关. 默认开启. 此开关关闭时任何消息都会被忽略.
        /// </summary>
        [JsonInclude]
        public bool Enabled { get; set; } = true;

        /// <summary>
        /// 构造一个模块实例。这个实例不会附加到任何宿主KLBot中
        /// </summary>
        public Module()
        {
            ModuleName = GetType().Name;
            ModuleId = ModuleName;
            _processWorker = Task.Run(() => { });
            //绑定Enable通知

        }

        /*** 公共API ***/
        /// <summary>
        /// 模块打印消息到控制台的标准方法
        /// </summary>
        /// <param name="message">模块要打印消息的内容</param>
        /// <param name="msgType">消息类型。提示=Info；警告=Warning；错误=Error；任务执行中=Task；</param>
        public void ModuleLog(string message, LogType msgType = LogType.Info)
            => this.Log(message, msgType);
        T IModuleAccessAPI.GetModule<T>(int index) => HostBot.GetModule<T>(index);
        bool IModuleAccessAPI.TryGetFieldAndProperty<T>(string name, out T value) where T : struct
        {
            value = default;
            Type type = GetType();
            var p = type.GetProperty(name);
            if (p != null)
            {
                if (p.PropertyType == typeof(T))
                {
                    object? obj = p.GetValue(this);
                    if (obj == null)
                        return false;
                    else
                        value = (T)obj;
                    return true;
                }
                else
                    return false;
            }
            var f = type.GetField(name);
            if (f != null)
            {
                if (f.FieldType == typeof(T))
                {
                    object? obj = f.GetValue(this);
                    if (obj == null)
                        return false;
                    value = (T)obj;
                    return true;
                }
                else
                    return false;
            }
            return false;
        }
        /// <summary>
        /// 尝试设置模块特定字段的值。只允许设置public字段。此方法为internal方法，可以保证非核心模块不能通过常规手段修改其他模块
        /// </summary>
        /// <typeparam name="T">字段类型</typeparam>
        /// <param name="name">字段名称</param>
        /// <param name="value">设置字段值</param>
        /// <returns>设置字段是否成功</returns>
        internal bool TrySetFieldAndProperty<T>(string name, T value)
        {
            Type type = GetType();
            var p = type.GetProperty(name);
            if (p != null)
            {
                if (p.PropertyType == typeof(T) && p.CanWrite)
                {
                    p.SetValue(this, value);
                    return true;
                }
                else return false;
            }
            var f = type.GetField(name);
            if (f != null)
            {
                if (f.FieldType == typeof(T) && !f.IsInitOnly)
                {
                    f.SetValue(this, value);
                    return true;
                }
                else return false;
            }
            return false;
        }
        bool IFileAPI.FileExist(string relativePath) => File.Exists(Path.Combine(HostBot.GetModuleCacheDir(this), relativePath));
        void IFileAPI.SaveFileAsString(string relativePath, string text)
        {
            string path = Path.Combine(HostBot.GetModuleCacheDir(this), relativePath);
            ModuleLog($"正在保存文件\"{Path.GetFileName(path)}\"到\"{Path.GetDirectoryName(path)}\"...", LogType.Task);
            if (File.Exists(path))
                ModuleLog($"文件\"{path}\"已经存在，将直接覆盖", LogType.Warning);
            File.WriteAllText(path, text);
        }
        void IFileAPI.SaveFileAsBinary(string relativePath, byte[] bin)
        {
            string path = Path.Combine(HostBot.GetModuleCacheDir(this), relativePath);
            ModuleLog($"正在保存文件\"{Path.GetFileName(path)}\"到\"{Path.GetDirectoryName(path)}\"...", LogType.Task);
            if (File.Exists(path))
                ModuleLog($"文件\"{path}\"已经存在，将直接覆盖", LogType.Warning);
            File.WriteAllBytes(path, bin);
        }
        string IFileAPI.ReadFileAsString(string relativePath)
        {
            string path = Path.Combine(HostBot.GetModuleCacheDir(this), relativePath);
            ModuleLog($"正在从\"{Path.GetDirectoryName(path)}\"读取文件\"{Path.GetFileName(path)}\"...", LogType.Task);
            if (!File.Exists(path))
            {
                ModuleLog($"文件\"{path}\"不存在，无法读取", LogType.Error);
                throw new ModuleException(this, $"文件\"{path}\"不存在，无法读取");
            }
            return File.ReadAllText(path);
        }
        string[] IFileAPI.ReadFileAsStringArrayByLines(string relativePath)
        {
            string path = Path.Combine(HostBot.GetModuleCacheDir(this), relativePath);
            ModuleLog($"正在从\"{Path.GetDirectoryName(path)}\"读取文件\"{Path.GetFileName(path)}\"...", LogType.Task);
            if (!File.Exists(path))
            {
                ModuleLog($"文件\"{path}\"不存在，无法读取", LogType.Error);
                throw new ModuleException(this, $"文件\"{path}\"不存在，无法读取");
            }
            return File.ReadAllLines(path);
        }
        byte[] IFileAPI.ReadFileAsBinary(string relativePath)
        {
            string path = Path.Combine(HostBot.GetModuleCacheDir(this), relativePath);
            ModuleLog($"正在从\"{Path.GetDirectoryName(path)}\"读取文件\"{Path.GetFileName(path)}\"...", LogType.Task);
            if (!File.Exists(path))
            {
                ModuleLog($"文件\"{path}\"不存在，无法读取", LogType.Error);
                throw new ModuleException(this, $"文件\"{path}\"不存在，无法读取");
            }
            return File.ReadAllBytes(path);
        }
        void IFileAPI.DeleteFile(string relativePath)
        {
            string path = Path.Combine(HostBot.GetModuleCacheDir(this), relativePath);
            ModuleLog($"正在删除文件\"{Path.GetFileName(path)}\"...", LogType.Task);
            if (!File.Exists(path))
                ModuleLog($"文件\"{path}\"不存在，未删除", LogType.Error);
            else
                File.Delete(path);
        }
        Task<Message> IMessagingAPI.GetMessageFromId(long target, long messageId)
            => HostBot.GetMessageFromId(target, messageId);
        Task IMessagingAPI.SendMessage(MessageContextType context, long userId, long groupId, Message msg)
            => HostBot.SendMessage(this, context, userId, groupId, msg);
        async Task IMessagingAPI.ReplyMessage(MessageContext originContext, Message msg)
        {
            //统一Assert
            AssertAttachedStatus(true);
            switch (originContext.Type)
            {
                case MessageContextType.Group:
                    await _hostBot.SendMessage(this, originContext.Type, originContext.UserId, originContext.GroupId, msg);
                    break;
                case MessageContextType.Temp:
                case MessageContextType.Private:
                    await _hostBot.SendMessage(this, originContext.Type, originContext.UserId, originContext.GroupId, msg);
                    break;
            }
        }
        Task IMessagingAPI.SendGroupMessage(long groupId, Message msg)
            => HostBot.SendMessage(this, MessageContextType.Group, -1, groupId, msg);
        Task IMessagingAPI.SendTempMessage(long userId, long groupId, Message msg)
            => HostBot.SendMessage(this, MessageContextType.Group, userId, groupId, msg);
        Task IMessagingAPI.SendPrivateMessage(long userId, Message msg)
            => HostBot.SendMessage(this, MessageContextType.Private, userId, -1, msg);
        [Obsolete]
        Task IMessagingAPI.UploadFile(MessageContextType context, long groupId, string uploadPath, string filePath)
        {
            return HostBot.UploadFile(this, groupId, uploadPath, filePath);
        }
        Task IOperationAPI.Mute(long userId, long groupId, uint durationSeconds)
            => HostBot.Mute(this, userId, groupId, durationSeconds);
        Task IOperationAPI.Unmute(long userId, long groupId)
            => HostBot.Unmute(this, userId, groupId);
        /// <summary>
        /// 返回当前模块的缓存目录绝对路径
        /// </summary>
        public string GetModuleCacheDirAbsolutePath()
            => Path.Combine(Environment.CurrentDirectory, HostBot.GetModuleCacheDir(this));

        /*** 暴露给程序集中其他类的API ***/
        //注册此模块的宿主KLBot和其他信息。这些信息逻辑上应该由调用者（即KLBot实例）传入
        internal void Register(KLBot host, string moduleId)
        {
            HostBot = host ?? throw new ArgumentNullException("附加的目标宿主KLBot为null，因此无法初始化模块");
            IsAttached = true;
            ModuleId = moduleId;
        }
        // 将模块从当前宿主KLBot上分离
        internal void Erase()
        {
            HostBot = null;
            ModuleId = GetType().Name;
            IsAttached = false;
        }
        // 从字典中导入模块属性(ModuleProperty)
        internal void ImportDict(Dictionary<string, JsonNode?> dict, bool ignoreNull = false)
        {
            Type type = GetType();
            foreach (var kvp in dict)
            {
                PropertyInfo? property = type.GetProperty_All(kvp.Key);
                if (property != null)
                {
                    if (!property.CanWrite)
                    {
                        ModuleLog($"配置文件或状态存档中包含模块{ModuleId}中的\"{property.Name}\"字段，但该字段没有set访问器，无法赋值", LogType.Warning);
                        continue;
                    }
                    else if (kvp.Value == null)
                    {
                        if (ignoreNull)
                            continue;
                        ModuleLog($"键值对导入失败: 配置文件中的\"{kvp.Key}\"字段值为null。请修改成非空值", LogType.Error);
                        throw new ModuleSetupException(this, "配置字段中出现null值，此行为不符合模块开发规范");
                    }
                    property.SetValue(this, Convert.ChangeType(kvp.Value, property.PropertyType));
                    continue;
                }
                else
                {
                    if (kvp.Key == null)
                        throw new NullReferenceException("键值对导入失败: 键中意外出现null值");
                    FieldInfo? field = type.GetField_All(kvp.Key);
                    if (field != null)
                    {
                        if (kvp.Value == null)
                        {
                            if (ignoreNull)
                                continue;
                            ModuleLog($"键值对导入失败: 配置文件中的\"{kvp.Key}\"字段值为null。请修改成非空值", LogType.Error);
                            throw new ModuleSetupException(this, "配置字段中出现null值，此行为不符合模块开发规范");
                        }
                        field.SetValue(this, Convert.ChangeType(kvp.Value, field.FieldType));
                        continue;
                    }
                    else
                        ModuleLog($"键值对导入失败: 模块中不存在字段\"{kvp.Key}\"", LogType.Warning);
                }
            }
        }
        // 把模块的所有模块状态导出到字典
        internal Dictionary<string, object?> ExportStatusDict() 
            => ExportMemberWithAttribute(typeof(JsonIncludeAttribute));
        //保存模块的状态
        internal void SaveModuleStatus(bool printInfo = true)
        {
            string json = KLBotJsonHelper.SerializeFile(ExportStatusDict());
            string filePath = HostBot.GetModuleStatusPath(this);
            if (printInfo)
                ModuleLog($"正在保存状态至\"{filePath}\"...", LogType.Task);
            File.WriteAllText(filePath, json);
        }

        //helper 
        /// <summary>
        /// 把模块中的所有含有attribute_type标记的成员导出到字典
        /// </summary>
        private Dictionary<string, object?> ExportMemberWithAttribute(Type attributeType)
        {
            Dictionary<string, object?> propertiesDict = new Dictionary<string, object?>();
            Type type = GetType();
            //export C# properties
            PropertyInfo[] properties = type.GetProperties_All().Where(x => x.GetCustomAttribute(attributeType) != null).ToArray();
            foreach (var property in properties)
            {
                propertiesDict.Add(property.Name, property.GetValue(this));
            }
            //export C# fields
            FieldInfo[] fields = type.GetFields_All().Where(x => x.GetCustomAttribute(attributeType) != null).ToArray();
            foreach (var field in fields)
            {
                propertiesDict.Add(field.Name, field.GetValue(this));
            }
            return propertiesDict;
        }
        // 检查此模块的附加情况是否与预期相同。如果没有将抛出异常
        private void AssertAttachedStatus(bool expected)
        {
            if (expected && !IsAttached)   //期望已经附加但未附加
            {
                throw new Exception("此模块尚未附加到宿主KLBot上，无法完成指定操作");
            }
            else if (!expected && IsAttached)    //期望未附加但已经附加
                throw new Exception("此模块已经附加到宿主KLBot上，无法完成指定操作");
        }
        //处理并调用KLBot回复
        internal async Task<bool> ProcessMessageAndReply(MessageContext context, Message msg)
        {
            if (!Enabled)
                return false;
            AssertAttachedStatus(true); //统一Assert附加情况
            try   //对处理器的异常控制
            {
                DiagData.RestartMeasurement();
                Message? output = await Processor(context, msg);
                DiagData.StopMeasurement();
                if (output == null) // 未处理，退出
                    return false;
                ModuleLog("已处理消息");
                if (output is not MessageEmpty)
                {
                    if (UseSignature)
                        output = new MessagePackage($"[{this}]\n", output);
                    await _hostBot.ReplyMessage(this, context, output);
                    ModuleLog("已调用回复接口.");
                }
                else
                    ModuleLog("任务结束, 无回复内容.");
                //判断模块是否是核心模块。在核心模块的情况下，需要保存全部模块的状态，因为核心模块具有修改其他模块的状态的能力；
                if (GetType().Assembly.Equals(typeof(KLBot).Assembly))
                    _hostBot.ModuleChain.ForEach( x => x.SaveModuleStatus(false));
                //否则可以假设模块只修改自身 所以只需保存自己
                else
                    SaveModuleStatus(false);
                DiagData.ProcessedMessageCount++;
                return true;
            }
            catch (Exception ex)
            {
                DiagData.LastException = ex;
                this.LogError(ex.ToString());
                await _hostBot.ReplyMessage(this, context, $"[KLBot]\n模块{ModuleId}未正确处理消息，已忽略");
                var debugNotice = $"{ModuleId}未正确处理消息：\n\n{ex}";
                foreach (var adminId in HostBot.AdminIds)
                {
                    try
                    {
                        await Messaging.SendPrivateMessage(adminId, debugNotice);
                    }
                    catch (Exception noticeEx)
                    {
                        this.LogError($"模块崩溃信息上报至ID{adminId}时失败：{noticeEx}");
                    }
                }
                return false;
            }
        }

        /// <summary>
        /// ToString()函数：未附加时返回模块名；已附加时返回模块ID
        /// </summary>
        public sealed override string ToString() => IsAttached ? ModuleId : ModuleName;
    }

    /// <summary>
    /// 方便实现只处理单个种类的Message的模块的基类
    /// 如果你的模块只处理单种消息（例如只处理文本消息），继承这玩意可以少写很多类型匹配的废话
    /// </summary>
    /// <typeparam name="T">模块所处理的特定消息类型</typeparam>
    public abstract class SingleTypeModule<T> : Module where T : Message
    {
        /// <summary>
        /// 处理器(Message -> string). 模块通过这个函数处理所有(通过了过滤器的)消息. 
        /// </summary>
        /// <param name="context">待判断消息的上下文信息</param>
        /// <param name="msg">待处理消息</param>
        /// <returns>用字符串表示的处理结果</returns>
        public abstract Task<Message?> Processor(MessageContext context, T msg);

        ///<Inheritdoc/>
        public sealed override async Task<Message?> Processor(MessageContext context, Message msg)
        {
            if (msg is T tmsg)
                return await Processor(context, tmsg);
            return null;
        }
    }
}
