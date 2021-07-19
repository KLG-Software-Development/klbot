using Gleee.Consoleee;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;

namespace klbotlib.Modules
{
    /// <summary>
    /// 消息处理模块基类.
    /// 这是KLBot功能实现的基本单位
    /// </summary>
    /// <typeparam name="T">该模块处理的消息类型。必须继承自Message类</typeparam>
    public abstract class Module
    {
        /// <summary>
        /// 模块ID. 是模块的唯一标识. 直接等于模块在源码中的类型名称.
        /// </summary>
        public string ModuleID { get => GetType().Name; }
        /// <summary>
        /// 决定此模块是否是透明模块(默认为否).
        /// 透明模块处理消息之后会继续向后传递，以使得Bot内部在它之后的模块能继续处理这条消息.
        /// 非透明模块处理消息之后会销毁消息.
        /// </summary>
        public virtual bool IsTransparent { get; } = false;
        /// <summary>
        /// 过滤器(Message -> bool). 模块通过这个函数判断是否要处理某一条消息. 
        /// 当模块总开关开启时，结果为true的消息会被处理，结果为false的函数会忽略.
        /// </summary>
        /// <param name="msg">待判断消息</param>
        public abstract bool Filter(Message msg);
        /// <summary>
        /// 处理器(Message -> string). 模块通过这个函数处理所有(通过了过滤器的)消息. 
        /// </summary>
        /// <param name="msg">待处理消息</param>
        /// <returns>用字符串表示的处理结果. 如果你的命令不输出处理结果，返回null或空字符串</returns>
        public abstract string Processor(Message msg);
        /// <summary>
        /// 综合过滤器和开关的影响, 返回一条消息是否应被处理
        /// </summary>
        /// <param name="msg">待判断消息</param>
        public bool ShouldProcess(Message msg) => Enabled && Filter(msg);
        /// <summary>
        /// 模块所属的Bot
        /// </summary>
        public KLBot HostBot { get; }

        /// <summary>
        /// 模块的总开关. 默认开启. 此开关关闭时任何消息都会被忽略.
        /// </summary>
        [ModuleStatus]    //模块属性Attribute. 只有打上这个标记的属性能被Module.ImportPropertiesDict()和Module.ExportPropertiesDict()读取或保存.
        public bool Enabled { get; set; } = true;

        public Module(KLBot host_bot)
        {
            HostBot = host_bot;
        }

        //配置和状态的存读档
        /// <summary>
        /// 从字典中导入模块属性(ModuleProperty)
        /// </summary>
        /// <param name="status_dict">要导入的属性字典</param>
        public void ImportDict(Dictionary<string, object> status_dict)
        {
            Type type = GetType();
            foreach (var kvp in status_dict)
            {
                PropertyInfo property = type.GetProperty(kvp.Key);
                if (property != null)
                {
                    if (!property.CanWrite)
                        HostBot.ModulePrint(this, $"\"{property.Name}\"字段被标记为[ModuleStatus]或[ModuleSetup]，但没有set访问器。请为该字段添加private set访问器", ConsoleMessageType.Warning, "  |  ");
                    else
                        property.SetValue(this, RestoreType(property.PropertyType, kvp.Value));
                    continue;
                }
                else
                {
                    FieldInfo field = type.GetField(kvp.Key);
                    if (field != null)
                    {
                        field.SetValue(this, RestoreType(field.FieldType, kvp.Value));
                        continue;
                    }
                    else throw new Exception($"Failed to serialize: property '{kvp.Key}' does not exist in module '{type.Name}'");
                }
            }
        }
        /// <summary>
        /// 把模块的所有模块状态(ModuleStatus)导出到字典
        /// </summary>
        public Dictionary<string, object> ExportStatusDict() => ExportMemberWithAttribute(typeof(ModuleStatusAttribute));
        /// <summary>
        /// 把模块的所有模块配置(ModuleStatus)导出到字典
        /// </summary>
        public Dictionary<string, object> ExportSetupDict() => ExportMemberWithAttribute(typeof(ModuleSetupAttribute));


        //存读模块自定义文件
        /// <summary>
        /// 保存文本到模块自定义文件
        /// </summary>
        /// <param name="relative_path">保存文件对模块文件夹的相对路径</param>
        /// <param name="text">保存的内容</param>
        public void SaveFile(string relative_path, string text)
        {
            string path = Path.Combine(HostBot.GetModulePrivateDir(this), relative_path);
            HostBot.ModulePrint(this, $"Saving \"{Path.GetFileName(path)}\" to \"{Path.GetDirectoryName(path)}\"...", ConsoleMessageType.Task);
            if (File.Exists(path))
                HostBot.ModulePrint(this, $"文件\"{path}\"已经存在，将直接覆盖", ConsoleMessageType.Warning);
            File.WriteAllText(path, text);
        }

        //helper 
        /// <summary>
        /// NewtonSoft.JsonConvert会把一切整数变成int64，一切浮点数变成double
        /// 丫这么整虽然源码赋值没事(会自动转换)，但反射赋值时会出问题，所以需要手动恢复原本的类型
        /// </summary>
        /// <param name="original_type">原始类型</param>
        /// <param name="value">待处理对象</param>
        /// <returns>转换为原始类型后的对象（如果无需转换则原样返回）</returns>
        private object RestoreType(Type original_type, object value)
        {
            if (original_type == typeof(byte) ||
                original_type == typeof(short) ||
                original_type == typeof(int) ||
                original_type == typeof(float))
                return Convert.ChangeType(value, original_type);
            else return value;
        }
        /// <summary>
        /// 把模块中的所有含有attribute_type标记的成员导出到字典
        /// </summary>
        private Dictionary<string, object> ExportMemberWithAttribute(Type attribute_type)
        {
            Dictionary<string, object> properties_dict = new Dictionary<string, object>();
            Type type = GetType();
            //export C# properties
            PropertyInfo[] properties = type.GetProperties(BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Static | BindingFlags.Public).Where(x => x.GetCustomAttribute(attribute_type) != null).ToArray();
            foreach (var property in properties)
            {
                properties_dict.Add(property.Name, property.GetValue(this));
            }
            //export C# fields
            FieldInfo[] fields = type.GetFields(BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Static | BindingFlags.Public).Where(x => x.GetCustomAttribute(attribute_type) != null).ToArray();
            foreach (var field in fields)
            {
                properties_dict.Add(field.Name, field.GetValue(this));
            }
            return properties_dict;
        }
    }

    /// <summary>
    /// 方便实现只处理单个种类的Message的模块的基类
    /// 如果你的模块只处理单种消息（例如只处理文本消息），继承这玩意可以少写很多类型匹配的废话
    /// </summary>
    /// <typeparam name="T">模块所处理的特定消息类型</typeparam>
    public abstract class SingleTypeModule<T> : Module where T : Message
    {
        /// <summary>
        /// 过滤器(Message -> bool). 模块通过这个函数判断是否要处理某一条消息. 
        /// 当模块总开关开启时，结果为true的消息会被处理，结果为false的函数会忽略.
        /// </summary>
        /// <param name="msg">待判断消息</param>
        public abstract bool Filter(T msg);
        /// <summary>
        /// 处理器(Message -> string). 模块通过这个函数处理所有(通过了过滤器的)消息. 
        /// </summary>
        /// <param name="msg">待处理消息</param>
        /// <returns>用字符串表示的处理结果</returns>
        public abstract string Processor(T msg);

        public override bool Filter(Message msg)
        {
            if (msg is T tmsg)
                return Filter(tmsg);
            else
                return false;
        }
        public override string Processor(Message msg)
        {
            if (msg is T tmsg)
                return Processor(tmsg);
            else
            {
                HostBot.ModulePrint(this, "意外遇到无法处理的消息类型", ConsoleMessageType.Error);
                throw new Exception("意外遇到无法处理的消息类型");
            }
        }

        public SingleTypeModule(KLBot host_bot) : base(host_bot) { }
    }
}
