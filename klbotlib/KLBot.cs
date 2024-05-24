using Gleee.Consoleee;
using klbotlib.Exceptions;
using klbotlib.Extensions;
using klbotlib.Json;
using klbotlib.Events;
using klbotlib.MessageDriver.Mirai;
using klbotlib.Modules;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net; // Debug配置下无用，但Release配置下需要
using System.Reflection;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Extensions.Configuration;
using Module = klbotlib.Modules.Module;

namespace klbotlib
{
    /// <summary>
    /// KLBot类。机器人本体
    /// </summary>
    public class KLBot
    {
        private readonly Consoleee _console = new Consoleee();       //扩展控制台对象
        private readonly StringBuilder _sb = new();
        private IMessageDriver _msgDriver;

        /// <summary>
        /// KLBot的模块链条。这个类可以被枚举
        /// </summary>
        public ModuleChain ModuleChain { get; } = new ModuleChain();
        /// <summary>
        /// 当前模块总数，即模块链条的长度
        /// </summary>
        public int ModuleCount { get => ModuleChain.Count; }
        /// <summary>
        /// 此KLBot的统计和诊断信息
        /// </summary>
        public KLBotDiagnosticData DiagData { get; } = new KLBotDiagnosticData();
        /// <summary>
        /// 此KLBot的轮询时间间隔（ms）。默认为250ms。过高的值会造成KLBot反应迟钝；过低的值可能会给mirai服务器造成压力。
        /// </summary>
        public int PollingTimeInterval { get; set; } = 250;
        /// <summary>
        /// 此KLBot的消息循环Flag。设为false时会停止消息循环。
        /// </summary>
        public bool IsLoopOn { get; set; } = false;
        /// <summary>
        /// 是否开启安静模式。开启时ObjectPrint()不打印任何内容
        /// </summary>
        public bool IsSilent { get; } = false;
        /// <summary>
        /// 配置项：此KLBot自身的QQ号
        /// </summary>
        public long SelfId { get; private set; }
        /// <summary>
        /// 配置项：此KLBot的监听群组QQ号列表
        /// </summary>
        public HashSet<long> TargetGroupIDList { get; private set; } = new();
        /// <summary>
        /// 配置项：模块私有目录。用来存取模块自己的自定义文件
        /// </summary>
        public string ModulesCacheDir { get; private set; } = string.Empty;
        /// <summary>
        /// 配置项：模块存档目录。KLBot保存或读取模块配置和模块状态的路径
        /// </summary>
        public string ModulesSaveDir { get; private set; } = string.Empty;
        /// <summary>
        /// 配置项：身份密钥。用于讨mirai服务器开心
        /// </summary>
        public string Key { get; set; } = string.Empty;

        // 最基本的私有构造函数
        private KLBot(IConfiguration config, IMessageDriver driver)
        {
            _console.WriteLn("初始化KLBot...", ConsoleMessageType.Info);
            _msgDriver = driver;
            _msgDriver.OnMessageReceived += MessageHandler;
            _console.WriteLn($"Driver info: {_msgDriver.DriverInfo}", ConsoleMessageType.Info);
            LoadConfig(config);
            //创建模块存档目录（如果不存在）
            CreateDirectoryIfNotExist(ModulesSaveDir, "模块存档目录");
        }
        // 构造并添加目标群组
        private KLBot(IConfiguration config, IMessageDriver driver, ISet<long> targetGroups) : this(config, driver)
        {
            foreach (var group in targetGroups)
            {
                TargetGroupIDList.Add(group);
            }
        }
        /// <summary>
        /// 构造函数。可用于模块开发调试
        /// </summary>
        /// <param name="config">使用的配置</param>
        /// <param name="driver">消息驱动器</param>
        /// <param name="targetGroups">监听的群组</param>
        /// <param name="isSilent">是否开启安静模式。开启时ObjectPrint()不打印任何内容</param>
        /// <param name="moduleCollection">模块合集程序集。此参数仅用于读取程序集版本</param>
        public KLBot(IConfiguration config, IMessageDriver driver, Assembly moduleCollection, ISet<long> targetGroups, bool isSilent = true) : this(config, driver, targetGroups)
        {
            IsSilent = isSilent;
            LoadCoreModule();
            if (moduleCollection != null)
                Info.ModuleCollectionInfo.SetMCVersion(moduleCollection);
            _console.WriteLn(GetListeningGroupListString());
        }
        /// <summary>
        /// 公开构造函数。基本构造后添加默认核心模块
        /// </summary>
        /// <param name="config">使用的配置</param>
        /// <param name="driver">KLBot使用的消息驱动器</param>
        /// <param name="loadCoreModule">是否加载核心模块</param>
        /// <param name="moduleCollection">模块合集程序集。此参数仅用于读取程序集版本</param>
        public KLBot(IConfiguration config, IMessageDriver driver, bool loadCoreModule = true, Assembly? moduleCollection = null) : this(config, driver)
        {
            if (loadCoreModule)
                LoadCoreModule();
            if (moduleCollection != null)
                Info.ModuleCollectionInfo.SetMCVersion(moduleCollection);
        }

        /// <summary>
        /// 加载配置
        /// </summary>
        /// <param name="config">待加载的配置</param>
        public void LoadConfig(IConfiguration config)
        {
            _console.WriteLn("加载配置...", ConsoleMessageType.Info);
            Key = config.ReadValue("key");
            TargetGroupIDList = config.ReadArray("targets").Select(long.Parse).ToHashSet();
            ModulesCacheDir = config.ReadValue("cache_dir");
            ModulesSaveDir = config.ReadValue("save_dir");
        }
        /// <summary>
        /// 加载核心模块
        /// </summary>
        /// <exception cref="KLBotInitializationException"></exception>
        public void LoadCoreModule()
        {
            try
            {
                //加载核心模块
                _console.WriteLn("加载自带核心模块...", ConsoleMessageType.Info);
                AddModule(new CommandModule(this)).Wait();
                _console.WriteLn(GetModuleChainString());
            }
            catch (Exception ex)
            {
                throw new KLBotInitializationException($"核心模块加载失败异常：{ex.Message}\n调用栈：\n{ex.StackTrace}");
            }
        }
        /// <summary>
        /// 把给定群号添加到监听列表
        /// </summary>
        /// <param name="targets">需要添加的群号</param>
        public void AddTarget(params long[] targets)
        {
            foreach (var target in targets)
                TargetGroupIDList.Add(target);
        }
        /// <summary>
        /// 把一组群号批量添加到监听列表
        /// </summary>
        /// <param name="targets">需要添加的群号集合</param>
        public void AddTarget(IEnumerable<long> targets)
        {
            foreach (var target in targets)
                TargetGroupIDList.Add(target);
        }

        // 模块的增加/删除/查询
        /// <summary>
        /// 在控制台列出并打印模块链条
        /// </summary>
        public void ListModules() => _console.WriteLn(GetModuleChainString(), ConsoleMessageType.Info);
        /// <summary>
        /// 根据模块类型获取模块
        /// </summary>
        /// <typeparam name="T">目标模块类型</typeparam>
        /// <param name="index">目标模块的索引</param>
        /// <returns>目标模块实例</returns>
        public T GetModule<T>(int index = 0) where T : Module => ModuleChain.GetModule<T>(index);
        /// <summary>
        /// 已知模块ID，获取模块
        /// </summary>
        /// <param name="moduleId">模块ID</param>
        public Module this[string moduleId] { get => ModuleChain[moduleId]; }
        /// <summary>
        /// 在当前模块链条的末尾手动添加一个或多个新模块
        /// </summary>
        public async Task AddModule(params Module[] modules)
        {
            foreach (var m in modules)
            {
                if (m.IsAttached)
                    m.Erase();
                m.Register(this, ModuleChain.CalcModuleID(m));
                ModuleChain.AddModule(m);
                //为已经加载的每个模块创建缓存目录和存档目录（如果不存在）
                CreateDirectoryIfNotExist(GetModuleCacheDir(m), $"模块{m}的缓存目录");
                //载入模块配置
                await LoadModuleSetup(m);
                await LoadModuleStatus(m);
                _console.WriteLn($"已添加{m.ModuleName}，模块ID为\"{m}\"", ConsoleMessageType.Info);
            }
        }

        //**** 内部API ****//
        //消息
        internal Task<Message> GetMessageFromId(long target, long messageId)
            => _msgDriver.GetMessageFromId(target, messageId);
        /// <summary>
        /// 发送消息
        /// </summary>
        /// <param name="module">编译MsgMarker时使用的模块</param>
        /// <param name="context">发送的消息上下文类型</param>
        /// <param name="userId">用户ID</param>
        /// <param name="groupId">群组ID</param>
        /// <param name="content">待编译MsgMarker文本</param>
        internal async Task SendMessage(Module module, MessageContext context, long userId, long groupId, string content)
            => await _msgDriver.SendMessage(module, context, userId, groupId, content);
        /// <summary>
        /// 发送群消息
        /// </summary>
        /// <param name="module">编译MsgMarker时使用的模块</param>
        /// <param name="groupId">目标群组ID</param>
        /// <param name="content">MsgMarker文本</param>
        internal Task SendGroupMessage(Module module, long groupId, string content)
            => SendMessage(module, MessageContext.Group, -1, groupId, content);
        /// <summary>
        /// 发送临时消息
        /// </summary>
        /// <param name="module">编译MsgMarker时使用的模块</param>
        /// <param name="userId">目标用户ID</param>
        /// <param name="groupId">通过的群组的ID</param>
        /// <param name="content">MsgMarker文本</param>
        internal Task SendTempMessage(Module module, long userId, long groupId, string content)
            => SendMessage(module, MessageContext.Group, userId, groupId, content);
        /// <summary>
        /// 发送私聊消息
        /// </summary>
        /// <param name="module">编译MsgMarker时使用的模块</param>
        /// <param name="userId">目标用户ID</param>
        /// <param name="content">MsgMarker文本</param>
        internal Task SendPrivateMessage(Module module, long userId, string content)
            => SendMessage(module, MessageContext.Group, userId, -1, content);
        /// <summary>
        /// 上传群文件
        /// </summary>
        /// <param name="module">模块</param>
        /// <param name="groupId">群聊ID</param>
        /// <param name="uploadPath">上传的目标路径</param>
        /// <param name="filePath">文件相对于模块私有目录的本地路径</param>
        internal async Task UploadFile(Module module, long groupId, string uploadPath, string filePath)
        {
            await _msgDriver.UploadFile(module, groupId, uploadPath, filePath);
        }
        /// <summary>
        /// 回复消息
        /// </summary>
        /// <param name="module">调用模块</param>
        /// <param name="originMsg">待回复的原始消息</param>
        /// <param name="content">回复内容</param>
        internal async Task ReplyMessage(Module module, Message originMsg, string content)
        {
            if (originMsg is MessageCommon originMsgCommon)
                await SendMessage(module, originMsg.Context, originMsgCommon.SenderID, originMsg.GroupID, content);
            else
            {
                switch (originMsg.Context)
                {
                    case MessageContext.Group:  //群聊特殊消息可以被回复：直接回复至群内
                        await SendMessage(module, MessageContext.Group, -1, originMsg.GroupID, content);
                        return;
                    default:
                        ObjectPrint(module, $"无法回复消息：消息类型为{originMsg.GetType().Name}，上下文为{originMsg.Context}，因此找不到回复对象");
                        return;
                }
            }
        }
        //操作
        /// <summary>
        /// 禁言
        /// </summary>
        /// <param name="module">模块</param>
        /// <param name="userId">禁言用户ID</param>
        /// <param name="groupId">群聊ID</param>
        /// <param name="durationSeconds">禁言时长</param>
        internal async Task Mute(Module module, long userId, long groupId, uint durationSeconds)
        {
            await _msgDriver.Mute(module, userId, groupId, durationSeconds);
        }
        /// <summary>
        /// 取消禁言
        /// </summary>
        /// <param name="module">模块</param>
        /// <param name="userId">取消禁言用户ID</param>
        /// <param name="groupId">群聊ID</param>
        internal async Task Unmute(Module module, long userId, long groupId)
        {
            await _msgDriver.Unmute(module, userId, groupId);
        }

        //暴露给Module类的一些成员
        /// <summary>
        /// 向控制台打印字符串。打印内容会自动包含消息源头的对象的名称
        /// </summary>
        /// <param name="source">消息来源的对象</param>
        /// <param name="message">需要向控制台打印的消息</param>
        /// <param name="msgType">消息类别 分为无、信息、警告、错误、任务. 默认为信息. </param>
        /// <param name="prefix">要在消息类别标识前附加的内容</param>
        public void ObjectPrint(object source, string message, ConsoleMessageType msgType = ConsoleMessageType.Info, string prefix = "")
        {
            if (IsSilent)
                return;
            string sourceName = source is Module m ? m.ModuleID : source.GetType().Name;
            _console.WriteLn($"[{sourceName}] {message}", msgType, prefix);
            _console.Write("> ", ConsoleColor.DarkYellow);
        }
        // 获取模块的私有文件夹路径。按照规范，模块存取自己的文件应使用这个目录
        internal string GetModuleCacheDir(Module module) => Path.Combine(ModulesCacheDir, module.ModuleID);
        // 获取模块的ModuleStatus存档文件路径
        internal string GetModuleStatusPath(Module module) => Path.Combine(ModulesSaveDir, module.ModuleID + "_status.json");
        // 获取模块的ModuleSetup配置文件路径
        internal string GetModuleSetupPath(Module module) => Path.Combine(ModulesSaveDir, module.ModuleID + "_setup.json");

        //消息事件处理
        private async void MessageHandler(object? sender, KLBotMessageEventArgs e)
        {
            Message msg = e.Message;
            // 私聊/临时会话需过滤，范围为目标群组
            if ((msg.Context == MessageContext.Group || msg.Context == MessageContext.Temp) && !TargetGroupIDList.Contains(msg.GroupID))
                goto processed;
            //优先处理所有帮助消息，避免低优先级模块的帮助消息被高优先级模块阻挡
            //另外，帮助消息不计入统计信息
            if (msg is MessagePlain pmsg && pmsg.Text.Trim().EndsWith("帮助"))
            {
                foreach (var module in ModuleChain)
                {
                    if (pmsg.Text.Trim() == module.FriendlyName + "帮助")
                    {
                        await ReplyMessage(module, msg, module.HelpInfo);
                        goto processed;
                    }
                }
            }
            //非帮助信息 遍历尝试处理
            foreach (var module in ModuleChain)
            {
                //模块会直接在一个单独的Task上依次处理并回复
                //防止因为处理或网络速度较慢阻塞其他消息的处理
                bool shouldProcess = await module.AddProcessTask(msg);
                if (shouldProcess)
                {
                    DiagData.ProcessedMessageCount++;
                    DiagData.LastUsedModule = module;
                    if (module.IsTransparent)
                        continue;
                    else
                        goto processed;
                }
            }
        processed:
            e.KLBotProcessed = true;
        }

        /// <summary>
        /// 总循环。包括消息循环和命令循环
        /// </summary>
        public async Task DefaultLoop()
        {
            object sync = new object();
            DiagData.SuccessPackageCount = 0;
            //消息循环线程
            //开始之前向服务器验证身份
            _console.WriteLn("正在向服务器验证身份...", ConsoleMessageType.Task);
            bool verifyResult = await _msgDriver.Verify(Key);
            if (!verifyResult)
            {
                _console.WriteLn("验证失败。请检查密钥和网络是否正确\n正在退出...", ConsoleMessageType.Error);
                Environment.Exit(0);
            }
            else
                _console.WriteLn("验证成功", ConsoleMessageType.Info);
            //获取自身ID
            _console.WriteLn("正在向服务器获取自身ID...", ConsoleMessageType.Task);
            bool getSelfIdSuccess = false;
            while (!getSelfIdSuccess)
            {
                try
                {
                    SelfId = await _msgDriver.GetSelfId();
                    getSelfIdSuccess = true;
                }
                catch (Exception ex)
                {
                    _console.WriteLn($"获取自身ID失败：{ex.Message}. 将重试. \n{ex.StackTrace}");
                }
                Thread.Sleep(1000);
            }
            _console.WriteLn($"获取成功。自身ID：{SelfId}", ConsoleMessageType.Info);
            bool exitFlag = false;
            //命令循环线程
            while (!exitFlag)
            {
                _console.Write("> ", ConsoleColor.DarkYellow);
                string? cmd = _console.ReadLn();
                try
                {
                    if (cmd == "")
                        continue;  //不执行操作
                    else if (cmd == "quit")
                        exitFlag = true;
                    else if (cmd == "status")
                    {
                        _console.WriteLn(GetModuleStatusString(), ConsoleMessageType.Info);
                        _console.WriteLn(DiagData.GetSummaryString(), ConsoleMessageType.Info);
                    }
                    else if (cmd.StartsWith("status "))
                    {
                        string id = cmd[7..];
                        if (!ModuleChain.ContainsModule(id))
                            _console.WriteLn($"找不到ID为\"{id}\"的模块", ConsoleMessageType.Error);
                        else
                            _console.WriteLn(ModuleChain[id].DiagData.GetSummaryString(), ConsoleMessageType.Info);
                    }
                    else if (cmd.StartsWith("enable "))
                    {
                        string id = cmd[7..];
                        if (!ModuleChain.ContainsModule(id))
                            _console.WriteLn($"找不到ID为\"{id}\"的模块", ConsoleMessageType.Error);
                        else
                        {
                            ModuleChain[id].Enabled = true;
                            _console.WriteLn($"成功启用{id}", ConsoleMessageType.Info);
                        }
                    }
                    else if (cmd.StartsWith("disable "))
                    {
                        string id = cmd[8..];
                        if (!ModuleChain.ContainsModule(id))
                            _console.WriteLn($"找不到ID为\"{id}\"的模块", ConsoleMessageType.Error);
                        else
                        {
                            ModuleChain[id].Enabled = false;
                            _console.WriteLn($"成功禁用{id}", ConsoleMessageType.Info);
                        }
                    }
                    else if (cmd == "save")
                    {
                        _console.WriteLn("手动保存所有模块状态到存档...", ConsoleMessageType.Info);
                        ModuleChain.ForEach(async m =>
                        {
                            await SaveModuleStatus(m);
                        });
                    }
                    else if (cmd == "save all")
                    {
                        _console.WriteLn("手动保存所有模块状态和模块配置到存档...", ConsoleMessageType.Info);
                        ModuleChain.ForEach(async m =>
                        {
                            await SaveModuleStatus(m);
#pragma warning disable CS0618 // 类型或成员已过时
                            await SaveModuleSetup(m);
#pragma warning restore CS0618 // 类型或成员已过时
                        });
                    }
                    else if (cmd == "reload")
                    {
                        _console.WriteLn("手动重载所有模块存档...", ConsoleMessageType.Info);
                        ReloadAllModules().Wait();
                        _console.WriteLn("重载已完成", ConsoleMessageType.Info);
                    }
                    else if (cmd == "lasterror")
                        _console.WriteLn($"最近一次错误信息：\n{DiagData.LastException}", ConsoleMessageType.Info);
                    else
                        _console.WriteLn($"未知命令：\"{cmd}\"", ConsoleMessageType.Error);
                }
                catch (Exception ex)
                {
                    _console.WriteLn($"命令执行意外终止：{ex.Message}", ConsoleMessageType.Error);
                }
            }
            //从容退出
            OnExit();
        }

        /// <summary>
        /// 重新载入所有模块配置和状态
        /// </summary>
        public async Task ReloadAllModules()
        {
            await Parallel.ForEachAsync<Module>(ModuleChain, async (module, _) =>
            {
                await LoadModuleSetup(module);
                await LoadModuleStatus(module);
            });
        }
        /// <summary>
        /// 保存该模块的配置
        /// </summary>
        [Obsolete("此方法只用于生成配置文件，正常情况下不应被使用。")]
        public async Task SaveModuleSetup(Module module, bool printInfo = true)
        {
            string json = JsonConvert.SerializeObject(module.ExportSetupDict(), JsonHelper.JsonSettings.FileSetting);
            string filePath = GetModuleSetupPath(module);
            if (printInfo)
                _console.WriteLn($"正在保存模块{module}的配置至\"{filePath}\"...", ConsoleMessageType.Task);
            await File.WriteAllTextAsync(filePath, json);
        }
        /// <summary>
        /// 保存该模块的状态
        /// </summary>
        public async Task SaveModuleStatus(Module module, bool printInfo = true)
        {
            string json = JsonConvert.SerializeObject(module.ExportStatusDict(), JsonHelper.JsonSettings.FileSetting);
            string filePath = GetModuleStatusPath(module);
            if (printInfo)
            {
                //由于涉及并行处理 需要加锁输出
                _console.WriteLn($"正在保存模块{module}的状态至\"{filePath}\"...", ConsoleMessageType.Task);
            }
            await File.WriteAllTextAsync(filePath, json);
        }
        //载入模块的状态
        private async Task LoadModuleStatus(Module module, bool printInfo = true)
        {
            try
            {
                string filePath = GetModuleStatusPath(module);
                if (File.Exists(filePath))
                {
                    if (printInfo)
                        _console.WriteLn($"正在从\"{filePath}\"加载模块{module}的状态...", ConsoleMessageType.Task);
                    var statusDict = JsonConvert.DeserializeObject<Dictionary<string, object>>(await File.ReadAllTextAsync(filePath), JsonHelper.JsonSettings.FileSetting);
                    if (statusDict != null)
                        module.ImportDict(statusDict, true);
                }
            }
            catch (Exception ex)
            {
                throw new ModuleStatusException(module, ex.Message);
            }
        }
        //载入所有模块配置
        private async Task LoadModuleSetup(Module module, bool printInfo = true)
        {
            try
            {
                string filePath = GetModuleSetupPath(module);
                if (File.Exists(filePath))
                {
                    if (printInfo)
                        _console.WriteLn($"正在从\"{filePath}\"加载模块{module.ModuleID}的配置...", ConsoleMessageType.Task);
                    Dictionary<string, object>? result = JsonConvert.DeserializeObject<Dictionary<string, object>>(await File.ReadAllTextAsync(filePath), JsonHelper.JsonSettings.FileSetting);
                    if (result == null)
                        throw new FormatException($"配置文件加载错误：无法将“{filePath}”反序列化为字典");
                    module.ImportDict(result);
                }
                else
                    ObjectPrint(module, $"找不到{module.ModuleID}的模块配置文件，模块将以默认状态启动。对于某些必须使用配置文件初始化的模块，这可能导致问题", ConsoleMessageType.Warning);
            }
            catch (Exception ex)
            {
                throw new ModuleSetupException(module, ex.Message);
            }
        }

        /// <summary>
        /// 有序退出函数
        /// </summary>
        public void OnExit()
        {
            ModuleChain.ForEach(m => SaveModuleStatus(m).Wait());
            _console.WriteLn("有序退出完成", ConsoleMessageType.Info);
        }

        //信息输出相关
        /// <summary>
        /// 返回字符串，其中列出当前模块链条
        /// </summary>
        public string GetModuleChainString()
        {
            _sb.Clear();
            lock (_sb)
            {
                _sb.AppendLine("模块链条：");
                int index = 0;
                ModuleChain.ForEach(module =>
                {
                    if (module.ModuleName == module.FriendlyName)
                        _sb.AppendLine($"  [{index}] {module}");
                    else
                        _sb.AppendLine($"  [{index}] {module}\n      ({module.FriendlyName})");
                    index++;
                });
                return _sb.ToString();
            }
        }
        /// <summary>
        /// 返回字符串，其中列出当前监听群组的列表
        /// </summary>
        public string GetListeningGroupListString()
        {
            _sb.Clear();
            lock (_sb)
            {
                _sb.AppendLine("监听群组列表：");
                int index = 0;
                foreach (var target in TargetGroupIDList)
                {
                    _sb.AppendLine($"  [{index}]  {target}");
                    index++;
                }
                return _sb.ToString();
            }
        }
        /// <summary>
        /// 返回字符串，其中列出当前各模块标记了ModuleStatus的属性值。但是ModuleStatus属性中IsHidden=true的字段会被忽略。
        /// </summary>
        public string GetModuleStatusString()
        {
            _sb.Clear();
            lock (_sb)
            {
                _sb.AppendLine("模块状态：");
                foreach (var module in ModuleChain)
                {
                    _sb.AppendLine($"<{module.ModuleID}>");
                    Type type = module.GetType();
                    List<MemberInfo> members = new List<MemberInfo>();
                    members.AddRange(type.GetProperties_All().Reverse());
                    members.AddRange(type.GetFields_All().Reverse());
                    foreach (var member in members)
                    {
                        if (member.IsNonHiddenModuleStatus())
                        {
                            member.TryGetValue(module, out object? value);  //忽略返回值。因为这个列表100%由PropertyInfo和FieldInfo组成
                            _sb.AppendLine($" {member.Name,-10} = {value}");
                        }
                    }
                }
                return _sb.ToString();
            }
        }

        private void CreateDirectoryIfNotExist(string path, string dirDescription)
        {
            if (!Directory.Exists(path))
            {
                _console.WriteLn($"{dirDescription}\"{path}\"不存在。正在自动创建...", ConsoleMessageType.Warning);
                Directory.CreateDirectory(path);
            }
        }

        //命令循环的状态。分别代表 未开始、等待命令输入、正在输出
        internal enum CmdLoopStatus { NotStarted, ReadLn, Output }
    }
}
