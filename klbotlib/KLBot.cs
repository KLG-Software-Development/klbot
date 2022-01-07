using Gleee.Consoleee;
using klbotlib.Exceptions;
using klbotlib.Extensions;
using klbotlib.Json;
using klbotlib.MessageServer.Mirai;
using klbotlib.Modules;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Reflection;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Module = klbotlib.Modules.Module;

namespace klbotlib
{
    /// <summary>
    /// KLBot类。机器人本体
    /// </summary>
    public class KLBot
    {
        private bool _isBooting = true;          //返回Bot是否刚刚启动且未处理过任何消息。KLBot用这个flag判断是否正在处理遗留消息，如果是，只处理遗留消息的最后一条。  
        private readonly Consoleee _console = new Consoleee();       //扩展控制台对象
        private readonly StringBuilder _sb = new();
        private CmdLoopStatus _cmdStat = CmdLoopStatus.NotStarted;     //命令循环状态。仅用于ModulePrint方法的实现
        private IMessageServer _msgServer;

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
        public long SelfID { get; }
        /// <summary>
        /// 配置项：此KLBot的监听群组QQ号列表
        /// </summary>
        public HashSet<long> TargetGroupIDList { get; }
        /// <summary>
        /// 配置项：模块私有目录。用来存取模块自己的自定义文件
        /// </summary>
        public string ModulesCacheDir { get; }
        /// <summary>
        /// 配置项：模块存档目录。KLBot保存或读取模块配置和模块状态的路径
        /// </summary>
        public string ModulesSaveDir { get; }

        private KLBot() { }
        /// <summary>
        /// 构造函数。可用于模块开发调试
        /// </summary>
        /// <param name="server">消息服务器</param>
        /// <param name="targetGroups">监听的群组</param>
        /// <param name="selfID">KLBot自身ID。默认为33550336</param>
        /// <param name="isSilent">是否开启安静模式。开启时ObjectPrint()不打印任何内容</param>
        /// <param name="moduleCollection">模块合集程序集。此参数仅用于读取程序集版本</param>
        public KLBot(IMessageServer server, Assembly moduleCollection, List<long> targetGroups, long selfID = 33550336, bool isSilent = true)
        {
            _console.WriteLn("初始化KLBot...", ConsoleMessageType.Info);
            _msgServer = server;
            _isBooting = true;
            IsSilent = isSilent;
            TargetGroupIDList = new();
            SelfID = selfID;
            targetGroups.ForEach(x => TargetGroupIDList.Add(x));
            ModulesCacheDir = "ModuleCacheDir";
            ModulesSaveDir = "ModuleSaveDir";
            CreateDirectoryIfNotExist(ModulesSaveDir, "模块存档目录");
            try
            {
                //加载核心模块
                _console.WriteLn("加载自带核心模块...", ConsoleMessageType.Info);
                AddModule(new CommandModule(this));
                _console.WriteLn(GetModuleChainString());
            }
            catch (Exception ex)
            {
                throw new KLBotInitializationException($"核心模块加载失败异常：{ex.Message}\n调用栈：\n{ex.StackTrace}");
            }
            _console.WriteLn($"成功初始化KLBot: ");
            if (_msgServer is MiraiMessageServer miraiServer)
                _console.WriteLn($"Url: {miraiServer.ServerURL}");
            _console.WriteLn(GetListeningGroupListString());
        }
        /// <summary>
        /// 私有构造函数。最基本的构造函数
        /// </summary>
        /// <param name="configPath">配置文件路径"</param>
        /// <param name="server">KLBot使用的消息服务器</param>
        private KLBot(IMessageServer server, string configPath)
        {
            _msgServer = server;
            _isBooting = true;
            _console.WriteLn("初始化KLBot...", ConsoleMessageType.Info);
            _console.WriteLn($"正在从\"{configPath}\"读取并解析KLBot配置...", ConsoleMessageType.Info);
            try
            {
                if (!File.Exists(configPath))
                {
                    _console.WriteLn($"KLBot配置文件{configPath}不存在", ConsoleMessageType.Error);
                    throw new KLBotInitializationException($"KLBot初始化失败：KLBot配置文件{configPath}不存在");
                }
                JKLBotConfig Config = JsonConvert.DeserializeObject<JKLBotConfig>(File.ReadAllText(configPath));
                if (Config.HasNull(out string field_name))
                {
                    _console.WriteLn($"KLBot配置文件解析结果中的{field_name}字段为null。请检查配置文件", ConsoleMessageType.Error);
                    throw new KLBotInitializationException("KLBot初始化失败：KLBot配置文件中含有null");
                }
                _console.WriteLn($"加载配置...", ConsoleMessageType.Info);
                //导出Config
                SelfID = Config.QQ.SelfID;
                TargetGroupIDList = Config.QQ.TargetGroupIDList;
                ModulesCacheDir = Config.Pathes.ModulesCacheDir;
                ModulesSaveDir = Config.Pathes.ModulesSaveDir;
                //创建模块存档目录（如果不存在）
                CreateDirectoryIfNotExist(Config.Pathes.ModulesSaveDir, "模块存档目录");
                _console.WriteLn($"成功初始化KLBot: ");
                if (_msgServer is MiraiMessageServer miraiServer)
                    _console.WriteLn($"Url: {miraiServer.ServerURL}");
                _console.WriteLn(GetListeningGroupListString());
            }
            catch (Exception ex)
            {
                throw new KLBotInitializationException($"意外异常：{ex.Message}\n调用栈：\n{ex.StackTrace}");
            }
        }
        /// <summary>
        /// 公开构造函数。基本构造后添加默认核心模块
        /// </summary>
        /// <param name="configPath">配置文件路径</param>
        /// <param name="server">KLBot使用的消息服务器</param>
        /// <param name="loadCoreModule">是否加载核心模块</param>
        /// <param name="moduleCollection">模块合集程序集。此参数仅用于读取程序集版本</param>
        public KLBot(IMessageServer server, string configPath = "config/config.json", bool loadCoreModule = true, Assembly moduleCollection = null) : this(server, configPath)
        {
            if (loadCoreModule)
            {
                try
                {
                    //加载核心模块
                    _console.WriteLn("加载自带核心模块...", ConsoleMessageType.Info);
                    AddModule(new CommandModule(this));
                    _console.WriteLn(GetModuleChainString());
                }
                catch (Exception ex)
                {
                    throw new KLBotInitializationException($"核心模块加载失败异常：{ex.Message}\n调用栈：\n{ex.StackTrace}");
                }
            }
            if (moduleCollection != null)
                Info.ModuleCollectionInfo.SetMCVersion(moduleCollection);
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
        public void AddModule(params Module[] modules)
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
                LoadModuleSetup(m);
                LoadModuleStatus(m);
                _console.WriteLn($"已添加{m.ModuleName}，模块ID为\"{m}\"", ConsoleMessageType.Info);
            }
        }

        //消息内部API
        internal Message GetMessageFromID(long id)
            => _msgServer.GetMessageFromID(id);
        /// <summary>
        /// 发送消息
        /// </summary>
        /// <param name="module">编译MsgMarker时使用的模块</param>
        /// <param name="context">发送的消息上下文类型</param>
        /// <param name="userId">用户ID</param>
        /// <param name="groupId">群组ID</param>
        /// <param name="content">待编译MsgMarker文本</param>
        internal void SendMessage(Module module, MessageContext context, long userId, long groupId, string content)
            => _msgServer.SendMessage(module, context, userId, groupId, content);
        /// <summary>
        /// 发送群消息
        /// </summary>
        /// <param name="module">编译MsgMarker时使用的模块</param>
        /// <param name="groupId">目标群组ID</param>
        /// <param name="content">MsgMarker文本</param>
        internal void SendGroupMessage(Module module, long groupId, string content)
            => SendMessage(module, MessageContext.Group, -1, groupId, content);
        /// <summary>
        /// 发送临时消息
        /// </summary>
        /// <param name="module">编译MsgMarker时使用的模块</param>
        /// <param name="userId">目标用户ID</param>
        /// <param name="groupId">通过的群组的ID</param>
        /// <param name="content">MsgMarker文本</param>
        internal void SendTempMessage(Module module, long userId, long groupId, string content)
            => SendMessage(module, MessageContext.Group, userId, groupId, content);
        /// <summary>
        /// 发送私聊消息
        /// </summary>
        /// <param name="module">编译MsgMarker时使用的模块</param>
        /// <param name="userId">目标用户ID</param>
        /// <param name="content">MsgMarker文本</param>
        internal void SendPrivateMessage(Module module, long userId, string content)
            => SendMessage(module, MessageContext.Group,  userId, -1, content);
        /// <summary>
        /// 上传群文件
        /// </summary>
        /// <param name="module">模块</param>
        /// <param name="groupId">群聊ID</param>
        /// <param name="uploadPath">上传的目标路径</param>
        /// <param name="filePath">文件相对于模块私有目录的本地路径</param>
        [Obsolete("该方法仍有问题")]
        internal void UploadFile(Module module, long groupId, string uploadPath, string filePath)
        {
            _msgServer.UploadFile(module, groupId, uploadPath, filePath);
        } 
        /// <summary>
        /// 回复消息
        /// </summary>
        /// <param name="module">调用模块</param>
        /// <param name="originMsg">待回复的原始消息</param>
        /// <param name="content">回复内容</param>
        internal void ReplyMessage(Module module, Message originMsg, string content)
        {
            if (originMsg is MessageCommon originMsgCommon)
                SendMessage(module, originMsg.Context, originMsgCommon.SenderID, originMsg.GroupID, content);
            else
            {
                switch (originMsg.Context)
                {
                    case MessageContext.Group:  //群聊特殊消息可以被回复：直接回复至群内
                        SendMessage(module, MessageContext.Group, -1, originMsg.GroupID, content);
                        return;
                    default:
                        ObjectPrint(module, $"无法回复消息：消息类型为{originMsg.GetType().Name}，上下文为{originMsg.Context}，因此找不到回复对象");
                        return;
                }
            }
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
            while (_cmdStat == CmdLoopStatus.Output)
            { Thread.Sleep(1); }

            string source_name = source is Module m ? m.ModuleID : source.GetType().Name;
            if (_cmdStat == CmdLoopStatus.ReadLn)
            {
                _console.WriteLn($"[{source_name}] {message}", msgType, "\b" + prefix);
                _console.Write("> ", ConsoleColor.DarkYellow);
            }
            else
                _console.WriteLn($"[{source_name}] {message}", msgType, prefix);
        }
        // 获取模块的私有文件夹路径。按照规范，模块存取自己的文件应使用这个目录
        internal string GetModuleCacheDir(Module module) => Path.Combine(ModulesCacheDir, module.ModuleID);
        // 获取模块的ModuleStatus存档文件路径
        internal string GetModuleStatusPath(Module module) => Path.Combine(ModulesSaveDir, module.ModuleID + "_status.json");
        // 获取模块的ModuleSetup配置文件路径
        internal string GetModuleSetupPath(Module module) => Path.Combine(ModulesSaveDir, module.ModuleID + "_setup.json");

        //消息获取和处理相关
        /// <summary>
        /// 从服务器获取新消息并进行初步过滤
        /// </summary>
        public List<Message> FetchMessages()
        {
            DiagData.SuccessPackageCount++;
            //过滤掉非监听群消息
            List<Message> msgs = _msgServer.FetchMessages().Where(msg => 
            msg.Context != MessageContext.Group || msg.Context != MessageContext.Temp   //非私聊、非临时会话时无需过滤
            || TargetGroupIDList.Contains(msg.GroupID)).ToList();   //私聊、临时会话时要求消息来自属于监听群之一
            DiagData.ReceivedMessageCount += msgs.Count;
            return msgs;
        }
        /// <summary>
        /// 用默认消息处理函数依次处理消息列表
        /// </summary>
        /// <param name="msgs">待处理消息列表</param>
        /// <returns>已处理的消息数量</returns>
        public void ProcessMessages(List<Message> msgs) => ProcessMessages(msgs, ModulesProcessMessage);
        /// <summary>
        /// 用processor依次处理消息列表。返回非空消息的个数
        /// </summary>
        /// <param name="msgs">待处理消息列表</param>
        /// <param name="mainProcessor">消息处理函数</param>
        /// <returns>已处理的消息数量</returns>
        public void ProcessMessages(List<Message> msgs, Action<Message> mainProcessor)
        {
            if (_isBooting && msgs.Count > 1)   //重启时有一条以上遗留消息，则只处理最后一条
            {
                msgs = new List<Message> { msgs.Last() };
                _isBooting = false;
            }
            msgs.ForEach(msg => { mainProcessor(msg); });
        }
        // 消息循环。轮询获取并处理消息。每次重新获取消息前等待一定时间，等待时间由PollingTimeInterval控制
        private void MsgLoop(ManualResetEvent waitForPauseMsgLoopSignal)
        {
            long successCounterCache = 0, continuousErrorCounter = 0;
            bool isLoopRestarting = true;
        start:
            IsLoopOn = true;
            Thread.Sleep(500);     //延迟启动 为命令循环线程预留至少0.5s时间
            try
            {
                while (IsLoopOn)
                {
                    List<Message> msgs = FetchMessages();
                    if (isLoopRestarting)
                    {
                        if (msgs.Count != 0)
                            ProcessMessages(new List<Message> { msgs.Last() });
                        isLoopRestarting = false;
                    }
                    else
                        ProcessMessages(msgs);
                    Thread.Sleep(PollingTimeInterval);
                    waitForPauseMsgLoopSignal.WaitOne();
                }
            }
            catch (Exception ex)
            {
                DiagData.LastException = ex;
                lock (_console)  //防止和命令循环线程输出混淆
                {
                    _console.WriteLn($"消息循环线程错误: {ex.Message}", ConsoleMessageType.Error, "\n");
                    if (ex is WebException)
                    {
                        _console.WriteLn("发生意外网络异常。检查URL是否正确，以及MCL进程是否在服务器上正常运行。六秒后将重试", ConsoleMessageType.Error);
                        Thread.Sleep(6000);
                        isLoopRestarting = true;
                        goto start;
                    }
                    else  //未知异常
                    {
                        _console.WriteLn($"调用栈：\n{ex.StackTrace}");
                        if (successCounterCache == DiagData.SuccessPackageCount)   //sucess_counter距离上次出错之后没有发生变化，意味着本次出错紧接着上一次
                            continuousErrorCounter++;
                        else                                         //否则意味着并非基本错误，此时优先保持服务运作，基本错误计数器归零
                            continuousErrorCounter = 0;
                        if (continuousErrorCounter > 3)
                        {
                            _console.WriteLn("连续3次发生致命错误，停止重试", ConsoleColor.DarkYellow);
                            goto quit;
                        }
                        else
                        {
                            successCounterCache = DiagData.SuccessPackageCount;    //记录此次错误的位置
                            _console.WriteLn($"[{DateTime.Now:G}] 正在自动重启消息循环线程...\n", ConsoleMessageType.Warning);
                            _console.ClearInputBuffer();
                            _console.Write("> ", ConsoleColor.DarkYellow);
                            isLoopRestarting = true;
                            goto start;
                        }
                    }
                quit:
                    _console.WriteLn("[Error]消息循环线程已退出。排查问题后可使用\"start\"命令尝试重启", ConsoleColor.Red);
                    _console.ClearInputBuffer();
                    _console.Write("> ", ConsoleColor.DarkYellow);
                }
            }
        }
        /// <summary>
        /// 总循环。包括消息循环和命令循环
        /// </summary>
        public void DefaultLoop()
        {
            object sync = new object();
            var waitForPauseMsgLoopSignal = new ManualResetEvent(true);
            DiagData.SuccessPackageCount = 0;
            //消息循环线程
            var msgLoop = Task.Run(() => MsgLoop(waitForPauseMsgLoopSignal));
            bool exitFlag = false;
            //命令循环线程
            while (!exitFlag)
            {
                _console.Write("> ", ConsoleColor.DarkYellow);
                _cmdStat = CmdLoopStatus.ReadLn;
                string cmd = _console.BufferedReadLn().Trim();
                _cmdStat = CmdLoopStatus.Output;
                try
                {
                    if (cmd == "")
                        continue;  //不执行操作
                    else if (cmd == "start")
                    {
                        if (!msgLoop.IsCompleted)
                            _console.WriteLn("消息循环线程已经在运行中", ConsoleMessageType.Error);
                        else
                        {
                            msgLoop = Task.Run(() => MsgLoop(waitForPauseMsgLoopSignal));
                            _console.WriteLn("成功启动消息循环线程", ConsoleMessageType.Info);
                        }
                    }
                    else if (cmd == "pause")
                    {
                        waitForPauseMsgLoopSignal.Reset();
                        _console.WriteLn("消息循环线程已暂停", ConsoleMessageType.Info);
                    }
                    else if (cmd == "resume")
                    {
                        _isBooting = true;   //为暂停继续情形引入重启忽略机制
                        waitForPauseMsgLoopSignal.Set();
                        _console.WriteLn("消息循环线程已重新开始", ConsoleMessageType.Info);
                    }
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
                        ModuleChain.ForEach(x =>
                        {
                            SaveModuleStatus(x);
                        });
                    }
                    else if (cmd == "save all")
                    {
                        _console.WriteLn("手动保存所有模块状态和模块配置到存档...", ConsoleMessageType.Info);
                        ModuleChain.ForEach(x =>
                        {
                            SaveModuleStatus(x);
#pragma warning disable CS0618 // 类型或成员已过时
                            SaveModuleSetup(x);
#pragma warning restore CS0618 // 类型或成员已过时
                        });
                    }
                    else if (cmd == "reload")
                    {
                        _console.WriteLn("手动重载所有模块存档...", ConsoleMessageType.Info);
                        ReloadAllModules();
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

        //默认处理函数。用Modules中的模块依次尝试处理消息
        //注意 空消息的过滤已经在上一级ProcessMessages()完成，所以此处入参的所有消息均为非空消息
        private void ModulesProcessMessage(Message msg)
        {
            //优先处理所有帮助消息，避免低优先级模块的帮助消息被高优先级模块阻挡
            //另外，帮助消息不计入统计信息
            if (msg is MessagePlain pmsg && pmsg.Text.Trim().EndsWith("帮助"))
            {
                foreach (var module in ModuleChain)
                {
                    if (pmsg.Text.Trim() == module.FriendlyName + "帮助")
                    {
                        ReplyMessage(module, msg, module.HelpInfo);
                        return;
                    }
                }
            }
            //非帮助信息 遍历尝试处理
            foreach (var module in ModuleChain)
            {
                //模块会直接在一个单独的Task上依次处理并回复
                //防止因为处理或网络速度较慢阻塞其他消息的处理
                bool shouldProcess = module.AddProcessTask(msg);
                if (shouldProcess)
                {
                    DiagData.ProcessedMessageCount++;
                    DiagData.LastUsedModule = module;
                    if (module.IsTransparent)
                        continue;
                    else
                        return;
                }
            }
        }

        /// <summary>
        /// 重新载入所有模块配置和状态
        /// </summary>
        public void ReloadAllModules()
        {
            ModuleChain.ForEach( module => 
            {
                LoadModuleSetup(module);
                LoadModuleStatus(module);
            });
        }
        // 保存该模块的配置
        [Obsolete("此方法只用于生成配置文件，正常情况下不应被使用。")]
        private void SaveModuleSetup(Module module, bool printInfo = true)
        {
            string json = JsonConvert.SerializeObject(module.ExportSetupDict(), JsonHelper.JsonSettings.FileSetting);
            string file_path = GetModuleSetupPath(module);
            if (printInfo)
                _console.WriteLn($"正在保存模块{module}的配置至\"{file_path}\"...", ConsoleMessageType.Task);
            File.WriteAllText(file_path, json);
        }
        //保存模块的状态
        private void SaveModuleStatus(Module module, bool printInfo = true)
        {
            string json = JsonConvert.SerializeObject(module.ExportStatusDict(), JsonHelper.JsonSettings.FileSetting);
            string file_path = GetModuleStatusPath(module);
            if (printInfo)
            {
                //由于涉及并行处理 需要加锁输出
                _console.WriteLn($"正在保存模块{module}的状态至\"{file_path}\"...", ConsoleMessageType.Task);
            }
            File.WriteAllText(file_path, json);
        }
        //载入模块的状态
        private void LoadModuleStatus(Module module, bool printInfo = true)
        {
            try
            {
                string file_path = GetModuleStatusPath(module);
                if (File.Exists(file_path))
                {
                    if (printInfo)
                        _console.WriteLn($"正在从\"{file_path}\"加载模块{module}的状态...", ConsoleMessageType.Task);
                    var status_dict = JsonConvert.DeserializeObject<Dictionary<string, object>>(File.ReadAllText(file_path), JsonHelper.JsonSettings.FileSetting);
                    if (status_dict != null)
                        module.ImportDict(status_dict);
                }
            }
            catch (Exception ex)
            {
                throw new ModuleStatusException(module, ex.Message);
            }
        }
        //载入所有模块配置
        private void LoadModuleSetup(Module module, bool printInfo = true)
        {
            try
            {
                string filePath = GetModuleSetupPath(module);
                if (File.Exists(filePath))
                {
                    if (printInfo)
                        _console.WriteLnWithLock($"正在从\"{filePath}\"加载模块{module.ModuleID}的配置...", ConsoleMessageType.Task);
                    module.ImportDict(JsonConvert.DeserializeObject<Dictionary<string, object>>(File.ReadAllText(filePath), JsonHelper.JsonSettings.FileSetting));
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
            ModuleChain.ForEach( m => SaveModuleStatus(m));
            _console.WriteLn("有序退出完成", ConsoleMessageType.Info);
        }

        //信息输出相关
        /// <summary>
        /// 返回字符串，其中列出当前模块链条
        /// </summary>
        public string GetModuleChainString()
        {
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
                            member.TryGetValue(module, out object value);  //忽略返回值。因为这个列表100%由PropertyInfo和FieldInfo组成
                            _sb.AppendLine($" {member.Name,-10} = {value}");
                        }
                    }
                }
                return _sb.ToString();
            }
        }

        private void CreateDirectoryIfNotExist(string path, string dir_description)
        {
            if (!Directory.Exists(path))
            {
                _console.WriteLn($"{dir_description}\"{path}\"不存在。正在自动创建...", ConsoleMessageType.Warning);
                Directory.CreateDirectory(path);
            }
        }

        //命令循环的状态。分别代表 未开始、等待命令输入、正在输出
        internal enum CmdLoopStatus { NotStarted, ReadLn, Output }
    }
}
