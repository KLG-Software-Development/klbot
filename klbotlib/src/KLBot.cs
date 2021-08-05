using Gleee.Consoleee;
using klbotlib.Exceptions;
using klbotlib.Extensions;
using klbotlib.Internal;
using klbotlib.Json;
using klbotlib.Modules;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Diagnostics;
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
        private bool IsBooting = true;          //返回Bot是否刚刚启动且未处理过任何消息。KLBot用这个flag判断是否正在处理遗留消息，如果是，只处理遗留消息的最后一条。  
        private readonly Consoleee console = new Consoleee();       //扩展控制台对象
        private readonly Dictionary<string, int> module_index_by_id = new Dictionary<string, int>();
        private readonly Dictionary<string, int> module_count_by_name = new Dictionary<string, int>();
        private Task<bool> network_task;
        private readonly List<Module> Modules = new List<Module>(); // 此KLBot的模块链条

        /// <summary>
        /// 当前模块总数，即模块链条的长度
        /// </summary>
        public int ModuleCount { get => Modules.Count; }
        /// <summary>
        /// 此KLBot的统计和诊断信息
        /// </summary>
        public KLBotDiagnosticData DiagData = new KLBotDiagnosticData();
        /// <summary>
        /// 此KLBot的轮询时间间隔（ms）。默认为250ms。过高的值会造成KLBot反应迟钝；过低的值可能会给mirai服务器造成压力。
        /// </summary>
        public int PollingTimeInterval { get; set; } = 250;
        /// <summary>
        /// 此KLBot的消息循环Flag。设为false时会停止消息循环。
        /// </summary>
        public bool IsLoopOn { get; set; } = false;
        /// <summary>
        /// 配置项：此KLBot自身的QQ号
        /// </summary>
        public long SelfID { get; }
        /// <summary>
        /// 配置项：此KLBot的监听群组QQ号列表
        /// </summary>
        public List<long> TargetGroupIDList { get; }
        /// <summary>
        /// 配置项：模块私有目录。用来存取模块自己的自定义文件
        /// </summary>
        public string ModulesCacheDir { get; }
        /// <summary>
        /// 配置项：模块存档目录。KLBot保存或读取模块配置和模块状态的路径
        /// </summary>
        public string ModulesSaveDir { get; }
        /// <summary>
        /// 配置项：模块存档目录。KLBot保存或读取模块配置和模块状态的路径
        /// </summary>
        public string ServerURL { get; }

        /// <summary>
        /// 构造函数
        /// </summary>
        /// <param name="config_path">配置文件路径。默认为"config/config.json"</param>
        public KLBot(string config_path = "config/config.json")
        {
            IsBooting = true;
            try
            {
                network_task = Task<bool>.Run(() =>  true);
                console.WriteLn("初始化KLBot...", ConsoleMessageType.Info);
                console.WriteLn($"正在从\"{config_path}\"读取并解析KLBot配置...", ConsoleMessageType.Info);
                if (!File.Exists(config_path))
                {
                    console.WriteLn($"KLBot配置文件{config_path}不存在", ConsoleMessageType.Error);
                    throw new KLBotInitializationException("KLBot初始化失败：KLBot配置文件不存在");
                }
                JKLBotConfig Config = JsonConvert.DeserializeObject<JKLBotConfig>(File.ReadAllText(config_path));
                if (Config.HasNull(out string field_name))
                {
                    console.WriteLn($"KLBot配置文件解析结果中的{field_name}字段为null。请检查配置文件", ConsoleMessageType.Error);
                    throw new KLBotInitializationException("KLBot初始化失败：KLBot配置文件中含有null");
                }
                console.WriteLn($"加载配置...", ConsoleMessageType.Info);
                //导出Config
                SelfID = Config.QQ.SelfID;
                TargetGroupIDList = Config.QQ.TargetGroupIDList;
                ServerURL = Config.Network.ServerURL;
                ModulesCacheDir = Config.Pathes.ModulesCacheDir;
                ModulesSaveDir = Config.Pathes.ModulesSaveDir;
                //创建模块存档目录（如果不存在）
                CreateDirectoryIfNotExist(Config.Pathes.ModulesSaveDir, "模块存档目录");
                //加载模块
                console.WriteLn("加载自带模块...", ConsoleMessageType.Info);
                new CommandModule(this).AttachTo(this);
                GetModuleChainString();
                Modules.ForEach( module => 
                {
                    CreateDirectoryIfNotExist(Path.Combine(Config.Pathes.ModulesCacheDir, module.ModuleID), $"{module}的缓存目录");
                });
                console.WriteLn($"成功初始化KLBot: ");
                console.WriteLn($"Url: {Config.Network.ServerURL}");
                console.WriteLn(GetListeningGroupListString());
                console.WriteLn(GetModuleChainString());
            }
            catch (Exception ex)
            {
                throw new KLBotInitializationException($"意外异常：{ex.Message}\n调用栈：\n{ex.StackTrace}");
            }
        }

        /// <summary>
        /// 把给定群号添加到监听列表
        /// </summary>
        /// <param name="target">需要添加的群号</param>
        public void AddTarget(params long[] target) => TargetGroupIDList.AddRange(target);
        /// <summary>
        /// 把一组群号批量添加到监听列表
        /// </summary>
        /// <param name="targets">需要添加的群号集合</param>
        public void AddTarget(IEnumerable<long> targets) => TargetGroupIDList.AddRange(targets);

        // 模块的增加/删除/查询
        /// <summary>
        /// 在控制台列出并打印模块链条
        /// </summary>
        public void ListModules() => console.WriteLn(GetModuleChainString(), ConsoleMessageType.Info);
        /// <summary>
        /// 已知模块ID，返回该模块是否存在于模块链条中
        /// </summary>
        /// <param name="module_id">模块ID</param>
        public bool IsModuleExist(string module_id) => module_index_by_id.ContainsKey(module_id);
        // 在当前模块链条的末尾手动添加一个或多个新模块
        internal void AddModule(params Module[] modules)
        {
            foreach (var m in modules)
            {
                module_index_by_id.Add(m.ModuleID, Modules.Count);      //添加ID到ID-索引字典
                if (!module_count_by_name.ContainsKey(m.ModuleName))    //递增模块类型-数量字典
                    module_count_by_name.Add(m.ModuleName, 1);
                else
                    module_count_by_name[m.ModuleName]++;
                Modules.Add(m);
                //为已经加载的每个模块创建缓存目录和存档目录（如果不存在）
                CreateDirectoryIfNotExist(ModulesCacheDir, $"模块{m}的缓存目录");
                //载入模块配置
                LoadModuleSetup(m);
                LoadModuleStatus(m);
                console.WriteLn($"已添加{m.ModuleName}，模块ID为\"{m}\"", ConsoleMessageType.Info);
            }
        }
        /// <summary>
        /// 根据模块类型获取模块
        /// </summary>
        /// <typeparam name="T">目标模块类型</typeparam>
        /// <param name="source">来源对象</param>
        /// <param name="index">目标模块的索引</param>
        /// <returns>目标模块实例</returns>
        public T GetModule<T>(object source, int index = 0) where T : Module
        {
            string id = CalcModuleID(typeof(T).Name, index);
            CheckModuleExist(source, id);
            return this[id] as T;
        }
        /// <summary>
        /// 已知模块ID，返回模块对象
        /// </summary>
        /// <param name="module_id"></param>
        /// <returns></returns>
        public Module this[string module_id]
        {
            get 
            {
                if (!module_index_by_id.ContainsKey(module_id))
                    throw new ModuleMissingException($"模块链条中找不到ID为\"{module_id}\"的模块");
                return Modules[module_index_by_id[module_id]];
            } 
        }
        /// <summary>
        /// 根据模块ID移除模块。不建议使用，因为完全可以只关闭模块的总开关
        /// </summary>
        /// <param name="module_id">要删除的模块ID</param>
        public void RemoveModule(string module_id)
        {
            CheckModuleExist(this, module_id);
            int module_index = module_index_by_id[module_id];
            Modules.RemoveAt(module_index);
            //更新索引字典
            foreach (var kvp in module_index_by_id)
            {
                if (kvp.Value == module_index)
                    module_index_by_id.Remove(kvp.Key);
                else if (kvp.Value > module_index)
                    module_index_by_id[kvp.Key]--;
            }
            console.WriteLn($"已移除ID为\"{module_id}\"的模块", ConsoleMessageType.Info);
        }
        /// <summary>
        /// 允许外部调用回复纯文本消息的接口。纯文本限制规避了MsgMarker编译错误，保证模块输出编译异常不会泄露到KLBot中。
        /// </summary>
        /// <param name="module">调用模块</param>
        /// <param name="origin_msg">待回复的原始消息</param>
        /// <param name="text">回复内容</param>
        public void ReplyPlainMessage(Module module, Message origin_msg, string text)
        {
            string full_json = origin_msg.BuildReplyMessageJson(MsgMarker.CompileMessageChainJson(module, text, true));
            if (!network_task.IsCompleted)
            {
                network_task.ContinueWith((x) =>
                {
                    CheckNetworkTaskResult(x.Result);
                    TryReplyMessage(origin_msg, full_json);
                });
            }
            else
            {
                network_task = Task.Run(() => TryReplyMessage(origin_msg, full_json));
                network_task.ContinueWith(x => CheckNetworkTaskResult(x.Result));
            }
        }

        //暴露给Module类的一些成员
        internal CmdLoopStatus CmdStat = CmdLoopStatus.NotStarted;     //命令循环状态。仅用于ModulePrint方法的实现
        /// <summary>
        /// 向控制台打印字符串。打印内容会自动包含消息源头的对象的名称
        /// </summary>
        /// <param name="source">消息来源的对象</param>
        /// <param name="message">需要向控制台打印的消息</param>
        /// <param name="msg_type">消息类别 分为无、信息、警告、错误、任务. 默认为信息. </param>
        /// <param name="prefix">要在消息类别标识前附加的内容</param>
        public void ObjectPrint(object source, string message, ConsoleMessageType msg_type = ConsoleMessageType.Info, string prefix = "")
        {
            while (CmdStat == CmdLoopStatus.Output)
            { Thread.Sleep(1); }

            string source_name = source is Module m ? m.ModuleID : source.GetType().Name;
            if (CmdStat == CmdLoopStatus.ReadLn)
            {
                console.WriteLn($"[{source_name}] {message}", msg_type, "\b" + prefix);
                console.Write("> ", ConsoleColor.DarkYellow);
            }
            else
                console.WriteLn($"[{source_name}] {message}", msg_type, prefix);
        }
        // 获取模块的私有文件夹路径。按照规范，模块存取自己的文件应使用这个目录
        internal string GetModuleCacheDir(Module module) => Path.Combine(ModulesCacheDir, module.ModuleID);
        // 获取模块的ModuleStatus存档文件路径
        internal string GetModuleStatusPath(Module module) => Path.Combine(ModulesSaveDir, module.ModuleID + "_status.json");
        // 获取模块的ModuleSetup配置文件路径
        internal string GetModuleSetupPath(Module module) => Path.Combine(ModulesSaveDir, module.ModuleID + "_setup.json");
        // 返回KLBot中给定模块类型的模块数量
        internal int GetModuleCountByName(string module_name)
        {
            if (module_count_by_name.ContainsKey(module_name))
                return module_count_by_name[module_name];
            else
                return 0;
        }
        /// <summary>
        /// 将回复纯文本消息的任务加入网络任务队列
        /// </summary>
        /// <param name="origin_msg">要回复的原始消息</param>
        /// <param name="chain">消息链条</param>
        internal void AddReplyMessageTaskWithChain(Message origin_msg, string chain)
        {
            string full_json = origin_msg.BuildReplyMessageJson(chain);
            if (!network_task.IsCompleted)
            {
                network_task.ContinueWith((x) =>
                {
                    CheckNetworkTaskResult(x.Result);
                    TryReplyMessage(origin_msg, full_json);
                });
            }
            else
            {
                network_task = Task.Run(() => TryReplyMessage(origin_msg, full_json));
                network_task.ContinueWith(x => CheckNetworkTaskResult(x.Result));
            }
        }

        /// <summary>
        /// 回复给定消息.
        /// 群组消息(MessageContext.Group)将回复至群组内，临时消息(MessageContext.Temp)和私聊(MessageContext.Private)会回复给发送者.
        /// </summary>
        /// <param name="origin_msg">待回复的原始消息</param>
        /// <param name="msg_json">回复消息json</param>
        internal bool TryReplyMessage(Message origin_msg, string msg_json)
        {
            string url = NetworkHelper.GetSendMessageUrl(ServerURL, origin_msg.Context);
            try
            {
                NetworkHelper.PostJSON(url, msg_json);
                return true;
            }
            catch (Exception ex)
            {
                DiagData.LastException = ex;
                return false;
            }
        }
        /// <summary>
        /// 从服务器获取新消息并进行初步过滤
        /// </summary>
        private List<Message> FetchMessages()
        {
            List<Message> msgs = new List<Message>();
            JFetchMessageResponse obj;
            do
            {
                string response_str = NetworkHelper.FetchMessageListJSON(ServerURL);
                //构建直接JSON对象
                obj = JsonConvert.DeserializeObject<JFetchMessageResponse>(response_str);
                //初步过滤
                var jmsgs = obj.data.Where(x =>
                {
                    if (x.type == "FriendMessage" || x.type == "TempMessage")
                        return true;
                    else if (x.type == "GroupMessage")   //如果是群组消息，还需要群组在监听列表里
                        return TargetGroupIDList.Contains(x.sender.group.id);
                    else return false;
                }).ToList();
                jmsgs.ForEach(jmsg => msgs.Add(MessageFactory.BuildMessage(jmsg)));
            }
            while (obj.data.Count != 0);   //无限轮询直到拿下所有消息
            DiagData.ReceivedMessageCount += msgs.Count;
            return msgs.Where( x => !(x is MessageEmpty)).ToList(); //提前过滤空消息
        }
        /// <summary>
        /// 用默认消息处理函数依次处理消息列表
        /// </summary>
        /// <param name="msgs">待处理消息列表</param>
        /// <returns>已处理的消息数量</returns>
        private void ProcessMessages(List<Message> msgs) => ProcessMessages(msgs, ModulesProcessMessage);
        /// <summary>
        /// 用processor依次处理消息列表。返回非空消息的个数
        /// </summary>
        /// <param name="msgs">待处理消息列表</param>
        /// <param name="main_processor">消息处理函数</param>
        /// <returns>已处理的消息数量</returns>
        private void ProcessMessages(List<Message> msgs, Action<Message> main_processor)
        {
            if (IsBooting && msgs.Count > 1)   //重启时有一条以上遗留消息，则只处理最后一条
            {
                msgs = new List<Message> { msgs.Last() };
                IsBooting = false;
            }
            msgs.ForEach(msg => { main_processor(msg); });
        }
        // 消息循环。轮询获取并处理消息。每次重新获取消息前等待一定时间，等待时间由PollingTimeInterval控制
        private void MsgLoop(ManualResetEvent wait_for_pause_msgLoop_signal)
        {
            long success_counter_cache = 0, continuous_error_counter = 0;
        start:
            IsLoopOn = true;
            Thread.Sleep(500);     //延迟启动 为命令循环线程预留至少0.5s时间
            try
            {
                while (IsLoopOn)
                {
                    ProcessMessages(FetchMessages());
                    DiagData.SuccessPackageCount++;

                    Thread.Sleep(PollingTimeInterval);
                    wait_for_pause_msgLoop_signal.WaitOne();
                }
            }
            catch (Exception ex)
            {
                DiagData.LastException = ex;
                lock (console)  //防止和命令循环线程输出混淆
                {
                    console.WriteLn($"消息循环线程错误: {ex.Message}", ConsoleMessageType.Error, "\n");
                    if (ex is WebException)
                    {
                        console.WriteLn("发生意外网络异常。检查URL是否正确，以及MCL进程是否在服务器上正常运行", ConsoleMessageType.Error);
                        goto quit;
                    }
                    else  //未知异常
                    {
                        if (success_counter_cache == DiagData.SuccessPackageCount)   //sucess_counter距离上次出错之后没有发生变化，意味着本次出错紧接着上一次
                            continuous_error_counter++;
                        else                                         //否则意味着并非基本错误，此时优先保持服务运作，基本错误计数器归零
                            continuous_error_counter = 0;
                        if (continuous_error_counter > 3)
                        {
                            console.WriteLn("连续3次发生致命错误，停止重试", ConsoleColor.DarkYellow);
                            goto quit;
                        }
                        else
                        {
                            success_counter_cache = DiagData.SuccessPackageCount;    //记录此次错误的位置
                            console.WriteLn($"[{DateTime.Now:G}] 正在自动重启消息循环线程...\n", ConsoleMessageType.Warning);
                            console.ClearInputBuffer();
                            console.Write("> ", ConsoleColor.DarkYellow);
                            goto start;
                        }
                    }
                quit:
                    console.WriteLn("[Error]消息循环线程已退出。排查问题后可使用\"start\"命令尝试重启", ConsoleColor.Red);
                    console.ClearInputBuffer();
                    console.Write("> ", ConsoleColor.DarkYellow);
                }
            }
        }
        /// <summary>
        /// 总循环。包括消息循环和命令循环
        /// </summary>
        public void MainLoop()
        {
            object _sync = new object();
            var wait_for_pause_msgLoop_signal = new ManualResetEvent(true);
            DiagData.SuccessPackageCount = 0;
            //消息循环线程
            var msg_loop = Task.Run(() => MsgLoop(wait_for_pause_msgLoop_signal));
            bool exit_flag = false;
            //命令循环线程
            while (!exit_flag)
            {
                console.Write("> ", ConsoleColor.DarkYellow);
                CmdStat = CmdLoopStatus.ReadLn;
                string cmd = console.BufferedReadLn().Trim();
                CmdStat = CmdLoopStatus.Output;
                if (cmd == "")
                    continue;
                else if (cmd == "start")
                {
                    if (!msg_loop.IsCompleted)
                        console.WriteLn("消息循环线程已经在运行中", ConsoleMessageType.Error);
                    else
                    {
                        msg_loop = Task.Run(() => MsgLoop(wait_for_pause_msgLoop_signal));
                        console.WriteLn("成功启动消息循环线程", ConsoleMessageType.Info);
                    }
                }
                else if (cmd == "pause")
                {
                    wait_for_pause_msgLoop_signal.Reset();
                    console.WriteLn("消息循环线程已暂停", ConsoleMessageType.Info);
                }
                else if (cmd == "resume")
                {
                    IsBooting = true;   //为暂停继续情形引入重启忽略机制
                    wait_for_pause_msgLoop_signal.Set();
                    console.WriteLn("消息循环线程已重新开始", ConsoleMessageType.Info);
                }
                else if (cmd == "quit")
                    exit_flag = true;
                else if (cmd == "status")
                {
                    console.WriteLn(GetModuleStatusString(), ConsoleMessageType.Info);
                    console.WriteLn(DiagData.GetSummaryString(), ConsoleMessageType.Info);
                }
                else if (cmd.StartsWith("status "))
                {
                    string id = cmd.Substring(7);
                    if (!module_index_by_id.ContainsKey(id))
                        console.WriteLn($"找不到ID为\"{id}\"的模块", ConsoleMessageType.Error);
                    else
                        console.WriteLn(Modules[module_index_by_id[id]].DiagData.GetSummaryString(), ConsoleMessageType.Info);
                }
                else if (cmd.StartsWith("enable "))
                {
                    string id = cmd.Substring(7);
                    if (!module_index_by_id.ContainsKey(id))
                        console.WriteLn($"找不到ID为\"{id}\"的模块", ConsoleMessageType.Error);
                    else
                    {
                        Modules[module_index_by_id[id]].Enabled = true;
                        console.WriteLn($"成功启用{id}", ConsoleMessageType.Info);
                    }
                }
                else if (cmd.StartsWith("disable "))
                {
                    string id = cmd.Substring(8);
                    if (!module_index_by_id.ContainsKey(id))
                        console.WriteLn($"找不到ID为\"{id}\"的模块", ConsoleMessageType.Error);
                    else
                    {
                        Modules[module_index_by_id[id]].Enabled = false;
                        console.WriteLn($"成功禁用{id}", ConsoleMessageType.Info);
                    }
                }
                else if (cmd == "save")
                {
                    console.WriteLn("手动保存所有模块到存档...", ConsoleMessageType.Info);
                    Modules.ForEach(x =>
                    {
                        SaveModuleStatus(x);
                        #pragma warning disable CS0618
                        SaveModuleSetup(x);
                        #pragma warning restore CS0618
                    });
                }
                else if (cmd == "reload")
                {
                    console.WriteLn("手动重载所有模块存档...", ConsoleMessageType.Info);
                    ReloadAllModules();
                }
                else if (cmd == "lasterror")
                    console.WriteLn($"最近一次错误信息：\n{DiagData.LastException}", ConsoleMessageType.Info);
                else
                    console.WriteLn($"未知命令：\"{cmd}\"", ConsoleMessageType.Error);
            }
            //从容退出
            OnExit();
        }

        //默认处理函数。用Modules中的模块依次尝试处理消息
        //注意 空消息的过滤已经在上一级ProcessMessages()完成，所以此处入参的所有消息均为非空消息
        private void ModulesProcessMessage(Message msg)
        {
            foreach (var module in Modules)
            {
                //if (module.ModuleName == "ImgRecgModule") Debugger.Break();
                //模块会直接在一个单独的Task上依次处理并回复
                //防止因为处理或网络速度较慢阻塞其他消息的处理
                bool should_process = module.AddProcessTask(msg);
                if (should_process)
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
            Modules.ForEach( module => 
            {
                LoadModuleSetup(module);
                LoadModuleStatus(module);
            });
        }
        // 保存该模块的配置
        [Obsolete("此方法只用于生成配置文件，正常情况下不应被使用。")]
        private void SaveModuleSetup(Module module, bool print_info = true)
        {
            string json = JsonConvert.SerializeObject(module.ExportSetupDict(), JsonHelper.JsonSettings.FileSetting);
            string file_path = GetModuleSetupPath(module);
            if (print_info)
                console.WriteLn($"正在保存模块{module}的配置至\"{file_path}\"...", ConsoleMessageType.Task);
            File.WriteAllText(file_path, json);
        }
        //保存模块的状态
        private void SaveModuleStatus(Module module, bool print_info = true)
        {
            string json = JsonConvert.SerializeObject(module.ExportStatusDict(), JsonHelper.JsonSettings.FileSetting);
            string file_path = GetModuleStatusPath(module);
            if (print_info)
            {
                //由于涉及并行处理 需要加锁输出
                console.WriteLn($"正在保存模块{module}的状态至\"{file_path}\"...", ConsoleMessageType.Task);
            }
            File.WriteAllText(file_path, json);
        }
        //载入模块的状态
        private void LoadModuleStatus(Module module, bool print_info = true)
        {
            string file_path = GetModuleStatusPath(module);
            if (File.Exists(file_path))
            {
                if (print_info)
                    console.WriteLn($"正在从\"{file_path}\"加载模块{module}的状态...", ConsoleMessageType.Task);
                module.ImportDict(JsonConvert.DeserializeObject<Dictionary<string, object>>(File.ReadAllText(file_path), JsonHelper.JsonSettings.FileSetting));
            }
        }
        //载入所有模块配置
        private void LoadModuleSetup(Module module, bool print_info = true)
        {
            string file_path = GetModuleSetupPath(module);
            if (File.Exists(file_path))
            {
                if (print_info)
                    console.WriteLnWithLock($"正在从\"{file_path}\"加载模块{module.ModuleID}的配置...", ConsoleMessageType.Task);
                module.ImportDict(JsonConvert.DeserializeObject<Dictionary<string, object>>(File.ReadAllText(file_path), JsonHelper.JsonSettings.FileSetting));
            }
            else
                ObjectPrint(module, $"找不到{module.ModuleID}的模块配置文件，模块将以默认状态启动。对于某些必须使用配置文件初始化的模块，这可能导致问题", ConsoleMessageType.Warning);
        }

        /// <summary>
        /// 有序退出函数
        /// </summary>
        public void OnExit()
        {
            Modules.ForEach( m => SaveModuleStatus(m));
            console.WriteLn("有序退出完成", ConsoleMessageType.Info);
        }

        //信息输出相关
        /// <summary>
        /// 返回字符串，其中列出当前模块链条
        /// </summary>
        public string GetModuleChainString()
        {
            StringBuilder sb = new StringBuilder("模块链条：\n");
            int index = 0;
            Modules.ForEach(module =>
            {
                sb.AppendLine($"  [{index}] {module}");
                index++;
            });
            return sb.ToString();
        }
        /// <summary>
        /// 返回字符串，其中列出当前监听群组的列表
        /// </summary>
        public string GetListeningGroupListString()
        {
            StringBuilder sb = new StringBuilder("监听群组列表:\n");
            int index = 0;
            TargetGroupIDList.ForEach(target_id =>
            {
                sb.AppendLine($"  [{index}]  {target_id}");
                index++;
            });
            return sb.ToString();
        }
        /// <summary>
        /// 返回字符串，其中列出当前各模块标记了ModuleStatus的属性值。但是ModuleStatus属性中IsHidden=true的字段会被忽略。
        /// </summary>
        public string GetModuleStatusString()
        {
            StringBuilder sb = new StringBuilder("模块状态：\n");
            foreach (var module in Modules)
            {
                sb.AppendLine($"<{module.ModuleID}>");
                Type type = module.GetType();
                List<MemberInfo> members = new List<MemberInfo>();
                members.AddRange(type.GetProperties_All().Reverse());
                members.AddRange(type.GetFields_All().Reverse());
                foreach (var member in members)
                {
                    if (member.IsNonHiddenModuleStatus())
                    {
                        member.TryGetValue(module, out object value);  //忽略返回值。因为这个列表100%由PropertyInfo和FieldInfo组成
                        sb.AppendLine($" {member.Name.ToString().PadRight(10)} = {value}");
                    }
                }
            }
            return sb.ToString();
        }

        //计算模块ID的函数
        internal string CalcModuleID(string module_name, int module_index)
        {
            if (module_index == 0)
                return $"{module_name}";
            else
                return $"{module_name}#{module_index}";
        }
        private void CheckModuleExist(object source, string id)
        {
            if (!module_index_by_id.ContainsKey(id))
                throw new ModuleMissingException($"对象\"{source}\"试图引用ID为\"{id}\"的模块，但该模块不存在");
        }
        private void CheckModuleExist<T>(object source, int module_index = 0) where T : Module
        {
            string id = CalcModuleID(typeof(T).Name, module_index);
            if (!module_index_by_id.ContainsKey(id))
                throw new ModuleMissingException($"对象\"{source}\"试图引用ID为\"{id}\"的模块，但该模块不存在");
        }
        private void CreateDirectoryIfNotExist(string path, string dir_description)
        {
            if (!Directory.Exists(path))
            {
                console.WriteLn($"{dir_description}\"{path}\"不存在。正在自动创建...", ConsoleMessageType.Warning);
                Directory.CreateDirectory(path);
            }
        }
        private void CheckNetworkTaskResult(bool result)
        {
            if (!result)   //检查上个发送任务是否正确完成
            {
                ObjectPrint(this, $"消息回复失败：{DiagData.LastException}", ConsoleMessageType.Error, "\b\b");
                console.Write("> ", ConsoleColor.DarkYellow);
            }
        }

        //命令循环的状态。分别代表 未开始、等待命令输入、正在输出
        internal enum CmdLoopStatus { NotStarted, ReadLn, Output }
    }
}
