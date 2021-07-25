﻿using Gleee.Consoleee;
using klbotlib.Exceptions;
using klbotlib.Internal;
using klbotlib.Modules;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Net;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Web;

namespace klbotlib
{
    public class KLBot
    {
        private bool IsBooting { get; set; } = true;                //返回Bot是否刚刚启动且未处理过任何消息
        private readonly Consoleee console = new Consoleee();       //扩展控制台对象
        private readonly Dictionary<string, int> module_index_by_id = new Dictionary<string, int>();
        private readonly Dictionary<string, uint> module_count = new Dictionary<string, uint>();

        //保存有一些性能和诊断信息
        public BotDiagnosticData DiagData = new BotDiagnosticData();  
        //所有模块
        public List<Module> Modules { get; } = new List<Module>();
        //Bot配置
        public BotConfig Config { get; }
        //Bot属性
        public int PollingTimeInterval { get; set; } = 500;   //轮询时间区间
        public bool IsLoopOn = false;   //总开关，决定是否继续消息循环

        //构造函数。默认配置文件路径config/config.json
        public KLBot(string config_path = "config/config.json")
        {
            console.WriteLn("初始化KLBot...", ConsoleMessageType.Info);
            //Config = new BotConfig("http://192.168.31.42", 3205508672, new long[] { 727414436 }, "cache/modules", "internal/modules");  //平滑升级用
            //File.WriteAllText(config_path, JsonConvert.SerializeObject(Config, JsonHelper.FileSetup));
            console.WriteLn($"正在从\"{config_path}\"读取并解析KLBot配置...", ConsoleMessageType.Info);
            if (!File.Exists(config_path))
            {
                console.WriteLn($"配置文件{config_path}不存在", ConsoleMessageType.Error);
                goto init_failed;
            }
            Config = JsonConvert.DeserializeObject<BotConfig>(File.ReadAllText(config_path));
            File.WriteAllText(config_path, JsonConvert.SerializeObject(Config, Json.JsonHelper.FileSetup));
            if (Config.HasNull(out string field_name))
            {
                console.WriteLn($"解析结果中的{field_name}字段为null。请检查配置文件以确保这是预期行为", ConsoleMessageType.Error);
                goto init_failed;
            }
            //创建模块存档目录（如果不存在）
            CreateDirectoryIfNotExist(Config.Pathes.ModulesSaveDir, "模块存档目录");
            //加载模块
            console.WriteLn("加载自带模块...", ConsoleMessageType.Info);
            AddModule(new CommandModule(this), new FuckModule(this), new ChatQYKModule(this));
            GetModuleListString();
            //SaveAllModuleSetup();   //平滑升级用
            console.WriteLn($"载入模块配置", ConsoleMessageType.Info);
            console.WriteLn($"成功初始化KLBot: ");
            console.WriteLn($"Url: {Config.Network.ServerURL}");
            console.WriteLn(GetListeningGroupListString());
            console.WriteLn(GetModuleListString());
            return;
        init_failed:
            throw new KLBotInitializationException("KLBot初始化失败");
        }

        /// <summary>
        /// 把给定群号添加到监听列表
        /// </summary>
        /// <param name="target">需要添加的群号</param>
        public void AddTarget(long target) => Config.QQ.TargetGroupIDList.Add(target);
        /// <summary>
        /// 把一组群号批量添加到监听列表
        /// </summary>
        /// <param name="targets">需要添加的群号集合</param>
        public void AddTarget(IEnumerable<long> targets) => Config.QQ.TargetGroupIDList.AddRange(targets);

        // 模块的增加/删除/查询
        //列出并打印所有模块
        public void ListModules() => console.WriteLn(GetModuleListString(), ConsoleMessageType.Info);
        //在当前模块列表末尾添加模块
        public void AddModule(params Module[] modules)
        {
            foreach (var m in modules)
            {
                string id;
                if (!module_index_by_id.ContainsKey(m.ModuleName))     //该模块类型的首个模块实例
                {
                    id = m.ModuleName;      //首个实例省略索引[0]
                    module_count.Add(m.ModuleName, 1);
                }
                else    //非首个实例
                {
                    //用module_count当前的值计算id
                    id = CalcModuleID(m.ModuleName, module_count[m.ModuleName]);
                    module_count[m.ModuleName]++;
                }
                m.ModuleID = id;
                module_index_by_id.Add(id, Modules.Count);
                Modules.Add(m);
                //为已经加载的每个模块创建缓存目录和存档目录（如果不存在）
                CreateDirectoryIfNotExist(Config.Pathes.ModulesCacheDir, $"模块{m}的缓存目录");
                //载入模块配置
                LoadModuleSetup(m);
                LoadModuleStatus(m);
                console.WriteLn($"已添加模块{m.ModuleName}，模块ID为\"{m}\"", ConsoleMessageType.Info);
            }
        }
        //根据模块类型获取模块
        public T GetModule<T>(object source, uint index = 0) where T : Module
        {
            string id = CalcModuleID<T>(index);
            CheckModuleExist(source, id);
            object module = Modules[module_index_by_id[id]];
            if (module is T tmodule)
                return tmodule;
            else
                throw new Exception("出大问题。利用自动生成的模块ID查找索引，然后获取的模块对象，其类型竟然和期望类型不符");
        } 
        //根据模块ID移除模块(不建议用，完全可以只关闭模块的总开关)
        public void RemoveModule<T>(object source, uint index = 0)
        {
            string id = CalcModuleID<T>(index);
            CheckModuleExist(source, id);
            int module_index = module_index_by_id[id];
            Modules.RemoveAt(module_index);
            //更新索引字典
            foreach (var kvp in module_index_by_id)
            {
                if (kvp.Value == module_index)
                    module_index_by_id.Remove(kvp.Key);
                else if (kvp.Value > module_index)
                    module_index_by_id[kvp.Key]--;
            }
        }

        //暴露给模块的一些方法
        /// <summary>
        /// 向控制台打印字符串。打印内容会自动包含消息源头的对象的名称
        /// </summary>
        /// <param name="source">消息来源. 此处应传入模块自身</param>
        /// <param name="message">需要向控制台打印的消息</param>
        /// <param name="msg_type">错误级别. 分为无、信息、警告、错误四种. 默认为信息. </param>
        internal void ModulePrint(object source, string message, ConsoleMessageType msg_type = ConsoleMessageType.Info, string prefix = "")
        {
            if (source is Module && !string.IsNullOrEmpty(source.ToString()))
                console.WriteLn($"[{source}] {message}", msg_type, prefix);
            else
                console.WriteLn($"[{source.GetType().Name}] {message}", msg_type, prefix);
        }
        /// <summary>
        /// 获取模块的私有文件夹路径.
        /// 按照规范，模块存取自己的文件都应使用这个目录
        /// </summary>
        /// <param name="module">模块对象</param>
        internal string GetModuleCacheDir(Module module) => Path.Combine(Config.Pathes.ModulesCacheDir, module.ModuleID);
        /// <summary>
        /// 获取模块的ModuleStatus存档文件路径
        /// </summary>
        /// <param name="module">模块</param>
        internal string GetModuleStatusPath(Module module) => Path.Combine(Config.Pathes.ModulesSaveDir, module.ModuleID + "_status.json");
        /// <summary>
        /// 获取模块的ModuleSetup配置文件路径
        /// </summary>
        /// <param name="module">模块</param>
        internal string GetModuleSetupPath(Module module) => Path.Combine(Config.Pathes.ModulesSaveDir, module.ModuleID + "_setup.json");


        //TODO: 把发文本消息的方法扩展成发消息链的方法，然后设计一个消息链标记语法
        /// <summary>
        /// 回复给定消息.
        /// 群组消息(MessageContext.Group)将回复至群组内，临时消息(MessageContext.Temp)和私聊(MessageContext.Private)会回复给发送者.
        /// </summary>
        /// <param name="origin_msg">待回复的原始消息</param>
        /// <param name="text">回复的内容. 暂时只实现了回复文本</param>
        public void ReplyMessagePlain(Message origin_msg, string text)
        {
            string url;
            if (origin_msg.Context == MessageContext.Group)
                url = $"{Config.Network.ServerURL}/sendGroupMessage";
            else if (origin_msg.Context == MessageContext.Private)
                url = $"{Config.Network.ServerURL}/sendFriendMessage";
            else if (origin_msg.Context == MessageContext.Temp)
                url = $"{Config.Network.ServerURL}/sendTempMessage";
            else
                throw new Exception($"暂不支持的消息上下文类型 \"{origin_msg}\"");
            NetworkHelper.PostJSON(url, origin_msg.BuildReplyPlainMessageBody(text));
        }
        /// <summary>
        /// 从服务器获取新消息并进行初步过滤
        /// </summary>
        public List<Message> FetchMessages()
        {
            string response_str = NetworkHelper.FetchMessageListJSON($"{Config.Network.ServerURL}/fetchMessage");
            //构建直接JSON对象
            JFetchMessageResponse obj = JsonConvert.DeserializeObject<JFetchMessageResponse>(response_str);
            //初步过滤
            var jmsgs = obj.data.Where(x =>
            {
                if (x.type == "FriendMessage" || x.type == "TempMessage")
                    return true;
                else if (x.type == "GroupMessage")   //如果是群组消息，还需要群组在监听列表里
                    return Config.QQ.TargetGroupIDList.Contains(x.sender.group.id);
                else return false;
            }).ToList();
            List<Message> msgs = new List<Message>();
            jmsgs.ForEach(jmsg => msgs.Add(MessageFactory.BuildMessage(jmsg))); 
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
        /// <param name="processor">消息处理函数</param>
        /// <returns>已处理的消息数量</returns>
        public void ProcessMessages(List<Message> msgs, Action<Message> processor)
        {
            if (IsBooting && msgs.Count > 1)   //重启时有一条以上遗留消息，则只处理最后一条
            {
                msgs = new List<Message> { msgs.Last() };
                IsBooting = false;
            }
            msgs.ForEach( msg => 
            {
                if (!(msg is MessageEmpty))    //过滤空消息
                {
                    processor(msg);
                    DiagData.ReceivedMessageCount++;
                }
            });
        }
        /// <summary>
        /// 消息循环。轮询获取并处理消息。每次重新获取消息前等待一定时间，等待时间由PollingTimeInterval控制
        /// </summary>
        /// <param name="success_count">已成功处理的数据包个数</param>
        void MsgLoop(ManualResetEvent wait_for_pause_msgLoop_signal)
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
                string cmd = console.BufferedReadLn().Trim();
                lock (console)
                {
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
                        wait_for_pause_msgLoop_signal.Set();
                        console.WriteLn("消息循环线程已重新开始", ConsoleMessageType.Info);
                    }
                    else if (cmd == "quit")
                        exit_flag = true;
                    else if (cmd == "status")
                        console.WriteLn(DiagData.GetSummaryString(), ConsoleMessageType.Info);
                    else if (cmd == "save")
                    {
                        console.WriteLn("手动保存所有模块到存档...", ConsoleMessageType.Info);
                        Modules.ForEach(x =>
                        {
                            SaveModuleSetup(x);
                            SaveModuleStatus(x);
                        });
                    }
                    else if (cmd == "reload")
                    {
                        console.WriteLn("手动重载所有模块存档...", ConsoleMessageType.Info);
                        Modules.ForEach(x =>
                        {
                            LoadModuleSetup(x);
                            LoadModuleStatus(x);
                        });
                    }
                    else if (cmd == "lasterror")
                        console.WriteLn($"最近一次错误信息：\n{DiagData.LastException}", ConsoleMessageType.Info);
                    else
                        console.WriteLn($"未知命令：\"{cmd}\"", ConsoleMessageType.Error);
                }
            }
            //从容退出
            OnExit();
        }

        //早期(v0.4及更早)的处理函数. 已经弃用
        [Obsolete]
#pragma warning disable IDE0051
        private void PaleMutant(Message msg)
#pragma warning restore IDE0051 // 删除未使用的私有成员
        {
            var cmdmod = GetModule<CommandModule>(this);
            var fuckmod = GetModule<FuckModule>(this);
            var chatmod = GetModule<ChatQYKModule>(this);
            if (msg is MessagePlain msg_plain)
            {
                //依次判断 以不同模块处理
                if (cmdmod.ShouldProcess(msg_plain))
                    ReplyMessagePlain(msg_plain, cmdmod.Processor(msg_plain));
                else if (fuckmod.ShouldProcess(msg_plain))
                    ReplyMessagePlain(msg_plain, fuckmod.Processor(msg_plain));
                else if (msg.Context != MessageContext.Group || msg.TargetID == Config.QQ.SelfID)
                    ReplyMessagePlain(msg_plain, chatmod.Processor(msg_plain));
            }
        }
        //默认处理函数。用Modules中的模块依次尝试处理消息
        //注意 空消息的过滤已经在上一级ProcessMessages()完成，所以此处入参的所有消息均为非空消息
        private void ModulesProcessMessage(Message msg)
        {
            Task send_msg_task = null;
            foreach (var module in Modules)
            {
                if (module.ShouldProcess(msg))
                {
                    bool has_error = false;
                    string output;
                    try  //此try用于控制所有模块的处理器异常
                    {
                        DiagData.Stopwatch.Restart();
                        output = module.Processor(msg);
                        DiagData.Stopwatch.Stop();
                        DiagData.ProcessedCount++;
                        DiagData.LastUsedModule = module;
                        DiagData.LastProcessingTime = DiagData.Stopwatch.ElapsedMilliseconds;
                    }
                    catch (Exception ex)
                    {
                        output = $"{module}在处理消息时崩溃。异常信息：\n{ex.GetType().Name}：{ex.Message}\n\n调用栈：\n{ex.StackTrace}";
                        has_error = true;
                    }
                    //直接新建一个线程做回复，防止因为网络速度较慢阻塞其他消息的处理
                    if (send_msg_task == null)
                        send_msg_task = Task.Run(() => ReplyOutput(msg, output, has_error, module));
                    else
                        send_msg_task.ContinueWith(x => ReplyOutput(msg, output, has_error, module));  //保序
                    SaveModuleStatus(module, false);   //保存模块状态
                    if (module.IsTransparent)
                        continue;
                    else
                        return;
                }
            }
        }

        //保存模块的状态
        private void SaveModuleStatus(Module module, bool print_info = true)
        {
            string json = JsonConvert.SerializeObject(module.ExportStatusDict(), Json.JsonHelper.FileSetup);
            string file_path = GetModuleStatusPath(module);
            if (print_info)
                console.WriteLn($"正在保存模块{module}的状态至\"{file_path}\"...", ConsoleMessageType.Task);
            File.WriteAllText(file_path, json);
        }
        //保存模块的配置（只是平滑转移用到了，一般通常不用。即使调用也应该由KLBot实例管理者调用）
        public void SaveModuleSetup(Module module, bool print_info = true)
        {
            string json = JsonConvert.SerializeObject(module.ExportSetupDict(), Json.JsonHelper.FileSetup);
            string file_path = GetModuleSetupPath(module);
            if (print_info)
                console.WriteLn($"正在保存模块{module}的配置至\"{file_path}\"...", ConsoleMessageType.Task);
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
                module.ImportDict(JsonConvert.DeserializeObject<Dictionary<string, object>>(File.ReadAllText(file_path), Json.JsonHelper.FileSetup));
            }
        }
        //载入所有模块配置
        private void LoadModuleSetup(Module module, bool print_info = true)
        {
            string file_path = GetModuleSetupPath(module);
            if (File.Exists(file_path))
            {
                if (print_info)
                    console.WriteLn($"正在从\"{file_path}\"加载模块{module.ModuleID}的配置...", ConsoleMessageType.Task);
                module.ImportDict(JsonConvert.DeserializeObject<Dictionary<string, object>>(File.ReadAllText(file_path), Json.JsonHelper.FileSetup));
            }
            else
                ModulePrint(module, $"找不到{module.ModuleID}的模块配置文件，模块将以默认状态启动。对于某些必须使用配置文件初始化的模块，这可能导致问题", ConsoleMessageType.Warning);
        }
        //重新载入所有模块配置和状态
        public void ReloadAllModules()
        {
            foreach (var module in Modules)
            {
                LoadModuleSetup(module);
                LoadModuleStatus(module);
            }
        }

        //有序退出
        public void OnExit()
        {
            foreach (var m in Modules)
            {
                SaveModuleStatus(m);
                SaveModuleSetup(m);
            }
            console.WriteLn("OnExit work done.", ConsoleMessageType.Info);
        }

        //信息输出相关
        public string GetModuleListString()
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
        public string GetListeningGroupListString()
        {
            StringBuilder sb = new StringBuilder($"监听群组列表:\n");
            int index = 0;
            Config.QQ.TargetGroupIDList.ForEach(target_id =>
            {
                sb.AppendLine($"  [{index}]  {target_id}");
                index++;
            });
            return sb.ToString();
        }

        private void ReplyOutput(Message original_msg, string output, bool has_error, Module module)
        {
            DiagData.Stopwatch.Restart();
            if (!string.IsNullOrEmpty(output))  //模块输出string.Empty或null时 根据约定意味着模块没有输出 这时啥也不回复哈
            {
                if (!has_error)
                {
                    if (module.UseSignature)
                        ReplyMessagePlain(original_msg, $"[{module}]\n{output}");
                    else
                        ReplyMessagePlain(original_msg, output);
                }
                else
                    ReplyMessagePlain(original_msg, $"[KLBot]\n{output}");
            }
            DiagData.Stopwatch.Stop();
            DiagData.LastReplyTime = DiagData.Stopwatch.ElapsedMilliseconds;
        }
        private void CreateDirectoryIfNotExist(string path, string dir_description)
        {
            if (!Directory.Exists(path))
            {
                console.WriteLn($"{dir_description}\"{path}\"不存在。正在自动创建...", ConsoleMessageType.Warning);
                Directory.CreateDirectory(path);
            }
        }
        private string CalcModuleID(string module_name, uint index) => index == 0 ? module_name : $"{module_name}[{index}]";  //计算模块的唯一ID，格式为"模块类型[模块索引]"。其中模块索引为该模块在所有同类模块中的排位
        private string CalcModuleID<T>(uint index) => index == 0 ? typeof(T).Name : $"{typeof(T).Name}[{index}]";  
        private void CheckModuleExist(object source, string id)
        {
            if (!module_index_by_id.ContainsKey(id))
                throw new ModuleMissingException($"对象\"{source}\"试图引用ID为\"{id}\"的模块，但该模块不存在");
        }
#pragma warning disable IDE0051
        private void CheckModuleExist<T>(object source, uint index = 0)
#pragma warning restore IDE0051 // 删除未使用的私有成员
        {
            string id = CalcModuleID<T>(index);
            if (!module_index_by_id.ContainsKey(id))
                throw new ModuleMissingException($"对象\"{source}\"试图引用ID为\"{id}\"的模块，但该模块不存在");
        }
    }
}
