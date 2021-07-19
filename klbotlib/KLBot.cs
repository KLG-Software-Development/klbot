using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Text;
using Newtonsoft.Json;
using Gleee.Consoleee;
using System.Threading;
using klbotlib.Modules;
using klbotlib.Internal;
using klbotlib.Modules.FuckModule;
using klbotlib.Modules.CommandModule;
using klbotlib.Modules.ChatQYKModule;

namespace klbotlib
{
    public class KLBot
    {
        /// <summary>
        /// 返回Bot是否刚刚启动且未处理过任何消息
        /// </summary>
        private bool IsBooting { get; set; } = true;
        private readonly Consoleee console = new Consoleee();   //控制台帮助对象

        public BotConfig Config { get; }
        //Bot属性
        public int PollingTimeInterval { get; set; } = 500;   //轮询时间区间
        public bool IsLoopOn = false;   //总开关，决定是否继续消息循环
        public bool IsRestoreModule { get; set; } = true;   //决定是否总是自动从存档中恢复所有模块的状态

        /// <summary>
        /// 该KLBot的模块列表. 
        /// 所有传入的消息会依次被其中的模块处理. 
        /// 如果该处理模块的Module.IsTransparent属性设定为true，则处理过的消息不会被消耗.
        /// 如果该处理模块的Module.IsTransparent属性设定为false(默认)，则被处理过的消息会从列表中移除.
        /// </summary>
        public List<Module> Modules { get; } = new List<Module>();
        [BotModule]
        public CommandModule CommandModule { get; }
        [BotModule]
        public FuckModule FuckModule { get; }
        [BotModule]
        public ChatQYKModule ChatQYKModule { get; }
        //构造函数
        public KLBot( string url = "http://localhost:3356", long self_id = 3205508672, params long[] targets)
        {
            Config = new BotConfig(url, self_id, targets, "modules");  //平滑转移用
            //创建Config中声明的模块目录
            Directory.CreateDirectory(Config.SavePathes.ModulesHomeDir);
            //加载模块
            Modules.Add(new CommandModule(this));
            Modules.Add(new FuckModule(this));
            Modules.Add(new ChatQYKModule(this));
            foreach (var module in Modules)
            {
                //为已经加载的每个模块创建私有目录（如果不存在）
                Directory.CreateDirectory(GetModulePrivateDir(module));
            }
            //载入模块配置
            LoadAllModuleSetup();
            console.WriteLn($"Sucessfully initiate KLBot: \n" +
                $"\tUrl: {Config.Network.ServerURL}\n" +
                $"\tTarget list:", ConsoleMessageType.Info);
            int count = 1;
            foreach (var t in Config.QQ.TargetGroupIDList)
            {
                console.WriteLn($"\t[{count}]  {t}");
                count++;
            }
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

        //暴露给模块的一些方法
        /// <summary>
        /// 向控制台打印字符串。打印内容里将包含消息源头的模块的名称
        /// </summary>
        /// <param name="source">消息来源. 此处应传入模块自身</param>
        /// <param name="message">需要向控制台打印的消息</param>
        /// <param name="msg_type">错误级别. 分为无、信息、警告、错误四种. 默认为信息. </param>
        internal void ModulePrint(object source, string message, ConsoleMessageType msg_type = ConsoleMessageType.Info, string prefix = "")
        {
            string source_name = source.ToString().Split('.').Last();
            console.WriteLn($"[{source_name}] {message}", msg_type, prefix);
        }
        /// <summary>
        /// 获取模块的私有文件夹路径.
        /// 按照规范，模块存取自己的文件都应使用这个目录
        /// </summary>
        /// <param name="module">模块对象</param>
        internal string GetModulePrivateDir(Module module) => Path.Combine(Config.SavePathes.ModulesHomeDir, module.ModuleID);

        /// <summary>
        /// 从服务器获取新消息
        /// </summary>
        public List<Message> FetchMessages()
        {
            HttpWebRequest request = WebRequest.CreateHttp($"{Config.Network.ServerURL}/fetchMessage");
            request.Method = "GET";
            string response_str = "";
            using (var stream = request.GetResponse().GetResponseStream())
            {
                StreamReader reader = new StreamReader(stream, Encoding.UTF8);
                response_str = reader.ReadToEnd();
            }
            //构建直接JSON对象
            JFetchMessageResponse obj = JsonConvert.DeserializeObject<JFetchMessageResponse>(response_str);
            //如果是群组 则过滤非监听的群组
            var jgroup_msgs = obj.data.Where(x =>
            {
                if (x.type == "FriendMessage" || x.type == "TempMessage")
                    return true;
                else if (x.type == "GroupMessage")   //如果是群组消息，还需要群组在监听列表里
                    return Config.QQ.TargetGroupIDList.Contains(x.sender.group.id);
                else return false;
            });
            List<Message> msgs = new List<Message>();
            foreach (var jgroup_msg in jgroup_msgs)
            {
                var msg = MessageFactory.BuildMessage(jgroup_msg);
                msgs.Add(msg);
            }
            return msgs;
        }
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
                throw new Exception($"Unsupported context '{origin_msg}'");
            PostJSON(url, BuildReplyPlainMessageBody(origin_msg, text.Replace("\"", "\\\"")));
        }

        /// <summary>
        /// 用默认消息处理函数依次处理消息列表
        /// </summary>
        /// <param name="msgs">待处理消息列表</param>
        /// <returns>已处理的消息数量</returns>
        public int ProcessMessages(IList<Message> msgs) => ProcessMessages(msgs, PaleMutant);
        /// <summary>
        /// 用processor依次处理消息列表
        /// </summary>
        /// <param name="msgs">待处理消息列表</param>
        /// <param name="processor">消息处理函数</param>
        /// <returns>已处理的消息数量</returns>
        public int ProcessMessages(IList<Message> msgs, Action<Message> processor)
        {
            int count = 0;
            if (IsBooting && msgs.Count > 1)   //重启时有一条以上遗留消息，则只处理最后一条
            {
                msgs = new Message[] { msgs.Last() };
                IsBooting = false;
            }
            foreach (var msg in msgs)
            {
                if (msg is MessageEmpty)
                    continue;
                else
                {
                    processor(msg);
                    count++;
                }
            }
            return count;
        }
        /// <summary>
        /// 消息循环。轮询获取并处理消息。每次重新获取消息前等待一定时间，等待时间由PollingTimeInterval控制
        /// </summary>
        /// <param name="success_count">已成功处理的数据包个数</param>
        public void Loop(out long success_count)
        {
            success_count = 0;
            int msg_count = 0;
            IsLoopOn = true;
            if (IsRestoreModule)
                LoadAllModuleStatus();
            while (IsLoopOn)
            {
                msg_count += ProcessMessages(FetchMessages());
                success_count++;
                console.ClearCurrentLine();
                console.Write($"Processed package: {success_count}  Processed message: {msg_count}", ConsoleMessageType.Info);
                Thread.Sleep(PollingTimeInterval);
            }
            //从容退出
            OnExit();
        }

        //早期(v0.4及更早)的处理函数. 准备弃用
        [Obsolete]
        private void PaleMutant(Message msg)
        {
            if (msg is MessagePlain msg_plain)
            {
                //依次判断 以不同模块处理
                if (CommandModule.ShouldProcess(msg_plain))
                    ReplyMessagePlain(msg_plain, CommandModule.Processor(msg_plain));
                else if (FuckModule.ShouldProcess(msg_plain))
                    ReplyMessagePlain(msg_plain, FuckModule.Processor(msg_plain));
                else if (msg.Context != MessageContext.Group || msg.TargetID == Config.QQ.SelfID)
                    ReplyMessagePlain(msg_plain, ChatQYKModule.Processor(msg_plain));
            }
        }
        //默认处理函数。用Modules中的模块依次尝试处理消息
        private void ModulesProcessMessage(Message msg)
        {
            foreach (var module in Modules)
            {
                if (module.ShouldProcess(msg))
                {
                    string output = module.Processor(msg);
                    if (!string.IsNullOrEmpty(output))
                        ReplyMessagePlain(msg, output);
                    if (!module.IsTransparent)
                        continue;
                    else
                        return;
                }
            }
        }

        //保存所有模块属性
        private void SaveAllModuleStatus()
        {
            foreach (var module in Modules)
            {
                string json = JsonConvert.SerializeObject(module.ExportStatusDict(), Formatting.Indented);
                string file_path = Path.Combine(Config.SavePathes.ModulesHomeDir, $"{module.ModuleID}_status.json");
                console.WriteLn($"Saving status of {module.ModuleID} to \"{file_path}\"...", ConsoleMessageType.Task);
                File.WriteAllText(file_path, json);
            }
        }
        //保存所有模块配置（通常不用）
        private void SaveAllModuleSetup()
        {
            foreach (var module in Modules)
            {
                string json = JsonConvert.SerializeObject(module.ExportSetupDict(), Formatting.Indented);
                string file_path = Path.Combine(GetModulePrivateDir(module), $"{module.ModuleID}_setup.json");
                console.WriteLn($"Saving setup of {module.ModuleID} to \"{file_path}\"...", ConsoleMessageType.Task);
                File.WriteAllText(file_path, json);
            }
        }
        //载入所有模块属性
        private void LoadAllModuleStatus()
        {
            foreach (var module in Modules)
            {
                string file_path = Path.Combine(Config.SavePathes.ModulesHomeDir, $"{module.ModuleID}_status.json");
                if (File.Exists(file_path))
                {
                    console.WriteLn($"Loading status of {module.ModuleID} from \"{file_path}\"...", ConsoleMessageType.Task);
                    module.ImportDict(JsonConvert.DeserializeObject<Dictionary<string, object>>(File.ReadAllText(file_path)));
                }
            }
        }
        //载入所有模块配置
        private void LoadAllModuleSetup()
        {
            foreach (var module in Modules)
            {
                string file_path = Path.Combine(Config.SavePathes.ModulesHomeDir, $"{module.ModuleID}_setup.json");
                if (File.Exists(file_path) && IsRestoreModule)
                {
                    console.WriteLn($"Loading setup of {module.ModuleID} from \"{file_path}\"...", ConsoleMessageType.Task);
                    module.ImportDict(JsonConvert.DeserializeObject<Dictionary<string, object>>(File.ReadAllText(file_path)));
                }
            }
        }

        //有序退出
        public void OnExit()
        {
            SaveAllModuleStatus();
            console.WriteLn("OnExit work done.", ConsoleMessageType.Info);
        }

        //其他helper函数
        private string BuildReplyPlainMessageBody(Message original_msg, string text)
        {
            var context = original_msg.Context;
            if (context == MessageContext.Group || context == MessageContext.Private)
                return $"{{\"target\":\"{original_msg.GroupID}\",\"messageChain\":[{{\"type\":\"Plain\", \"text\":\"{text}\" }}]}}";
            else if (context == MessageContext.Private)
                return $"{{\"target\":\"{original_msg.SenderID}\",\"messageChain\":[{{\"type\":\"Plain\", \"text\":\"{text}\" }}]}}";
            else if (context == MessageContext.Temp)
                return $"{{\"qq\":\"{original_msg.SenderID}\",\"group\":\"{original_msg.GroupID}\",\"messageChain\":[{{\"type\":\"Plain\", \"text\":\"{text}\" }}]}}";
            else throw new Exception($"Unsupported context type '{context}'");
        }
        private void PostJSON(string url, string json_string)
        {
            HttpWebRequest request = WebRequest.CreateHttp(url);
            request.Method = "POST";
            request.ContentType = "application/json";
            using (var stream = request.GetRequestStream())
            {
                byte[] data = Encoding.UTF8.GetBytes(json_string);
                stream.Write(data, 0, data.Length);
                stream.Close();
            }
            using (var stream = request.GetResponse().GetResponseStream())
            {
                StreamReader reader = new StreamReader(stream, Encoding.UTF8);
                string response_str = reader.ReadToEnd();
                JsonConvert.DeserializeObject<JMiraiResponse>(response_str).CheckStatusCode();
            }
        }
    }
}
