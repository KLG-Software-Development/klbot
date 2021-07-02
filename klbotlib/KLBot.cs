using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Text;
using Newtonsoft.Json;
using Gleee.Consoleee;
using System.Text.RegularExpressions;
using System.Security.Cryptography;
using System.Threading;
using klbotlib.Modules;
using klbotlib.Modules.Commands;

//重构逻辑: KLBot应该只负责组织模块和模块间通讯, 消息判断/处理应该完全放在模块类里处理
//这会导致模块类更大, KLBot类更小, 所以模块类似乎需要分文件存放
namespace klbotlib
{
    public class KLBot
    {
        private RNGCryptoServiceProvider ro = new RNGCryptoServiceProvider();
        #region Patterns
        private Regex fuckPat = JsonConvert.DeserializeObject<Regex>(File.ReadAllText($"{AppDomain.CurrentDomain.BaseDirectory}/fuckPat"));
        private Regex questionPat = new Regex(@"^\?$|^？$");
        #endregion
        #region Modules
        public CommandModule CommandModule { get; }
        public FuckModule FuckModule { get; } = new FuckModule();
        #endregion
        public const long KLG = 670406903, DEBUG = 727414436;
        public long SelfID { get; }
        public List<long> TargetList { get; } = new List<long>();   //监听的目标群号
        public string ServerURL { get; }
        public Action<Message> DefaultProcessor { get; set; }
        public Consoleee console = new Consoleee();
        #region Bot Properties
        public bool IsTagMe { get; set; } = false;          //嘴臭模块是否只处理@自身的消息（不适用于聊天模块。聊天模块永远只处理@自身的消息）
        public bool IsFuckModuleEnabled { get; set; } = true; 
        public int PollingTimeInterval { get; set; } = 500;   //轮询时间区间
        public bool IsOn = true;   //总开关，决定是否继续消息循环
        #endregion

        public KLBot( string url = "http://localhost:3356", long self_id = 3205508672, params long[] targets)
        {
            ServerURL = url;
            TargetList.AddRange(targets);
            SelfID = self_id;
            CommandModule = new CommandModule(this,
                new TagMeCmd(), 
                new PtiCmd(), 
                new ShutdownCmd(),
                new FuckModCmd(),
                new FuckModCascadeCmd()
                );
            Init();
        }
        //一些初始化处理和信息打印
        public void Init()
        {
            DefaultProcessor = PaleMutant;
            console.WriteLn($"Sucessfully initiate groupbot: \n" +
                $"\tUrl: {ServerURL}\n" +
                $"\tTargetList:", ConsoleErrorLevel.Info);
            int count = 1;
            foreach (var t in TargetList)
            {
                console.WriteLn($"\t[{count}]  {t}");
            }
        }

        public void AddTarget(long target) => TargetList.Add(target);
        public void AddTarget(IList<long> targets) => TargetList.AddRange(targets);

        //收发消息
        public List<Message> FetchMessages()
        {
            HttpWebRequest request = WebRequest.CreateHttp($"{ServerURL}/fetchMessage");
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
                    return TargetList.Contains(x.sender.group.id);
                else return false;
            });
            List<Message> msgs = new List<Message>();
            foreach (var jgroup_msg in jgroup_msgs)
            {
                var msg = MessageFactory.BuildMessage(jgroup_msg);
                if (!IsTagMe ||  (msg.TargetID == SelfID))
                    msgs.Add(msg);
            }
            //if (msgs.Count != 0)
            //    Debug.Print(response_str);
            return msgs;
        }
        public void ReplyMessagePlain(Message origin_msg, string text)
        {
            string url = "";
            if (origin_msg.Context == MessageContext.Group)
                url = $"{ServerURL}/sendGroupMessage";
            else if (origin_msg.Context == MessageContext.Private)
                url = $"{ServerURL}/sendFriendMessage";
            else if (origin_msg.Context == MessageContext.Temp)
                url = $"{ServerURL}/sendTempMessage";
            else
                throw new Exception($"Unsupported context '{origin_msg}'");
            PostJSON(url, BuildReplyPlainMessageBody(origin_msg, text));
            HttpWebRequest request = WebRequest.CreateHttp(url);
        }

        //使用传入的处理函数，依次处理消息列表
        public int ProcessMessages(IList<Message> msgs) => ProcessMessages(msgs, DefaultProcessor);
        //使用传入的处理函数，依次处理消息列表
        public int ProcessMessages(IList<Message> msgs, Action<Message> processor)
        {
            int count = 0;
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
        //使用传入的处理函数，依次处理消息列表，但只处理某个特定类型的消息
        public int ProcessMessages<Message>(IList<Message> msgs, Action<Message> processor)
        {
            int count = 0;
            foreach (var msg in msgs)
            {
                if (msg is MessageEmpty || !(msg is Message))
                    continue;
                else
                {
                    processor(msg);
                    count++;
                }
            }
            return count;
        }
        //循环获取并处理消息。每次重新获取消息前等待一定时间
        public void Loop(out long success_count)
        {
            success_count = 0;
            int msg_count = 0;
            IsOn = true;
            while (IsOn)
            {
                msg_count += ProcessMessages(FetchMessages());
                success_count++;
                console.ClearCurrentLine();
                console.Write($"Processed package: {success_count}  Processed message: {msg_count}", ConsoleErrorLevel.Info);
                Thread.Sleep(PollingTimeInterval);
            }
        }

        //嘴臭整活函数。初始的默认处理函数
        private void PaleMutant(Message msg)
        {
            if (msg is MessagePlain msg_plain)
            {
                //依次判断 以不同模块处理
                if (CommandModule.ShouldProcess(msg_plain))
                    ReplyMessagePlain(msg_plain, CommandModule.Processor(msg_plain));
                else if (IsFuckModuleEnabled && fuckPat.IsMatch(msg_plain.Text))
                    ReplyMessagePlain(msg_plain, FuckModule.FuckGenerator(ro));
                else if (questionPat.IsMatch(msg_plain.Text))
                    ReplyMessagePlain(msg_plain, $"问号你妈呢{FuckModule.FuckGenerator(ro)}");
                else if (msg.Context != MessageContext.Group || msg.TargetID == SelfID)
                    ReplyMessagePlain(msg_plain, ChatQYKModule.GetReply(msg_plain.Text));
            }
        }

        //helper函数
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
