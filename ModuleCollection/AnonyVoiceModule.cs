#pragma warning disable IDE0044 
using klbotlib.Modules.AnonyVoiceModuleNamespace;
using klbotlib.Modules.ModuleUtils;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Runtime.InteropServices;
using System.Text.RegularExpressions;

namespace klbotlib.Modules
{
    public class AnonyVoiceModule : SingleTypeModule<MessagePlain>
    {
        public override bool UseSignature => false;
        public override bool IsAsync => true;
        const string url = "https://ai.baidu.com/aidemo";
        const string prefix = "data:audio/x-mpeg;base64,";
        const string temp_mpeg_name = "tmp.mpeg";
        private Regex pri_req_pat = new Regex(@"^说骚话 (\d{9,11})$");
        Dictionary<string, string> per_by_name = new Dictionary<string, string> 
        {
            { "可爱女童", "4103"},
            { "磁性男声", "4003" },
            { "甜美女声", "4119"},
            { "情感男声1", "4106"},
            { "清澈女声", "4105"},
            { "情感男声2", "4115" },
            { "成熟女声", "4100"},
            { "情感女声", "4117"},
        };

        [ModuleStatus]
        string Person = "磁性男声";
        [ModuleStatus(IsHidden = true)]
        Dictionary<long, UserStatus> user_stat = new Dictionary<long, UserStatus>();
        [ModuleStatus(IsHidden = true)]
        Dictionary<long, long> target_groups = new Dictionary<long, long>();
        HttpHelper http_helper = new HttpHelper();

        public override int Filter(MessagePlain msg)
        {
            string text = msg.Text.Trim();
            if ((msg.Context == MessageContext.Temp || msg.Context == MessageContext.Private))
            {
                //初始请求
                if (IsNewOrIdleUser(msg.SenderID))
                {
                    if (msg.Context == MessageContext.Temp && text == "说骚话")
                        return 1;
                    else if (msg.Context == MessageContext.Private && pri_req_pat.IsMatch(text))
                        return 2;
                }
                //转语音请求
                else if (user_stat.TryGetValue(msg.SenderID, out UserStatus value) && value == UserStatus.ReadyToSendVoice)
                    return 3;
            }
            else if (text.StartsWith("设置音色 "))
                return 4;
            else if (text == "匿名语音模块帮助")
                return 5;
            return 0;
        }
        public override string Processor(MessagePlain msg, int status_code)
        {
            switch (status_code)
            {
                case 1:     //临时会话 发起请求
                    ToWaitForTextState(msg.SenderID, msg.GroupID);
                    return "准备好了，你说";
                case 2:     //私聊会话 发起请求。这种情况需要额外解析一个群号
                    if (long.TryParse(pri_req_pat.Match(msg.Text).Groups[1].Value, out long group_id))
                        ToWaitForTextState(msg.SenderID, group_id);
                    return "准备好了，你说";
                case 3:
                    string body = $"type=tns&per={Person}&spd=6&pit=5&vol=15&aue=6&tex={Uri.EscapeDataString(msg.Text.Trim())}";
                    string json = http_helper.PostString(url, body);
                    JReply reply = JsonConvert.DeserializeObject<JReply>(json);
                    if (reply.errno != 0)
                        return $"错误[{reply.errno}]：{reply.msg}\n重新说点别的吧";
                    string mpeg_b64 = reply.data.Substring(prefix.Length); 
                    //SaveFileAsBinary(temp_mpeg_name, Convert.FromBase64String(mpeg_b64));
                    user_stat[msg.SenderID] = UserStatus.Idle;
                    //string b64_amr = ConvertToAmr();
                    HostBot.SendGroupMessage(this, target_groups[msg.SenderID], @"\voice:\base64:" + mpeg_b64);
                    //HostBot.SendGroupMessage(this, target_groups[msg.SenderID], "[DEBUG]上面是原mpeg编码。接下来是PCM编码测试：");
                    //HostBot.SendGroupMessage(this, target_groups[msg.SenderID], @"\voice:\base64:" + ConvertToSlk());
                    return "已发送";
                case 4:
                    string tone = msg.Text.Trim().Substring(5);
                    if (!per_by_name.TryGetValue(tone, out string per))
                        return "不支持这个音色";
                    Person = per;
                    return $"音色已设置为{Person}";
                case 5:
                    string re = "发起临时会话后发送\"说骚话\"激活功能，模块会把下一句话转换成语音发送到临时会话所通过的群里。\n" +
                        "可以在群里发送\"设置音色[空格][音色名称]\"来修改生成的音色。目前支持的音色有：\n";
                    foreach (var key in per_by_name.Keys)
                        re += "\t" + key;
                    return re;
            }
            return null;
        }

        bool IsNewOrIdleUser(long id) => !user_stat.ContainsKey(id) || user_stat[id] == UserStatus.Idle;
        void ToWaitForTextState(long user_id, long group_id)    //转移至等待输入文本状态
        {
            if (!user_stat.ContainsKey(user_id))
                user_stat.Add(user_id, UserStatus.ReadyToSendVoice);
            else
                user_stat[user_id] = UserStatus.ReadyToSendVoice;
            if (!target_groups.ContainsKey(user_id))
                target_groups.Add(user_id, group_id);
            else
                target_groups[user_id]= group_id;

        }
        string ConvertToAmr()
        {
            Process p = new Process();
            if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux) || RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
            {
                p.StartInfo.FileName = "ffmpeg";
                p.StartInfo.WorkingDirectory = ModuleCacheDir;
                p.StartInfo.Arguments = $"-i {temp_mpeg_name} -ar 8000 tmp.amr -y";
                p.StartInfo.UseShellExecute = false;
                p.StartInfo.RedirectStandardOutput = true;
                p.StartInfo.RedirectStandardError = true;
                p.Start();
                p.BeginErrorReadLine();
                p.BeginOutputReadLine();
                p.WaitForExit(15000);
                byte[] amr_bin = ReadFileAsBinary("tmp.amr");
                if (amr_bin.Length == 0)
                    throw new Exception("AMR文件转换失败。FFMpeg进程没有正确完成任务");
                return Convert.ToBase64String(amr_bin);
            }
            else
                return $"KLBot暂时不支持在此运行平台下转换";
        }
        string ConvertToSlk()
        {
            if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux) || RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
            {
                //mpeg -> pcm
                Process ffmpeg = new Process();
                ffmpeg.StartInfo.FileName = "ffmpeg";
                ffmpeg.StartInfo.WorkingDirectory = ModuleCacheDir;
                ffmpeg.StartInfo.Arguments = $"-i {temp_mpeg_name} -ar 16000 -ac 1 -f s16le -acodec pcm_s16be tmp.pcm -y";  //16000Hz 16bit
                ffmpeg.StartInfo.UseShellExecute = false;
                ffmpeg.StartInfo.RedirectStandardOutput = true;
                ffmpeg.StartInfo.RedirectStandardError = true;
                ffmpeg.Start();
                ffmpeg.BeginErrorReadLine();
                ffmpeg.BeginOutputReadLine();
                ffmpeg.WaitForExit(15000);
                if (!FileExist("tmp.pcm"))
                    throw new Exception("编码转换失败。FFmpeg运行失败");
                Process slkenc = new Process();
                slkenc.StartInfo.FileName = "slkenc";
                slkenc.StartInfo.WorkingDirectory = ModuleCacheDir;
                slkenc.StartInfo.Arguments = $"tmp.pcm tmp.amr -Fs_API 16000 -Fs_maxInternal 16000 -quiet -tencent -rate 128000 -DTX 1";
                slkenc.StartInfo.UseShellExecute = false;
                slkenc.StartInfo.RedirectStandardOutput = true;
                slkenc.StartInfo.RedirectStandardError = true;
                slkenc.Start();
                slkenc.BeginErrorReadLine();
                slkenc.BeginOutputReadLine();
                slkenc.WaitForExit(15000);
                //DeleteFile("tmp.pcm");      //用完删除 可以一定程度上指示运行结果
                if (!FileExist("tmp.amr"))
                    throw new Exception("编码转换失败。slkenc运行失败");
                byte[] amr_bin = ReadFileAsBinary("tmp.amr");
                if (amr_bin.Length == 0)
                    throw new Exception("编码转换失败。FFmpeg运行失败");
                DeleteFile("tmp.amr");  //同理
                return Convert.ToBase64String(amr_bin);
            }
            else
                return $"KLBot暂时不支持在此运行平台下转换";
        }
    }
}

namespace klbotlib.Modules.AnonyVoiceModuleNamespace
{
    public class JReply { public int errno; public string msg; public string data; }
    enum UserStatus
    {
        Idle = 1, ReadyToSendVoice = 2
    }
}
