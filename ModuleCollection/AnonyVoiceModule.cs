using klbotlib.Modules.ModuleUtils;
using Newtonsoft.Json;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace klbotlib.Modules;

/// <summary>
/// 匿名语音模块
/// </summary>
public class AnonyVoiceModule : SingleTypeModule<MessagePlain>
{
    [ModuleStatus]
    private string _person = "磁性男声";
    [ModuleStatus(IsHidden = true)]
    private readonly Dictionary<long, UserStatus> _userStat = new();
    [ModuleStatus(IsHidden = true)]
    private readonly Dictionary<long, long> _targetGroups = new();
    private const string _url = "https://ai.baidu.com/aidemo";
    private const string _prefix = "data:audio/x-mpeg;base64,";
    private readonly Regex _priReqPat = new(@"^说骚话 (\d{9,11})$");
    private readonly Dictionary<string, string> _perByName = new()
    {
        { "可爱女童", "4103" },
        { "磁性男声", "4003" },
        { "甜美女声", "4119" },
        { "情感男声1", "4106" },
        { "清澈女声", "4105" },
        { "情感男声2", "4115" },
        { "成熟女声", "4100" },
        { "情感女声", "4117" },
    };
    private readonly HttpHelper _httpHelper = new();

    /// <inheritdoc/>
    public sealed override bool UseSignature => false;
    /// <inheritdoc/>
    public sealed override bool IsAsync => true;
    /// <inheritdoc/>
    public sealed override string FriendlyName => "匿名语音模块";
    /// <inheritdoc/>
    public sealed override string HelpInfo
    {
        get
        {
            string re = "发起临时会话后发送\"说骚话\"激活功能，模块会把下一句话转换成语音发送到临时会话所通过的群里。\n" +
                "可以在群里发送\"设置音色[空格][音色名称]\"来修改生成的音色。目前支持的音色有：\n";
            foreach (var key in _perByName.Keys)
                re += "\t" + key;
            return re;
        }
    }
    /// <inheritdoc/>
    public override string Filter(MessagePlain msg)
    {
        string text = msg.Text.Trim();
        if (msg.Context is MessageContext.Temp or MessageContext.Private)
        {
            //初始请求
            if (IsNewOrIdleUser(msg.SenderID))
            {
                if (msg.Context == MessageContext.Temp && text == "说骚话")
                    return "request";
                else if (msg.Context == MessageContext.Private && _priReqPat.IsMatch(text))
                    return "private request";
            }
            //转语音请求
            else if (_userStat.TryGetValue(msg.SenderID, out UserStatus value) && value == UserStatus.ReadyToSendVoice)
                return "content";
        }
        else if (text.StartsWith("设置音色 "))
            return "set tone";
        return null;
    }
    /// <inheritdoc/>
    public override string Processor(MessagePlain msg, string filterOut)
    {
        switch (filterOut)
        {
            case "request":     //临时会话 发起请求
                ToWaitForTextState(msg.SenderID, msg.GroupID);
                return "准备好了，你说";
            case "private request":     //私聊会话 发起请求。这种情况需要额外解析一个群号
                if (long.TryParse(_priReqPat.Match(msg.Text).Groups[1].Value, out long group_id))
                    ToWaitForTextState(msg.SenderID, group_id);
                return "准备好了，你说";
            case "content":
                _userStat[msg.SenderID] = UserStatus.Idle;
                Messaging.ReplyMessage(msg, "正在薅羊毛...");
                string body = $"type=tns&per={_person}&spd=5&pit=5&vol=15&aue=6&tex={msg.Text.Trim()}";
                string json = _httpHelper.PostFormUrlEncodedAsync(_url, body).Result;
                JReply reply = JsonConvert.DeserializeObject<JReply>(json);
                if (reply.errno != 0)
                    return $"错误[{reply.errno}]：{reply.msg}\n重新说点别的吧";
                string mpeg_b64 = reply.data[_prefix.Length..];
                //SaveFileAsBinary(temp_mpeg_name, Convert.FromBase64String(mpeg_b64));
                //string b64_amr = ConvertToAmr();
                Messaging.SendGroupMessage(_targetGroups[msg.SenderID], @"\voice:\base64:" + mpeg_b64);
                //HostBot.SendGroupMessage(this, target_groups[msg.SenderID], "[DEBUG]上面是原mpeg编码。接下来是PCM编码测试：");
                //HostBot.SendGroupMessage(this, target_groups[msg.SenderID], @"\voice:\base64:" + ConvertToSlk());
                return "已发送";
            case "set tone":
                string tone = msg.Text.Trim()[5..];
                if (!_perByName.TryGetValue(tone, out string per))
                    return "不支持这个音色";
                _person = per;
                return $"音色已设置为{_person}";
            default:
                return "[匿名语音模块]意外遇到不应处理的消息，KLBot框架有大问题！";
        }
    }
    
    /// <summary>
    /// 文字转语音。音色取决于模块当前的设置
    /// </summary>
    /// <param name="text">待转换的文字内容</param>
    /// <returns>转换后语音的Base64编码</returns>
    public string TextToSpeech(string text)
    {
        string body = $"type=tns&per={_person}&spd=5&pit=5&vol=15&aue=6&tex={text.Trim()}";
        string json = _httpHelper.PostFormUrlEncodedAsync(_url, body).Result;
        JReply reply = JsonConvert.DeserializeObject<JReply>(json);
        if (reply.errno != 0)
            return $"匿名语音模块TTS API错误[{reply.errno}]：{reply.msg}\n重新说点别的吧";
        string mpeg_b64 = reply.data[_prefix.Length..];
        return @"\voice:\base64:" + mpeg_b64;
    }

    private bool IsNewOrIdleUser(long id) => !_userStat.ContainsKey(id) || _userStat[id] == UserStatus.Idle;
    private void ToWaitForTextState(long userId, long groupId)    //转移至等待输入文本状态
    {
        if (!_userStat.ContainsKey(userId))
            _userStat.Add(userId, UserStatus.ReadyToSendVoice);
        else
            _userStat[userId] = UserStatus.ReadyToSendVoice;
        if (!_targetGroups.ContainsKey(userId))
            _targetGroups.Add(userId, groupId);
        else
            _targetGroups[userId] = groupId;

    }

    private class JReply { public int errno; public string msg; public string data; }
    private enum UserStatus
    {
        Idle = 1, ReadyToSendVoice = 2
    }

}
