using klbotlib.Modules.ModuleUtils;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace klbotlib.Modules;

/// <summary>
/// 匿名语音模块
/// </summary>
public class AnonyVoiceModule : SingleTypeModule<MessagePlain>
{
    [JsonInclude]
    private string _person = "磁性男声";
    [JsonInclude]
    [HiddenStatus]
    private readonly Dictionary<long, UserStatus> _userStat = new();
    [JsonInclude]
    [HiddenStatus]
    private readonly Dictionary<long, long> _targetGroups = new();
    private const string _url = "https://ai.baidu.com/aidemo";
    private const string _prefix = "data:audio/x-mpeg;base64,";
    private readonly Regex _priReqPat = new(@"^说骚话 (\d{9,11})$", RegexOptions.Compiled);
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

    /// <summary>
    /// 构造匿名语音模块
    /// </summary>
    public AnonyVoiceModule()
    {
        _httpHelper.InnerClient.DefaultRequestHeaders.Add("Referer", "https://ai.baidu.com/tech/speech/tts_online");
    }

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
    public override Task<Message?> Processor(MessageContext context, MessagePlain msg)
    {
        string text = msg.Text.Trim();
        if (context.Type is MessageContextType.Temp or MessageContextType.Private)
        {
            //初始请求
            if (IsNewOrIdleUser(context.UserId))
            {
                if (context.Type == MessageContextType.Temp && text == "说骚话")
                {
                    ToWaitForTextState(context.UserId, context.GroupId);
                    return (Message?)"准备好了，你说";
                }
                else if (context.Type == MessageContextType.Private && _priReqPat.IsMatch(text))
                {
                    if (long.TryParse(_priReqPat.Match(msg.Text).Groups[1].Value, out long group_id))
                        ToWaitForTextState(context.UserId, group_id);
                    return (Message?)"准备好了，你说";
                }
            }
            //转语音请求
            else if (_userStat.TryGetValue(context.UserId, out UserStatus value) && value == UserStatus.ReadyToSendVoice)
            {
                _userStat[context.UserId] = UserStatus.Idle;
                Messaging.ReplyMessage(context, "正在薅羊毛...");
                string body = $"type=tns&per={_person}&spd=5&pit=5&vol=15&aue=6&tex={msg.Text.Trim()}";
                string json = _httpHelper.PostFormUrlEncodedAsync(_url, body).Result;
                JReply? reply = JsonSerializer.Deserialize<JReply>(json);
                if (reply == null)
                    throw new JsonException("返回结果解析失败：产生了null结果");
                if (reply.errno != 0)
                    return (Message?)$"错误[{reply.errno}]：{reply.msg}\n重新说点别的吧";
                string mpegB64 = reply.data[_prefix.Length..];
                //SaveFileAsBinary(temp_mpeg_name, Convert.FromBase64String(mpeg_b64));
                //string b64_amr = ConvertToAmr();
                Messaging.SendGroupMessage(_targetGroups[context.UserId], new MessageVoice(mpegB64)); // 当前MessageVoice不支持直接包含语音数据
                //HostBot.SendGroupMessage(this, target_groups[msg.SenderID], "[DEBUG]上面是原mpeg编码。接下来是PCM编码测试：");
                //HostBot.SendGroupMessage(this, target_groups[msg.SenderID], @"\voice:\base64:" + ConvertToSlk());
                return (Message?)"已发送";
            }
        }
        else if (text.StartsWith("设置音色 "))
        {
            string tone = msg.Text.Trim()[5..];
            if (!_perByName.TryGetValue(tone, out string? per))
                return (Message?)"不支持这个音色";
            _person = per;
            return (Message?)$"音色已设置为{_person}";
        }
        return (Message?)null;
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
        JReply? reply = JsonSerializer.Deserialize<JReply>(json);
        if (reply == null)
            throw new JsonException("返回结果解析失败：产生了null结果");
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

    private class JReply { public int errno; public string? msg; public string? data; }
    private enum UserStatus
    {
        Idle = 1, ReadyToSendVoice = 2
    }

}
