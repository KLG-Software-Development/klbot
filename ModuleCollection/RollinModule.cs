#pragma warning disable CS1591 // 缺少对公共可见类型或成员的 XML 注释
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace klbotlib.Modules;

/// <summary>
/// 抽奖模块
/// </summary>
public class RollinModule : SingleTypeModule<MessagePlain>
{
    private static readonly Random _ro = new();
    private static readonly StringBuilder _sb = new();
    [ModuleStatus]
    private bool _hasRollStarted = false;
    [ModuleStatus(IsHidden = true)]
    private readonly HashSet<long> _hash = new();
    [ModuleStatus]
    private readonly List<long> _list = new();
    [ModuleStatus]
    private long _owner = -1;

    public override string FriendlyName => "抽奖模块";
    public override bool UseSignature => false;
    public override string HelpInfo => "发起快速抽奖：发送“抽奖”并@机器人和抽奖包括的群成员，发起快速抽奖。抽奖结果直接公布\n\n发起自愿抽奖：@机器人并发送“抽奖”，可以发起一次自愿抽奖；接下来@机器人并发送“加入”的人员可以参与抽奖；人员加入完成后，发起人@机器人并发送“开始抽奖”可以启动抽奖并公示抽奖结果";

    public override string Filter(MessagePlain msg)
    {
        string text = msg.Text.Trim();
        if (text == "抽奖" && msg.ContainsTargetID(HostBot.SelfID))
        {
            if (msg.TargetID.Count() == 1)  //只@了机器人，主动加入
                return "rollStart";
            else
                return "rollFast";
        }
        else if (text == "加入")
            return "join";
        else if (text == "开始抽奖")
            return "begin";
        else
            return null;
    }

    public override string Processor(MessagePlain msg, string filterOut)
    {
        switch (filterOut)
        {
            case "rollFast":
                List<long> targets = msg.TargetID.ToList();
                targets.Remove(HostBot.SelfID);
                int index = _ro.Next(targets.Count);
                return @$"抽奖结果为：{{\tag:{targets[index]}}}";
            case "rollStart":
                if (_hasRollStarted)
                    return $@"当前已经有尚未结束的抽奖。发起人：{{\tag:{_owner}}}";
                _hasRollStarted = true;
                _owner = msg.SenderID;
                return @$"用户 {{\tag:{_owner}}} 发起了一次抽奖。@机器人并发送“加入”参与抽奖";
            case "join":
                if (!_hasRollStarted)
                    return "当前没有进行中的抽奖";
                if (_hash.Add(msg.SenderID))
                {
                    _list.Add(msg.SenderID);
                    return $"用户 {{\\tag:{msg.SenderID}}} 加入了抽奖。\n当前参与人数：{_hash.Count}";
                }
                else
                    return $"你已经加入过了，{ModuleAccess.GetModule<FuckModule>().SingleSentence()}";
            case "begin":
                if (msg.SenderID != _owner)
                    return @$"只有抽奖发起人可以控制抽奖进程";
                if (_list.Count == 0)
                {
                    //抽奖结束，清理与重置
                    _hash.Clear();
                    _list.Clear();
                    _owner = -1;
                    _hasRollStarted = false;
                    return "无人参加，抽奖已直接结束";
                }
                _sb.Clear();
                foreach (long id in _list)
                {
                    _sb.AppendFormat(@"{{\tag:{0}}}", id);
                    _sb.Append('\n');
                }
                Messaging.ReplyMessage(msg, "抽奖开始。参与者列表：\n" + _sb.ToString());
                index = _ro.Next(_list.Count);
                long winner = _list[index];
                //抽奖结束，清理与重置
                _hash.Clear();
                _list.Clear();
                _owner = -1;
                _hasRollStarted = false;
                return @$"抽奖结果为：{{\tag:{winner}}}";
            default:
                return "未知过滤器输出，建议上吊反悔";
        }
    }
}
