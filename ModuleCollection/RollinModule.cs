#pragma warning disable CS1591 // 缺少对公共可见类型或成员的 XML 注释
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace klbotlib.Modules;

/// <summary>
/// 抽奖模块
/// </summary>
public class RollinModule : SingleTypeModule<MessagePackage>
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

    public override async Task<Message?> Processor(MessageContext context, MessagePackage msg)
    {
        if (msg.TargetIds.Count == 0 || !msg.TargetIds.Contains(HostBot.SelfId))
            return null;
        string text = msg.AsPlain().Trim();
        if (text == "抽奖")
        {
            if (msg.TargetIds.Count == 1)  //只@了机器人，主动加入
            {
                if (_hasRollStarted)
                    return $"当前已经有尚未结束的抽奖。发起人：[{_owner}]";
                _hasRollStarted = true;
                _owner = context.UserId;
                return $"用户 [{_owner}] 发起了一次抽奖。@机器人并发送“加入”参与抽奖";
            }
            else
            {
                List<long> targets = [.. msg.TargetIds];
                targets.Remove(HostBot.SelfId);
                int index = _ro.Next(targets.Count);
                return $"抽奖结果为：{targets[index]}";
            }
        }
        else if (text == "加入")
        {
            if (!_hasRollStarted)
                return "当前没有进行中的抽奖";
            if (_hash.Add(context.UserId))
            {
                _list.Add(context.UserId);
                return $"用户 {{\\tag:{context.UserId}}} 加入了抽奖。\n当前参与人数：{_hash.Count}";
            }
            else
                return $"你已经加入过了，{ModuleAccess.GetModule<FuckModule>().SingleSentence()}";
        }
        else if (text == "开始抽奖")
        {
            if (context.UserId != _owner)
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
            List<Message> msgs = [ "抽奖开始。参与者列表：\n" ];
            msgs.AddRange(_list.Select(id => new MessageAt(id)));
            await Messaging.ReplyMessage(context, new MessagePackage(msgs));
            int index = _ro.Next(_list.Count);
            long winner = _list[index];
            //抽奖结束，清理与重置
            _hash.Clear();
            _list.Clear();
            _owner = -1;
            _hasRollStarted = false;
            return @$"抽奖结果为：{{\tag:{winner}}}";
        }
        else
            return null;
    }
}
