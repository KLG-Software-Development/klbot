#pragma warning disable CS1591 // 缺少对公共可见类型或成员的 XML 注释
using System.Text.Json.Serialization;

namespace klbotlib.Modules;

/// <summary>
/// 抽奖模块
/// </summary>
public class RollinModule : SingleTypeModule<MessagePackage>
{
    [JsonInclude]
    private bool HasRollStarted { get; set; } = false;
    [JsonInclude]
    [HiddenStatus]
    private HashSet<long> Hash { get; set; } = [];
    [JsonInclude]
    private List<long> List { get; set; } = [];
    [JsonInclude]
    private long Owner { get; set; } = -1;

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
                if (HasRollStarted)
                    return $"当前已经有尚未结束的抽奖。发起人：[{Owner}]";
                HasRollStarted = true;
                Owner = context.UserId;
                return $"用户 [{Owner}] 发起了一次抽奖。@机器人并发送“加入”参与抽奖";
            }
            else
            {
                List<long> targets = [.. msg.TargetIds];
                _ = targets.Remove(HostBot.SelfId);
                int index = Random.Shared.Next(targets.Count);
                return $"抽奖结果为：{targets[index]}";
            }
        }
        else if (text == "加入")
        {
            if (!HasRollStarted)
                return "当前没有进行中的抽奖";
            if (Hash.Add(context.UserId))
            {
                List.Add(context.UserId);
                return $"用户 {{\\tag:{context.UserId}}} 加入了抽奖。\n当前参与人数：{Hash.Count}";
            }
            else
                return $"你已经加入过了，{ModuleAccess.GetModule<FuckModule>().SingleSentence()}";
        }
        else if (text == "开始抽奖")
        {
            if (context.UserId != Owner)
                return @$"只有抽奖发起人可以控制抽奖进程";
            if (List.Count == 0)
            {
                //抽奖结束，清理与重置
                Hash.Clear();
                List.Clear();
                Owner = -1;
                HasRollStarted = false;
                return "无人参加，抽奖已直接结束";
            }
            List<Message> msgs = ["抽奖开始。参与者列表：\n"];
            msgs.AddRange(List.Select(id => new MessageAt(id)));
            await Messaging.ReplyMessage(context, new MessagePackage(msgs));
            int index = Random.Shared.Next(List.Count);
            long winner = List[index];
            //抽奖结束，清理与重置
            Hash.Clear();
            List.Clear();
            Owner = -1;
            HasRollStarted = false;
            return @$"抽奖结果为：{{\tag:{winner}}}";
        }
        else
            return null;
    }
}
