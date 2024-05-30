using System;
using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;

namespace klbotlib.Modules;

/// <summary>
/// 上号模块
/// </summary>
public class ResetModule : SingleTypeModule<MessagePlain>
{
    [ModuleSetup]
    private readonly TimeSpan _smallTimeSpan = new(12, 0, 0);
    [ModuleStatus]
    private readonly Dictionary<long, DateTime> _lastUpdatedDays = new();
    [ModuleStatus]
    private readonly Dictionary<long, TimeSpan> _bestRecords = new();

    /// <inheritdoc/>
    public override string FriendlyName => "Reset模块";
    /// <inheritdoc/>
    public override string HelpInfo => "@机器人并：\n发送“day?”可以查询数据；发送“reset”或“day0”可以重置数据";

    /// <inheritdoc/>
    public override Task<Message?> Processor(MessageContext context, MessagePlain msg)
    {
        if (msg.ContainsTargetId(HostBot.SelfId))
        {
            string msgText = msg.Text.Trim().ToLower();
            if (msgText == "day?")
            {
                if (!_lastUpdatedDays.ContainsKey(context.UserId))
                    return (Message)"未找到数据。@机器人并发送“reset”或“day0”创建第一条数据";
                else
                {
                    TimeSpan dt = DateTime.Now - _lastUpdatedDays[context.UserId];
                    return (Message)$"距离上次reset已经过去{TimeSpanToString(dt)}";
                }
            }
            else if (msgText == "reset" || msgText == "day0")
            {
                if (!_lastUpdatedDays.ContainsKey(context.UserId))
                {
                    _lastUpdatedDays.Add(context.UserId, DateTime.Now);
                    _bestRecords.Add(context.UserId, new TimeSpan());
                    return (Message)@$"成功为用户[{context.UserId}]创建数据";
                }
                else
                {
                    TimeSpan dt = DateTime.Now - _lastUpdatedDays[context.UserId];
                    _lastUpdatedDays[context.UserId] = DateTime.Now;
                    //是否打破了记录
                    TimeSpan record = _bestRecords[context.UserId];
                    if (dt > record)
                    {
                        _bestRecords[context.UserId] = dt;
                        return (Message)$"[{context.UserId}]成功创造了{TimeSpanToString(_bestRecords[context.UserId])}的新纪录！";
                    }
                    else
                    {
                        TimeSpan distanceToGoal = record - dt;
                        if (distanceToGoal < _smallTimeSpan)
                            return (Message)$"已重置数据。\n非常可惜，[{{\\tag:{context.UserId}}}]距离刷新纪录仅剩{TimeSpanToString(distanceToGoal)}";
                        else
                            return (Message)"已重置数据";
                    }
                }
            }
            else
                return (Message?)null;
        }
        else
            return (Message?)null;
   }

    private string TimeSpanToString(TimeSpan ts)
    {
        StringBuilder sb = new();
        if (ts.Days != 0)
            sb.AppendFormat("{0}天", ts.Days);
        if (ts.Hours != 0)
            sb.AppendFormat("{0}小时", ts.Hours);
        if (ts.Minutes != 0)
            sb.AppendFormat("{0}分钟", ts.Minutes);
        if (ts.Seconds != 0)
            sb.AppendFormat("{0}秒", ts.Seconds);
        return sb.ToString();
    }
}
