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
    public override string? Filter(MessagePlain msg)
    {
        if (msg.ContainsTargetID(HostBot.SelfId))
        {
            string msgText = msg.Text.Trim().ToLower();
            if (msgText == "day?")
                return "day?";
            else if (msgText == "reset" || msgText == "day0")
                return "reset";
            else
                return null;
        }
        else
            return null;
    }
    /// <inheritdoc/>
    public override Task<string> Processor(MessagePlain msg, string? filterOut)
    {
        switch (filterOut)
        {
            case "day?":

                if (!_lastUpdatedDays.ContainsKey(msg.SenderID))
                    return Task.FromResult("未找到数据。@机器人并发送“reset”或“day0”创建第一条数据");
                else
                {
                    TimeSpan dt = DateTime.Now - _lastUpdatedDays[msg.SenderID];
                    return Task.FromResult($"距离上次reset已经过去{TimeSpanToString(dt)}");
                }
            case "reset":
                if (!_lastUpdatedDays.ContainsKey(msg.SenderID))
                {
                    _lastUpdatedDays.Add(msg.SenderID, DateTime.Now);
                    _bestRecords.Add(msg.SenderID, new TimeSpan());
                    return Task.FromResult(@$"成功为用户[{{\tag:{msg.SenderID}}}]创建数据");
                }
                else
                {
                    TimeSpan dt = DateTime.Now - _lastUpdatedDays[msg.SenderID];
                    _lastUpdatedDays[msg.SenderID] = DateTime.Now;
                    //是否打破了记录
                    TimeSpan record = _bestRecords[msg.SenderID];
                    if (dt > record)
                    {
                        _bestRecords[msg.SenderID] = dt;
                        return Task.FromResult(@$"[{{\tag:{msg.SenderID}}}]成功创造了{TimeSpanToString(_bestRecords[msg.SenderID])}的新纪录！");
                    }
                    else
                    {
                        TimeSpan distanceToGoal = record - dt;
                        if (distanceToGoal < _smallTimeSpan)
                            return Task.FromResult($"已重置数据。\n非常可惜，[{{\\tag:{msg.SenderID}}}]距离刷新纪录仅剩{TimeSpanToString(distanceToGoal)}");
                        else
                            return Task.FromResult("已重置数据");
                    }
                }
            default:
                throw new Exception($"意外遇到未实现的过滤器输出\"{filterOut}\"");
        }
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
