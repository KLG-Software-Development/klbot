using klbotlib;
using klbotlib.Modules;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using System;
using System.Collections.Generic;
using System.Net;
using System.Net.Http;
using System.Net.Http.Json;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;

namespace ModuleCollection;

/// <summary>
/// 锐评模块
/// </summary>
public class SharpReviewModule : SingleTypeModule<MessagePlain>
{
    private readonly StringBuilder _sb = new();
    private readonly HttpClient _client;
    private Task _monitorTask;

    [ModuleSetup]
    private int _sleepTimeSeconds = 5;
    [ModuleSetup]
    private readonly HashSet<long> _notificationGroupIds = new();           //通知群ID
    [ModuleStatus]
    private readonly Dictionary<Account, DateTime> _monitoringAccounts = new();    //监听账号 - 上次更新时间
    [ModuleStatus]
    private bool _monitoringEnabled = true;

    /// <summary>
    /// 构造锐评模块
    /// </summary>
    public SharpReviewModule()
    {
        HttpClientHandler handler = new HttpClientHandler()
        {
            AutomaticDecompression = DecompressionMethods.GZip | DecompressionMethods.Deflate
        };
        _client = new(handler);
        //初始化header
        _client.DefaultRequestHeaders.Add("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8");
        _client.DefaultRequestHeaders.Add("Accept-Language", "zh-CN,zh;q=0.8,zh-TW;q=0.7,zh-HK;q=0.5,en-US;q=0.3,en;q=0.2");
        _client.DefaultRequestHeaders.Add("Accept-Encoding", "gzip, deflate, br");
        _client.DefaultRequestHeaders.Add("DNT", "1");
        _client.DefaultRequestHeaders.Add("Referer", "https://m.weibo.cn/u/5040443615");
        _client.DefaultRequestHeaders.Add("Upgrade-Insecure-Requests", "1");
        _client.DefaultRequestHeaders.Add("Sec-Fetch-Dest", "document");
        _client.DefaultRequestHeaders.Add("Sec-Fetch-Mode", "navigate");
        _client.DefaultRequestHeaders.Add("Sec-Fetch-Site", "none");
        _client.DefaultRequestHeaders.Add("Sec-Fetch-User", "?1");
        _monitorTask = Task.Run(Monitor);
        _monitoringAccounts.Add(new Account("dpjj", "https://m.weibo.cn/api/container/getIndex?type=uid&value=5040443615&containerid=1076035040443615"), new DateTime(2022, 6, 18));
    }

    /// <summary>
    /// <inheritdoc/>
    /// </summary>
    public override string Filter(MessagePlain msg)
    {
        //响应守护
        MonitorDaemon();
        //过滤
        string text = msg.Text;
        if (text == "锐评模块状态")
            return "ls";
        else if (text == "暂停锐评监听")
            return "pause";
        else if (text == "开始锐评监听")
            return "resume";
        else
            return null;
    }
    /// <summary>
    /// <inheritdoc/>
    /// </summary>
    public override string Processor(MessagePlain msg, string filterOut)
    {
        switch (filterOut)
        {
            case "ls":
                if (_monitoringAccounts.Count != 0)
                {
                    _sb.Clear();
                    _sb.AppendLine("账号列表：");
                    foreach (var kvp in _monitoringAccounts)
                    {
                        _sb.AppendLine(kvp.Key.Name);
                    }
                    return _sb.ToString();
                }
                else
                    return "锐评账号列表为空";
            case "pause":
                _monitoringEnabled = false;
                return "已暂停";
            case "resume":
                MonitorDaemon();
                _monitoringEnabled = true;
                return "已开始";
            default:
                throw new Exception($"意外遭遇未知过滤器输出“{filterOut}”");
        }
    }

    //监听器
    private void Monitor()
    {
        while (!IsAttached) { };
        ModulePrint("启动监视任务...");
        try
        {
            while (true)
            {
                if (_monitoringEnabled)
                {
                    foreach (var kvp in _monitoringAccounts)
                    {
                        string url = kvp.Key.Url;
                        string json = _client.GetStringAsync(url).Result;
                        var result = JsonConvert.DeserializeObject<JObject>(json);
                        if (result["ok"].ToString() != "1")
                        {
                            ModulePrint("成功获取JSON但ok字段非1");
                            return;
                        }
                        //DateTime time = new(result);
                    }
                }
                Thread.Sleep(_sleepTimeSeconds * 1000);
            }
        }
        catch (Exception ex)
        {
            if (IsAttached)
            {
                foreach (long groupId in _notificationGroupIds)
                {
                    Messaging.SendGroupMessage(groupId, "锐评模块监听器发生异常：\n" + ex);
                }
            }
            else
                Console.WriteLine("锐评模块监听器发生异常：\n" + ex);
        }
    }
    //监听守护
    private void MonitorDaemon()
    {
        if (_monitorTask.Status != TaskStatus.Running)
        {
            if (IsAttached)
                ModulePrint("监听任务意外终止。重启中...");
            else
                Console.WriteLine("监听任务意外终止。重启中...");
            _monitorTask = Task.Run(Monitor);
        }
    }

    private class Account  //帐号类。包含监控和管理需要的全部信息
    {
        public string Name { get; }   //账号名称，用于管理监控账号
        public string Url { get; }  //账号监控URL，用于爬取账号动态

        public Account(string name, string url)
        {
            Name = name;
            Url = url;
        }
    }
    private class WeiboMessage
    {
        public DateTime Time { get; }
        public string Source { get; }
        public string Text { get; }

        public WeiboMessage(DateTime time, string source, string text)
        {
            Time = time;
            Source = source;
            Text = text;
        }
    }
}
