﻿using klbotlib;
using klbotlib.Exceptions;
using klbotlib.MessageDriver.DebugLocal;
using klbotlib.Modules;
using Microsoft.Extensions.Configuration;
using System.Reflection;
using Module = klbotlib.Modules.Module;

namespace localbot;

public class Program
{
    private static readonly HashSet<long> s_debugTargetGroupId = [7355608];  //调试时监听的群组列表
    private static readonly MessageDriver_Debug s_localServer = new(
        33550336,   //自身ID
        AddMsgCallback_PrintInfo,
        SendMsgCallback_PrintInfo,
        UploadCallback_PrintInfo,
        MuteCallback_PrintInfo,
        UnmuteCallback_PrintInfo); //调试用消息驱动器
    private static long s_userId = 2044164212;    //调试时发出的所有消息的用户ID
    private static long s_groupId = 7355608;   //调试时发出的所有消息的群组ID
    private static MessageContextType s_context = MessageContextType.Group;   //调试时发出的所有消息的上下文类型。默认为群组
    private static bool s_tagMe = false;
    private static bool s_verbose = false;

    private static KLBot? s_lcb = null;

    public static void Main()
    {
        Console.ResetColor();
        DateTime lastErrorTime = DateTime.MinValue;
        int serialErrorCounter = 0;
    start:
        try
        {
            Assembly? asm = Assembly.GetAssembly(typeof(ImageModule)) ?? throw new NullReferenceException("无法获取模块集合所在的程序集");
            IConfigurationRoot config = new ConfigurationBuilder().AddInMemoryCollection(
                new Dictionary<string, string?>()
                {
                    { "targets", "" },
                    { "cache_dir", "cache" },
                    { "save_dir", "save" }
                })
            .Build();

            s_lcb = new KLBot(config, s_localServer, asm, s_debugTargetGroupId);
            s_lcb.AddModule(new TimeModule());

            Console.WriteLine(s_lcb.GetModuleChainString());
            Console.WriteLine("初始化完成。命令调用格式：<命令> <值>");
            PrintHelp();
            Console.WriteLine("输入命令开始调试");
            MainLoop(s_lcb);
        }
        catch (Exception ex)
        {
            if (ex is KLBotInitializationException)
            {
                Console.ForegroundColor = ConsoleColor.DarkYellow;
                Console.WriteLine(ex.Message);
                Console.WriteLine("初始化失败。退出中...");
                Console.ResetColor();
            }
            else  //无法处理的未知情况
            {
                Console.ForegroundColor = ConsoleColor.DarkYellow;
                Console.WriteLine(ex.Message);
                Console.WriteLine($"意外异常：{ex.Message}\n调用栈：\n{ex.StackTrace}\n");
                Console.ResetColor();
                serialErrorCounter = DateTime.Now - lastErrorTime < TimeSpan.FromSeconds(6) ? serialErrorCounter + 1 : 0;
                if (serialErrorCounter > 10)
                {
                    Console.WriteLine("连续10次发生致命错误。将停止重试并有序退出");
                    s_lcb?.OnExit();
                    return;
                }
                else
                {
                    s_lcb?.OnExit();
                    Console.WriteLine($"[{DateTime.Now:G}] 正在尝试重启KLBot...\n");
                    goto start;
                }
            }
        }
    }

    //**** Callback ****
    //添加消息（即收到消息）
    private static void AddMsgCallback_PrintInfo(MessageContext context, Message msgPkg)
    {
        Console.WriteLine(GetMessageDebugInfo(context, msgPkg));
        PrintVerboseInfoIfRequired(msgPkg);
    }
    //发送消息
    private static void SendMsgCallback_PrintInfo(Module module, MessageContextType context, long userId, long groupId, Message msg)
        => Console.WriteLine(GetMessageDebugInfo(module, context, userId, groupId, msg.ToString()));
    //上传文件
    private static void UploadCallback_PrintInfo(Module module, long groupId, string uploadPath, string filePath)
        => Console.WriteLine(GetUploadDebugInfo(module, groupId, uploadPath, filePath));
    //禁言callback
    private static void MuteCallback_PrintInfo(Module module, long userId, long groupId, uint durationSeconds)
        => Console.WriteLine(GetMuteDebugInfo(module, userId, groupId, durationSeconds));
    //解除禁言callback
    private static void UnmuteCallback_PrintInfo(Module module, long userId, long groupId)
        => Console.WriteLine(GetUnmuteDebugInfo(module, userId, groupId));

    //**** 用于生成调试信息的函数 ****
    //消息
    /// <summary>
    /// 生成消息的调试信息字符串
    /// </summary>
    /// <returns>消息的调试信息</returns>
    private static string GetMessageDebugInfo(MessageContext context, Message msg)
    {
        string content = msg is MessagePlain pmsg
            ? pmsg.Text
            : msg is MessageImage imsg
            ? imsg.IsFlashImage ? $"[闪照:{imsg.Url}]" : $"[图片x{imsg.Url}]"
            : msg is MessageVoice vmsg ? $"[语音消息:<{vmsg.Url.Length} chars>]" : $"[未知类型消息：{msg}]";
        return context.Type switch
        {
            MessageContextType.Group => $"* 用户[{context.UserId}]向群组[{context.GroupId}]发送：\n------------------------------------\n  {content}\n------------------------------------",
            MessageContextType.Temp => $"* 用户[{context.UserId}]通过群组[{context.GroupId}]发送：\n------------------------------------\n  {content}\n------------------------------------",
            MessageContextType.Private => $"* 用户[{context.UserId}]发送：\n------------------------------------\n  {content}\n------------------------------------",
            _ => $"* 用户[{context.UserId}]向群组[{context.GroupId}]或机器人发送了未知类型[{context.Type}]的消息，内容：\n------------------------------------\n  {content}\n------------------------------------",
        };
    }
    /// <summary>
    /// 生成消息的调试信息字符串
    /// </summary>
    /// <param name="module">模块</param>
    /// <param name="context">消息上下文</param>
    /// <param name="userId">目标用户ID</param>
    /// <param name="groupId">目标群组ID</param>
    /// <param name="content">待发送消息</param>
    /// <returns>消息的调试信息</returns>
    private static string GetMessageDebugInfo(Module module, MessageContextType context, long userId, long groupId, string content)
    {
        return context switch
        {
            MessageContextType.Group => $"* 模块[{module}]向群组[{groupId}]发送：\n------------------------------------\n  {content}\n------------------------------------",
            MessageContextType.Temp => $"* 模块[{module}]通过群组[{groupId}]向用户[{userId}]发送：\n------------------------------------\n  {content}\n------------------------------------",
            MessageContextType.Private => $"* 模块[{module}]向用户[{userId}]发送：\n------------------------------------\n  {content}\n------------------------------------",
            _ => $"* 模块[{module}]向群组[{groupId}]或用户[{userId}]发送了未知类型[{context}]的消息，内容：\n------------------------------------\n  {content}\n------------------------------------",
        };
    }
    //上传文件
    private static string GetUploadDebugInfo(Module module, long groupId, string uploadPath, string filePath)
    {
        return $"* 模块[{module}]向群组[{groupId}]上传文件[{filePath}]到群文件夹[{uploadPath}]";
    }
    //禁言
    private static string GetMuteDebugInfo(Module module, long userId, long groupId, uint durationSeconds)
    {
        return $"* 模块[{module}]在群组[{groupId}]中将用户[{userId}]禁言{durationSeconds}秒";
    }
    //解除禁言
    private static string GetUnmuteDebugInfo(Module module, long userId, long groupId)
    {
        return $"* 模块[{module}]在群组[{groupId}]中解除了用户[{userId}]的禁言";
    }

    //**** Helper函数 ****
    private static void MainLoop(KLBot lcb)
    {
        while (true)
        {
            Console.Write("\n>");
            string? s = Console.ReadLine();
            if (s == null)
                continue;
            SplitCommand(s, out string cmd, out string arg);
            ProcessCommand(lcb, cmd, arg);
            Thread.Sleep(1000);
        }
    }
    private static void PrintHelp()
    {
        Console.WriteLine("\nLocalBot设定：");
        Console.WriteLine("help                                          打印帮助信息");
        Console.WriteLine("verbose                                       开启详细模式");
        Console.WriteLine("\n全局消息设定：");
        Console.WriteLine("tag-me                                        开启Tag Me模式（自动@机器人）");
        Console.WriteLine("set-user-id <id>                              设置全局用户ID");
        Console.WriteLine("set-group-id <id>                             设置全局群聊ID");
        Console.WriteLine("set-context <private|group|temp>              设置全局消息上下文");
        Console.WriteLine("\n发送消息：");
        Console.WriteLine("send-plain <text>                             发送纯文本消息");
        Console.WriteLine("send-image <URL1,URL2,URL3...>                发送图像消息");
        Console.WriteLine("send-flashimage <URL1,URL2,URL3...>           发送闪照消息");
        Console.WriteLine("send-image-plain <URL1,URL2,URL3...> <text>   发送图文消息");
        Console.WriteLine("send-voice <URL>                              发送语音消息");
        Console.WriteLine("\n其他操作：");
        Console.WriteLine("recall <author>,<message id>                  撤回指定消息");
        Console.WriteLine("mute <user>,<duration (s)>                    禁言指定用户");
        Console.WriteLine("unmute <user>                                 解禁指定用户");
        Console.WriteLine("\n调试命令：");
        Console.WriteLine("save all                                      保存所有模块的配置与状态");
    }
    private static void SendMessageAndPrint(KLBot lcb, Message msg)
    {
        if (s_tagMe)
        {
            msg = msg.Pack();
            ((MessagePackage)msg).AddTargetId(lcb.SelfId);
        }
        MessageContext context = new(s_context, s_userId, s_groupId);
        _ = s_localServer.AddReceivedMessage(context, msg);
        if (s_verbose)
        {
            Console.WriteLine("\n详细信息：");
            Console.WriteLine($"{context}\n{msg}");
            Console.WriteLine();
        }
    }
    private static void SplitCommand(string cmd, out string command, out string argument)
    {
        for (int i = 0; i < cmd.Length; i++)
        {
            if (cmd[i] == ' ')
            {
                command = cmd[..i];
                argument = cmd[(i + 1)..];
                return;
            }
        }
        command = cmd;
        argument = string.Empty;
        return;
    }
    private static void ProcessCommand(KLBot lcb, string cmd, string arg)
    {
        switch (cmd)
        {
            case "help":
                PrintHelp();
                return;
            case "tag-me":
                s_tagMe = !s_tagMe;
                string status = s_tagMe ? "开启" : "关闭";
                Console.WriteLine($"Tag Me模式已{status}");
                return;
            case "verbose":
                s_verbose = !s_verbose;
                status = s_verbose ? "开启" : "关闭";
                Console.WriteLine($"详细模式已{status}");
                return;
            case "set-user-id":
                if (long.TryParse(arg, out long userId))
                {
                    s_userId = userId;
                    Console.WriteLine($"消息发送者ID已设置为[{s_userId}]");
                }
                else
                    Console.WriteLine($"{arg}不是有效的ID");
                return;
            case "set-group-id":
                if (long.TryParse(arg, out long groupId))
                {
                    s_groupId = groupId;
                    Console.WriteLine($"消息发送所在群ID已设置为[{s_groupId}]");
                }
                else
                    Console.WriteLine($"{arg}不是有效的ID");
                return;
            case "set-context":
                Command_SetContext(arg);
                return;
            case "send-plain":
                SendMessageAndPrint(lcb, new MessagePackage(arg));
                return;
            case "send-image":
                SendMessageAndPrint(lcb, new MessageImage(arg, false));
                return;
            case "send-flashimage":
                SendMessageAndPrint(lcb, new MessageImage(arg, true));
                return;
            case "send-voice":
                SendMessageAndPrint(lcb, new MessageVoice(arg));
                return;
            case "recall":
                Command_Recall(arg);
                return;
            case "unmute":
                CommandUnmute(arg);
                return;
            case "mute":
                CommandMute(arg);
                return;
            case "save":
                lcb.ModuleChain.ForEach(async m =>
                {
                    await m.SaveModuleStatus();
                });
                return;
            default:
                Console.WriteLine("Unknown command.");
                return;
        }
    }
    private static void Command_SetContext(string arg)
    {
        switch (arg.ToLower())
        {
            case "group":
                s_context = MessageContextType.Group;
                Console.WriteLine($"会话上下文已设置为群聊");
                return;
            case "temp":
                s_context = MessageContextType.Temp;
                Console.WriteLine($"会话上下文已设置为临时会话");
                return;
            case "private":
                s_context = MessageContextType.Private;
                Console.WriteLine($"会话上下文已设置为私聊");
                return;
            default:
                Console.WriteLine("未知上下文类型。已忽略");
                return;
        }
    }
    private static void Command_Recall(string arg)
    {
        string[] args = arg.Split(',');
        if (args.Length < 2)
        {
            Console.WriteLine("参数不足。应有2个参数：消息发送者ID,消息ID");
            return;
        }
        if (!long.TryParse(args[0], out long authorId))
        {
            Console.WriteLine("无效发送者ID");
            return;
        }
        if (!long.TryParse(args[1], out long msgId))
        {
            Console.WriteLine("无效消息ID");
            return;
        }
        _ = s_localServer.AddReceivedMessage(new(s_context, s_userId, s_groupId), new MessageRecall(authorId, msgId));
    }
    private static void CommandMute(string arg)
    {
        string[] args = arg.Split(',');
        if (args.Length != 2)
        {
            Console.WriteLine("参数数量错误。应有2个参数：待禁言用户ID, 禁言时长(秒)");
            return;
        }
        if (!long.TryParse(args[0], out _))
        {
            Console.WriteLine("无效成员ID");
            return;
        }
        if (!uint.TryParse(args[1], out uint durationSeconds))
        {
            Console.WriteLine("无效禁言时长");
            return;
        }
        MessageMute mute = new(true, s_userId, durationSeconds);
        _ = s_localServer.AddReceivedMessage(new(MessageContextType.Group, s_userId, s_groupId), mute.Pack());
    }
    private static void CommandUnmute(string arg)
    {
        string[] args = arg.Split(',');
        if (args.Length != 1)
        {
            Console.WriteLine("参数数量错误。应有1个参数：待解除禁言用户ID");
            return;
        }
        if (!long.TryParse(args[0], out long userId))
        {
            Console.WriteLine("无效成员ID");
            return;
        }
        MessageMute unmute = new(true, userId);
        _ = s_localServer.AddReceivedMessage(new(MessageContextType.Group, s_groupId, s_userId), unmute);
    }
    private static void PrintVerboseInfoIfRequired(Message msg)
    {
        if (s_verbose)
        {
            Console.WriteLine("\n详细信息：");
            Console.WriteLine(msg.ToString());
            Console.WriteLine();
        }
    }
}