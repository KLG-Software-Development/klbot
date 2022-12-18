#pragma warning disable CS8600 // 将 null 字面量或可能为 null 的值转换为非 null 类型。
using klbotlib;
using klbotlib.Exceptions;
using klbotlib.MessageServer.Debug;
using klbotlib.Modules;
using ModuleCollection;
using System;
using System.Reflection;
using System.Text.RegularExpressions;
using System.Threading;
using Module = klbotlib.Modules.Module;

namespace localbot;

public class Program
{
    private static readonly HashSet<long> _debugTargetGroupID = new() { 7355608 };  //调试时监听的群组列表
    private static readonly DebugMessageServer _localServer = new(
        AddMsgCallback_PrintInfo, 
        SendMsgCallback_PrintInfo, 
        UploadCallback_PrintInfo,
        MuteCallback_PrintInfo,
        UnmuteCallback_PrintInfo); //调试用消息服务器
    private static long _userId = 2044164212;    //调试时发出的所有消息的用户ID
    private static long _groupId = 7355608;   //调试时发出的所有消息的群组ID
    private static MessageContext _context = MessageContext.Group;   //调试时发出的所有消息的上下文类型。默认为群组
    private static bool _tagMe = false;
    private static bool _verbose = false;

    public static async Task Main()
    {
        Console.ResetColor();
        long queryCounterCache = 0;
        int fatalFailureCounter = 0;
start:
        KLBot lcb = null;
        try
        {
            Assembly? asm = Assembly.GetAssembly(typeof(ImageModule));
            if (asm == null)
                throw new NullReferenceException("无法获取模块集合所在的程序集");
            lcb = new KLBot(_localServer, asm, _debugTargetGroupID);
            await lcb.AddModule(new CounterBanModule());

            Console.WriteLine(lcb.GetModuleChainString());
            Console.WriteLine("初始化完成。命令调用格式：<命令> <值>");
            PrintHelp();
            Console.WriteLine("输入命令开始调试");
            MainLoop(lcb);
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
            else if (ex is ModuleSetupException)
            {
                Console.ForegroundColor = ConsoleColor.DarkYellow;
                Console.WriteLine(ex.Message);
                Console.WriteLine($"模块配置异常。检查模块的配置文件是否正确，以及该模块是否恰当遵守了模块开发规范");
                Console.WriteLine("退出中...");
                Console.ResetColor();
            }
            else  //无法处理的未知情况
            {
                Console.ForegroundColor = ConsoleColor.DarkYellow;
                Console.WriteLine(ex.Message);
                Console.WriteLine($"意外异常：{ex.Message}\n调用栈：\n{ex.StackTrace}\n");
                Console.ResetColor();
                if (lcb != null && queryCounterCache == lcb.DiagData.SuccessPackageCount)   //sucess_counter距离上次出错之后没有发生变化，意味着本次出错紧接着上一次
                    fatalFailureCounter++;
                else                                         //否则意味着并非基本错误，此时优先保持服务运作，基本错误计数器归零
                    fatalFailureCounter = 0;
                if (fatalFailureCounter > 10)
                {
                    Console.WriteLine("连续10次发生致命错误。将停止重试并有序退出");
                    if (lcb != null)
                        lcb.OnExit();
                    return;
                }
                else
                {
                    //query_counter_cache = klg.DiagData.SuccessPackageCount;
                    if (lcb != null)
                        lcb.OnExit();
                    Console.WriteLine($"[{DateTime.Now:G}] 正在尝试重启KLBot...\n");
                    goto start;
                }
            }
        }
    }

    //**** Callback ****
    //添加消息（即收到消息）
    private static void AddMsgCallback_PrintInfo(Message msg)
    {
        Console.WriteLine(GetMessageDebugInfo(msg));
        PrintVerboseInfoIfRequired(msg);
    }
    //发送消息
    private static void SendMsgCallback_PrintInfo(Module module, MessageContext context, long userId, long groupId, string content)
        => Console.WriteLine(GetMessageDebugInfo(module, context, userId, groupId, content));
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
    private static string GetMessageDebugInfo(Message msg)
    {
        string content;
        if (msg is MessageCommon msgCommon)
        {
            if (msgCommon is MessagePlain pmsg)
                content = pmsg.Text;
            else if (msgCommon is MessageImage imsg)
                content = $"[图片x{imsg.UrlList.Count}]";
            else if (msgCommon is MessageFlashImage fmsg)
                content = $"[闪照x{fmsg.UrlList.Count}]";
            else if (msgCommon is MessageImagePlain ipmsg)
                content = $"[图片x{ipmsg.UrlList.Count}] {ipmsg.Text}";
            else if (msgCommon is MessageVoice vmsg)
                content = $"[语音消息]";
            else
                content = $"[未知类型消息：{msgCommon}]";
            return msgCommon.Context switch
            {
                MessageContext.Group => $"* 用户[{msgCommon.SenderID}]向群组[{msgCommon.GroupID}]发送：\n------------------------------------\n  {content}\n------------------------------------",
                MessageContext.Temp => $"* 用户[{msgCommon.SenderID}]通过群组[{msgCommon.GroupID}]发送：\n------------------------------------\n  {content}\n------------------------------------",
                MessageContext.Private => $"* 用户[{msgCommon.SenderID}]发送：\n------------------------------------\n  {content}\n------------------------------------",
                _ => $"* 用户[{msgCommon.SenderID}]向群组[{msgCommon.GroupID}]或机器人发送了未知类型[{msgCommon.Context}]的消息，内容：\n------------------------------------\n  {content}\n------------------------------------",
            };
        }
        else if (msg is MessageRecall msgRecall)
        {
            string person = msgRecall.AuthorID == msgRecall.OperatorID ? "自己" : $"用户[{msgRecall.AuthorID}]";
            return msgRecall.Context switch
            {
                MessageContext.Group => $"用户[{msgRecall.OperatorID}]在群聊[{msgRecall.GroupID}]中撤回了{person}发送的消息[{msgRecall.MessageID}]",
                MessageContext.Temp => $"* 用户[{msgRecall.OperatorID}]在通过群组[{msgRecall.GroupID}]的临时会话中撤回了{person}发送的消息[{msgRecall.MessageID}]",
                MessageContext.Private => $"* 用户[{msgRecall.OperatorID}]撤回了{person}发送的消息[{msgRecall.MessageID}]",
                _ => $"* 用户[{msgRecall.OperatorID}]在机器人可感知的范围内撤回了{person}发送的消息[{msgRecall.MessageID}]"
            };
        }
        else if (msg is MessageMute msgMute)
        {
            if (msgMute.IsUnmute)
            {
                string person = msgMute.MemberID == msgMute.OperatorID ? "自己" : $"用户[{msgMute.MemberID}]";
                return $"用户[{msgMute.OperatorID}]在群聊[{msgMute.GroupID}]中解除了{person}的禁言";
            }
            else
                return $"用户[{msgMute.OperatorID}]在群聊[{msgMute.GroupID}]中禁言了用户[{msgMute.MemberID}]";
        }
        else
            return $"[未知类型消息：{msg}]";
    }
    /// <summary>
    /// 生成消息的调试信息字符串
    /// </summary>
    /// <param name="module">模块</param>
    /// <param name="context">消息上下文</param>
    /// <param name="userId">目标用户ID</param>
    /// <param name="groupId">目标群组ID</param>
    /// <param name="content">MsgMarker内容</param>
    /// <returns>消息的调试信息</returns>
    private static string GetMessageDebugInfo(Module module, MessageContext context, long userId, long groupId, string content)
    {
        return context switch
        {
            MessageContext.Group => $"* 模块[{module}]向群组[{groupId}]发送：\n------------------------------------\n  {content}\n------------------------------------",
            MessageContext.Temp => $"* 模块[{module}]通过群组[{groupId}]向用户[{userId}]发送：\n------------------------------------\n  {content}\n------------------------------------",
            MessageContext.Private => $"* 模块[{module}]向用户[{userId}]发送：\n------------------------------------\n  {content}\n------------------------------------",
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
            string s = Console.ReadLine();
            if (s == null)
                continue;
            SplitCommand(s, out string cmd, out string arg);
            ProcessCommand(lcb, cmd, arg);
            lcb.ProcessMessages(lcb.FetchMessages()).Wait();
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
        Console.WriteLine("mute <user>,<duration (s)>                     禁言指定用户");
        Console.WriteLine("unmute <user>                                 解禁指定用户");
    }
    private static void SendMessageCommonAndPrint(KLBot lcb, MessageCommon msg)
    {
        if (_tagMe)
            msg.AddTargetID(lcb.SelfID);
        _localServer.AddReceivedMessage(msg);
        if (_verbose)
        {
            Console.WriteLine("\n详细信息：");
            Console.WriteLine(msg.ToString());
            Console.WriteLine();
        }
    }
    private static void SplitCommand(string cmd, out string command, out string argument)
    {
        for (int i = 0; i < cmd.Length; i++)
        {
            if (cmd[i] == ' ')
            {
                command = cmd.Substring(0, i);
                argument = cmd.Substring(i + 1);
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
                _tagMe = !_tagMe;
                string status = _tagMe ? "开启" : "关闭";
                Console.WriteLine($"Tag Me模式已{status}");
                return;
            case "verbose":
                _verbose = !_verbose;
                status = _verbose ? "开启" : "关闭";
                Console.WriteLine($"详细模式已{status}");
                return;
            case "set-user-id":
                if (long.TryParse(arg, out long userId))
                {
                    _userId = userId;
                    Console.WriteLine($"消息发送者ID已设置为[{_userId}]");
                }
                else
                    Console.WriteLine($"{arg}不是有效的ID");
                return;
            case "set-group-id":
                if (long.TryParse(arg, out long groupId))
                {
                    _groupId = groupId;
                    Console.WriteLine($"消息发送所在群ID已设置为[{_groupId}]");
                }
                else
                    Console.WriteLine($"{arg}不是有效的ID");
                return;
            case "set-context":
                Command_SetContext(arg);
                return;
            case "send-plain":
                SendMessageCommonAndPrint(lcb, new MessagePlain(_context, _userId, _groupId, arg));
                return;
            case "send-image":
                SendMessageCommonAndPrint(lcb, new MessageImage(_context, _userId, _groupId, arg.Split(',')));
                return;
            case "send-flashimage":
                SendMessageCommonAndPrint(lcb, new MessageFlashImage(_context, _userId, _groupId, arg.Split(',')));
                return;
            case "send-voice":
                SendMessageCommonAndPrint(lcb, new MessageVoice(_context, _userId, _groupId, arg));
                return;
            case "recall":
                Command_Recall(arg);
                return;
            case "send-image-plain":
                Command_SendImagePlain(arg, lcb);
                return;
            case "unmute":
                CommandUnmute(arg);
                return;
            case "mute":
                CommandMute(arg);
                return;
            default:
                return;
        }
    }
    private static void Command_SetContext(string arg)
    {
        switch (arg.ToLower())
        {
            case "group":
                _context = MessageContext.Group;
                Console.WriteLine($"会话上下文已设置为群聊");
                return;
            case "temp":
                _context = MessageContext.Temp;
                Console.WriteLine($"会话上下文已设置为临时会话");
                return;
            case "private":
                _context = MessageContext.Private;
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
        MessageRecall recall = new(_context, authorId, _userId, _groupId, msgId);
        _localServer.AddReceivedMessage(recall);
    }
    private static void Command_SendImagePlain(string arg, KLBot lcb)
    {
        string[] args = arg.Split();
        if (args.Length != 2)
        {
            Console.WriteLine("参数不足。应有2个参数：图片URL(s), 文本内容");
            return;
        }
        string[] urls = args[0].Split(',');
        string text = args[1];
        SendMessageCommonAndPrint(lcb, new MessageImagePlain(_context, _userId, _groupId, text, urls));
    }
    private static void CommandMute(string arg)
    {
        string[] args = arg.Split(',');
        if (args.Length != 2)
        {
            Console.WriteLine("参数数量错误。应有2个参数：待禁言用户ID, 禁言时长(秒)");
            return;
        }
        if (!long.TryParse(args[0], out long userId))
        {
            Console.WriteLine("无效成员ID");
            return;
        }
        if (!uint.TryParse(args[1], out uint durationSeconds))
        {
            Console.WriteLine("无效禁言时长");
            return;
        }
        MessageMute mute = new(false, _groupId, _userId, userId, durationSeconds);
        _localServer.AddReceivedMessage(mute);
    }
    private static void CommandUnmute(string arg)
    {
        string[] args = arg.Split(',');
        args = arg.Split(',');
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
        MessageMute unmute = new(true, _groupId, _userId, userId);
        _localServer.AddReceivedMessage(unmute);
    }

    private static void PrintVerboseInfoIfRequired(Message msg)
    {
        if (_verbose)
        {
            Console.WriteLine("\n详细信息：");
            Console.WriteLine(msg.ToString());
            Console.WriteLine();
        }
    }
}