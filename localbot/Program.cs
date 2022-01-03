#pragma warning disable CS8600 // 将 null 字面量或可能为 null 的值转换为非 null 类型。
using klbotlib;
using klbotlib.Exceptions;
using klbotlib.Extensions;
using klbotlib.MessageServer.Debug;
using klbotlib.Modules;
using System.Reflection;

namespace localbot;

public class Program
{
    private static readonly List<long> _debugTargetGroupID = new() { 7355608 };  //调试时监听的群组列表
    private static readonly DebugMessageServer _localServer = new(PrintInfo, PrintInfo, PrintInfo); //调试用消息服务器
    private static long _userId = 42;    //调试时发出的所有消息的用户ID
    private static long _groupId = 7355608;   //调试时发出的所有消息的群组ID
    private static MessageContext _context = MessageContext.Group;   //调试时发出的所有消息的上下文类型。默认为群组

    public static void Main()
    {
        Console.WriteLine("localbot Debug environment");
        Environment.SetEnvironmentVariable("DOTNET_ReadyToRun", "0");           //Disable AOT
        Environment.SetEnvironmentVariable("DOTNET_TieredPGO", "1");            //Turn on layered PGO
        Environment.SetEnvironmentVariable("DOTNET_TC_QuickJitForLoops", "1");  //Enable Quick Jit for loop
        Console.ResetColor();
        Version? exeVersion = Assembly.GetExecutingAssembly().GetName().Version;
        if (exeVersion == null)
            exeVersion = new Version();
        Version lib_version = klbotlib.Info.CoreLibInfo.GetLibVersion();
        Version mcVersion = ModuleCollection.Info.CollectionInfo.GetLibVersion();
        long queryCounterCache = 0;
        int fatalFailureCounter = 0;
        Console.WriteLine($"KLBot via mirai");
        Console.WriteLine($"exe version: {exeVersion.Major}.{exeVersion.Minor} Build {exeVersion.ToKLGBuildString()}");
        Console.WriteLine($"corelib version: {lib_version.Major}.{lib_version.Minor} Build {lib_version.ToKLGBuildString()}");
        Console.WriteLine($"MC version: {mcVersion.Major}.{mcVersion.Minor} Build {mcVersion.ToKLGBuildString()}\n");
start:
        KLBot lcb = null;
        try
        {
            
            lcb = new KLBot(_localServer, Assembly.GetAssembly(typeof(ImageModule)), _debugTargetGroupID);
            lcb.AddModule(new RollinModule());
            lcb.AddModule(new CollapseModule());
            lcb.AddModule(new CompilerModule());
            lcb.AddModule(new ImageModule());
            lcb.AddModule(new IMGPModule());
            lcb.AddModule(new InvisibleModule());
            lcb.AddModule(new WelcomekxggModule());
            lcb.AddModule(new AnonyVoiceModule());
            lcb.AddModule(new TimeModule());
            lcb.AddModule(new 上号Module());
            lcb.AddModule(new ChatQYKModule());
            lcb.AddModule(new FuckModule());
            Console.WriteLine(lcb.GetModuleChainString());
            Console.WriteLine("初始化完成。输入命令开始调试");
            while (true)
            {
                Console.Write("> ");
                string s = Console.ReadLine();
                if (s == null)
                    continue;
                string[] cmdToken = s.Split();
                if (cmdToken.Length != 2)
                    continue;
                else if (cmdToken.Length == 2)
                {
                    switch (cmdToken[0])
                    {
                        case "set-user-id":
                            if (long.TryParse(cmdToken[1], out long userId))
                            {
                                _userId = userId;
                                Console.WriteLine($"消息发送者ID已设置为[{_userId}]");
                            }
                            else
                                Console.WriteLine($"{cmdToken[1]}不是有效的ID");
                            continue;
                        case "set-group-id":
                            if (long.TryParse(cmdToken[1], out long groupId))
                            {
                                _groupId = groupId;
                                Console.WriteLine($"消息发送所在群ID已设置为[{_groupId}]");
                            }
                            else
                                Console.WriteLine($"{cmdToken[1]}不是有效的ID");
                            continue;
                        case "set-context":
                            switch (cmdToken[1].ToLower())
                            {
                                case "group":
                                    _context = MessageContext.Group;
                                    Console.WriteLine($"会话上下文已设置为群聊");
                                    continue;
                                case "temp":
                                    _context = MessageContext.Temp;
                                    Console.WriteLine($"会话上下文已设置为临时会话");
                                    continue;
                                case "private":
                                    _context = MessageContext.Private;
                                    Console.WriteLine($"会话上下文已设置为私聊");
                                    continue;
                                default:
                                    Console.WriteLine("未知上下文类型。已忽略");
                                    continue;
                            }
                        case "send-plain":
                            MessagePlain msg = new(_userId, _groupId, cmdToken[1]);
                            msg.Context = _context;
                            _localServer.AddReceivedMessage(msg);
                            break;
                    }
                }
                Console.WriteLine("\n");
                lcb.ProcessMessages(lcb.FetchMessages());
                Thread.Sleep(1000);
            }
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
    private static void PrintInfo(string msgInfo)
    {
        Console.WriteLine($"{msgInfo}");
    }
}