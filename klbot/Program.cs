using klbotlib;
using klbotlib.Exceptions;
using klbotlib.Extensions;
using klbotlib.MessageServer.Mirai;
using klbotlib.Modules;
using ModuleCollection;
using System;
using System.Reflection;
using System.Threading.Tasks;

namespace klbot;

class Program
{
    static void Main(string[] args)
    {
        Console.ResetColor();
        Version? exeVersion = Assembly.GetExecutingAssembly().GetName().Version;
        Version? libVersion = klbotlib.Info.CoreLibInfo.GetLibVersion();
        Version? mcVersion = ModuleCollection.Info.CollectionInfo.GetLibVersion();
        long queryCounterCache = 0;
        int fatalFailureCounter = 0;
        Console.WriteLine($"KLBot via mirai");
        Console.WriteLine($"exe version: {exeVersion.Major}.{exeVersion.Minor} Build {exeVersion.ToKLGBuildString()}");
        Console.WriteLine($"corelib version: {libVersion.Major}.{libVersion.Minor} Build {libVersion.ToKLGBuildString()}");
        Console.WriteLine($"MC version: {mcVersion.Major}.{mcVersion.Minor} Build {mcVersion.ToKLGBuildString()}\n");
start:
        KLBot? klg = null;
        try
        {
            MiraiMessageServer miraiServer;
            if (args.Length == 0)
                miraiServer = new("http://localhost:3356");
            else
                miraiServer = new(args[0]);
            klg = new KLBot(miraiServer, moduleCollection: Assembly.GetAssembly(typeof(ImageModule)));
            klg.AddModule(new ResetModule());
            klg.AddModule(new PLJJModule());
            klg.AddModule(new ZombieeeModule());
            klg.AddModule(new FlashGambleModule());
            klg.AddModule(new RollinModule());
            klg.AddModule(new CollapseModule());
            klg.AddModule(new CompilerModule());
            klg.AddModule(new InvisibleModule());
            klg.AddModule(new WelcomekxggModule());
            klg.AddModule(new ImageModule());
            klg.AddModule(new IMGPModule());
            klg.AddModule(new AnonyVoiceModule());
            klg.AddModule(new TimeModule());
            klg.AddModule(new 上号Module());
            klg.AddModule(new ChatXiaoIceModule());
            klg.AddModule(new FuckModule());
            Console.WriteLine(klg.GetModuleChainString());
            klg.DefaultLoop();
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
                if (queryCounterCache == klg.DiagData.SuccessPackageCount)   //sucess_counter距离上次出错之后没有发生变化，意味着本次出错紧接着上一次
                    fatalFailureCounter++;
                else                                         //否则意味着并非基本错误，此时优先保持服务运作，基本错误计数器归零
                    fatalFailureCounter = 0;
                if (fatalFailureCounter > 10)
                {
                    Console.WriteLine("连续10次发生致命错误。将停止重试并有序退出");
                    if (klg != null)
                        klg.OnExit();
                    return;
                }
                else
                {
                    //query_counter_cache = klg.DiagData.SuccessPackageCount;
                    if (klg != null)
                        klg.OnExit();
                    Console.WriteLine($"[{DateTime.Now:G}] 正在尝试重启KLBot...\n");
                    goto start;
                }
            }
        }
    }
}
