using klbotlib;
using klbotlib.Exceptions;
using klbotlib.Extensions;
using klbotlib.Modules;
using System;
using System.Reflection;

namespace klbot
{

    class Program
    {
        static void Main(string[] args)
        {
            Console.ResetColor();
            Version exe_version = Assembly.GetExecutingAssembly().GetName().Version;
            Version lib_version = klbotlib.Info.CoreLibInfo.GetLibVersion();
            Version mc_version = ModuleCollection.Info.CollectionInfo.GetLibVersion();
            long query_counter_cache = 0;
            int fatal_failure_counter = 0;
            Console.WriteLine($"KLBot via mirai");
            Console.WriteLine($"exe version: {exe_version.Major}.{exe_version.Minor} Build {exe_version.ToKLGBuildString()}");
            Console.WriteLine($"corelib version: {lib_version.Major}.{lib_version.Minor} Build {lib_version.ToKLGBuildString()}");
            Console.WriteLine($"MC version: {mc_version.Major}.{mc_version.Minor} Build {mc_version.ToKLGBuildString()}\n");
        start:
            KLBot klg = null;
            try
            {
                if (args.Length != 0)
                    klg = new(args[0]);
                else
                    klg = new KLBot();
                klg.AddModule(new ImageModule());
                klg.AddModule(new IMGPModule());
                klg.AddModule(new AnonyVoiceModule());
                klg.AddModule(new TimeModule());
                klg.AddModule(new 上号Module());
                klg.AddModule(new ChatQYKModule());
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
                    Console.WriteLine($"模块配置异常。检查模块的配置文件是否正确，以及该模块是否恰当遵守了模块开发规范");
                    Console.WriteLine("退出中...");
                    Console.ResetColor();
                }
                else  //无法处理的未知情况
                {
                    if (query_counter_cache == klg.DiagData.SuccessPackageCount)   //sucess_counter距离上次出错之后没有发生变化，意味着本次出错紧接着上一次
                        fatal_failure_counter++;
                    else                                         //否则意味着并非基本错误，此时优先保持服务运作，基本错误计数器归零
                        fatal_failure_counter = 0;
                    if (fatal_failure_counter > 10)
                    {
                        Console.WriteLine("连续10次发生致命错误。将停止重试并有序退出");
                        if (klg != null)
                            klg.OnExit();
                        return;
                    }
                    else
                    {
                        query_counter_cache = klg.DiagData.SuccessPackageCount;
                        if (klg != null)
                            klg.OnExit();
                        Console.WriteLine($"[{DateTime.Now:G}] 正在尝试重启KLBot...\n");
                        goto start;
                    }
                }
            }
        }
    }

}
