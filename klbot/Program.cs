using klbotlib;
using klbotlib.Exceptions;
using klbotlib.Extensions;
using klbotlib.MessageDriver.Mirai;
using klbotlib.MessageDriver.OneBot;
using klbotlib.Modules;
using System;
using System.Reflection;
using System.Threading.Tasks;
using Microsoft.Extensions.Configuration;
using System.Threading;

namespace klbot;

class Program
{
    private static readonly ConfigurationManager _config = new();
    private static IMessageDriver GetMessageDriver()
    {
        string driverId = _config.ReadValue("driver_id");
        switch (driverId)
        {
            case "mirai-http":
                string serviceUrl = _config.ReadValue("service_url");
                return new MessageDriver_MiraiHttp(serviceUrl);
            case "onebot-http":
                serviceUrl = _config.ReadValue("service_url");
                string webhookBindUrl = _config.ReadValue("webhook_url");
                string token = _config.ReadValue("token");
                return new MessageDriver_OneBotHttp(serviceUrl, webhookBindUrl, token);
            default:
                throw new KLBotInitializationException($"Unknown message driver ID \"{driverId}\"");
        }
    }

    private static void Init(string? configPath)
    {
        Console.ResetColor();
        Version? exeVersion = Assembly.GetExecutingAssembly().GetName().Version;
        Version? libVersion = klbotlib.Info.CoreLibInfo.GetLibVersion();
        Version? mcVersion = ModuleCollection.Info.CollectionInfo.GetLibVersion();
        Console.WriteLine($"KLBot via mirai");
        Console.WriteLine($"exe version: {exeVersion.Major}.{exeVersion.Minor} Build {exeVersion.ToKLGBuildString()}");
        Console.WriteLine($"corelib version: {libVersion.Major}.{libVersion.Minor} Build {libVersion.ToKLGBuildString()}");
        Console.WriteLine($"MC version: {mcVersion.Major}.{mcVersion.Minor} Build {mcVersion.ToKLGBuildString()}\n");

        _config.Sources.Clear();
        if (string.IsNullOrEmpty(configPath))
        {
            Console.WriteLine("Program starts without arguments, reading config from environs");
            _config.AddEnvironmentVariables();
        }
        else
        {
            Console.WriteLine($"Config file: {configPath}");
            _config.AddIniFile(configPath);
        }
    }

    private static async Task Main(string[] args)
    {
        string? configPath = args.Length == 0 ? null : args[0];
        Init(configPath);
start:
        long queryCounterCache = 0;
        int fatalFailureCounter = 0;
        KLBot? klg = null;
        try
        {
            IMessageDriver driver = GetMessageDriver();
            klg = new KLBot(_config, driver, moduleCollection: Assembly.GetAssembly(typeof(ImageModule)));
            Console.CancelKeyPress += (_, _) =>
            {
                klg.OnExit();
                Environment.Exit(-1);
            };
            await klg.AddModule(new ResetModule());
            await klg.AddModule(new PLJJModule());
            await klg.AddModule(new ZombieeeModule());
            await klg.AddModule(new FlashGambleModule());
            await klg.AddModule(new RollinModule());
            await klg.AddModule(new CollapseModule());
            await klg.AddModule(new CompilerModule());
            await klg.AddModule(new InvisibleModule());
            await klg.AddModule(new WelcomekxggModule());
            await klg.AddModule(new ImageModule());
            await klg.AddModule(new IMGPModule());
            await klg.AddModule(new AnonyVoiceModule());
            await klg.AddModule(new TimeModule());
            await klg.AddModule(new 上号Module());
            await klg.AddModule(new ChatQYKModule());
            await klg.AddModule(new FuckModule());
            Console.WriteLine(klg.GetModuleChainString());
            await klg.DefaultLoop();
        }
        catch (KLBotInitializationException ex)
        {
            Console.ForegroundColor = ConsoleColor.DarkYellow;
            Console.WriteLine(ex.Message);
            Console.WriteLine("初始化失败。退出中...");
            Console.ResetColor();
            Environment.Exit(-1);
        }
        catch (ModuleSetupException ex)
        {
            Console.ForegroundColor = ConsoleColor.DarkYellow;
            Console.WriteLine(ex.Message);
            Console.WriteLine($"模块配置异常。检查模块的配置文件是否正确，以及该模块是否恰当遵守了模块开发规范");
            Console.WriteLine("退出中...");
            Console.ResetColor();
            Environment.Exit(-1);
        }
        catch (ModuleStatusException ex)
        {
            Console.ForegroundColor = ConsoleColor.DarkYellow;
            Console.WriteLine(ex.Message);
            Console.WriteLine($"模块状态异常。检查模块的状态文件是否正确，以及该模块是否恰当遵守了模块开发规范");
            Console.WriteLine(ex.StackTrace);
            Console.WriteLine("退出中...");
            Console.ResetColor();
            Environment.Exit(-1);
        }
        catch (Exception ex)
        {
            Console.ForegroundColor = ConsoleColor.DarkYellow;
            Console.WriteLine(ex.Message);
            Console.WriteLine($"意外异常：[{ex.GetType()}] {ex.Message}\n调用栈：\n{ex.StackTrace}\n");
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
                queryCounterCache = klg.DiagData.SuccessPackageCount;
                if (klg != null)
                    klg.OnExit();
                Console.WriteLine($"[{DateTime.Now:G}] 正在尝试重启KLBot...\n");
                Thread.Sleep(1000);
                goto start;
            }
        }
    }
}
