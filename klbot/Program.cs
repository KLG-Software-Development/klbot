using klbotlib;
using klbotlib.Exceptions;
using klbotlib.Extensions;
using klbotlib.MessageDriver.OneBot;
using klbotlib.Modules;
using System;
using System.Reflection;
using System.Threading.Tasks;
using Microsoft.Extensions.Configuration;
using System.Threading;
using klbotlib.MessageDriver;
using System.Diagnostics;
using System.IO;

namespace klbot;

class Program
{
    private static readonly ConfigurationManager _config = new();
    private static IMessageDriver GetMessageDriver()
    {
        string driverId = _config.ReadValue("driver_id");
        switch (driverId)
        {
            case BuiltinMessageDriverId.OneBotHttpId:
                string serviceUrl = _config.ReadValue("service_url", driverId);
                string webhookBindUrl = _config.ReadValue("webhook_url", driverId);
                string token = _config.ReadValue("token", driverId);
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
        Console.WriteLine($"KLBot");
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
            Console.WriteLine($"Config file: {configPath}\n");
            _config.AddIniFile(configPath);
        }
    }

    private static void InitLog()
    {
        Trace.Listeners.Clear();
        DefaultTraceListener fileLog = new()
        {
            LogFileName = $"klbot.log"
        };
        Trace.Listeners.Add(fileLog);
        ConsoleTraceListener consoleLog = new();
        Trace.Listeners.Add(consoleLog);
    }

    private static async Task Main(string[] args)
    {
        InitLog();
        string? configPath = args.Length == 0 ? null : args[0];
        Init(configPath);
start:
        DateTime lastErrorTime = DateTime.MinValue;
        int serialErrorCounter = 0;
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
            if (_config.TryReadValue("modules", out string? moduleListFile))
            {
                if (!File.Exists(moduleListFile))
                {
                    Console.WriteLine($"模块列表文件[{moduleListFile}]不存在，将使用默认模块列表");
                    AddDefaultModules(klg);
                }
                else
                    await AddModulesFromList(klg, moduleListFile);
            }
            else
                AddDefaultModules(klg);
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
            serialErrorCounter = DateTime.Now - lastErrorTime < TimeSpan.FromSeconds(6) ? serialErrorCounter + 1 : 0;
            if (serialErrorCounter > 10)
            {
                Console.WriteLine("连续10次发生致命错误。将停止重试并有序退出");
                if (klg != null)
                    klg.OnExit();
                return;
            }
            else
            {
                if (klg != null)
                    klg.OnExit();
                Console.WriteLine($"[{DateTime.Now:G}] 正在尝试重启KLBot...\n");
                Thread.Sleep(1000);
                goto start;
            }
        }
    }

    private static void AddDefaultModules(KLBot klg)
    {
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
        klg.AddModule(new ChatQYKModule());
        klg.AddModule(new FuckModule());
    }

    private static async Task<(string, string)[]> ParseModuleList(string moduleListFile)
    {
        string[] lines = await File.ReadAllLinesAsync(moduleListFile);
        (string, string)[] moduleList = new (string, string)[lines.Length];
        for (int i = 0; i < lines.Length; i++)
        {
            string[] tokens = lines[i].Split(':');
            if (tokens.Length != 2)
                throw new Exception($"无效的模块列表文件：{moduleListFile}: \n行号：{i + 1}: 语法应为\"<模块名> <模块存档路径>\"");
            moduleList[i] = (tokens[0].Trim(), tokens[1].Trim());
        }
        return moduleList;
    }

    private static async Task AddModulesFromList(KLBot klg, string moduleListFile)
    {
        (string, string)[] moduleList = await ParseModuleList(moduleListFile);
        for (int i = 0; i < moduleList.Length; i++)
        {
            (string moduleName, string statusFile) = moduleList[i];
            try
            {
                await klg.LoadModule(moduleName, statusFile);
            }
            catch (Exception ex)
            {
                Console.WriteLine($"\n从文件[{statusFile}]加载模块[{moduleName}]失败: {ex}");
                return;
            }
        }
    }
}
