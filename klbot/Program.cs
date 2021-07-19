using klbotlib;
using System;
using System.Net;
using System.Reflection;

namespace klbot
{

    class Program
    {
#if !DEBUG
        const string server = "http://localhost:3356";
#endif
#if DEBUG
        const string server = "http://192.168.31.42:3356";
#endif
        const long SELF_ID = 3205508672, DEBUG = 727414436, KLG = 670406903;

        static void Main(string[] args)
        {
            Console.ResetColor();
            Version exe_version = Assembly.GetExecutingAssembly().GetName().Version;
            Version lib_version = klbotlib.Info.LibInfo.GetLibVersion();
            long sucess_counter = 0, query_counter_cache = 0;
            int fatal_failure_counter = 0;
            Console.WriteLine($"KLBot via mirai - Build {lib_version.ToKLGBuildString()}");
            Console.WriteLine($"exe version: {exe_version.Major}.{exe_version.Minor}");
            Console.WriteLine($"core library version: {lib_version.Major}.{lib_version.Minor}\n");
        start:

            KLBot klg = null;
            try
            {
#if DEBUG
                klg = new(server, SELF_ID, DEBUG);
#else
                klg = new(server, SELF_ID, KLG, DEBUG);
#endif
                klg.Loop(out sucess_counter);
            }
            catch (Exception ex)
            {
                Console.ForegroundColor = ConsoleColor.Red;
                Console.WriteLine($"\nError: {ex.Message}");
                Console.WriteLine($"StackTrace: \n{ex.StackTrace}\n");
                Console.ResetColor();

                if (ex is WebException)
                {
                    Console.ForegroundColor = ConsoleColor.Cyan;
                    Console.WriteLine("A network error has been catched. Make sure the URL is correct and mcl is properly running on the target server. ");
                    Console.WriteLine("Quiting...");
                    Console.ResetColor();
                }
                else  //无法处理的未知情况
                {
                    if (query_counter_cache == sucess_counter)   //sucess_counter距离上次出错之后没有发生变化，意味着本次出错紧接着上一次
                        fatal_failure_counter++;
                    else                                         //否则意味着并非基本错误，此时优先保持服务运作，基本错误计数器归零
                        fatal_failure_counter = 0;
                    if (fatal_failure_counter > 10)
                    {
                        Console.WriteLine("10 error in a row. Something is wrong. Stop retrying and gracfully quit.");
                        if (klg != null)
                            klg.OnExit();
                        return;
                    }
                    else
                    {
                        query_counter_cache = sucess_counter;
                        if (klg != null)
                            klg.OnExit();
                        Console.WriteLine($"[{DateTime.Now:G}] Trying to restart...\n");
                        goto start;
                    }
                }
            }
        }
    }

}
