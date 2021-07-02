using klbotlib;
using System;
using Newtonsoft.Json;
using System.Diagnostics.Contracts;
using System.Net;
using System.Threading.Tasks;
using System.Reflection;

//[assembly: AssemblyVersionAttribute("0.1.*")]

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
        const long self_id = 3205508672;
        static void Main(string[] args)
        {
            Console.ResetColor();
            Version version = Assembly.GetExecutingAssembly().GetName().Version;
            long sucess_counter = 0, query_counter_cache = 0;
            int vital_failure_counter = 0;
            Console.WriteLine($"KLBot via mirai V{version.Major}.{version.Minor} Build {version.Build}");
        start:
            try
            {
                KLBot klg;
                if (args.Length != 0 && args[0] == "klg")
                    klg = new(server, self_id, KLBot.KLG, KLBot.DEBUG);
                else
                    klg = new(server, self_id, KLBot.DEBUG);
                klg.Loop(out sucess_counter);
            }
            catch (Exception ex)
            {
                Console.ForegroundColor = ConsoleColor.Red;
                Console.WriteLine($"Error: {ex.Message}");
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
                        vital_failure_counter++;
                    else                                         //否则意味着并非基本错误，此时优先保持服务运作，基本错误计数器归零
                        vital_failure_counter = 0;
                    if (vital_failure_counter > 10)
                    {
                        Console.WriteLine("10 error in a row. Something is wrong. Stop retrying and quit.");
                        return;
                    }
                    else
                    {
                        query_counter_cache = sucess_counter;
                        Console.WriteLine($"[{DateTime.Now:G}] Trying to restart...\n");
                        goto start;
                    }
                }
            }
        }
    }

}
