#pragma warning disable CS1591 // 缺少对公共可见类型或成员的 XML 注释
#pragma warning disable IDE0044 // 添加只读修饰符
#pragma warning disable IDE1006 // 命名样式

using klbotlib.Modules.KLDNamespace;
using System;
using System.Threading.Tasks;

namespace klbotlib.Modules
{
    /// <summary>
    /// kx打招呼模块
    /// </summary>
    public class WelcomekxggModule : SingleTypeModule<MessagePlain>
    {
        [ModuleStatus]
        private Common1 common = new();
        public override bool UseSignature => false;
        public override string? Filter(MessagePlain msg)
        {
            long x = msg.SenderId;
            if (x == 2044164212)
            {
                if (common.Y != DateTime.UtcNow.Month || common.Z != DateTime.UtcNow.Day)
                {
                    common.K = 1;
                    common.Y = DateTime.Now.Month;
                    common.Z = DateTime.Now.Day;
                }
                if (common.K == 1)
                {
                    common.K = 0;
                    return "yes";
                }
                else
                    return null;
            }
            else
                return null;
        }
        public override Task<string> Processor(MessagePlain msg, string? filterOut)
        {
            if (filterOut == "yes")
                return Task.FromResult("welcome KXGG!");
            else 
                return Task.FromResult(string.Empty);
        }
    }
}
namespace klbotlib.Modules.KLDNamespace
{
    public class Common1
    {
        public int K
        {
            get;
            set;
        }
        public int Y
        {
            get;
            set;
        }
        public int Z
        {
            get;
            set;
        }
        public override string ToString()
        {
            return $"[K:{K}, Y:{Y}, Z:{Z}]";
        }
    }
}
