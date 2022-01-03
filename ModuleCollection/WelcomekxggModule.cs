#pragma warning disable CS1591 // 缺少对公共可见类型或成员的 XML 注释
#pragma warning disable IDE0044 // 添加只读修饰符
#pragma warning disable IDE1006 // 命名样式

using klbotlib.Modules.KLDNamespace;
using System;

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
        public override string Filter(MessagePlain msg)
        {
            long x = msg.SenderID;
            if (x == 2044164212 && common.Z != DateTime.UtcNow.Day)
                return "reply";
            else 
                return null;
        }
        public override string Processor(MessagePlain msg, string filter_out)
        {
            if (filter_out == "reply")
            {
                common.Z = DateTime.UtcNow.Day;
                return "welcome KXGG!";
            }
            else 
                return "未知过滤器输出";
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
