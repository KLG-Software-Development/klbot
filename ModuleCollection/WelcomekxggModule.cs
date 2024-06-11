#pragma warning disable CS1591 // 缺少对公共可见类型或成员的 XML 注释
#pragma warning disable IDE0044 // 添加只读修饰符
#pragma warning disable IDE1006 // 命名样式

using klbotlib.Modules.KLDNamespace;
using System.Text.Json.Serialization;

namespace klbotlib.Modules
{
    /// <summary>
    /// kx打招呼模块
    /// </summary>
    public class WelcomekxggModule : SingleTypeModule<MessagePlain>
    {
        [JsonInclude]
        private Common1 common = new();
        public override bool UseSignature => false;
        public override Task<Message?> Processor(MessageContext context, MessagePlain msg)
        {
            string? filterOut;
            long x = context.UserId;
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
                    filterOut = "yes";
                }
                else
                    filterOut = null;
            }
            else
                filterOut = null;
            return filterOut == "yes" ? (Task<Message?>)(Message)"welcome KXGG!" : (Task<Message?>)(Message?)null;
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
