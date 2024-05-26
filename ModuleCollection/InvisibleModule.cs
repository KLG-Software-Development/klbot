#pragma warning disable CS1591 // 缺少对公共可见类型或成员的 XML 注释

using klbotlib.Modules.KLDNamespace;
using System.Threading.Tasks;

namespace klbotlib.Modules
{
    public class InvisibleModule : SingleTypeModule<MessagePlain>
    {
        [ModuleStatus]
        private Invisible ruan = new Invisible();

        public override bool UseSignature => false;
        public override string? Filter(MessagePlain msg)
        {
            long x = msg.SenderId;
            if (x == 2044164212)
            {
                if (msg.Text == "隐身")
                {
                    ruan.K = 0;
                }
                else if (msg.Text == "现身")
                {
                    ruan.K = 1;

                }
                if (ruan.K == 0 || ruan.K == 1)
                {
                    return "yes";
                }
                else return null;
            }
            else return null;
        }
        public override Task<string> Processor(MessagePlain msg, string? filterOut)
        {
            if (filterOut.Equals("yes") && ruan.K == 0)
                return Task.FromResult(string.Empty);
            else if (filterOut.Equals("yes") && ruan.K == 1)
            {
                ruan.K = 2;
                return Task.FromResult("kxgg back!");
            }
            else 
                return Task.FromResult(string.Empty);
        }
    }
}
namespace klbotlib.Modules.KLDNamespace
{
    public class Invisible
    {
        public int K { get; set; } = 2;
    }
}
