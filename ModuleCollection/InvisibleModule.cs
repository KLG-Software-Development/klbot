#pragma warning disable CS1591 // 缺少对公共可见类型或成员的 XML 注释
#pragma warning disable IDE0078 // 使用模式匹配
#pragma warning disable IDE2001 // 嵌入的语句必须放在其自己的行上
#pragma warning disable IDE0045 // 转换为条件表达式
#pragma warning disable IDE1006 // 命名样式

using klbotlib.Modules.KLDNamespace;
using System.Text.Json.Serialization;

namespace klbotlib.Modules
{
    public class InvisibleModule : SingleTypeModule<MessagePlain>
    {
        [JsonInclude]
        private readonly Invisible ruan = new();

        public override bool UseSignature => false;
        public override Task<Message?> Processor(MessageContext context, MessagePlain msg)
        {
            string? filterOut;
            long x = context.UserId;
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
                    filterOut = "yes";
                }
                else filterOut = null;
            }
            else filterOut = null;
            if (filterOut == null)
                return (Message?)null;
            if (filterOut.Equals("yes") && ruan.K == 0)
                return (Message?)null;
            else if (filterOut.Equals("yes") && ruan.K == 1)
            {
                ruan.K = 2;
                return (Message?)"kxgg back!";
            }
            else
                return (Message?)null;
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
