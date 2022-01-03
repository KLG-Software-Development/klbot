using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using klbotlib.Modules.KLDNamespace;


namespace klbotlib.Modules
{
    public class WelcomekxggModule : SingleTypeModule<MessagePlain>
    {
        [ModuleStatus]
        private common1 common = new common1();
        public override bool IsTransparent { get; } = true;
        public override bool UseSignature => false;
        public override string Filter(MessagePlain msg)
        {
            long x = msg.SenderID;
            if (x == 2044164212)
                return "yes";
            else return null;
        }
        public override string Processor(MessagePlain msg, string filter_out)
        {
            if (filter_out == "yes")
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
                    return "welcome KXGG!";
                }
                else return null;
            }
            else return null;
        }
    }
}
namespace klbotlib.Modules.KLDNamespace
{
    public class common1
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
    }
}
