using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using klbotlib.Modules.KLDNamespace;
using System.Reflection;
namespace klbotlib.Modules
{
    public class InvisibleModule : SingleTypeModule<MessagePlain>
    {
        [ModuleStatus]
        private Invisible ruan = new Invisible();

        public override bool IsTransparent { get; } = false;
        public override bool UseSignature => false;
        public override string Filter(MessagePlain msg)
        {
           
            long x = msg.SenderID;
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
        public override string Processor(MessagePlain msg, string filter_out)
        {
            if (filter_out.Equals("yes") && ruan.K == 0)
            {

                return null;
            }
            else if (filter_out.Equals("yes") && ruan.K == 1)
            {
                ruan.K = 2;
                return "kxgg back!";
            }
            else return null;
            
        }
    }
}
namespace klbotlib.Modules.KLDNamespace
{        public class Invisible
    {
        private int k = 2;
        public int K { get; set; }
    }

}
