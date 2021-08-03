using klbotlib;
using klbotlib.Modules;
using System.Text.RegularExpressions;

namespace ModuleCollection
{
    public class SpeechModule : SingleTypeModule<MessagePlain>
    {
        static readonly Regex pattern = new Regex(@"说：(.+)");
        public override int Filter(MessagePlain msg)
        {
            if (pattern.IsMatch(msg.Text))
                return 1;
            else
                return 0;
        }
        public override string Processor(MessagePlain msg, int _)
        {
            throw new System.NotImplementedException();
        }
    }
}
