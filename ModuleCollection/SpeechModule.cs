using klbotlib;
using klbotlib.Modules;

namespace ModuleCollection
{
    public class SpeechModule : SingleTypeModule<MessagePlain>
    {
        public override int Filter(MessagePlain msg)
        {
            throw new System.NotImplementedException();
        }

        public override string Processor(MessagePlain msg, int status_code)
        {
            throw new System.NotImplementedException();
        }
    }
}
