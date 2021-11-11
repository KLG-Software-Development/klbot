using klbotlib.Extensions;
using System.Linq;
using System.Security.Cryptography;
using System.Text;
using System.Text.RegularExpressions;

namespace klbotlib.Modules
{
    // 嘴臭模块
    public class FuckModule : SingleTypeModule<MessagePlain>
    {
        private readonly RNGCryptoServiceProvider _ro = new RNGCryptoServiceProvider();
        [ModuleSetup]
        private readonly Regex _pattern;
        [ModuleSetup]
        private readonly string[] _sub, _you, _v, _human, _organ, _subfix, _adjOfOrgan, _adv, _connector, _combine, _stuff, _status;

        // TagMe开关. 决定嘴臭模块是否只处理@自身的消息（不适用于聊天模块。聊天模块永远只处理@自身的消息）
        [ModuleStatus]
        public bool IsTagMe { get; set; } = false;
        // 串联模式 开启时嘴臭模块会输出一系列长嘴臭句子. 否则将输出单句.
        [ModuleStatus]
        public bool IsCascade { get; set; } = true;
        // 生成连续嘴臭句子时的终止概率
        public int TermProb { get; set; } = 25;
        // 超过此长度时，串联模式不再累加
        [ModuleStatus]
        public int MaxLength { get; set; } = 20;

        private string Pick(string[] a) => a[_ro.Next(a.Length)];
        public string SingleSentence()
        {
            int mode = _ro.Next(20);
            switch (mode)
            {
                case 0: return Pick(_sub) + Pick(_v) + Pick(_human);//(主)谓宾 (我)操你妈
                case 1: return Pick(_sub) + Pick(_v) + Pick(_human) + Pick(_connector) + Pick(_organ);//(主)谓宾连接词器官 (我)操你妈了个比
                case 2: return Pick(_sub) + Pick(_v) + Pick(_human) + Pick(_connector) + Pick(_adjOfOrgan) + Pick(_organ);//(主)谓宾连接词形容词器官 (我)操你妈了个臭比
                case 3: return Pick(_sub) + Pick(_v) + Pick(_adv) + Pick(_human) + Pick(_connector) + Pick(_adjOfOrgan) + Pick(_organ);//(主)谓副宾连接词形容词器官
                case 4: return Pick(_sub) + Pick(_v) + Pick(_adv) + Pick(_human) + Pick(_connector) + Pick(_adjOfOrgan) + Pick(_organ) + " " + Pick(_v) + Pick(_human) + "的";
                //(主)谓副宾连接词形容词器官 谓宾
                case 5: return Pick(_sub) + Pick(_v) + Pick(_human) + Pick(_connector) + Pick(_combine) + Pick(_stuff);//(主)谓宾连接词称号玩意
                case 6: return Pick(_sub) + Pick(_v) + Pick(_adv) + Pick(_human) + Pick(_connector) + Pick(_combine) + Pick(_stuff);//(主)谓副宾连接词称号玩意
                case 7: return Pick(_combine) + Pick(_stuff);//称号玩意
                case 8: return Pick(_you) + Pick(_status) + Pick(_combine) + Pick(_stuff);//(你)(他妈)称号玩意
                case 9: return Pick(_sub) + Pick(_v) + Pick(_human) + Pick(_connector) + Pick(_organ) + Pick(_subfix);//(主)谓宾连接词器官
                case 10: return Pick(_sub) + Pick(_v) + Pick(_human) + Pick(_connector) + Pick(_adjOfOrgan) + Pick(_organ) + Pick(_subfix);//(主)谓宾连接词形容词器官
                case 11: return Pick(_sub) + Pick(_v) + Pick(_adv) + Pick(_human) + Pick(_connector) + Pick(_adjOfOrgan) + Pick(_organ) + Pick(_subfix);//(主)谓副宾连接词形容词器官
                case 12: return Pick(_sub) + Pick(_v) + Pick(_adv) + Pick(_human) + Pick(_connector) + Pick(_adjOfOrgan) + Pick(_organ) + Pick(_subfix) + Pick(_v) + Pick(_human) + "的";
                //(主)谓副宾连接词形容词器官谓宾
                case 13: return Pick(_human) + Pick(_connector) + Pick(_organ);//宾连接词器官
                case 14: return Pick(_human) + Pick(_connector) + Pick(_adjOfOrgan) + Pick(_organ);//宾连接词形容词器官
                case 15: return Pick(_human) + Pick(_connector) + Pick(_adjOfOrgan) + Pick(_organ) + Pick(_v) + Pick(_human) + "的";
                //谓副宾连接词形容词器官谓宾
                case 16: return Pick(_sub) + Pick(_v) + Pick(_human) + Pick(_connector) + Pick(_combine) + Pick(_stuff);//(主)谓宾连接词称号玩意
                case 17: return Pick(_sub) + Pick(_v) + Pick(_human) + Pick(_connector) + Pick(_organ) + Pick(_subfix);//谓宾连接词器官
                case 18: return Pick(_sub) + Pick(_v) + Pick(_human) + Pick(_connector) + Pick(_adjOfOrgan) + Pick(_organ) + Pick(_subfix);//谓宾连接词形容词器官
                case 19: return Pick(_sub) + Pick(_v) + Pick(_human) + Pick(_connector) + Pick(_adjOfOrgan) + Pick(_organ) + Pick(_subfix) + " " + Pick(_v) + Pick(_human) + "的";
                //主谓副宾连接词形容词器官谓宾
            }
            return "操你妈的";
        }
        private string GenerateFuck()
        {
            if (IsCascade)
            {
                StringBuilder builder = new StringBuilder(SingleSentence());
                while (_ro.Next(100) > TermProb && builder.Length <= MaxLength)
                {
                    builder.AppendFormat(" {0}", SingleSentence());
                }
                return builder.ToString();
            }
            else 
                return SingleSentence();
        }

        public sealed override bool UseSignature => false; //隐藏返回消息中的模块签名
        public sealed override string FriendlyName => "嘴臭模块";
        public sealed override string Filter(MessagePlain msg)
        {
            return _pattern.IsMatch(msg.Text) && (!IsTagMe || msg.TargetID.Contains(HostBot.SelfID)) 
                ? "ok" 
                : null;
        }
        public sealed override string Processor(MessagePlain msg, string _)
            => ModuleAccess.GetModule<AnonyVoiceModule>().TextToSpeech(GenerateFuck());
    }
}
