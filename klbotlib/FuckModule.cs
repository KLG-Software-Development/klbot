using Newtonsoft.Json;
using System;
using System.IO;
using System.Security.Cryptography;
using System.Text;
using System.Text.RegularExpressions;

namespace klbotlib.Modules.FuckModule
{
    /// <summary>
    /// 嘴臭模块
    /// </summary>
    public class FuckModule : SingleTypeModule<MessagePlain>
    {
        private readonly RNGCryptoServiceProvider ro = new RNGCryptoServiceProvider();
        [ModuleSetup]
        public string PatternPath { get; private set; } = "fuckPat";
        [ModuleSetup]
        private readonly Regex pattern = new Regex(@"^傻\w|操\w妈|[\w]逼|恶臭|东百|贱|垃圾|尼哥|瓦坎达|nigga|\w猡|猪|\w*吃屎|滚|肏|草\w妈|艹|日你|屌|\w卵|\w妈的|\w*你妈|刁|nmsl|死马|叼毛|吊毛|NMSL|NM\$L|猥|琐|叼|妈批|废物|尼玛|骚货|奶子|龟头|烂|cnm|狗日|骂我|(^\?$)|(^？$)");
        #region 词库
        [ModuleSetup]
        private readonly string[] sub = {"", "我" };
        [ModuleSetup]
        private readonly string[] you = { "你" };
        [ModuleSetup]
        private readonly string[] v = { "操", "草", "日", "叼", "翘", "吊", "掉", "干", "肛", "去", "切", "拧", "锤", "割", "捅", "碾", "剁" , "屌", "刁"};
        [ModuleSetup]
        private readonly string[] human = { "你妈", "你爸", "你哥", "他妈", "你妹", "你大爷", "他大爷", "你弟", "他奶奶" };
        [ModuleSetup]
        private readonly string[] organ = {  
            "毛毛虫", "牛子", "牛头", "牛牛", "小泥鳅", "根尖", "阴茎", "几把", "鸡巴", "叼", "雕", "刁",  "吊", "屌",  "龟头", "睾", "睾丸", "蛋", "狗蛋",
            "鲍鱼", "逼",  "骚逼", "卵",  "逼窿", "批", "b",  "B",  "憋", "鳖", "奶头", "奶子", "乳头", "鲶鱼", "阴道", "阴毛", "阴蒂", "阴囊", "卵", "卵子", "卵泡", "卵巢",
            "屁眼",  "菊花", "尻", "肛门", "尿", "尿道", "尿道口",
            "鼻", "脑", "臀", "腋",
            "冬瓜", "",
        };
        [ModuleSetup]
        private readonly string[] suffix = { "水", "液", "毛", "窿", "油", "头", "尖", "屎", "洞"};
        [ModuleSetup]
        private readonly string[] adj_organ = { 
            "你妹的", "他吗的", "他妈的", 
            "酸甜", "香", "臭","酸","甜", "香辣", "咸", "美味", "臭臭",
            "仙女", "萝莉", "少女", "公主", "萌萌", "萌萌哒", "二次元",
            "软", "硬", "肥", "瘦", "小", "巨型", "烂",
            "红", "粉", "黄", "绿", "黑", "七彩",
            "可爱", "清纯", "漂亮", "美丽", "温柔",
            "傻", "蠢", "脏", "丑", "恶心", "油", "污", "废", "骚", "弱智", "拉",
            "生煎", "油炸", "焖", "爆炒", "水煮", "红烧",  
            "卷", "圆", "方", "尖", "钝", "厚",
            "猪", "鸡", "狗", "鸭", "马", "牛", "羊", "猫", "猡", "蟑螂", "蛆", "龟",
            "炫酷", "奇幻", "炫光", "",
            "不规则", "流体", "固态", "食品级", 
            "宇宙", "无敌", "机械",  "痛痛", "螺旋", "至尊",  "激光", "快乐", "飞天", "高科技" };
        [ModuleSetup]
        private readonly string[] adv = { "烂", "爆", "废", "穿", "炸", "黑", "肿", "瘫" };
        [ModuleSetup]
        private readonly string[] connector = { "的", "的", "的", "个" };
        [ModuleSetup]
        private readonly string[] combine = { "傻逼", "弱智", "脑残", "垃圾", "废物", "傻吊", "傻卵", "傻嗨", "猥琐", "司马", "死妈", "吊毛", "叼毛", "卵毛" };
        [ModuleSetup]
        private readonly string[] stuff = { "东西", "玩意", "儿子", "孙子", "弟弟" };
        [ModuleSetup]
        private readonly string[] status = { "个", "他妈的", "他妈个" };
        #endregion

        /// <summary>
        /// TagMe开关. 决定嘴臭模块是否只处理@自身的消息（不适用于聊天模块。聊天模块永远只处理@自身的消息）
        /// </summary>
        [ModuleStatus]
        public bool IsTagMe { get; set; } = false;
        /// <summary>
        /// 串联模式 开启时嘴臭模块会输出一系列长嘴臭句子. 否则将输出单句.
        /// </summary>
        [ModuleStatus]
        public bool IsCascadeMode { get; set; } = true;
        /// <summary>
        /// 生成连续嘴臭句子时的终止概率
        /// </summary>
        [ModuleStatus]
        public int TerminateProbability { get; set; } = 25;
        [ModuleStatus]
        public int MaximumLength { get; set; } = 50;

        public FuckModule(KLBot bot) : base(bot) 
        {
            string full_path = $"{AppDomain.CurrentDomain.BaseDirectory}{PatternPath}";
            if (File.Exists(full_path))
            {
                HostBot.ModulePrint(this, $"Loading sensitive pattern from '{PatternPath}'...");
                pattern = JsonConvert.DeserializeObject<Regex>(File.ReadAllText(full_path));
            }
            else
                HostBot.ModulePrint(this, $"Cannot find sensitive pattern file, using default. ");
        }

        private string Pick(string[] a) => a[ro.Next(a.Length)];
        private string SingleSentence()
        {
            int mode = ro.Next(20);
            switch (mode)
            {
                case 0: return Pick(sub) + Pick(v) + Pick(human);//(主)谓宾 (我)操你妈
                case 1: return Pick(sub) + Pick(v) + Pick(human) + Pick(connector) + Pick(organ);//(主)谓宾连接词器官 (我)操你妈了个比
                case 2: return Pick(sub) + Pick(v) + Pick(human) + Pick(connector) + Pick(adj_organ) + Pick(organ);//(主)谓宾连接词形容词器官 (我)操你妈了个臭比
                case 3: return Pick(sub) + Pick(v) + Pick(adv) + Pick(human) + Pick(connector) + Pick(adj_organ) + Pick(organ);//(主)谓副宾连接词形容词器官
                case 4: return Pick(sub) + Pick(v) + Pick(adv) + Pick(human) + Pick(connector) + Pick(adj_organ) + Pick(organ) + " " + Pick(v) + Pick(human) + "的";
                //(主)谓副宾连接词形容词器官 谓宾
                case 5: return Pick(sub) + Pick(v) + Pick(human) + Pick(connector) + Pick(combine) + Pick(stuff);//(主)谓宾连接词称号玩意
                case 6: return Pick(sub) + Pick(v) + Pick(adv) + Pick(human) + Pick(connector) + Pick(combine) + Pick(stuff);//(主)谓副宾连接词称号玩意
                case 7: return Pick(combine) + Pick(stuff);//称号玩意
                case 8: return Pick(you) + Pick(status) + Pick(combine) + Pick(stuff);//(你)(他妈)称号玩意
                case 9:  return Pick(sub) + Pick(v) + Pick(human) + Pick(connector) + Pick(organ) + Pick(suffix);//(主)谓宾连接词器官
                case 10: return Pick(sub) + Pick(v) + Pick(human) + Pick(connector) + Pick(adj_organ) + Pick(organ) + Pick(suffix);//(主)谓宾连接词形容词器官
                case 11: return Pick(sub) + Pick(v) + Pick(adv) + Pick(human) + Pick(connector) + Pick(adj_organ) + Pick(organ) + Pick(suffix);//(主)谓副宾连接词形容词器官
                case 12: return Pick(sub) + Pick(v) + Pick(adv) + Pick(human) + Pick(connector) + Pick(adj_organ) + Pick(organ) + Pick(suffix) + Pick(v) + Pick(human) + "的";
                //(主)谓副宾连接词形容词器官谓宾
                case 13: return Pick(human) + Pick(connector) + Pick(organ);//宾连接词器官
                case 14: return Pick(human) + Pick(connector) + Pick(adj_organ) + Pick(organ);//宾连接词形容词器官
                case 15: return Pick(human) + Pick(connector) + Pick(adj_organ) + Pick(organ) + Pick(v) + Pick(human) + "的";
                //谓副宾连接词形容词器官谓宾
                case 16: return Pick(sub) + Pick(v) + Pick(human) + Pick(connector) + Pick(combine) + Pick(stuff);//(主)谓宾连接词称号玩意
                case 17: return Pick(sub) + Pick(v) + Pick(human) + Pick(connector) + Pick(organ) + Pick(suffix);//谓宾连接词器官
                case 18: return Pick(sub) + Pick(v) + Pick(human) + Pick(connector) + Pick(adj_organ) + Pick(organ) + Pick(suffix);//谓宾连接词形容词器官
                case 19: return Pick(sub) + Pick(v) + Pick(human) + Pick(connector) + Pick(adj_organ) + Pick(organ) + Pick(suffix) + " " + Pick(v) + Pick(human) + "的";
                //主谓副宾连接词形容词器官谓宾
            }
            return "操你妈的";
        }
        private string GenerateFuck()
        {
            if (IsCascadeMode)
            {
                StringBuilder builder = new StringBuilder(SingleSentence());
                while (ro.Next(100) > TerminateProbability && builder.Length <= MaximumLength)
                {
                    builder.AppendFormat(" {0}", SingleSentence());
                }
                return builder.ToString();
            }
            else return SingleSentence();
        }

        public override bool Filter(MessagePlain msg)
        {
            if (pattern.IsMatch(msg.Text))
            {
                return !IsTagMe || msg.TargetID == HostBot.Config.QQ.SelfID;
            }
            else return false;
        } 
        public override string Processor(MessagePlain msg) => GenerateFuck();
    }
}
