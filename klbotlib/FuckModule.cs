using System.Security.Cryptography;

namespace klbotlib.Modules
{
    ///所有模块在本文件中定义
    ///所有bot间共有的模块定义为static类


    ///嘴臭模块
    public class FuckModule
    {
        private static readonly string[] sub = {"", "我" };
        private static readonly string[] you = { "", "你" };
        private static readonly string[] v = { "", "操", "草", "日", "叼", "翘", "吊", "掉", "干", "肛", "去", "切", "拧", "锤", "割", "捅", "碾", "剁" , "屌", "刁"};
        private static readonly string[] human = { "你妈", "你爸", "你哥", "他妈", "你妹", "你大爷", "他大爷", "你弟", "他奶奶" };
        private static readonly string[] organ = {  
            "毛毛虫", "牛子", "牛头", "牛牛", "小泥鳅", "根尖", "阴茎", "几把", "鸡巴", "叼", "雕", "刁",  "吊", "屌",  "龟头", "睾", "睾丸", "蛋", 
            "鲍鱼", "逼",  "骚逼", "卵",  "逼窿", "批", "b",  "B",  "憋", "鳖", "奶头", "奶子", "乳头", "鲶鱼", "阴道", "阴毛", "阴蒂", "卵", "卵子", "卵泡", "卵巢",
            "屁眼",  "菊花", "尻", "肛门", "尿", "尿道", "尿道口",
            "鼻", "脑", "臀", "腋",
            "冬瓜", "",
        };
        private static readonly string[] suffix = { "水", "液", "毛", "窿", "油", "头", "尖", "屎", "洞"};
        private static readonly string[] adj_organ = { 
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
        private static readonly string[] adv = { "烂", "爆", "废", "穿", "炸", "黑", "肿", "瘫" };
        private static readonly string[] connector = { "的", "的", "的", "个" };
        private static readonly string[] combine = { "傻逼", "弱智", "脑残", "垃圾", "废物", "傻吊", "傻卵", "傻嗨", "猥琐", "司马", "死妈", "吊毛", "叼毛", "卵毛" };
        private static readonly string[] stuff = { "东西", "玩意", "儿子", "孙子", "弟弟" };
        private static readonly string[] status = { "", "", "", "", "", "他妈的", "他妈", "直接" };

        /// <summary>
        /// 串联模式 开启时嘴臭模块会输出一系列长嘴臭句子
        /// </summary>
        public bool IsCascadeMode { get; set; } = true;
        public string FuckGenerator(RNGCryptoServiceProvider ro)
        {
            if (IsCascadeMode)
            {
                string re = "";
                while (ro.Next(5) != 0)
                {
                    re += GenerateSingleFuck(ro) + " ";
                }
                return re;
            }
            else return GenerateSingleFuck(ro);
        }

        private static string Pick(RNGCryptoServiceProvider ro, string[] a) => a[ro.Next(a.Length)];
        private static string GenerateSingleFuck(RNGCryptoServiceProvider ro)
        {
            int mode = ro.Next(21);
            switch (mode)
            {
                case 0: return Pick(ro, sub) + Pick(ro, v) + Pick(ro, human);//(主)谓宾 (我)操你妈
                case 1: return Pick(ro, sub) + Pick(ro, v) + Pick(ro, human) + Pick(ro, connector) + Pick(ro, organ);//(主)谓宾连接词器官 (我)操你妈了个比
                case 2: return Pick(ro, sub) + Pick(ro, v) + Pick(ro, human) + Pick(ro, connector) + Pick(ro, adj_organ) + Pick(ro, organ);//(主)谓宾连接词形容词器官 (我)操你妈了个臭比
                case 3: return Pick(ro, sub) + Pick(ro, v) + Pick(ro, adv) + Pick(ro, human) + Pick(ro, connector) + Pick(ro, adj_organ) + Pick(ro, organ);//(主)谓副宾连接词形容词器官
                case 4: return Pick(ro, sub) + Pick(ro, v) + Pick(ro, adv) + Pick(ro, human) + Pick(ro, connector) + Pick(ro, adj_organ) + Pick(ro, organ) + " " + Pick(ro, v) + Pick(ro, human) + "的";
                //(主)谓副宾连接词形容词器官 谓宾
                case 5: return Pick(ro, sub) + Pick(ro, v) + Pick(ro, human) + Pick(ro, connector) + Pick(ro, combine) + Pick(ro, stuff);//(主)谓宾连接词称号玩意
                case 6: return Pick(ro, sub) + Pick(ro, v) + Pick(ro, adv) + Pick(ro, human) + Pick(ro, connector) + Pick(ro, combine) + Pick(ro, stuff);//(主)谓副宾连接词称号玩意
                case 7: return Pick(ro, combine) + Pick(ro, stuff);//(你)(他妈)称号玩意
                case 8: return Pick(ro, you) + Pick(ro, status) + Pick(ro, combine) + Pick(ro, stuff);//(你)(他妈)称号玩意
                case 9: return Pick(ro, sub) + Pick(ro, v) + Pick(ro, human) + Pick(ro, connector) + Pick(ro, organ) + Pick(ro, suffix);//(主)谓宾连接词器官
                case 10: return Pick(ro, sub) + Pick(ro, v) + Pick(ro, human) + Pick(ro, connector) + Pick(ro, adj_organ) + Pick(ro, organ) + Pick(ro, suffix);//(主)谓宾连接词形容词器官
                case 11: return Pick(ro, sub) + Pick(ro, v) + Pick(ro, adv) + Pick(ro, human) + Pick(ro, connector) + Pick(ro, adj_organ) + Pick(ro, organ) + Pick(ro, suffix);//(主)谓副宾连接词形容词器官
                case 12: return Pick(ro, sub) + Pick(ro, v) + Pick(ro, adv) + Pick(ro, human) + Pick(ro, connector) + Pick(ro, adj_organ) + Pick(ro, organ) + Pick(ro, suffix) + Pick(ro, v) + Pick(ro, human) + "的";
                //(主)谓副宾连接词形容词器官谓宾
                case 13: return Pick(ro, human) + Pick(ro, connector) + Pick(ro, organ);//宾连接词器官
                case 14: return Pick(ro, human) + Pick(ro, connector) + Pick(ro, adj_organ) + Pick(ro, organ);//宾连接词形容词器官
                case 15: return Pick(ro, human) + Pick(ro, connector) + Pick(ro, adj_organ) + Pick(ro, organ) + Pick(ro, v) + Pick(ro, human) + "的";
                //谓副宾连接词形容词器官谓宾
                case 16: return Pick(ro, sub) + Pick(ro, v) + Pick(ro, human) + Pick(ro, connector) + Pick(ro, combine) + Pick(ro, stuff);//(主)(谓)宾连接词称号玩意
                case 17: return Pick(ro, sub) + Pick(ro, v) + Pick(ro, combine) + Pick(ro, stuff);//(主)(谓)称号玩意
                case 18: return Pick(ro, sub) + Pick(ro, v) + Pick(ro, human) + Pick(ro, connector) + Pick(ro, organ) + Pick(ro, suffix);//谓宾连接词器官
                case 19: return Pick(ro, sub) + Pick(ro, v) + Pick(ro, human) + Pick(ro, connector) + Pick(ro, adj_organ) + Pick(ro, organ) + Pick(ro, suffix);//谓宾连接词形容词器官
                case 20: return Pick(ro, sub) + Pick(ro, v) + Pick(ro, human) + Pick(ro, connector) + Pick(ro, adj_organ) + Pick(ro, organ) + Pick(ro, suffix) + " " + Pick(ro, v) + Pick(ro, human) + "的";
                //主谓副宾连接词形容词器官谓宾
            }
            return "操你妈的";
        }
    }

}
