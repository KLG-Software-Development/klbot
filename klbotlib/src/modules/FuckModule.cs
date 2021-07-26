﻿using System.Security.Cryptography;
using System.Text;
using System.Text.RegularExpressions;

namespace klbotlib.Modules
{
    /// <summary>
    /// 嘴臭模块
    /// </summary>
    public class FuckModule : SingleTypeModule<MessagePlain>
    {
        private readonly RNGCryptoServiceProvider ro = new RNGCryptoServiceProvider();
        [ModuleSetup]
        private readonly Regex pattern;
        [ModuleSetup]
        private readonly string[] sub, you, v, human, organ, subfix, adj_of_organ, adv, connector, combine, stuff, status;

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
        public int MaximumLength { get; set; } = 20;

        private string Pick(string[] a) => a[ro.Next(a.Length)];
        private string SingleSentence()
        {
            int mode = ro.Next(20);
            switch (mode)
            {
                case 0: return Pick(sub) + Pick(v) + Pick(human);//(主)谓宾 (我)操你妈
                case 1: return Pick(sub) + Pick(v) + Pick(human) + Pick(connector) + Pick(organ);//(主)谓宾连接词器官 (我)操你妈了个比
                case 2: return Pick(sub) + Pick(v) + Pick(human) + Pick(connector) + Pick(adj_of_organ) + Pick(organ);//(主)谓宾连接词形容词器官 (我)操你妈了个臭比
                case 3: return Pick(sub) + Pick(v) + Pick(adv) + Pick(human) + Pick(connector) + Pick(adj_of_organ) + Pick(organ);//(主)谓副宾连接词形容词器官
                case 4: return Pick(sub) + Pick(v) + Pick(adv) + Pick(human) + Pick(connector) + Pick(adj_of_organ) + Pick(organ) + " " + Pick(v) + Pick(human) + "的";
                //(主)谓副宾连接词形容词器官 谓宾
                case 5: return Pick(sub) + Pick(v) + Pick(human) + Pick(connector) + Pick(combine) + Pick(stuff);//(主)谓宾连接词称号玩意
                case 6: return Pick(sub) + Pick(v) + Pick(adv) + Pick(human) + Pick(connector) + Pick(combine) + Pick(stuff);//(主)谓副宾连接词称号玩意
                case 7: return Pick(combine) + Pick(stuff);//称号玩意
                case 8: return Pick(you) + Pick(status) + Pick(combine) + Pick(stuff);//(你)(他妈)称号玩意
                case 9:  return Pick(sub) + Pick(v) + Pick(human) + Pick(connector) + Pick(organ) + Pick(subfix);//(主)谓宾连接词器官
                case 10: return Pick(sub) + Pick(v) + Pick(human) + Pick(connector) + Pick(adj_of_organ) + Pick(organ) + Pick(subfix);//(主)谓宾连接词形容词器官
                case 11: return Pick(sub) + Pick(v) + Pick(adv) + Pick(human) + Pick(connector) + Pick(adj_of_organ) + Pick(organ) + Pick(subfix);//(主)谓副宾连接词形容词器官
                case 12: return Pick(sub) + Pick(v) + Pick(adv) + Pick(human) + Pick(connector) + Pick(adj_of_organ) + Pick(organ) + Pick(subfix) + Pick(v) + Pick(human) + "的";
                //(主)谓副宾连接词形容词器官谓宾
                case 13: return Pick(human) + Pick(connector) + Pick(organ);//宾连接词器官
                case 14: return Pick(human) + Pick(connector) + Pick(adj_of_organ) + Pick(organ);//宾连接词形容词器官
                case 15: return Pick(human) + Pick(connector) + Pick(adj_of_organ) + Pick(organ) + Pick(v) + Pick(human) + "的";
                //谓副宾连接词形容词器官谓宾
                case 16: return Pick(sub) + Pick(v) + Pick(human) + Pick(connector) + Pick(combine) + Pick(stuff);//(主)谓宾连接词称号玩意
                case 17: return Pick(sub) + Pick(v) + Pick(human) + Pick(connector) + Pick(organ) + Pick(subfix);//谓宾连接词器官
                case 18: return Pick(sub) + Pick(v) + Pick(human) + Pick(connector) + Pick(adj_of_organ) + Pick(organ) + Pick(subfix);//谓宾连接词形容词器官
                case 19: return Pick(sub) + Pick(v) + Pick(human) + Pick(connector) + Pick(adj_of_organ) + Pick(organ) + Pick(subfix) + " " + Pick(v) + Pick(human) + "的";
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

        public override bool UseSignature => false;         //隐藏返回消息中的模块签名
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
