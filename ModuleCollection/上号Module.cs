﻿using System;

namespace klbotlib.Modules
{
    /// <summary>
    /// 上号模块
    /// </summary>
    public class 上号Module : SingleTypeModule<MessagePlain>
    {
        [ModuleStatus(IsHidden = true)]
        private string _lastMsg = "";
        [ModuleStatus(IsHidden = true)]
        private string _last2Msg = "";
        [ModuleStatus(IsHidden = true)]
        private DateTime _lastHal = new DateTime();
        [ModuleSetup]
        private readonly TimeSpan _coolDownTime = new TimeSpan(0, 0, 60);


        private bool Is上号(string text) => text.Length <= 5 && text.Contains("上号");
        /// <summary>
        /// 关闭模块签名
        /// </summary>
        public sealed override bool UseSignature => false;
        public sealed override string FriendlyName => "上号模块";
        /// <summary>
        /// 过滤器：处理任何消息
        /// </summary>
        public sealed override string Filter(MessagePlain msg)
        {
            string msg_text = msg.Text.Trim();
            string output = null;
            if (Is上号(msg_text) && !Is上号(_lastMsg))
                output = "上号";
            else if (msg_text.Contains("蛤儿") && DateTime.Now - _lastHal > _coolDownTime)
            {
                output = "蛤儿";
                //刷新冷却时间
                _lastHal = DateTime.Now;
            }
            else if (!Is上号(_lastMsg) && msg_text == _lastMsg && _lastMsg != _last2Msg)
                output = "跟风";
            _last2Msg = _lastMsg;
            _lastMsg = msg_text;
            return output;
        }
        /// <summary>
        /// 处理器：内容包含上号且不长于五个字符，则复读内容；
        /// 另外，缓存当前消息到LastMsg中，用于下一次判断是否是同一轮上号消息。如果是同一轮则不回复。
        /// </summary>
        public sealed override string Processor(MessagePlain msg, string filter_out)
        {
            string msg_text = msg.Text.Trim();
            switch (filter_out)
            {
                case "上号":
                    return msg_text;
                case "蛤儿":
                    return @"蛤儿，我的蛤儿{\face:大哭}{\face:大哭}{\face:大哭}";
                case "跟风":
                    return msg_text;
                default:
                    throw new Exception($"意外遇到未实现的过滤器输出\"{filter_out}\"");
            }
        }
    }
}
