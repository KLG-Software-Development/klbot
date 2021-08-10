﻿using klbotlib.Internal;
using System;
using System.Collections.Generic;
using System.Net;

namespace klbotlib
{
    //消息工厂类。用来从JMessagePackage对象中生成相应的Message类型
    internal static class MessageFactory
    {
        static Dictionary<string, int> IndexOf = new Dictionary<string, int>
        {
            { "Plain", 0 },
            { "Image", 1 },
            { "FlashImage", 1 },    //闪照和正常图片统一按图片处理
            { "Voice", 2 },
        };

        //从消息链生成Message对象
        internal static Message BuildMessage(JMessagePackage msg_package)
        {
            Message ret = null;
            var type = CalcMessageType(msg_package, out var targets);
            if (type == typeof(MessagePlain))
                ret = BuildPlain(msg_package);
            else if (type == typeof(MessageImage))
                ret = BuildImage(msg_package);
            else if (type == typeof(MessageVoice))
                ret = BuildVoice(msg_package);
            else if (type == typeof(MessageImagePlain))
                ret = BuildImagePlain(msg_package);
            else
                return Message.Empty;
            //添加统计生成的@列表
            ret.AddTargetID(targets);
            //加工合适的上下文和ID
            RefineContext(msg_package, ret);
            return ret;
        }

        //给定一条消息链，统计并判断应该生成的消息类型，并顺便输出@的ID
        private static Type CalcMessageType(JMessagePackage msg_package, out List<long> targets)
        {
            targets = new List<long>();
            //忽略所有不在映射表里的消息
            int[] count = new int[16];            
            //遍历消息链，统计每种类型子消息的数量。如果遇到At，添加目标ID到输出
            foreach (var sub_msg in msg_package.messageChain)
            {
                if (IndexOf.ContainsKey(sub_msg.type))
                    count[IndexOf[sub_msg.type]]++;
                else if (sub_msg.type == "At")
                    targets.Add(sub_msg.target);
            }
            bool has_plain = count[IndexOf["Plain"]] != 0;
            bool has_img = count[IndexOf["Image"]] != 0;
            bool has_voice = count[IndexOf["Voice"]] != 0;

            //根据统计结果判断消息类型
            if (has_plain)
            {
                if (!has_img)
                    return typeof(MessagePlain);
                else
                    return typeof(MessageImagePlain);
            }
            else if (has_img)
                return typeof(MessageImage);
            else if (has_voice)
                return typeof(MessageVoice);
            else
                return typeof(MessageEmpty);
        }
        //根据消息链，将Message对象的Context字段和相应的ID字段修改成合适值
        private static void RefineContext(JMessagePackage msg_package, Message msg)
        {
            switch (msg_package.type)
            {
                case "FriendMessage":
                    msg.Context = MessageContext.Private;
                    msg.SenderID = msg_package.sender.id;
                    break;
                case "TempMessage":
                    msg.Context = MessageContext.Temp;
                    msg.GroupID = msg_package.sender.group.id;
                    break;
                case "GroupMessage":
                    msg.Context = MessageContext.Group;
                    msg.GroupID = msg_package.sender.group.id;
                    break;
            }
        }
        //从消息链构造各种Message对象
        private static MessagePlain BuildPlain(JMessagePackage msg_package)
        {
            bool is_after_at = false;
            MessagePlain ret;
            if (msg_package.sender.group == null)
                ret = new MessagePlain(msg_package.sender.id, -1);
            else
                ret = new MessagePlain(msg_package.sender.id, msg_package.sender.group.id);
            foreach (var sub_msg in msg_package.messageChain)
            {
                if (sub_msg.type == "Plain")
                {
                    if (is_after_at)           //意味着之前还有别的Plain消息，而且上一条子消息是At消息。用Substring()处理@后面无缘无故冒出来的傻逼空格
                    {
                        ((MessagePlain)ret).AppendText(sub_msg.text.Substring(1));
                        is_after_at = false;
                    }
                    else                            //意味着之前还有别的Plain消息。则简单将文本追加到已有对象的文本中
                        ((MessagePlain)ret).AppendText(sub_msg.text);
                }
                else if (sub_msg.type == "At")
                    is_after_at = true;
            }
            return ret;
        }
        private static MessageImage BuildImage(JMessagePackage msg_package)
        {
            MessageImage ret = new MessageImage(msg_package.sender.id, msg_package.sender.group.id);
            //由于作图像消息处理，只关心图像消息，所以一旦找到Url直接返回
            foreach (var sub_msg in msg_package.messageChain)
            {
                if (sub_msg.type == "Image")
                    ret.Add(sub_msg.url);
            }
            return ret;
        }
        private static MessageVoice BuildVoice(JMessagePackage msg_package)
        {
            MessageVoice ret = new MessageVoice(msg_package.sender.id, msg_package.sender.group.id);
            //只关心音频子消息，所以一旦找到Url直接返回
            foreach (var sub_msg in msg_package.messageChain)
            {
                if (sub_msg.type == "Voice")
                {
                    ret.Url = sub_msg.url;
                    return ret;
                }
            }
            return ret;
        }
        private static MessageImagePlain BuildImagePlain(JMessagePackage msg_package)
        {
            var img_part = BuildImage(msg_package); //按图像解析，得到图像部分
            var plain_part = BuildPlain(msg_package);   //按纯文本解析，得到文字部分
            //结合两个部分生成图文消息
            return new MessageImagePlain(msg_package.sender.id, msg_package.sender.group.id, plain_part.Text, img_part.UrlList);
        }
    }
}