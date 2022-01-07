using klbotlib.MessageServer.Mirai.JsonPrototypes;
using System;
using System.Collections.Generic;

namespace klbotlib.MessageServer.Mirai
{
    //消息工厂类。用来从JMessagePackage对象中生成相应的Message类型
    internal static class MiraiMessageFactory
    {
        private static readonly Dictionary<string, int> _indexOf = new()
        {
            { "Plain", 0 },
            { "Image", 1 },
            { "Voice", 2 },
            { "FlashImage", 3 },
        };
        private static readonly HashSet<string> _commonMessageTypes = new() { "GroupMessage", "TempMessage", "FriendMessage" };


        //从消息链生成Message对象
        internal static Message BuildMessage(JMiraiMessagePackage msgPackage)
        {
            var type = CalcMessageType(msgPackage, out var targets);
            Message ret = null;

            MessageCommon retCommon = null;
            if (type == typeof(MessagePlain))
                retCommon = BuildPlain(msgPackage);
            else if (type == typeof(MessageImage))
                retCommon = BuildImage(msgPackage);
            else if (type == typeof(MessageVoice))
                retCommon = BuildVoice(msgPackage);
            else if (type == typeof(MessageFlashImage))
                retCommon = BuildFlashImage(msgPackage);
            else if (type == typeof(MessageImagePlain))
                retCommon = BuildImagePlain(msgPackage);
            else if (type == typeof(MessageRecall))
                ret = new MessageRecall(msgPackage.authorId, msgPackage.@operator.id, -1, msgPackage.messageId);
            else
                return Message.Empty;
            if (retCommon != null)
            {
                retCommon.AddTargetID(targets);
                ret = retCommon;
            }
            //加工合适的上下文和ID
            RefineContext(msgPackage, ret);
            return ret;
        }
        //直接从mirai JSON生成Message对象列表

        //给定一条消息链，统计并判断应该生成的消息类型，并顺便输出@的ID
        private static Type CalcMessageType(JMiraiMessagePackage msgPackage, out List<long> targets)
        {
            targets = new List<long>();
            //(Issue#22) 判断是否是普通消息 i.e.群聊消息、临时会话消息、私聊消息中的一种
            //如果是，则正常走统计构建流程
            if (_commonMessageTypes.Contains(msgPackage.type))
            {
                //忽略所有不在映射表里的消息
                int[] count = new int[16];
                //遍历消息链，统计每种类型子消息的数量。如果遇到At，添加目标ID到输出
                foreach (var sub_msg in msgPackage.messageChain)
                    if (_indexOf.ContainsKey(sub_msg.type))
                        count[_indexOf[sub_msg.type]]++;
                    else if (sub_msg.type == "At")
                        targets.Add(sub_msg.target);
                bool has_plain = count[_indexOf["Plain"]] != 0;
                bool has_img = count[_indexOf["Image"]] != 0;
                bool has_voice = count[_indexOf["Voice"]] != 0;
                bool hasFlashImg = count[_indexOf["FlashImage"]] != 0;

                //根据统计结果判断消息类型
                if (has_plain)
                    if (!has_img)
                        return typeof(MessagePlain);
                    else
                        return typeof(MessageImagePlain);
                else if (has_img)
                    return typeof(MessageImage);
                else if (has_voice)
                    return typeof(MessageVoice);
                else if (hasFlashImg)
                    return typeof(MessageFlashImage);
                else
                    return typeof(MessageEmpty);
            }
            //否则具体情况具体分析
            else
            {
                if (msgPackage.type == "GroupRecallEvent")
                    return typeof(MessageRecall);
                else
                    return typeof(MessageEmpty);
            }

        }
        //根据消息链，将Message对象的Context字段和相应的ID字段修改成合适值
        private static void RefineContext(JMiraiMessagePackage msgPackage, Message msg)
        {
            switch (msgPackage.type)
            {
                case "FriendMessage":
                    msg.Context = MessageContext.Private;
                    if (msg is MessageCommon msgc)
                        msgc.SenderID = msgPackage.sender.id;
                    break;
                case "TempMessage":
                    msg.Context = MessageContext.Temp;
                    msg.GroupID = msgPackage.sender.group.id;
                    break;
                case "GroupMessage":
                    msg.Context = MessageContext.Group;
                    msg.GroupID = msgPackage.sender.group.id;
                    break;
                case "GroupRecallEvent":
                    msg.Context = MessageContext.Group;
                    msg.GroupID = msgPackage.@operator.group.id;
                    break;
            }
        }
        //从消息链构造各种Message对象
        private static MessagePlain BuildPlain(JMiraiMessagePackage msgPackage)
        {
            bool isAfterAt = false;
            MessagePlain ret;
            if (msgPackage.sender.group == null)
                ret = new MessagePlain(msgPackage.sender.id, -1);
            else
                ret = new MessagePlain(msgPackage.sender.id, msgPackage.sender.group.id);
            foreach (var subMsg in msgPackage.messageChain)
                if (subMsg.type == "Plain")
                    if (isAfterAt)           //意味着之前还有别的Plain消息，而且上一条子消息是At消息。用Substring()处理@后面无缘无故冒出来的傻逼空格
                    {
                        ret.AppendText(subMsg.text.Substring(1));
                        isAfterAt = false;
                    }
                    else                            //意味着之前还有别的Plain消息。则简单将文本追加到已有对象的文本中
                        ret.AppendText(subMsg.text);
                else if (subMsg.type == "Face")
                    ret.AppendText(subMsg.text);
                else if (subMsg.type == "At")
                    isAfterAt = true;
            return ret;
        }
        private static MessageImage BuildImage(JMiraiMessagePackage msgPackage)
        {
            MessageImage ret = new MessageImage(msgPackage.sender.id, msgPackage.sender.group.id);
            //由于作图像消息处理，只关心图像消息，所以一旦找到Url直接返回
            foreach (var subMsg in msgPackage.messageChain)
                if (subMsg.type == "Image")
                    ret.Add(subMsg.url);
            return ret;
        }
        private static MessageFlashImage BuildFlashImage(JMiraiMessagePackage msgPackage)
        {
            MessageFlashImage ret = new MessageFlashImage(msgPackage.sender.id, msgPackage.sender.group.id);
            //由于作图像消息处理，只关心图像消息，所以一旦找到Url直接返回
            foreach (var subMsg in msgPackage.messageChain)
                if (subMsg.type == "FlashImage")
                    ret.Add(subMsg.url);
            return ret;
        }
        private static MessageVoice BuildVoice(JMiraiMessagePackage msgPackage)
        {
            MessageVoice ret = new MessageVoice(msgPackage.sender.id, msgPackage.sender.group.id);
            //只关心音频子消息，所以一旦找到Url直接返回
            foreach (var subMsg in msgPackage.messageChain)
                if (subMsg.type == "Voice")
                {
                    ret.Url = subMsg.url;
                    return ret;
                }
            return ret;
        }
        private static MessageImagePlain BuildImagePlain(JMiraiMessagePackage msgPackage)
        {
            var imgPart = BuildImage(msgPackage); //按图像解析，得到图像部分
            var plainPart = BuildPlain(msgPackage);   //按纯文本解析，得到文字部分
            //结合两个部分生成图文消息
            return new MessageImagePlain(msgPackage.sender.id, msgPackage.sender.group.id, plainPart.Text, imgPart.UrlList);
        }
    }
}
