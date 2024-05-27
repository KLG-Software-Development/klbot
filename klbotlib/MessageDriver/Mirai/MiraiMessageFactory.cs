using klbotlib.MessageDriver.Mirai.JsonPrototypes;
using System;
using System.Collections.Generic;

namespace klbotlib.MessageDriver.Mirai
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
            Message? ret = null;

            MessageCommon? retCommon = null;
            if (type == typeof(MessagePlain))
                retCommon = BuildPlain(msgPackage);
            else if (type == typeof(MessageImage))
                retCommon = BuildImage(msgPackage);
            else if (type == typeof(MessageVoice))
                retCommon = BuildVoice(msgPackage);
            else if (type == typeof(MessageFlashImage))
                retCommon = BuildFlashImage(msgPackage);
            else if (type == typeof(MessageRecall))
                ret = new MessageRecall(msgPackage.authorId, msgPackage.@operator.id, -1, msgPackage.messageId);
            else if (type == typeof(MessageMute))
            {
                //根据情况构造相应的禁言/取消禁言消息
                switch (msgPackage.type)
                {
                    case "MemberMuteEvent":
                        ret = new MessageMute(false, msgPackage.@operator.GroupId, msgPackage.@operator.id, msgPackage.member.id, msgPackage.durationSeconds);
                        break;
                    case "BotMuteEvent":
                        ret = new MessageMute(false, msgPackage.@operator.GroupId, msgPackage.@operator.id, 0, msgPackage.durationSeconds);
                        break;
                    case "MemberUnmuteEvent":
                        ret = new MessageMute(true, msgPackage.@operator.GroupId, msgPackage.@operator.id, msgPackage.member.id);
                        break;
                    case "BotUnmuteEvent":
                        ret = new MessageMute(true, msgPackage.@operator.GroupId, msgPackage.@operator.id, 0);
                        break;
                }
            }
            else
                return Message.Empty;
            List<Message> msgArray = [ ];
            if (retCommon != null)
            {
                msgArray.Add(retCommon);
                foreach (var id in targets)
                {
                    msgArray.Add(new MessageAt(msgPackage.sender.id, msgPackage.@operator.GroupId, id));
                }
            }
            ret = new MessageArray(msgPackage.sender.id, msgPackage.@operator.GroupId, msgArray);
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
            if (msgPackage.type != null && _commonMessageTypes.Contains(msgPackage.type))
            {
                //忽略所有不在映射表里的消息
                int[] count = new int[16];
                //遍历消息链，统计每种类型子消息的数量。如果遇到At，添加目标ID到输出
                foreach (JMiraiMessage subMsg in msgPackage.messageChain)
                {
                    if (subMsg == null || subMsg.type == null)
                        throw new Exception("意外遭遇null");
                    else if (_indexOf.ContainsKey(subMsg.type))
                        count[_indexOf[subMsg.type]]++;
                    else if (subMsg.type == "At")
                        targets.Add(subMsg.target);
                }
                bool hasPlain = count[_indexOf["Plain"]] != 0;
                bool hasImg = count[_indexOf["Image"]] != 0;
                bool hasVoice = count[_indexOf["Voice"]] != 0;
                bool hasFlashImg = count[_indexOf["FlashImage"]] != 0;

                //根据统计结果判断消息类型
                if (hasPlain)
                    if (!hasImg)
                        return typeof(MessagePlain);
                    else
                        return typeof(MessageImagePlain);
                else if (hasImg)
                    return typeof(MessageImage);
                else if (hasVoice)
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
                else if (msgPackage.type == "BotMuteEvent" || msgPackage.type == "MemberMuteEvent"
                    || msgPackage.type == "BotUnmuteEvent" || msgPackage.type == "MemberUnmuteEvent")
                    return typeof(MessageMute);
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
                        msgc.SenderId = msgPackage.sender.id;
                    break;
                case "TempMessage":
                    msg.Context = MessageContext.Temp;
                    msg.GroupId = msgPackage.sender.group.id;
                    break;
                case "GroupMessage":
                    msg.Context = MessageContext.Group;
                    msg.GroupId = msgPackage.sender.group.id;
                    break;
                case "GroupRecallEvent":
                case "BotMuteEvent":
                case "MemberMuteEvent":
                case "BotUnmuteEvent":
                case "MemberUnmuteEvent":
                    msg.Context = MessageContext.Group;
                    msg.GroupId = msgPackage.@operator.group.id;
                    break;
            }
        }
        //从消息链构造各种Message对象
        private static MessagePlain BuildPlain(JMiraiMessagePackage msgPackage)
        {
            bool isAfterAt = false;
            MessagePlain ret;
            ret = new MessagePlain(msgPackage.sender.id, msgPackage.sender.GroupId);
            foreach (JMiraiMessage subMsg in msgPackage.messageChain)
            {
                if (subMsg.type == "Plain" && subMsg.text != null)
                {
                    if (isAfterAt)           //意味着之前还有别的Plain消息，而且上一条子消息是At消息。用Substring()处理@后面无缘无故冒出来的傻逼空格
                    {
                        ret.AppendText(subMsg.text.Substring(1));
                        isAfterAt = false;
                    }
                    else                            //意味着之前还有别的Plain消息。则简单将文本追加到已有对象的文本中
                        ret.AppendText(subMsg.text);
                }
                else if (subMsg.type == "Face")
                    ret.AppendText(@$"{{\face:{subMsg.name}}}");
                else if (subMsg.type == "At")
                    isAfterAt = true;
            }
            return ret;
        }
        private static MessageImage BuildImage(JMiraiMessagePackage msgPackage)
        {
            MessageImage ret = new MessageImage(msgPackage.sender.id, msgPackage.sender.GroupId);
            //由于作图像消息处理，只关心图像消息，所以一旦找到Url直接返回
            foreach (var subMsg in msgPackage.messageChain)
                if (subMsg.type == "Image" && subMsg.url != null)
                    ret.UrlList.Add(subMsg.url);
            return ret;
        }
        private static MessageFlashImage BuildFlashImage(JMiraiMessagePackage msgPackage)
        {
            MessageFlashImage ret = new MessageFlashImage(msgPackage.sender.id, msgPackage.sender.GroupId);
            //由于作图像消息处理，只关心图像消息，所以一旦找到Url直接返回
            foreach (var subMsg in msgPackage.messageChain)
                if (subMsg.type == "FlashImage" && subMsg.url != null)
                    ret.UrlList.Add(subMsg.url);
            return ret;
        }
        private static MessageVoice BuildVoice(JMiraiMessagePackage msgPackage)
        {
            MessageVoice ret = new MessageVoice(msgPackage.sender.id, msgPackage.sender.GroupId);
            //只关心音频子消息，所以一旦找到Url直接返回
            foreach (var subMsg in msgPackage.messageChain)
                if (subMsg.type == "Voice" && subMsg.url != null)
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
            return new MessageImagePlain(msgPackage.sender.id, msgPackage.sender.GroupId, plainPart.Text, imgPart.UrlList);
        }
    }
}
