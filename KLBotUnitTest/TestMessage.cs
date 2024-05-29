﻿using klbotlib;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections;

namespace KLBotUnitTest;
[TestClass]
public class TestMessage
{
    // 测试Message*.ToString()
    [TestMethod]
    public void TestToString()
    {
        Random ro = new();
        //Context = Private
        {
            long senderId = ro.NextInt64();
            long groupId = ro.NextInt64();
            long targetId = ro.NextInt64();
            long authorId = ro.NextInt64();
            long msgId = ro.NextInt64();
            string text = ro.NextInt64().ToString();
            MessageContext context = MessageContext.Private;
            string[] urlList = new string[] { ro.NextDouble().ToString(), ro.NextDouble().ToString() };
            // MessagePlain
            Message m = new MessagePlain(context, senderId, groupId, text);
            string s = m.ToString();
            Assert.AreEqual($"Type: MessagePlain\nContext: {context}\nFrom: {senderId}\nText: {text}", s, "检查MessagePlain.ToString()");
            var p = m as MessagePlain;
            p.AddTargetId(targetId);
            s = m.ToString();
            Assert.AreEqual($"Type: MessagePlain\nContext: {context}\nFrom: {senderId}\nTarget[0]: {targetId}\nText: {text}", s, "检查MessagePlain.ToString()");
            p.AddTargetId(msgId);
            s = m.ToString();
            Assert.AreEqual($"Type: MessagePlain\nContext: {context}\nFrom: {senderId}\nTarget[0]: {targetId}\nTarget[1]: {msgId}\nText: {text}", s, "检查MessagePlain.ToString()");
            // MessageImage
            m = new MessageImage(context, senderId, groupId, urlList);
            s = m.ToString();
            Assert.AreEqual($"Type: MessageImage\nContext: {context}\nFrom: {senderId}\nUrl[0]: {urlList[0]}\nUrl[1]: {urlList[1]}", s, "检查MessageImage.ToString()");
            // MessageFlashImage
            m = new MessageFlashImage(context, senderId, groupId, urlList);
            s = m.ToString();
            Assert.AreEqual($"Type: MessageFlashImage\nContext: {context}\nFrom: {senderId}\nUrl[0]: {urlList[0]}\nUrl[1]: {urlList[1]}", s, "检查MessageFlashImage.ToString()");
            // MessageVoice
            m = new MessageVoice(context, senderId, groupId, urlList[1]);
            s = m.ToString();
            Assert.AreEqual($"Type: MessageVoice\nContext: {context}\nFrom: {senderId}\nUrl: {urlList[1]}", s, "检查MessageVoice.ToString()");
            //MessageRecall
            MessageRecall recall = new(context, authorId, senderId, groupId, msgId);
            s = recall.ToString();
            Assert.AreEqual($"Type: MessageRecall\nContext: {context}\nOperator: {senderId}\nMessageID: {msgId}\nAuthor: {authorId}", s, "检查MessageRecall.ToString()");
        }
        //Context = Group || Temp
        for (int i = 1; i <= 2; i++)
        {
            long senderId = ro.NextInt64();
            long groupId = ro.NextInt64();
            long authorId = ro.NextInt64();
            long msgId = ro.NextInt64();
            string text = ro.NextInt64().ToString();
            MessageContext context = (MessageContext)i;
            string[] urlList = new string[] { ro.NextDouble().ToString(), ro.NextDouble().ToString() };
            // MessagePlain
            Message m = new MessagePlain(context, senderId, groupId, text);
            string s = m.ToString();
            Assert.AreEqual($"Type: MessagePlain\nContext: {context}\nGroup: {groupId}\nFrom: {senderId}\nText: {text}", s, "检查MessagePlain.ToString()");
            // MessageImage
            m = new MessageImage(context, senderId, groupId, urlList);
            s = m.ToString();
            Assert.AreEqual($"Type: MessageImage\nContext: {context}\nGroup: {groupId}\nFrom: {senderId}\nUrl[0]: {urlList[0]}\nUrl[1]: {urlList[1]}", s, "检查MessageImage.ToString()");
            // MessageFlashImage
            m = new MessageFlashImage(context, senderId, groupId, urlList);
            s = m.ToString();
            Assert.AreEqual($"Type: MessageFlashImage\nContext: {context}\nGroup: {groupId}\nFrom: {senderId}\nUrl[0]: {urlList[0]}\nUrl[1]: {urlList[1]}", s, "检查MessageFlashImage.ToString()");
            // MessageVoice
            m = new MessageVoice(context, senderId, groupId, urlList[1]);
            s = m.ToString();
            Assert.AreEqual($"Type: MessageVoice\nContext: {context}\nGroup: {groupId}\nFrom: {senderId}\nUrl: {urlList[1]}", s, "检查MessageVoice.ToString()");
            //MessageRecall
            MessageRecall recall = new(context, authorId, senderId, groupId, msgId);
            s = recall.ToString();
            Assert.AreEqual($"Type: MessageRecall\nContext: {context}\nGroup: {groupId}\nOperator: {senderId}\nMessageID: {msgId}\nAuthor: {authorId}", s, "检查MessageRecall.ToString()");
        }
    }
    // 测试MessageCommon.TargetID
    [TestMethod]
    public void TestTargetID()
    {
        Random ro = new();
        int length = 0;
        long id = 0;
        long[] ids = new long[] { 1, 2, 3};
        MessagePlain plain = new(MessageContext.Group, 2, 3, ro.Next().ToString());
        Assert.AreEqual(0, plain.TargetId.Count, "默认情况下@目标数量应为0");
        plain.AddTargetId(id);
        length++;
        Assert.AreEqual(length, plain.TargetId.Count, "AddTargetID(long)每次应只添加一个目标ID");
        Assert.AreEqual(id, plain.TargetId[0], "AddTargetID(long)应正确添加目标ID");
        Assert.IsTrue(plain.ContainsTargetId(id), "ContainsTargetID()结果应保持一致");
        plain.AddTargetID(ids);
        length += ids.Length;
        Assert.AreEqual(length, plain.TargetId.Count, "AddTargetID(long[])应一次添加所有元素");
        for (int i = 0; i < ids.Length; i++)
        {
            Assert.AreEqual(ids[i], plain.TargetId[i + 1], "AddTargetID(long[])应按顺序添加所有元素");
        }
        plain.AddTargetId(id);
        length++;
        Assert.AreEqual(length, plain.TargetId.Count, "应允许重复添加同一ID");
        Assert.AreEqual(id, plain.TargetId[length - 1], "应允许重复添加同一ID");
        plain.ClearTargetId();
        Assert.AreEqual(0, plain.TargetId.Count, "ClearTargetID()应清空目标ID列表");
        Assert.IsFalse(plain.ContainsTargetId(id), "ClearTargetID()应清空目标ID列表");
    }
    // 测试深拷贝
    [TestMethod]
    public void TestDeepCopy()
    {
        Random ro = new();
        long targetId = ro.Next(1000);
        string[] urlList = new string[] { "url1", "url2", "url3" };
        //Message & MessagePlain
        {
            MessagePlain plain = new(ro.Next(1000), ro.Next(1000));
            plain.AddTargetId(targetId);
            MessagePlain copyPlain = (MessagePlain)plain.DeepCopy();
            //TargetID
            CollectionAssert.AreEquivalent((ICollection)plain.TargetId, (ICollection)copyPlain.TargetId, "拷贝结果内容应与原始内容相等 (Message.TargetID)");
            copyPlain.AddTargetId(ro.NextInt64());
            CollectionAssert.AreNotEquivalent((ICollection)plain.TargetId, (ICollection)copyPlain.TargetId, "修改拷贝结果不应影响原始内容 (Message.TargetID)");
            //Message.GroupID和Message.Context字段无需测试：外部只读
            //Message.SenderID 字段无需测试：外部只读
            //MessagePlain.Text 字段无需测试：外部只读
        }
        //MessageImage
        {
            MessageImage image = new(ro.Next(1000), ro.Next(1000), urlList);
            MessageImage copyImage = (MessageImage)image.DeepCopy();
            //UrlList
            CollectionAssert.AreEquivalent(image.UrlList, copyImage.UrlList, "拷贝结果内容应与原始内容相等 (MessageImage.UrlList)");
            copyImage.UrlList.Add("url4");
            CollectionAssert.AreNotEquivalent(image.UrlList, copyImage.UrlList, "修改拷贝结果不应影响原始内容 (MessageImage.UrlList)");
        }
        //MessageFlashImage
        {
            MessageFlashImage image = new(ro.Next(1000), ro.Next(1000), urlList);
            MessageFlashImage copyImage = (MessageFlashImage)image.DeepCopy();
            //UrlList
            CollectionAssert.AreEquivalent(image.UrlList, copyImage.UrlList, "拷贝结果内容应与原始内容相等 (MessageFlashImage.UrlList)");
            copyImage.UrlList.Add("url4");
            CollectionAssert.AreNotEquivalent(image.UrlList, copyImage.UrlList, "修改拷贝结果不应影响原始内容 (MessageFlashImage.UrlList)");
        }
        //MessageImagePlain
        {
            MessageImagePlain imagePlain = new(ro.Next(1000), ro.Next(1000), ro.NextDouble().ToString());
            MessageImagePlain copyImagePlain = (MessageImagePlain)imagePlain.DeepCopy();
            //UrlList
            CollectionAssert.AreEquivalent(imagePlain.UrlList, copyImagePlain.UrlList, "拷贝结果内容应与原始内容相等 (MessageImagePlain.UrlList)");
            copyImagePlain.UrlList.Add("url4");
            CollectionAssert.AreNotEquivalent(imagePlain.UrlList, copyImagePlain.UrlList, "修改拷贝结果不应影响原始内容 (MessageImagePlain.UrlList)");
            //Text字段无需测试：外部只读
        }
        //MessageRecall 无需测试：全部字段外部只读
        //MessageVoice 无需测试：全部字段外部只读
    }
}