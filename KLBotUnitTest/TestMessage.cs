using klbotlib;
using klbotlib.MessageServer.Debug;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections;
using System.Collections.Generic;

namespace KLBotUnitTest;
[TestClass]
public class TestMessage
{
    // 测试TargetID
    [TestMethod]
    public void TestTargetID()
    {
        Random ro = new();
        int length = 0;
        long id = 0;
        long[] ids = new long[] { 1, 2, 3};
        MessagePlain plain = new(2, 3, MessageContext.Group, ro.Next().ToString());
        Assert.AreEqual(0, plain.TargetID.Count, "默认情况下@目标数量应为0");
        plain.AddTargetID(id);
        length++;
        Assert.AreEqual(length, plain.TargetID.Count, "AddTargetID(long)每次应只添加一个目标ID");
        Assert.AreEqual(id, plain.TargetID[0], "AddTargetID(long)应正确添加目标ID");
        Assert.IsTrue(plain.ContainsTargetID(id), "ContainsTargetID()结果应保持一致");
        plain.AddTargetID(ids);
        length += ids.Length;
        Assert.AreEqual(length, plain.TargetID.Count, "AddTargetID(long[])应一次添加所有元素");
        for (int i = 0; i < ids.Length; i++)
        {
            Assert.AreEqual(ids[i], plain.TargetID[i + 1], "AddTargetID(long[])应按顺序添加所有元素");
        }
        plain.AddTargetID(id);
        length++;
        Assert.AreEqual(length, plain.TargetID.Count, "应允许重复添加同一ID");
        Assert.AreEqual(id, plain.TargetID[length - 1], "应允许重复添加同一ID");
        plain.ClearTargetID();
        Assert.AreEqual(0, plain.TargetID.Count, "ClearTargetID()应清空目标ID列表");
        Assert.IsFalse(plain.ContainsTargetID(id), "ClearTargetID()应清空目标ID列表");
    }
    // 测试深拷贝
    [TestMethod]
    public void TestDeepCopy()
    {
        Random ro = new();
        long targetId = ro.Next(1000);
        string[] urlList = new string[] { "url1", "url2", "url3" };
        //MessageCommon & MessagePlain
        {
            MessagePlain plain = new(ro.Next(1000), ro.Next(1000));
            plain.AddTargetID(targetId);
            MessagePlain copyPlain = plain.DeepCopy() as MessagePlain;
            //TargetID
            CollectionAssert.AreEquivalent((ICollection)plain.TargetID, (ICollection)copyPlain.TargetID, "拷贝结果内容应与原始内容相等 (MessageCommon.TargetID)");
            copyPlain.AddTargetID(ro.NextInt64());
            CollectionAssert.AreNotEquivalent((ICollection)plain.TargetID, (ICollection)copyPlain.TargetID, "修改拷贝结果不应影响原始内容 (MessageCommon.TargetID)");
            //Message.GroupID和Message.Context字段无需测试：外部只读
            //MessageCommon.SenderID 字段无需测试：外部只读
            //MessagePlain.Text 字段无需测试：外部只读
        }
        //MessageImage
        {
            MessageImage image = new(ro.Next(1000), ro.Next(1000), urlList);
            MessageImage copyImage = image.DeepCopy() as MessageImage;
            //UrlList
            CollectionAssert.AreEquivalent(image.UrlList, copyImage.UrlList, "拷贝结果内容应与原始内容相等 (MessageImage.UrlList)");
            copyImage.UrlList.Add("url4");
            CollectionAssert.AreNotEquivalent(image.UrlList, copyImage.UrlList, "修改拷贝结果不应影响原始内容 (MessageImage.UrlList)");
        }
        //MessageFlashImage
        {
            MessageFlashImage image = new(ro.Next(1000), ro.Next(1000), urlList);
            MessageFlashImage copyImage = image.DeepCopy() as MessageFlashImage;
            //UrlList
            CollectionAssert.AreEquivalent(image.UrlList, copyImage.UrlList, "拷贝结果内容应与原始内容相等 (MessageFlashImage.UrlList)");
            copyImage.UrlList.Add("url4");
            CollectionAssert.AreNotEquivalent(image.UrlList, copyImage.UrlList, "修改拷贝结果不应影响原始内容 (MessageFlashImage.UrlList)");
        }
        //MessageImagePlain
        {
            MessageImagePlain imagePlain = new(ro.Next(1000), ro.Next(1000), ro.NextDouble().ToString());
            MessageImagePlain copyImagePlain = imagePlain.DeepCopy() as MessageImagePlain;
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