using klbotlib;
using klbotlib.MessageServer.Debug;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;

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
        MessagePlain plain = new(2, 3, ro.Next().ToString());
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
}