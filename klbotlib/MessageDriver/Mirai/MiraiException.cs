using System;

namespace klbotlib.MessageDriver.Mirai;

internal class MiraiException : Exception
{
    public MiraiException(int code, string? msg) : base($"Mirai服务器错误[{code}]：{msg}") { }
}
