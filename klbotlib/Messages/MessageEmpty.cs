namespace klbotlib
{
    //忽略的Message类。显然其内部所有不需要处理的JMessage直接对象都会被构建成这个类型。bot在后续处理中会把这种类型的消息全部忽略
    internal class MessageEmpty : Message { }
}
