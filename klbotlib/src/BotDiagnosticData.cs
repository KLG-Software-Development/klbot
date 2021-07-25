using klbotlib.Modules;
using System;
using System.Diagnostics;

namespace klbotlib
{
    public class BotDiagnosticData
    {
        public readonly Stopwatch Stopwatch = new Stopwatch();
        //最后一次处理消息的模块
        public Module LastUsedModule { get; set; }
        //最后一次处理消息耗时（毫秒）
        public long LastProcessingTime = 0;
        //最后一次发送消息耗时（毫秒）
        public long LastReplyTime = 0;
        //成功进行查询的次数
        public long SuccessPackageCount { get; set; }
        //接收到的消息条数
        public long ReceivedMessageCount { get; set; }
        //所有模块处理消息的总次数
        public long ProcessedCount { get; set; }
        //最后一个错误的调用栈
        public Exception LastException { get; set; }

        public string GetSummaryString()
        {
            string re = $"已发起{SuccessPackageCount}次查询；共收到{ReceivedMessageCount}条消息；经过各模块处理{ProcessedCount}次";
            if (LastUsedModule != null)
                re += $"最后一个参与处理的模块为{ LastUsedModule}；该次处理耗时{ LastProcessingTime}ms；回复消息耗时{ LastReplyTime }ms";
            return re;
        }
    }
}
