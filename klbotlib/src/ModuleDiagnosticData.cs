using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

namespace klbotlib
{
    public class ModuleDiagnosticData
    {
        private Stopwatch stopwatch = new Stopwatch();
        /// <summary>
        /// 最后一次处理消息消耗的时间
        /// </summary>
        public long LastProcessTime { get; internal set; } = -1;
        /// <summary>
        /// 经过该模块实例处理的消息总数
        /// </summary>
        public long ProcessedMessageCount { get; internal set; }
        /// <summary>
        /// 最近一次异常的调用栈
        /// </summary>
        public Exception LastException { get; internal set; }
        /// <summary>
        /// 重新开始处理计时
        /// </summary>
        internal void RestartMeasurement() => stopwatch.Restart();
        /// <summary>
        /// 结束计时并记录处理时间
        /// </summary>
        internal void StopMeasurement()
        {
            stopwatch.Stop();
            LastProcessTime = stopwatch.ElapsedMilliseconds;
        }
        /// <summary>
        /// 获取此模块的统计信息
        /// </summary>
        public string GetSummaryString()
        {
            string re = $"共处理了{ProcessedMessageCount}条消息";
            if (LastProcessTime != -1)
                re += $"；最后一次处理耗时{LastProcessTime}ms";
            return re;
        }
    }
}
