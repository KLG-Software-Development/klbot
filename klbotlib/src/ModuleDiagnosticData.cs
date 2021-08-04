using klbotlib.Extensions;
using System;
using System.Diagnostics;

namespace klbotlib
{
    /// <summary>
    /// 模块诊断统计信息
    /// </summary>
    public class ModuleDiagnosticData
    {
        private Stopwatch stopwatch = new Stopwatch();
        /// <summary>
        /// 最后一次处理消息消耗的时间
        /// </summary>
        public string LastProcessTime { get; internal set; } = "N/A";
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
            LastProcessTime = stopwatch.Elapsed.ToMsString();
        }
        /// <summary>
        /// 获取此模块的统计信息
        /// </summary>
        public string GetSummaryString()
        {
            string re = $"共处理了{ProcessedMessageCount}条消息";
            if (LastProcessTime != "N/A")
                re += $"；最后一次处理耗时{LastProcessTime}";
            if (LastException != null)
                re += $"；\n最近一次异常信息：{LastException.Message}\n调用栈：\n{LastException.StackTrace}";
            return re;
        }
    }
}
