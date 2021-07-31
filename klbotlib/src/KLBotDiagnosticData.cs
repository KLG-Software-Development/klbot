﻿using klbotlib.Modules;
using System;

namespace klbotlib
{
    /// <summary>
    /// KLBot诊断数据类
    /// </summary>
    public class KLBotDiagnosticData
    {
        /// <summary>
        /// 最近一次处理消息的模块
        /// </summary>
        public Module LastUsedModule { get; set; }
        /// <summary>
        /// KLBot初始化后，成功进行查询的总次数
        /// </summary>
        public long SuccessPackageCount { get; set; }
        /// <summary>
        /// KLBot初始化后，接收到的消息总数
        /// </summary>
        public long ReceivedMessageCount { get; set; }
        /// <summary>
        /// KLBot初始化后，所有模块处理消息的总次数。不同模块处理次数会叠加，比如一条消息被两个模块分别处理两次，则计为两次。
        /// </summary>
        public long ProcessedMessageCount { get; set; }
        /// <summary>
        /// 最近一次KLBot异常的调用栈
        /// </summary>
        public Exception LastException { get; set; }
        /// <summary>
        /// 获取KLBot的统计信息
        /// </summary>
        public string GetSummaryString()
        {
            string re = $"已发起{SuccessPackageCount}次查询；共收到{ReceivedMessageCount}条消息；经过各模块处理{ProcessedMessageCount}次";
            if (LastUsedModule != null)
                re += $"；最后一个参与处理的模块为{ LastUsedModule}；处理耗时{LastUsedModule.DiagData.LastProcessTime}ms";
            return re;
        }
    }
}