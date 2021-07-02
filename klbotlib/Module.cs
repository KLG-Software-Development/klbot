using System;
using System.Collections.Generic;
using System.Text;

namespace klbotlib
{
    public abstract class Module<T> where T : Message
    {
        /// <summary>
        /// 模块的开关. 默认开启
        /// </summary>
        public bool Enabled { get; set; } = true;
        /// <summary>
        /// 过滤器. 此函数根据消息判断其是否进入模块做进一步处理. 返回false的消息将被忽略.
        /// </summary>
        /// <param name="msg">待判断消息</param>
        /// <returns></returns>
        public abstract bool Filter(T msg);
        /// <summary>
        /// 处理器. 此函数用来处理消息
        /// </summary>
        /// <param name="msg">待处理消息</param>
        /// <returns></returns>
        public abstract string Processor(T msg);
        /// <summary>
        /// 综合过滤器和开关的影响, 返回消息是否应被处理
        /// </summary>
        /// <param name="msg">待判断消息</param>
        /// <returns></returns>
        public bool ShouldProcess(T msg) => Enabled && Filter(msg);
    }
}
