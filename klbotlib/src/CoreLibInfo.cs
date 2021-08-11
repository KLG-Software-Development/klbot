using System;
using System.Reflection;

namespace klbotlib.Info
{
    /// <summary>
    /// klbotlib的程序集信息
    /// </summary>
    public static class CoreLibInfo
    {
        /// <summary>
        /// 获取程序集版本
        /// </summary>
        /// <returns>程序集版本</returns>
        public static Version GetLibVersion() => Assembly.GetExecutingAssembly().GetName().Version;
    }
}
