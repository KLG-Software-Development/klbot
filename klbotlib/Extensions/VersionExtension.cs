using System;

namespace klbotlib.Extensions
{
    /// <summary>
    /// Version扩展
    /// </summary>
    public static class VersionExtension
    {
        /// <summary>
        /// 从版本号计算KLG标准的Build编号
        /// </summary>
        /// <param name="version">版本号</param>
        /// <returns>KLG标准的Build编号</returns>
        public static string ToKLGBuildString(this Version version)
        {
            TimeSpan seconds = new TimeSpan(0, 0, version.Revision * 2);
            return version.Build + "_" + seconds.ToString("hhmm");
        }
    }
}
