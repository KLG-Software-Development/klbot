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
    /// <summary>
    /// 模块合集程序集信息
    /// </summary>
    public static class ModuleCollectionInfo
    {
        private static Version _mc_version = null;

        /// <summary>
        /// 保存模块合集版本
        /// </summary>
        public static void SetMCVersion(Assembly mc_assembly)
        {
            _mc_version = mc_assembly.GetName().Version;
        }
        /// <summary>
        /// 保存模块合集版本
        /// </summary>
        public static void SetMCVersion(Type mc_type)
        {
            _mc_version = Assembly.GetAssembly(mc_type).GetName().Version;
        }
        /// <summary>
        /// 获取模块合集版本
        /// </summary>
        /// <returns>模块合集版本</returns>
        public static Version GetMCVersion() => _mc_version;
    }
}
