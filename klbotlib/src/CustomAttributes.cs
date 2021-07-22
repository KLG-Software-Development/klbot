using System;

namespace klbotlib.Modules
{
    /// <summary>
    /// 模块状态Attribute，用来标记模块属性。
    /// 模块状态用来标记那些决定模块状态的成员，比如某个功能是否开启。
    /// 只有打上此标记之后，该成员的值才会被Module.ExportStatusDict()保存，且机器人启动或关闭时会自动读取或保存这些成员的值
    /// </summary>
    public class ModuleStatusAttribute : Attribute { }
    /// <summary>
    /// 模块配置Attribute，用来标记模块配置。
    /// 此Attribute用来标记那些决定模块配置的成员，比如特定文件的存取路径。
    /// 只有打上此标记之后，该成员的值才能被Module.ExportSetupDict()保存
    /// </summary>
    public class ModuleSetupAttribute : Attribute { }      
}