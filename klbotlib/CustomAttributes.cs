namespace klbotlib;

/// <summary>
/// 模块状态Attribute，用来标记模块属性。
/// 模块状态用来标记那些决定模块状态的成员，比如某个功能是否开启。
/// </summary>
[AttributeUsage(AttributeTargets.Field | AttributeTargets.Property)]
public class HiddenStatusAttribute : Attribute { }
/// <summary>
/// 模块合集Attribute，用来标记模块合集。
/// 此Attribute用于标记Assembly为模块合集。使用模块名初始化KLBot时仅会在含此标记的程序集中搜索模块类型
/// </summary>
[AttributeUsage(AttributeTargets.Assembly | AttributeTargets.Module)]
public class ModuleCollectionAttribute : Attribute { }
