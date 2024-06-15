using klbotlib.Events;
using klbotlib.Exceptions;
using klbotlib.Extensions;
using klbotlib.Modules;
using Microsoft.Extensions.Configuration;
using System.Reflection;
using System.Text;
using Module = klbotlib.Modules.Module;

namespace klbotlib;

/// <summary>
/// KLBot类。机器人本体
/// </summary>
public class KLBot : IKLBotLogUnit
{
    private readonly IMessageDriver _msgDriver;

    /// <summary>
    /// KLBot的模块链条。这个类可以被枚举
    /// </summary>
    public ModuleChain ModuleChain { get; } = new ModuleChain();
    /// <summary>
    /// 当前模块总数，即模块链条的长度
    /// </summary>
    public int ModuleCount { get => ModuleChain.Count; }
    /// <summary>
    /// 此KLBot的统计和诊断信息
    /// </summary>
    public KLBotDiagnosticData DiagData { get; } = new KLBotDiagnosticData();
    /// <summary>
    /// 是否开启安静模式。开启时ObjectPrint()不打印任何内容
    /// </summary>
    public bool IsSilent { get; } = false;
    /// <summary>
    /// 配置项：此KLBot自身的QQ号
    /// </summary>
    public long SelfId { get; private set; }
    /// <summary>
    /// 配置项：此KLBot的监听群组QQ号列表
    /// </summary>
    public HashSet<long> TargetGroupIdList { get; private set; } = [];
    /// <summary>
    /// 配置项：模块私有目录。用来存取模块自己的自定义文件
    /// </summary>
    public string ModulesCacheDir { get; private set; } = string.Empty;
    /// <summary>
    /// 配置项：模块存档目录。KLBot保存或读取模块配置和模块状态的路径
    /// </summary>
    public string ModulesSaveDir { get; private set; } = string.Empty;
    /// <summary>
    /// 配置项：身份密钥。用于讨mirai服务器开心
    /// </summary>
    public string Key { get; set; } = string.Empty;
    /// <summary>
    /// 处理的时间窗。超出该时间窗的消息将被强制忽略
    /// </summary>
    public TimeSpan ProcessWindow { get; } = TimeSpan.FromSeconds(6);
    /// <summary>
    /// 管理员ID
    /// </summary>
    public List<long> AdminIds { get; private set; } = [];
    /// <summary>
    /// Log unit name
    /// </summary>
    public string LogUnitName => "Core/KLBot";

    // 最基本的私有构造函数
    private KLBot(IConfiguration config, IMessageDriver driver)
    {
        this.LogInfo("初始化KLBot...");
        _msgDriver = driver;
        var msgDriverType = _msgDriver.GetType();
        MessageDriverType = $"[{msgDriverType.Namespace}] {msgDriverType.Name}";
        _msgDriver.OnMessageReceived += MessageHandler;
        this.LogInfo($"Driver info: {_msgDriver.DriverInfo}");
        LoadConfig(config);
        //创建模块存档目录（如果不存在）
        CreateDirectory(ModulesSaveDir, "模块存档目录");
    }
    // 构造并添加目标群组
    private KLBot(IConfiguration config, IMessageDriver driver, ISet<long> targetGroups) : this(config, driver)
    {
        foreach (var group in targetGroups)
        {
            _ = TargetGroupIdList.Add(group);
        }
    }
    /// <summary>
    /// 构造函数。可用于模块开发调试
    /// </summary>
    /// <param name="config">使用的配置</param>
    /// <param name="driver">消息驱动器</param>
    /// <param name="targetGroups">监听的群组</param>
    /// <param name="isSilent">是否开启安静模式。开启时ObjectPrint()不打印任何内容</param>
    /// <param name="moduleCollection">模块合集程序集。此参数仅用于读取程序集版本</param>
    public KLBot(IConfiguration config, IMessageDriver driver, Assembly moduleCollection, ISet<long> targetGroups, bool isSilent = true) : this(config, driver, targetGroups)
    {
        IsSilent = isSilent;
        LoadCoreModule();
        if (moduleCollection != null)
            Info.ModuleCollectionInfo.SetMCVersion(moduleCollection);
        this.Log(GetListeningGroupListString());
    }
    /// <summary>
    /// 公开构造函数。基本构造后添加默认核心模块
    /// </summary>
    /// <param name="config">使用的配置</param>
    /// <param name="driver">KLBot使用的消息驱动器</param>
    /// <param name="loadCoreModule">是否加载核心模块</param>
    /// <param name="moduleCollection">模块合集程序集。此参数仅用于读取程序集版本</param>
    public KLBot(IConfiguration config, IMessageDriver driver, bool loadCoreModule = true, Assembly? moduleCollection = null) : this(config, driver)
    {
        if (loadCoreModule)
            LoadCoreModule();
        if (moduleCollection != null)
            Info.ModuleCollectionInfo.SetMCVersion(moduleCollection);
    }

    /// <summary>
    /// 加载配置
    /// </summary>
    /// <param name="config">待加载的配置</param>
    public void LoadConfig(IConfiguration config)
    {
        this.LogInfo("加载配置...");
        Key = config.ReadValue("key");
        TargetGroupIdList = config.ReadArray("targets").Select(long.Parse).ToHashSet();
        ModulesCacheDir = config.ReadValue("cache_dir");
        ModulesSaveDir = config.ReadValue("save_dir");
        AdminIds.Clear();
        AdminIds.AddRange(config.ReadArray("admins").Select(long.Parse));
    }
    /// <summary>
    /// 加载核心模块
    /// </summary>
    /// <exception cref="KLBotInitializationException"></exception>
    public void LoadCoreModule()
    {
        try
        {
            //加载核心模块
            this.LogInfo("加载自带核心模块...");
            AddModule(new CommandModule());
            this.Log(GetModuleChainString());
        }
        catch (Exception ex)
        {
            throw new KLBotInitializationException($"核心模块加载失败异常：{ex.Message}\n调用栈：\n{ex.StackTrace}");
        }
    }
    /// <summary>
    /// 把给定群号添加到监听列表
    /// </summary>
    /// <param name="targets">需要添加的群号</param>
    public void AddTarget(params long[] targets)
    {
        foreach (var target in targets)
            _ = TargetGroupIdList.Add(target);
    }
    /// <summary>
    /// 把一组群号批量添加到监听列表
    /// </summary>
    /// <param name="targets">需要添加的群号集合</param>
    public void AddTarget(IEnumerable<long> targets)
    {
        foreach (var target in targets)
            _ = TargetGroupIdList.Add(target);
    }

    /// <summary>
    /// 获取此KLBot的消息驱动器信息
    /// </summary>
    public string MessageDriverType { get; private set; }

    // 模块的增加/删除/查询
    /// <summary>
    /// 在控制台列出并打印模块链条
    /// </summary>
    public void ListModules() => this.LogInfo(GetModuleChainString());
    /// <summary>
    /// 根据模块类型获取模块
    /// </summary>
    /// <typeparam name="T">目标模块类型</typeparam>
    /// <param name="index">目标模块的索引</param>
    /// <returns>目标模块实例</returns>
    public T GetModule<T>(int index = 0) where T : Module => ModuleChain.GetModule<T>(index);
    /// <summary>
    /// 已知模块ID，获取模块
    /// </summary>
    /// <param name="moduleId">模块ID</param>
    public Module this[string moduleId] { get => ModuleChain[moduleId]; }
    /// <summary>
    /// 在当前模块链条的末尾手动添加一个或多个新模块
    /// </summary>
    public void AddModule(params Module[] modules)
    {
        foreach (var m in modules)
        {
            m.Register(this, ModuleChain.CalcModuleId(m));
            ModuleChain.AddModule(m);
            //为已经加载的每个模块创建缓存目录和存档目录（如果不存在）
            CreateDirectory(GetModuleCacheDir(m), $"模块{m}的缓存目录");
            //载入模块配置
            this.LogInfo($"已添加{m.ModuleName}，模块ID为\"{m}\"");
        }
    }
    /// <summary>
    /// 从模块存档文件中加载模块实例
    /// </summary>
    public async Task LoadModule(string moduleName, string moduleStatusFile)
    {
        string statusFilePath = Path.Combine(ModulesSaveDir, moduleStatusFile);
        object obj = await ModuleLoader.LoadModuleFromFileByName(moduleName, statusFilePath);
        Module m = (Module)obj;
        m.Register(this, ModuleChain.CalcModuleId(m));
        ModuleChain.AddModule(m);
        //为已经加载的每个模块创建缓存目录和存档目录（如果不存在）
        CreateDirectory(GetModuleCacheDir(m), $"模块{m}的缓存目录");
        this.LogInfo($"已添加{m.ModuleName}，模块ID为\"{m}\"");
    }

    //**** 内部API ****//
    //消息
    internal Task<Message> GetMessageFromId(long target, long messageId)
        => _msgDriver.GetMessageFromId(target, messageId);
    /// <summary>
    /// 发送消息
    /// </summary>
    /// <param name="module">模块实例</param>
    /// <param name="context">发送的消息上下文类型</param>
    /// <param name="userId"></param>
    /// <param name="groupId"></param>
    /// <param name="msg">待发送消息</param>
    internal async Task SendMessage(Module module, MessageContextType context, long userId, long groupId, Message msg)
        => await _msgDriver.SendMessage(module, context, userId, groupId, msg);
    /// <summary>
    /// 发送群消息
    /// </summary>
    /// <param name="module">模块实例</param>
    /// <param name="groupId">目标群组ID</param>
    /// <param name="msg">待发送消息</param>
    internal async Task SendGroupMessage(Module module, long groupId, Message msg)
        => await _msgDriver.SendMessage(module, MessageContextType.Group, -1, groupId, msg);
    /// <summary>
    /// 发送临时消息
    /// </summary>
    /// <param name="module">模块实例</param>
    /// <param name="userId">目标用户ID</param>
    /// <param name="groupId">通过的群组的ID</param>
    /// <param name="msg">待发送消息</param>
    internal async Task SendTempMessage(Module module, long userId, long groupId, Message msg)
        => await _msgDriver.SendMessage(module, MessageContextType.Group, userId, groupId, msg);
    /// <summary>
    /// 发送私聊消息
    /// </summary>
    /// <param name="module">模块实例</param>
    /// <param name="userId">目标用户ID</param>
    /// <param name="msg">待发送消息</param>
    internal async Task SendPrivateMessage(Module module, long userId, Message msg)
        => await _msgDriver.SendMessage(module, MessageContextType.Private, userId, -1, msg);
    /// <summary>
    /// 上传群文件
    /// </summary>
    /// <param name="module">模块</param>
    /// <param name="groupId">群聊ID</param>
    /// <param name="uploadPath">上传的目标路径</param>
    /// <param name="filePath">文件相对于模块私有目录的本地路径</param>
    internal async Task UploadFile(Module module, long groupId, string uploadPath, string filePath)
    {
        await _msgDriver.UploadFile(module, groupId, uploadPath, filePath);
    }
    /// <summary>
    /// 快速使用给定上下文回复消息
    /// </summary>
    /// <param name="module">调用模块</param>
    /// <param name="originContext">待回复的原始消息上下文</param>
    /// <param name="msg">回复内容</param>
    internal async Task ReplyMessage(Module module, MessageContext originContext, Message msg)
        => await _msgDriver.SendMessage(module, originContext.Type, originContext.UserId, originContext.GroupId, msg);
    //操作
    /// <summary>
    /// 禁言
    /// </summary>
    /// <param name="module">模块</param>
    /// <param name="userId">禁言用户ID</param>
    /// <param name="groupId">群聊ID</param>
    /// <param name="durationSeconds">禁言时长</param>
    internal async Task Mute(Module module, long userId, long groupId, uint durationSeconds)
    {
        await _msgDriver.Mute(module, userId, groupId, durationSeconds);
    }
    /// <summary>
    /// 取消禁言
    /// </summary>
    /// <param name="module">模块</param>
    /// <param name="userId">取消禁言用户ID</param>
    /// <param name="groupId">群聊ID</param>
    internal async Task Unmute(Module module, long userId, long groupId)
    {
        await _msgDriver.Unmute(module, userId, groupId);
    }

    //暴露给Module类的一些成员
    // 获取模块的私有文件夹路径。按照规范，模块存取自己的文件应使用这个目录
    internal string GetModuleCacheDir(Module module) => Path.Combine(ModulesCacheDir, module.ModuleId);
    // 获取模块的ModuleStatus存档文件路径
    internal string GetModuleStatusPath(Module module) => Path.Combine(ModulesSaveDir, module.ModuleId + ".json");

    //消息事件处理
    private async Task MessageHandler(object? sender, KLBotMessageEventArgs e)
    {
        // 时间窗强制过滤：早于一定时段的消息将彻底丢弃并且不进入统计信息
        if (DateTime.Now - e.Timestamp > ProcessWindow)
        {
            this.LogInfo($"[KLBotEvent/Message] Dropped: now - [{e.Timestamp:g}] = {(DateTime.Now - e.Timestamp).TotalSeconds}s > {ProcessWindow.TotalSeconds}s");
            goto processed;
        }
        this.LogInfo($"[KLBotEvent/Message] {e.Context} ; {e.Description.Replace('\n', ';')}");
        DiagData.ReceivedMessageCount++;
        var contextType = e.Context.Type;
        // 私聊/临时会话需过滤，范围为目标群组
        if ((contextType == MessageContextType.Group || contextType == MessageContextType.Temp) && !TargetGroupIdList.Contains(e.Context.GroupId))
            goto processed;
        Message msg = e.Message;
        //优先处理所有帮助消息，避免低优先级模块的帮助消息被高优先级模块阻挡
        //另外，帮助消息不计入统计信息
        if (msg is MessagePlain pmsg && pmsg.Text.Trim().EndsWith("帮助"))
        {
            foreach (var module in ModuleChain)
            {
                if (pmsg.Text.Trim() == module.FriendlyName + "帮助")
                {
                    await ReplyMessage(module, e.Context, new MessagePlain(module.HelpInfo));
                    goto processed;
                }
            }
        }
        //非帮助信息 遍历尝试处理
        foreach (var module in ModuleChain)
        {
            //模块会直接在一个单独的Task上依次处理并回复
            //防止因为处理或网络速度较慢阻塞其他消息的处理
            bool shouldProcess = await module.ProcessMessageAndReply(e.Context, msg);
            if (shouldProcess)
            {
                DiagData.ProcessedMessageCount++;
                DiagData.LastUsedModule = module;
                if (module.IsTransparent)
                    continue;
                break;
            }
        }
    processed:
        return;
    }

    /// <summary>
    /// 总循环。包括消息循环和命令循环
    /// </summary>
    public async Task DefaultLoop()
    {
        object sync = new();
        //开始之前向服务器验证身份
        this.LogTask("正在向服务器验证身份...");
        bool verifyResult = await _msgDriver.Verify(Key);
        if (!verifyResult)
        {
            this.LogError("验证失败。请检查密钥和网络是否正确\n正在退出...");
            Environment.Exit(-1);
        }
        else
            this.LogInfo("验证成功");
        //获取自身ID
        this.LogTask("正在向服务器获取自身ID...");
        try
        {
            SelfId = await _msgDriver.GetSelfId();
        }
        catch (Exception ex)
        {
            this.LogError($"获取自身ID失败：{ex.Message}. 请检查密钥和网络是否正确. 正在退出...\n{ex.StackTrace}");
            Environment.Exit(-1);
        }
        this.LogTask($"获取成功。自身ID：{SelfId}");
        bool exitFlag = false;
        //命令循环线程
        while (!exitFlag)
        {
            Console.Write("> ".ToAnsiColor(ConsoleColor.Yellow));
            string? cmd = Console.ReadLine();
            try
            {
                if (cmd is "" or null)
                    continue;  //不执行操作
                else if (cmd == "quit")
                    exitFlag = true;
                else if (cmd == "status")
                {
                    this.LogInfo(GetModuleStatusString());
                    this.LogInfo(DiagData.GetSummaryString());
                }
                else if (cmd.StartsWith("status "))
                {
                    string id = cmd[7..];
                    if (!ModuleChain.ContainsModule(id))
                        this.LogError($"找不到ID为\"{id}\"的模块");
                    else
                        this.LogInfo(ModuleChain[id].DiagData.GetSummaryString());
                }
                else if (cmd.StartsWith("enable "))
                {
                    string id = cmd[7..];
                    if (!ModuleChain.ContainsModule(id))
                        this.LogError($"找不到ID为\"{id}\"的模块");
                    else
                    {
                        ModuleChain[id].Enabled = true;
                        this.LogInfo($"成功启用{id}");
                    }
                }
                else if (cmd.StartsWith("disable "))
                {
                    string id = cmd[8..];
                    if (!ModuleChain.ContainsModule(id))
                        this.LogError($"找不到ID为\"{id}\"的模块");
                    else
                    {
                        ModuleChain[id].Enabled = false;
                        this.LogInfo($"成功禁用{id}");
                    }
                }
                else if (cmd == "save")
                {
                    this.LogInfo("手动保存所有模块状态到存档...");
                    ModuleChain.ForEach(async m =>
                    {
                        await m.SaveModuleStatus();
                    });
                }
                else if (cmd == "reload")
                {
                    this.LogTask("手动重载所有模块存档...");
                    await ReloadAllModules();
                    this.LogInfo("重载已完成");
                }
                else if (cmd == "lasterror")
                    this.LogInfo($"最近一次错误信息：\n{DiagData.LastException}");
                else
                    this.LogError($"未知命令：\"{cmd}\"");
            }
            catch (Exception ex)
            {
                this.LogError($"命令执行意外终止：{ex.Message}");
                this.DebugLog(ex.ToString());
            }
        }
        //从容退出
        OnExit();
    }

    /// <summary>
    /// 重新载入所有模块配置和状态
    /// </summary>
    public Task ReloadAllModules()
    {
        throw new NotImplementedException();
    }

    /// <summary>
    /// 有序退出函数
    /// </summary>
    public void OnExit()
    {
        ModuleChain.ForEach(async m => await m.SaveModuleStatus());
        this.LogInfo("有序退出完成");
    }

    //信息输出相关
    /// <summary>
    /// 返回字符串，其中列出当前模块链条
    /// </summary>
    public string GetModuleChainString()
    {
        StringBuilder sb = new();
        _ = sb.AppendLine("模块链条：");
        int index = 0;
        ModuleChain.ForEach(module =>
        {
            _ = module.ModuleName == module.FriendlyName
                ? sb.AppendLine($"  [{index}] {module}")
                : sb.AppendLine($"  [{index}] {module}\n      ({module.FriendlyName})");
            index++;
        });
        return sb.ToString();
    }
    /// <summary>
    /// 返回字符串，其中列出当前监听群组的列表
    /// </summary>
    public string GetListeningGroupListString()
    {
        StringBuilder sb = new();
        _ = sb.AppendLine("监听群组列表：");
        int index = 0;
        foreach (var target in TargetGroupIdList)
        {
            _ = sb.AppendLine($"  [{index}]  {target}");
            index++;
        }
        return sb.ToString();
    }
    /// <summary>
    /// 返回字符串，其中列出当前各模块标记了ModuleStatus的属性值。但是ModuleStatus属性中IsHidden=true的字段会被忽略。
    /// </summary>
    public string GetModuleStatusString()
    {
        StringBuilder sb = new();
        _ = sb.AppendLine("模块状态：");
        foreach (var module in ModuleChain)
        {
            _ = sb.AppendLine($"<{module.ModuleId}>");
            Type type = module.GetType();
            List<MemberInfo> members = [.. type.GetProperties_All().Reverse(), .. type.GetFields_All().Reverse()];
            foreach (var member in members)
            {
                if (member.IsNonHiddenModuleStatus())
                {
                    _ = member.TryGetValue(module, out object? value);  //忽略返回值。因为这个列表100%由PropertyInfo和FieldInfo组成
                    _ = sb.AppendLine($" {member.Name,-10} = {value}");
                }
            }
        }
        return sb.ToString();
    }

    private void CreateDirectory(string path, string dirDescription)
    {
        if (!Directory.Exists(path))
        {
            this.LogWarning($"{dirDescription}\"{path}\"不存在。正在自动创建...");
            _ = Directory.CreateDirectory(path);
        }
    }
}
