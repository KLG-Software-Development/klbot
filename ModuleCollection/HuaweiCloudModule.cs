using KLG.CloudApi.Huawei;
using KLG.CloudApi.Huawei.Apis;
using KLG.CloudApi.Huawei.Server;
using System;
using System.Threading.Tasks;

namespace klbotlib.Modules;

/// <summary>
/// 华为云模块
/// </summary>
public class HuaweiCloudModule : SingleTypeModule<MessagePlain>
{

    [ModuleSetup]
    private readonly string _endpoint = string.Empty;
    [ModuleSetup]
    private readonly string _user = string.Empty;
    [ModuleSetup]
    private readonly string _password = string.Empty;
    [ModuleSetup]
    private readonly string _domain = string.Empty;
    [ModuleSetup]
    private readonly string _projectId = string.Empty;
    [ModuleSetup]
    private readonly string _serverId = string.Empty;
    [ModuleStatus]
    private HuaweiToken? _iamToken = new("abc", "2000/04/14");
    
    private readonly HuaweiCloudApiCaller _caller;
    private readonly GetIamTokenApi _iamTokenApi;
    private readonly EcsStatusApi _ecsStatusApi;
    private readonly EcsSwitchApi _ecsSwitchApi;

    /// <inheritdoc/>
    public HuaweiCloudModule()
    {
        _caller = new(_endpoint);
        _iamTokenApi = new();
        _ecsStatusApi = new(_projectId, _serverId);
        _ecsSwitchApi = new(_projectId, _serverId);
    }

    /// <inheritdoc/>
    public override string FriendlyName => "华为云模块";

    /// <inheritdoc/>
    public override string? Filter(MessagePlain msg)
    {
        if (!msg.ContainsTargetID(HostBot.SelfID))
            return null;
        switch (msg.Text.ToLower())
        {
            case "ecs开机":
                return "ecs-on";
            case "ecs":
            case "ecs状态":
                return "ecs-status";
            case "ecs关机":
                return "ecs-off";
            default:
                return null;
        }
    }

    /// <inheritdoc/>
    public override async Task<string> Processor(MessagePlain msg, string? filterOut)
    {
        UpdateProperties();
        switch (filterOut)
        {
            case "ecs-on":
                //确定ECS不处于开机状态，否则无需开机
                var statusResult = await EcsStatus();
                if (!statusResult.Success)
                    return $"ECS状态查询失败。信息：{statusResult.Message}";
                if (statusResult.Value.Status == HuaweiServerStatus.ACTIVE)
                    return "ECS已处于开启状态，无需开机";
                var upResult = await EcsUp();
                if (!upResult.Success)
                    return $"开机命令下发失败。原因：{upResult.Message}"; 
                return "开机命令已成功下发";
            case "ecs":
            case "ecs-status":
                //更新状态
                statusResult = await EcsStatus();
                if (!statusResult.Success)
                    return $"ECS状态查询失败。信息：{statusResult.Message}";
                return statusResult.Value.ToString();
            case "ecs-off":
                statusResult = await EcsStatus();
                if (!statusResult.Success)
                    return $"ECS状态查询失败。信息：{statusResult.Message}";
                if (statusResult.Value.Status == HuaweiServerStatus.SHUTOFF)
                    return "ECS已处于关闭状态，无需关机";
                var downResult = await EcsUp();
                if (!downResult.Success)
                    return $"关机命令下发失败。原因：{downResult.Message}";
                return "关机命令已成功下发";

            default:
                throw new Exception($"遭遇未知过滤器输出：{filterOut}");
        }
    }

    /* Helper */
    private void UpdateProperties()
    {
        //更新属性
        _caller.Endpoint = _endpoint;
        _iamTokenApi.User = _user;
        _iamTokenApi.Password = _password;
        _iamTokenApi.Domain = _domain;
    }
    private async Task<ApiResult<HuaweiToken>> GetIamToken()
    {
        ModulePrint("Getting IAM token...");
        var result = await _caller.CallApi(_iamTokenApi, false);
        if (result.Success)
            _iamToken = result.Value;
        return result;
    }
    //更新IAM token。成功或不需要返回true，失败返回false
    private async Task<ApiResult> UpdateIamToken()
    {
        //更新IAM token
        if (_iamToken == null || _iamToken.IsExpired)
        {
            var getIamResult = await GetIamToken();
            return getIamResult.ToNonGeneric();
        }
        return new(true, "Token尚未过期");
    }
    private async Task<ApiResult<HuaweiServer>> EcsStatus()
    {
        var updateResult = await UpdateIamToken();
        if (!updateResult)
            return updateResult.ToGeneric<HuaweiServer>();
        if (_iamToken == null)
            return new(false, "更新IAM token失败：更新后IAM token意外为null", default);
        var result = await _caller.CallApi(_ecsStatusApi, true, _iamToken);
        return result;
    }
    private async Task<ApiResult> EcsUp()
    {
        return await _caller.CallApi(_ecsSwitchApi, false, _iamToken.TokenValue, true);
    }
    private async Task<ApiResult> EcsDown()
    {
        return await _caller.CallApi(_ecsSwitchApi, false, _iamToken.TokenValue, false);
    }
}
