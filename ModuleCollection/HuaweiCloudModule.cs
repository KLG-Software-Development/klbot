using KLG.CloudApi.Huawei;
using KLG.CloudApi.Huawei.Apis;
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
    [ModuleStatus]
    private HuaweiToken? _iamToken;
    
    private readonly HuaweiCloudApiCaller _caller;
    private readonly GetIamTokenApi _iamTokenApi;

    /// <inheritdoc/>
    public HuaweiCloudModule()
    {
        _caller = new(_endpoint);
        _iamTokenApi = new(_user, _password, _domain);
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
        switch (filterOut)
        {
            case "ecs-on":
                //确定ECS不处于开机状态，否则无需开机
                var statusResult = await EcsStatus();
                if (!statusResult.Success)
                    return $"ECS状态查询失败。信息：{statusResult.Message}";
                if (statusResult.Value)
                    return "ECS已处于开机状态，无需开机";
                
                var onResult = await EcsOn();
                if (!onResult.Success)
                    return $"开机命令下发失败。原因：{onResult.Message}"; 
                return "开机命令已成功下发";
            case "ecs":
            case "ecs-status":
                
                return string.Empty;
            case "ecs-off":
                return string.Empty;
            default:
                throw new Exception($"遭遇未知过滤器输出：{filterOut}");
        }
    }

    /* Helper */
    private async Task<ApiResult<HuaweiToken>> GetIamToken()
    {
        ModulePrint("Getting IAM token...");
        var result = await _caller.CallAPI(_iamTokenApi, false);
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
        return new(true, "Token already up-to-date");
    }
    private async Task<ApiResult<bool>> EcsStatus()
    {
        var updateResult = await UpdateIamToken();
        if (!updateResult)
            return updateResult.ToGeneric<bool>();
        
        throw new NotImplementedException();
    }
    private async Task<ApiResult> EcsOn()
    {
        string message = string.Empty;
        throw new NotImplementedException();
    }
}
