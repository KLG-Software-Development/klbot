using KLG.CloudApi.Huawei;
using KLG.CloudApi.Huawei.Apis;
using System;
using System.Collections.Generic;
using System.Net;
using System.Net.Http;
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
    private readonly string _iamToken = string.Empty;
    
    private readonly RequestUriInfo _uri;
    private readonly HuaweiCloudApiCaller _caller;
    private readonly GetIamTokenApi _iamTokenApi;

    /// <inheritdoc/>
    public HuaweiCloudModule()
    {
        _uri = new(_endpoint);
        _caller = new(_uri);
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

    private async Task<ApiResult<string>> GetIamToken()
    {
        ModulePrint("Getting IAM token...");
        var baseRequest = _iamTokenApi.BuildBaseRequest(_uri);
        var content = _iamTokenApi.BuildRequestContent();
        var json = await content.ReadAsStringAsync();
        ModulePrint("Sending Request...");
        HttpResponseMessage response;
        string message = string.Empty;
        try
        {
            response = await _caller.CallAPI(_iamTokenApi, false);
            
        }
        catch (Exception ex)
        {
            message = ex.Message;
            goto failed;
        }
        if (response.StatusCode != HttpStatusCode.Created)
        {
            message = $"网络错误：HTTP返回码异常。{response.StatusCode}";
            goto failed;
        }
        string[] tokens = (string[])response.Headers.GetValues("X-Subject-Token");
        if (tokens.Length == 0)
        {
            message = "返回值";
            goto failed;
        }
    failed:
        return new(false, message, null);
    }
    
    //Helper
    private async Task<ApiResult<bool>> EcsStatus()
    {
        ApiResult<bool> result = new(true, string.Empty, true);
        throw new NotImplementedException();
    }
    private async Task<ApiResult> EcsOn()
    {
        string message = string.Empty;
        throw new NotImplementedException();
    }
}
