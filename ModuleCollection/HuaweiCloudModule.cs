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
                if (await EcsStatus())
                    return "ECS已处于开机状态";
                (bool success, string output) = await EcsOn();
                if (success)
                    return "开机命令已成功下发";
                else
                    return $"开机命令下发失败。原因：{output}";
                return string.Empty;
            case "ecs":
            case "ecs-status":
                return string.Empty;
            case "ecs-off":
                return string.Empty;
            default:
                throw new Exception($"遭遇未知过滤器输出：{filterOut}");
        }
    }

    private async Task<(bool, string)> GetIamToken()
    {
        ModulePrint("Getting IAM token...");
        var baseRequest = _iamTokenApi.BuildBaseRequest(_uri);
        var content = _iamTokenApi.BuildRequestContent();
        var json = await content.ReadAsStringAsync();
        ModulePrint("Sending Request...");
        var response = await _caller.CallAPI(_iamTokenApi, false);
        //response.Content.Co
    }
    private async Task<bool> EcsStatus()
    {

    }
    private async Task<(bool, string)> EcsOn()
    {
        string output = string.Empty;
        return (true, output);
    }
}
