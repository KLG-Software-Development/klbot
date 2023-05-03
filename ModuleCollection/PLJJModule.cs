using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using klbotlib.Modules.ModuleUtils;

namespace klbotlib.Modules;

/// 图像模块
public class PLJJModule : SingleTypeModule<MessagePlain>
{
    private readonly Random _ro = new();
    private readonly HttpHelper _httpHelper = new();
    
    [ModuleSetup]
    private int _maxRetryCount = 3;
    [ModuleStatus]
    private List<string>? _urlList;
    [ModuleStatus]
    private DateTime _lastActivateTime = DateTime.UnixEpoch;
    /// <inheritdoc/>
    public sealed override bool UseSignature => false;
    /// <inheritdoc/>
    public sealed override string FriendlyName => "漂亮姐姐模块";
    /// <inheritdoc/>
    public sealed override string HelpInfo => "发送“早安”触发每日漂亮姐姐图片";

    /// <inheritdoc/>
    public override string? Filter(MessagePlain msg)
    {
        if (msg.Text.Trim() == "早安" && DateTime.Now.Date != _lastActivateTime.Date)
            return "每日一图";
        else
            return null;
    }
    /// <inheritdoc/>
    public override async Task<string> Processor(MessagePlain msg, string? filterOut)
    {
        _lastActivateTime = DateTime.Now;
        await Messaging.ReplyMessage(msg, "早安！");
        (bool success, string url) = await GetRandomUrl("图片", msg, silent: true);
        if (success)
            return $@"\image:\url:{url}";
        else
            return $"已重试{_maxRetryCount}次。运气太差，放弃获取";
    }

    /// <summary>
    /// 随机返回一条图库内的URL
    /// </summary>
    /// <returns>(是否成功，图片URL)</returns>
    public async Task<(bool, string)> GetRandomUrl(string enhanceWord, MessageCommon originMsg, bool silent = false)
    {
        int trials = 0;
        //预检查URL可用性
        while (true)
        {
            int index = _ro.Next(_urlList.Count);
            string url = _urlList[index];
            if (trials > _maxRetryCount)
                return (false, string.Empty);
            var urlStatus = await VerifyUrl(url);
            switch (urlStatus)
            {
                case UrlStatus.Ok:
                    return (true, url);
                case UrlStatus.Timeout:
                    _urlList.RemoveAt(index);
                    if (!silent)
                        await Messaging.ReplyMessage(originMsg, $"[{trials}]发现缓慢{enhanceWord}，已踢出。将重新获取");
                    trials++;
                    continue;
                case UrlStatus.Error:
                    _urlList.RemoveAt(index);
                    if (!silent)
                        await Messaging.ReplyMessage(originMsg, $"[{trials}]发现无效{enhanceWord}，已踢出。将重新获取");
                    trials++;
                    continue;
                default:
                    throw new Exception($"[{trials}]未知URL status [{urlStatus}]");
            }
        }
    }
    
    private async Task<UrlStatus> VerifyUrl(string url)
    {
        try
        {
            var response = await _httpHelper.GetAsync(url);
            response.EnsureSuccessStatusCode();
            return UrlStatus.Ok;
        }
        catch (Exception ex)
        {
            if (ex is TimeoutException)
            {
                ModulePrint($"URL<{url}>超时，已踢出");
                return UrlStatus.Timeout;
            }
            ModulePrint($"URL<{url}>不可用：{ex.Message}");
            return UrlStatus.Error;
        }
    }
    private enum UrlStatus
    {
        Ok,
        Timeout,
        Error
    }
}
