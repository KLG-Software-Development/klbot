using klbotlib.Json;
using klbotlib.MessageDriver.Mirai.JsonPrototypes;
using System;
using System.IO;
using System.Net.Http;
using System.Threading.Tasks;

namespace klbotlib.MessageDriver.Mirai;

internal static class MiraiNetworkHelper
{
    private static HttpClientHandler _handler = new HttpClientHandler() { UseProxy = false }; //忽略系统代理
    private static HttpClient _client = new(_handler);
    private static StringContent? _verifyRequestBody = null;

    //返回发送特定上下文消息的url
    internal static string GetSendMessageUrl(string serverUrl, MessageContext context)
    {
        string url;
        if (context == MessageContext.Group)
            url = $"{serverUrl}/sendGroupMessage";
        else if (context == MessageContext.Private)
            url = $"{serverUrl}/sendFriendMessage";
        else if (context == MessageContext.Temp)
            url = $"{serverUrl}/sendTempMessage";
        else
            throw new Exception($"暂不支持的消息上下文类型 \"{context}\"");
        return url;
    }
    internal static string GetUploadFileUrl(string serverUrl, MessageContext context)
        => serverUrl + "/file/upload";
    internal static string GetVerifyUrl(string serverUrl)
        => serverUrl + "/verify";
    internal static string GetMuteUrl(string serverUrl)
        => serverUrl + "/mute";
    internal static string GetUnmuteUrl(string serverUrl)
        => serverUrl + "/unmute";
    //返回获取消息的url
    internal static string GetFetchMessageUrl(string serverUrl)
        => $"{serverUrl}/fetchMessage";
    //返回从ID获取消息的url
    internal static string GetMessageFromIDUrl(string serverUrl)
        => $"{serverUrl}/messageFromId";
    internal static async Task<string> FetchMessageListJSON(string serverUrl)
    {
        return await _client.GetStringAsync(GetFetchMessageUrl(serverUrl));
    }
    
    //验证身份
    internal static async Task<string> Verify(string serverUrl, string key)
    {
        if (_verifyRequestBody == null)
            _verifyRequestBody = KLBotJsonHelper.CreateAsJson("{\"verifyKey\":\"" + key + "\"}");
        HttpResponseMessage response = await _client.PostAsync(GetVerifyUrl(serverUrl), _verifyRequestBody);
        response.EnsureSuccessStatusCode();
        return await response.Content.ReadAsStringAsync();
    }
    //禁言
    internal static async Task<string> Mute(string serverUrl, long userId, long groupId, uint durationSeconds)
    {
        StringContent content = KLBotJsonHelper.CreateAsJson($"{{\"target\":{groupId},\"memberId\":{userId},\"time\":{durationSeconds}}}");
        HttpResponseMessage response = await _client.PostAsync(GetMuteUrl(serverUrl), content);
        response.EnsureSuccessStatusCode();
        return await response.Content.ReadAsStringAsync();
    }
    //解除禁言
    internal static async Task<string> Unmute(string serverUrl, long userId, long groupId)
    {
        StringContent content = KLBotJsonHelper.CreateAsJson($"{{\"target\":{groupId},\"memberId\":{userId}}}");
        HttpResponseMessage response = await _client.PostAsync(GetUnmuteUrl(serverUrl), content);
        response.EnsureSuccessStatusCode();
        return await response.Content.ReadAsStringAsync();
    }
    //获取机器人列表
    internal static async Task<string> GetBotListJson(string serverUrl)
    {
        HttpResponseMessage response = await _client.GetAsync($"{serverUrl}/botList");
        response.EnsureSuccessStatusCode();
        return await response.Content.ReadAsStringAsync();
    }
    //尝试发送消息
    internal static async Task TrySendMessage(string serverUrl, MessageContext context, string fullMsgJson)
    {
        string url = GetSendMessageUrl(serverUrl, context);
        StringContent content = KLBotJsonHelper.CreateAsJson(fullMsgJson);
        HttpResponseMessage response = await _client.PostAsync(url, content);
        response.EnsureSuccessStatusCode();
        bool result = response.IsSuccessStatusCode;
        string responseStr = await response.Content.ReadAsStringAsync();
        if (!result)
            throw new Exception($"HTTP返回码非成功：{responseStr}");
        var miraiResponse = MiraiJsonHelper.Deserialize<JMiraiSendMessageResponse>(responseStr);
        if (miraiResponse.code != 0)
            throw new MiraiException(miraiResponse.code, miraiResponse.msg);
    }
    //尝试上传文件
    internal static async Task<Exception?> TryUploadFile(string serverUrl, MessageContext context, FileStream fs, MultipartFormDataContent fullContent)
    {
        string url = GetUploadFileUrl(serverUrl, context);
        try
        {
            var response = await _client.PostAsync(url, fullContent);
            bool result = response.IsSuccessStatusCode;
            string responseMsg = response.Content.ReadAsStringAsync().Result;
            if (!result)
                throw new Exception($"非成功返回码：{responseMsg}");
            var miraiResponse = MiraiJsonHelper.Deserialize<JMiraiSendMessageResponse>(responseMsg);
            if (miraiResponse.code != 0 || miraiResponse.msg == null)
                throw new MiraiException(miraiResponse.code, miraiResponse.msg);
            fs.Close();
            return null;
        }
        catch (Exception ex)    //错误会被记录在DiagData中
        {
            fs.Close();
            return ex;
        }
    }

    internal static async Task<string> GetMessageByIdJSON(string serverUrl, long target, long messageId)
    {
        return await _client.GetStringAsync(GetMessageFromIDUrl(serverUrl) + $"?messageId={messageId}&target={target}");
    }
}
