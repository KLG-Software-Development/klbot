using System;
using System.Net.Http;

namespace klbotlib.MessageServer.Mirai;

internal static class MiraiNetworkHelper
{
    private static HttpClient _client = new();
    private static StringContent _verifyRequestBody = null;

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

    //返回获取消息的url
    internal static string GetFetchMessageUrl(string serverUrl)
        => $"{serverUrl}/fetchMessage";
    //返回从ID获取消息的url
    internal static string GetMessageFromIDUrl(string serverUrl)
    => $"{serverUrl}/messageFromId";
    internal static string FetchMessageListJSON(string serverUrl)
    {
        return _client.GetStringAsync(GetFetchMessageUrl(serverUrl)).Result;
    }
    //验证身份
    internal static string Verify(string serverUrl, string key)
    {
        if (_verifyRequestBody == null)
            _verifyRequestBody = new StringContent("{\"verifyKey\":\"" + key + "\"}");
        HttpResponseMessage response = _client.PostAsync(GetVerifyUrl(serverUrl), _verifyRequestBody).Result;
        response.EnsureSuccessStatusCode();
        return response.Content.ReadAsStringAsync().Result;
    }
    internal static string GetMessageByIdJSON(string serverUrl, long id)
    {
        return _client.GetStringAsync(GetMessageFromIDUrl(serverUrl) + "?id=" + id).Result;
    }
}
